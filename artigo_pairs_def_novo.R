library(readxl)
library(timetk)
library(purrr)
library(dplyr)
library(doParallel)
library(xts)
library(stringr)
library(partialCI)
library(lubridate)

##### Organizando os Dados
Rcpp::sourceCpp("cpp_return.cpp") ### Código Auxiliar em C++ para estimar os retornos
Dados_2000_2019 <- read_excel("Dados_2000_2019.xlsx")
Dados_2000_2019$Dates <- as.Date(Dados_2000_2019$Dates)
Dados_2000_2019 %>%
  map_if(is.character,as.numeric) %>%
  tk_tbl(timetk_idx = T) %>%
  tk_xts-> Dados_2000_2019

Dados_2008_2018 <- Dados_2000_2019["2008/2018"]
rm(Dados_2000_2019)
Dados_2008_2018[,apply(Dados_2008_2018,2,
                      function(x) any(is.na(x)) == F), 
                drop = F] -> Dados_2008_2018

Nomes <- colnames(Dados_2008_2018) ## Taking the names of equity's
Nomes <- str_remove(str_sub(Nomes, 1,6),"\\.")
colnames(Dados_2008_2018) <- Nomes
rm(Nomes)

##### Estimando as combinações de pares
## Ano de 360 dias. 4 anos 1460 dias. 6 meses 180 dias

sem_ini <- endpoints(Dados_2008_2018,"months",k=6)+1 ### Demarca os inicios de cada semestre
for(i in sem_ini[13]){
  if(date(Dados_2008_2018)[i]+1642 <= date(Dados_2008_2018)[nrow(Dados_2008_2018)]){
  datas_form <- paste0(date(Dados_2008_2018)[i],"/",date(Dados_2008_2018)[i]+1460)
  dados_per_form <- Dados_2008_2018[datas_form]
  print(paste0("Periodo de Formação ",datas_form))
  
  no_cores <- detectCores()
  pares <- gtools::permutations(n=ncol(dados_per_form),
                                2,colnames(dados_per_form))
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  pares_coint <- foreach(i=1:nrow(pares),
                         .errorhandling = "pass", 
                         .packages = "partialCI") %dopar%{
                           fit.pci(dados_per_form[,pares[i,1]],dados_per_form[,pares[i,2]], 
                                   pci_opt_method = "twostep")
                         }
  stopCluster(cl)
} else{break}
  
  ###### Retirando Pares co rho e R2 maior que 0.5
  
  pares_coint <- pares_coint[!sapply(pares_coint, function(x) is.na(x$rho.se))] ### Retirando Pares com valores vazios
  print("Retirando apenas os pares com rho e R² acima de 0.5")
  paresR <- pares_coint[sapply(pares_coint,function(x) x$pvmr > 0.5)]
  paresR <- paresR[sapply(paresR,function(x) x$rho > 0.5)]
  pares_nomes <- sapply(paresR,
                        function(x) paste0(x$target_name," ",x$factor_names)) 
  
  ###### Teste de Cointegração Parcial
  
  print("Teste de Cointegração Parcial")
  cl <- makeCluster(no_cores)
  clusterExport(cl, "paresR")
  clusterEvalQ(cl, library(partialCI))
  pci_teste <- parLapply(cl,paresR, function(x) test.pci(x))
  stopCluster(cl)
  pci_teste <- pci_teste[!sapply(pci_teste,function(x) x$p.value[1] > 0.05)]
  pci_teste <- pci_teste[!sapply(pci_teste,function(x) x$p.value[2] > 0.05)]
  pares_parcial_coint <- lapply(pci_teste, function(x) x$data.name)
  pares_parcial_coint <- str_replace(pares_parcial_coint,"/","")
  pares_parcial_coint <- str_replace(pares_parcial_coint," ","")
  pares_formation <- paresR[which(pares_nomes %in% pares_parcial_coint)]
  
  ###### Estimação dos estados Ocultos
  
  print("Estimando Estados Ocultos")
  M_norm <- lapply(pares_formation, function(x) statehistory.pci(x)$M) 
  M_norm <- lapply(M_norm, function(x) x/sd(x)) ## Normaliozando os erros
  names(M_norm) <- pares_parcial_coint
  
  
  ###### Preparação para o período de formação dentro da amostra
  
  betas <- as.vector(sapply(pares_formation, function(x) x$beta)) ## estraindo os betas
  pares_datas <- lapply(pares_formation, function(x) cbind(x$data,x$basis)) ## Colocando os preços em pares
  resultados_form <- list(NULL) ### Resultados do Período de Formação
  tr <- c(1,0.5) ## Threshold parta abertura e fechamento de posições
  
  ###### Estimando os trades do período de formação
  
  print("Calculando Retornos")
  for(j in 1:length(pares_formation)){
    invest <- c(1,rep(0,nrow(dados_per_form)-1))
    results <- returcalc(sinal = M_norm[[j]],
                                 par = pares_datas[[j]],
                                 betas =  betas[j],
                                 tr = tr,
                                 invest = invest)
    resultados_form[[j]] <- results
  }
  names(resultados_form) <- pares_parcial_coint
  
  
  ###### Cáculo dos Sharpes e retornos ######
  
  print("Cáculo dos Sharpes")
  portret <- NULL
  portret$Pares <- pares_parcial_coint
  portret$Retorno <- sapply(resultados_form, function(x) ((tail(x$invest,1)/1)-1)*100)
  portret$Desvio <- sapply(resultados_form, function(x) sd(x$invest))
  portret$Sharp <- portret$Retorno/portret$Desvio
  portret <- as_tibble(portret)
  
  ###### Selecionando os pares com melhor sharpe a ######
  ###### partir de cada ativo na ponta dependente  ######
  
  
  lapply(unique(str_sub(portret$Pares, end = -7)), function(x){
    portret %>%
      filter(str_sub(Pares, end = -7) == x) %>%
      filter(Sharp == max(Sharp)) 
  }) %>% 
    bind_rows() %>%
    arrange(desc(Sharp)) -> pares_trading  
  
  ###### Selecionando os 20 melhores pares ######
  
  pares_trading_20 <- pares_trading[1:20,]
  
  ###### Formatando dados para período de trading
  
  datas_trading <- paste0(date(Dados_2008_2018)[i],"/",date(Dados_2008_2018)[i]+1642)
  print(paste0("Periodo de Trading ",date(Dados_2008_2018)[i]+1461,"/",
               date(Dados_2008_2018)[i]+1642))
  dados_per_trading <- Dados_2008_2018[datas_trading]
  
  ###### Estimando Periodo de trading
  
  print("Trading")
  pares_coint_trading <- list(NULL)
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  pares_coint_trading <- foreach(k=nrow(dados_per_form):nrow(dados_per_trading),
                                 .errorhandling = "pass",
                                 .packages = c("partialCI","stringr")) %dopar%{
                                   lapply(pares_trading_20$Pares, function(x) {
                                     fit.pci(dados_per_trading[1:k,str_trim(str_sub(x,end = -7))],
                                             dados_per_trading[1:k,str_trim(str_sub(x, start = -6))],
                                             pci_opt_method = "twostep")}
                           )
                         }
  stopCluster(cl)
  
  ###### Estimando Estados Ocultos do período de tradings e normalizando
  ###### O componente de media
  
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  M_norm_t <- foreach(k=1:nrow(pares_trading_20),
                      .errorhandling = "pass",
                      .packages = c("partialCI","stringr")) %dopar%{
                        lapply(pares_coint_trading[-1], function(x){
                          tail((statehistory.pci(x[[k]])$M/sd(statehistory.pci(x[[k]])$M))
                               ,1)  
                        })
                        
                      }
  
  stopCluster(cl)
  M_norm_t <- lapply(M_norm_t, function(x) unlist(x))
  
  ###### Preparação para os trades
  
  pares_datas <- lapply(pares_coint_trading[[131]], 
                        function(x) cbind(tail(x$data,130),tail(x$basis,130)))
  resultados_trading <- list(NULL)
  betas <- list(NULL)
  for (k in 1:nrow(pares_trading_20)) {
    b <- lapply(pares_coint_trading, function(x) x[[k]]$beta) ### Função para extrair
    betas[[k]] <- mean(unlist(b))                             ### os betas médios       
  }
  
  ###### Realizando os tradings
  
  for(j in 1:nrow(pares_trading_20)){
    invest <- c(1,rep(0,length(M_norm_t[[1]])-1))
    results <- returcalc(sinal = M_norm_t[[j]],
                         par = pares_datas[[j]],
                         betas =  betas[[j]],
                         tr = tr,
                         invest = invest)
    resultados_trading[[j]] <- results
  }
   
  ###### Realizando Trades
  
    print("Cáculo dos Sharpes")
    portret_trading <- NULL
    portret_trading$Pares <- pares_trading_20$Pares
    portret_trading$Retorno <- sapply(resultados_trading, function(x) ((tail(x$invest,1)/1)-1)*100)
    portret_trading$Desvio <- sapply(resultados_trading, function(x) sd(x$invest))
    portret_trading$Sharp <- portret_trading$Retorno/portret_trading$Desvio
    portret_trading <- as_tibble(portret_trading)

}






