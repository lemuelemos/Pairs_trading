library(readxl)
library(purrr)
library(dplyr)
library(doParallel)
library(xts)
library(stringr)
library(egcm)
library(timetk)
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

#formationp <- 48
#tradep <- 6
#pares_sele_crit <- "random"

##### Estimando as combinações de pares
## Ano de 360 dias. 4 anos 1460 dias. 6 meses 180 dias
resultados1 <- NULL
resultados2 <- NULL
resultados <- NULL

sem_ini <- endpoints(Dados_2008_2018,"months",k=tradep)+1 ### Demarca os inicios de cada semestre
sem_fim <- endpoints(Dados_2008_2018,"months",k=tradep)
for(i in 1:length(sem_ini)){
  if((date(Dados_2008_2018)[sem_ini[i]]+months(formationp)+months(tradep)-1)<=date(Dados_2008_2018)[nrow(Dados_2008_2018)]){
    datas_form <- paste0(date(Dados_2008_2018)[sem_ini[i]],"/",
                         date(Dados_2008_2018)[sem_ini[i]]+months(formationp)-1)
    dados_per_form <- Dados_2008_2018[datas_form]
    print(paste0("Periodo de Formação ",datas_form))
    
    no_cores <- detectCores() 
    pares <- gtools::permutations(n=ncol(dados_per_form),
                                  2,colnames(dados_per_form))
    cl <- makeCluster(no_cores) 
    registerDoParallel(cl)
    pares_adf <- foreach(j=1:nrow(pares),
                           .errorhandling = "pass", 
                           .packages = "egcm") %dopar%{
                             egcm(dados_per_form[,pares[j,2]],dados_per_form[,pares[j,1]],
                                  urtest = "pp",
                                  p.value = 0.05)
                           }
    
    pares_jo <- foreach(j=1:nrow(pares),
                           .errorhandling = "pass", 
                           .packages = "egcm") %dopar%{
                             egcm(dados_per_form[,pares[j,2]],dados_per_form[,pares[j,1]],
                                  urtest = "pgff",
                                  p.value = 0.05)
                           }
    stopCluster(cl)
  } else{break}
  
  names(pares_adf) <- paste0(pares[,1]," ",pares[,2])
  names(pares_jo) <- paste0(pares[,1]," ",pares[,2])
  pares_jo <- pares_jo[!sapply(pares_jo,is.null)]
  pares_adf <- pares_adf[!sapply(pares_adf,is.null)]
  
  ############# Testing for cointegration #####################
  print("Pairs Testing")   
  
  pares_coint_adf <- pares_adf[sapply(pares_adf,is.cointegrated)]
  pares_coint_adf <- pares_coint_adf[sapply(pares_coint_adf,is.ar1)]
  pares_coint_jo <- pares_jo[sapply(pares_jo,is.cointegrated)]
  pares_coint_jo <- pares_coint_jo[sapply(pares_coint_jo,is.ar1)]
  
  rm(pares_jo)
  rm(pares_adf)
  
  ############################################
  ##### Comparing the cointegrationg on the 2 diferent's test's
  nomes_adf <- names(pares_coint_adf)
  nomes_adf <- data_frame(nomes_adf)
  nomes_jo <- names(pares_coint_jo)
  nomes_jo <- data_frame(nomes_jo)
  
  pares_coint_ci1 <- nomes_adf %>% dplyr::filter(nomes_adf %in% nomes_jo$nomes_jo)
  pares_coint_ci1s <- pares_coint_adf[pares_coint_ci1$nomes_adf]
  ###### Normalizando os resíduos
  M_norm <- lapply(pares_coint_ci1s, function(x) x$residuals/x$residuals.sd)
  
  ###### Preparação para o período de formação dentro da amostra
  
  betas <- sapply(pares_coint_ci1s, function(x) x$beta)
  pares_datas <- lapply(pares_coint_ci1s, function(x) cbind(x$S2,x$S1))
  resultados_form <- list(NULL) ### Resultados do Período de Formação
  tr <- c(1,0.5) ## Threshold parta abertura e fechamento de posições
  
  print("Calculando Retornos")
  for(k in 1:length(pares_coint_ci1s)){
    invest <- c(1,rep(0,nrow(dados_per_form)-1))
    results <- returcalc_for(sinal = M_norm[[k]],
                         par = pares_datas[[k]],
                         betas =  betas[k],
                         tr = tr,
                         invest = invest)
    resultados_form[[k]] <- results
  }
  names(resultados_form) <- names(pares_coint_ci1s)
  
  ###### Cáculo dos Sharpes e retornos ######
  
  print("Cáculo dos Sharpes")
  portret <- NULL
  portret$Pares <- names(pares_coint_ci1s)
  portret$Retorno <- sapply(resultados_form, function(x) ((tail(x$invest,1)/1)-1)*100)
  portret$Desvio <- sapply(resultados_form, function(x) sd(x$invest))
  portret$Sharp <- portret$Retorno/portret$Desvio
  #portret$R2 <- sapply(pares_formation, function(x) x$pvmr)
  portret <- as_tibble(portret)
  
  resultados1[[paste0("Perido de Formação ",datas_form)]][["Sumario"]] <- portret
  resultados1[[paste0("Perido de Formação ",datas_form)]][["Trades"]] <- resultados_form
  resultados1[[paste0("Perido de Formação ",datas_form)]][["ParesF"]] <- pares_coint_ci1s
  #######################################################
  ###### Selecionando os pares com melhor sharpe a ######
  ###### partir de cada ativo na ponta dependente  ######
  #######################################################
  ###### Selecionando os 20 melhores pares ##############
  
  if(pares_sele_crit == "top_sharp_balanced"){
    lapply(unique(str_sub(portret$Pares, end = -7)), function(x){
      portret %>%
        filter(str_sub(Pares, end = -7) == x) %>%
        filter(Sharp == max(Sharp)) 
    }) %>% 
      bind_rows() %>%
      arrange(desc(Sharp)) -> pares_trading
    pares_trading_20 <- pares_trading[1:20,]
    pares_trading_20 <- na.omit(pares_trading_20)
  } else if(pares_sele_crit == "top_return_balanced"){
    lapply(unique(str_sub(portret$Pares, end = -7)), function(x){
      portret %>%
        filter(str_sub(Pares, end = -7) == x) %>%
        filter(Retorno == max(Retorno)) 
    }) %>% 
      bind_rows() %>%
      arrange(desc(Retorno)) -> pares_trading
    pares_trading_20 <- pares_trading[1:20,]
    pares_trading_20 <- na.omit(pares_trading_20)
  }else if(pares_sele_crit == "top_sharp"){
    pares_trading <- arrange(portret, desc(Sharp))
    pares_trading_20 <- pares_trading[1:20,]
    pares_trading_20 <- na.omit(pares_trading_20)
  } else if(pares_sele_crit == "top_return") {
    pares_trading <- arrange(portret, desc(Retorno))
    pares_trading_20 <- pares_trading[1:20,]
    pares_trading_20 <- na.omit(pares_trading_20)
  } else if(pares_sele_crit == "random" & nrow(portret) > 20){
    pares_trading_20 <- portret[sample(1:nrow(portret),20,replace = F),]
    pares_trading_20 <- na.omit(pares_trading_20)
  } else if(pares_sele_crit == "random" & nrow(portret) < 20){
    pares_trading_20 <- portret
    pares_trading_20 <- na.omit(pares_trading_20)
  }
  
  ###### Formatando dados para período de trading
  
  datas_trading <- paste0(date(Dados_2008_2018)[sem_ini[i]],"/",
                          date(Dados_2008_2018)[sem_ini[i]]+months(formationp)+months(tradep)-1)

  print(paste0("Periodo de Trading ",
               date(Dados_2008_2018)[sem_ini[i]]+months(formationp),"/",
               date(Dados_2008_2018)[sem_ini[i]]+months(formationp)+months(tradep)-1))

  dados_per_trading <- Dados_2008_2018[datas_trading]
  
  print(paste0("Periodo de Trading ",(date(dados_per_form)[nrow(dados_per_form)]+1),"/",
               date(Dados_2008_2018)[sem_ini[i]]+months(formationp)+months(tradep)-1))
  ###### Estimando Periodo de trading
  
  print("Estimando")
  pares_coint_trading <- list(NULL)
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  pares_coint_trading <- foreach(l=nrow(dados_per_form):nrow(dados_per_trading),
                                 .errorhandling = "pass",
                                 .packages = c("egcm","stringr")) %dopar%{
                                   lapply(pares_trading_20$Pares, function(x) {
                                     egcm(dados_per_trading[1:l,str_trim(str_sub(x, start = -6))],
                                          dados_per_trading[1:l,str_trim(str_sub(x,end = -7))])}
                                   )
                                 }
  
  ###### Estimando Estados Ocultos do período de tradings e normalizando
  ###### O componente de media

  M_norm_t <- foreach(m=1:nrow(pares_trading_20),
                      .errorhandling = "pass",
                      .packages = c("egcm","stringr")) %dopar%{
                        lapply(pares_coint_trading, function(x){
                          tail(x[[m]]$residuals,1)  
                        })
                      }
  
  stopCluster(cl)
  M_norm_t <- lapply(M_norm_t, function(x) unlist(x))
  M_norm_t <- lapply(M_norm_t, function(x) x/sd(x))
  ###### Preparação para os trades
  
  pares_datas <- lapply(pares_coint_trading[[length(pares_coint_trading)]], 
                        function(x) cbind(tail(x$S2,length(pares_coint_trading)),
                                          tail(x$S1,length(pares_coint_trading))))
  
  resultados_trading <- list(NULL)
  betas <- list(NULL)
  for (n in 1:nrow(pares_trading_20)) {
    b <- lapply(pares_coint_trading, function(x) x[[n]]$beta) ### Função para extrair
    betas[[n]] <- tibble(Beta = mean(unlist(b)),DP = sd(unlist(b))) ### os betas médios 
        
  }
  
  ###### Realizando os tradings
  print("Trading")
  for(k in 1:nrow(pares_trading_20)){
    invest <- c(1,rep(0,length(M_norm_t[[1]])-1))
    results <- returcalc_trad(sinal = M_norm_t[[k]],
                         par = pares_datas[[k]],
                         betas =  betas[[k]]$Beta,
                         tr = tr,
                         invest = invest,
                         lmt_perca = 0.8)
    resultados_trading[[k]] <- results
  }
  names(resultados_trading) <- pares_trading_20$Pares
  
  ###### Realizando Trades
  
  print("Cáculo dos Sharpes")
  portret_trading <- NULL
  portret_trading$Pares <- pares_trading_20$Pares
  portret_trading$Retorno <- sapply(resultados_trading, function(x) ((tail(x$invest,1)/1)-1)*100)
  portret_trading$Desvio <- sapply(resultados_trading, function(x) sd(x$invest))
  portret_trading$Sharp <- portret_trading$Retorno/portret_trading$Desvio
  portret_trading$Beta_voL <- sapply(betas, function(x) x$DP)
  portret_trading <- as_tibble(portret_trading)
  
  aux <- paste0("Periodo de Trading ",
                date(Dados_2008_2018)[nrow(dados_per_form)+1],"/",
                date(Dados_2008_2018)[sem_ini[i]]+months(formationp)+months(tradep)-1) 
  resultados2[[aux]][["Sumario"]] <- portret_trading
  resultados2[[aux]][["Trades"]] <- resultados_trading
  resultados2[[aux]][["ParesT"]] <- pares_coint_trading

}

resultados[["Periodo de Formação"]] <- resultados1
resultados[["Periodo de Trading"]] <- resultados2
saveRDS(resultados,paste0(getwd(),"/resultados/resultados_ci_",
                          pares_sele_crit,"_",formationp,"f_",tradep,"t",".rds"))

  
  
  