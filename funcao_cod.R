
pairs.estimation.pci <- function(dados=NULL,
                                 formationp=NULL,
                                 tradep=NULL,tr=NULL,
                                 pares_sele_crit=NULL,
                                 stop=0.8,
                                 window_est="fixed",
                                 pci.method = "twostep",
                                 no_cores = NULL){
  
  criterios <- c('top_sharp_balanced',
                 "top_return_balanced",
                 "top_sharp",
                 "top_return",
                 "random")
  if(is.null(tr)){
    tr <- c(1,0.5)
  }
  if(is.null(dados)){
    stop('Faltando Dados')
  } else if(is.null(formationp)){
    stop('Faltando Janela de Formação')
  } else if(is.null(tradep)){
    stop('Faltando Janela de Trade')
  } else if(is.null(pares_sele_crit) || !pares_sele_crit %in% criterios){
    stop('Faltando Critério de Escolha de Par ou Critério Inválido')
  }
  
  ### Carregando os Pacotes
  Rcpp::sourceCpp("cpp_return.cpp")
  require(readxl)
  require(timetk)
  require(purrr)
  require(dplyr)
  require(doParallel)
  require(xts)
  require(stringr)
  require(partialCI)
  require(lubridate)
  require(gtools)

  ##### Estimando as combinações de pares
  resultados1 <- NULL
  resultados2 <- NULL
  resultados <- NULL
  pares_form <- NULL
  pares_trade <- NULL
  pares_estimados <- NULL
  
  Rcpp::sourceCpp("cpp_return.cpp") 
  sem_ini <- endpoints(Dados_2008_2018,"months",k=tradep)+1 ### Demarca os inicios de cada semestre

for(i in 42:length(sem_ini)){
  ############################# Datas Formação
  
  datas_form <- paste0(date(Dados_2008_2018)[sem_ini[i]],"/",
                       date(Dados_2008_2018)[sem_ini[i]]+months(formationp))
  form_end_index <- endpoints(Dados_2008_2018[datas_form], "months", k=formationp)[2]
  datas_form <- paste0(date(Dados_2008_2018)[sem_ini[i]],
                       "/",date(Dados_2008_2018[datas_form][form_end_index,]))
  
  ############################# Datas Trade
  
  datas_trading <- paste0(date(Dados_2008_2018)[sem_ini[i]],"/",
                          date(Dados_2008_2018)[sem_ini[i]]+months(formationp)+months(tradep))
  trade_end_index <- endpoints(Dados_2008_2018[datas_trading], "months", k=formationp+tradep)[2]
  datas_trading <- paste0(date(Dados_2008_2018)[sem_ini[i]],
                          "/",date(Dados_2008_2018[datas_trading][trade_end_index,]))
  
  #############################
  
  if(!is.na(trade_end_index)){
    dados_per_form <- Dados_2008_2018[datas_form]
    print(paste0("Periodo de Formação ",datas_form))
    
    if(is.null(no_cores)) {
      no_cores <- detectCores()
    } else if(no_cores > detectCores()){
      stop("Inexist this number of cores")
    }

    pares <- gtools::permutations(n=ncol(dados_per_form),
                                  2,colnames(dados_per_form))
    cl <- makeCluster(no_cores) 
    registerDoParallel(cl)
    pares_coint <- foreach(i=1:nrow(pares),
                           .errorhandling = "pass", 
                           .packages = "partialCI") %dopar%{
                             fit.pci(dados_per_form[,pares[i,1]],dados_per_form[,pares[i,2]], 
                                     pci_opt_method = pci.method)
                           }
    stopCluster(cl)
  } else{break}

  
  ###### Retirando Pares co rho e R2 maior que 0.5
  
  pares_coint <- pares_coint[!sapply(pares_coint,is.null)]
  pares_coint <- pares_coint[which(sapply(pares_coint, function(x) is.na(x$rho.se))==F)] ### Retirando Pares com valores vazios
  print("Retirando apenas os pares com rho e R² acima de 0.5")
  paresR <- pares_coint[sapply(pares_coint,function(x) x$pvmr > 0.5)]
  paresR <- paresR[sapply(paresR,function(x) x$rho > 0.5)]
  pares_nomes <- sapply(paresR,
                        function(x) paste0(x$target_name," ",x$factor_names)) 
  ###### Teste de Cointegração Parcial
  print("Teste de Cointegração Parcial")
  cl <- makeCluster(no_cores) 
  registerDoParallel(cl)
  pci_teste <- foreach(i=1:length(paresR),
                       .errorhandling = "pass",
                       .packages = "partialCI") %dopar%{
                         test.pci(paresR[[i]])
                       }
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
  #tr <- c(1,0.5) ## Threshold parta abertura e fechamento de posições
  
  ###### Estimando os trades do período de formação
  
  print("Calculando Retornos")
  for(j in 1:length(pares_formation)){
    invest <- c(1,rep(0,nrow(dados_per_form)-1))
    results <- returcalc_for(sinal = M_norm[[j]],
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
  portret$Sharp <- (portret$Retorno)/100/portret$Desvio
  portret$R2 <- sapply(pares_formation, function(x) x$pvmr)
  portret <- as_tibble(portret)
  
  resultados1[[paste0("Perido de Formação ",datas_form)]][["Sumario"]] <- portret
  resultados1[[paste0("Perido de Formação ",datas_form)]][["Trades"]] <- resultados_form
  pares_form[[paste0("Perido de Formação ",datas_form)]][["ParesF"]] <- pares_coint
  
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

  print(paste0("Periodo de Trading ",
               date(Dados_2008_2018)[sem_ini[i]]+months(formationp),"/",
               date(Dados_2008_2018[datas_trading][trade_end_index,])))
  
  dados_per_trading <- Dados_2008_2018[datas_trading]
  
  ###### Estimando Periodo de trading
  
  print("Estimando")
  if(window_est == "fixed"){
  pares_coint_trading <- list(NULL)
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  pares_coint_trading <- foreach(k=nrow(dados_per_form):nrow(dados_per_trading),
                                 .errorhandling = "pass",
                                 .packages = c("partialCI","stringr")) %dopar%{
                                   lapply(pares_trading_20$Pares, function(x) {
                                     fit.pci(dados_per_trading[1:k,str_trim(str_sub(x,end = -7))],
                                             dados_per_trading[1:k,str_trim(str_sub(x, start = -6))],
                                             pci_opt_method = pci.method)}
                                   )
                                 }
  } else if(window_est == "mov"){
    pares_coint_trading <- list(NULL)
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)
    pares_coint_trading <- foreach(k=nrow(dados_per_form):nrow(dados_per_trading),
                                   j=1:(nrow(dados_per_trading)-nrow(dados_per_form)+1),
                                   .errorhandling = "pass",
                                   .packages = c("partialCI","stringr")) %dopar%{
                                     lapply(pares_trading_20$Pares, function(x) {
                                       fit.pci(dados_per_trading[j:k,str_trim(str_sub(x,end = -7))],
                                               dados_per_trading[j:k,str_trim(str_sub(x, start = -6))],
                                               pci_opt_method = pci.method)}
                                     )
                                   }
  } else {
    stop("Faltando método de estimação")
  }
  stopCluster(cl)
  
  ###### Estimando Estados Ocultos do período de tradings e normalizando
  ###### O componente de media
  
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  M_norm_t <- foreach(k=1:nrow(pares_trading_20),
                      .errorhandling = "stop",
                      .packages = c("partialCI","stringr")) %dopar%{
                        lapply(pares_coint_trading, function(x){
                          tail((statehistory.pci(x[[k]])$M),1)  
                        })
                      }
  
  stopCluster(cl)
  M_norm_t <- lapply(M_norm_t, function(x) unlist(x))
  M_norm_t <- lapply(M_norm_t, function(x) x/sd(x))
  
  ###### Preparação para os trades
  
  pares_datas <- lapply(pares_coint_trading[[length(pares_coint_trading)]], 
                        function(x) cbind(tail(x$data,length(pares_coint_trading)),
                                          tail(x$basis,length(pares_coint_trading))))
  resultados_trading <- list(NULL)
  betas <- list(NULL)
  for (k in 1:nrow(pares_trading_20)) {
    b <- lapply(pares_coint_trading, function(x) x[[k]]$beta) ### Função para extrair
    betas[[k]] <- tibble(Beta = mean(unlist(b)),DP = sd(unlist(b)))              ### os betas médios       
  }
  #for (k in 1:nrow(pares_trading_20)) {
  # b <- lapply(pares_coint_trading, function(x) x[[k]]$beta) ### Função para extrair
  #  betas[[k]][,2] <- tibble(DP = sd(unlist(b)))              ### os desvios dos betas       
  #  }
  
  ###### Realizando os tradings
  print("Trading")
  for(j in 1:nrow(pares_trading_20)){
    invest <- c(1,rep(0,length(M_norm_t[[1]])-1))
    results <- returcalc_trad(sinal = M_norm_t[[j]],
                              par = pares_datas[[j]],
                              betas =  betas[[j]]$Beta,
                              tr = tr,
                              invest = invest,
                              lmt_perca = stop)
    resultados_trading[[j]] <- results
  }
  
  names(resultados_trading) <- pares_trading_20$Pares
  ###### Realizando Trades
  
  print("Cáculo dos Sharpes")
  portret_trading <- NULL
  portret_trading$Pares <- pares_trading_20$Pares
  portret_trading$Retorno <- sapply(resultados_trading, function(x) ((tail(x$invest,1)/1)-1)*100)
  portret_trading$Desvio <- sapply(resultados_trading, function(x) sd(x$invest))
  portret_trading$Sharp <- (portret_trading$Retorno)/100/portret_trading$Desvio
  portret_trading$Beta_voL <- sapply(betas, function(x) x$DP)
  portret_trading <- as_tibble(portret_trading)
  
  aux <- paste0("Periodo de Trading ",
                date(Dados_2008_2018)[sem_ini[i]]+months(formationp),"/",
                date(Dados_2008_2018[datas_trading][trade_end_index,])) 
  
  resultados2[[aux]][["Sumario"]] <- portret_trading
  resultados2[[aux]][["Trades"]] <- resultados_trading
  pares_trade[[aux]][["ParesT"]] <- pares_coint_trading
  
}

print("Salvando os resultados")
resultados[["Periodo de Formação"]] <- resultados1
resultados[["Periodo de Trading"]] <- resultados2
pares_estimados[["Periodo de Formação"]] <- pares_form
pares_estimados[["Periodo de Trading"]] <- pares_trade

saveRDS(resultados,paste0(getwd(),"/resultados/resultados_pci/resultados_pci_",
                          pares_sele_crit,"_",
                          formationp,"f_",
                          tradep,"t_",
                          window_est,".rds"))

saveRDS(pares_estimados,paste0(getwd(),"/resultados/resultados_pci/pares_pci_",
                               pares_sele_crit,"_",
                               formationp,"f_",
                               tradep,"t_",
                               window_est,".rds"))


return(resultados)
}

