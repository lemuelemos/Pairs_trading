
pairs.estimation.ci <- function(dados=NULL,
                                formationp=NULL,
                                tradep=NULL,
                                tr=NULL,
                                pares_sele_crit=NULL,
                                stop=0.8,
                                window_est="Fixed",
                                no_cores = 1){

    
### Mensagens de erro
  criterios <- c('top_sharp_balanced',"top_return_balanced","top_sharp","top_return","random")
  
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
  
  ### Pacotes  
  Rcpp::sourceCpp("cpp_return.cpp")
  require(readxl)
  require(purrr)
  require(dplyr)
  require(doParallel)
  require(xts)
  require(stringr)
  require(egcm)  
  require(timetk)
  require(lubridate)
  require(gtools)
  
##### Estimando as combinações de pares
## Ano de 360 dias. 4 anos 1460 dias. 6 meses 180 dias
  resultados1 <- NULL
  resultados2 <- NULL
  resultados <- NULL
  pares_form <- NULL
  pares_trade <- NULL
  pares_estimados <- NULL

sem_ini <- endpoints(Dados_2008_2018,"months",k=tradep)+1 ### Demarca os inicios de cada semestre

for(i in 1:length(sem_ini)){
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
    
    if(no_cores > detectCores()) {
      stop("Inexist this number of cores")
    } else if(is.null(no_cores)){
      no_cores <- detectCores()
    }
    
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
  nomes_adf <- tibble(nomes_adf)
  nomes_jo <- names(pares_coint_jo)
  nomes_jo <- tibble(nomes_jo)
  
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
  portret$Sharp <- (portret$Retorno)/100/portret$Desvio
  #portret$R2 <- sapply(pares_formation, function(x) x$pvmr)
  portret <- as_tibble(portret)
  
  resultados1[[paste0("Perido de Formação ",datas_form)]][["Sumario"]] <- portret
  resultados1[[paste0("Perido de Formação ",datas_form)]][["Trades"]] <- resultados_form
  pares_form[[paste0("Perido de Formação ",datas_form)]][["ParesF"]] <- pares_coint_ci1s
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
    pares_coint_trading <- foreach(l=nrow(dados_per_form):nrow(dados_per_trading),
                                   .errorhandling = "pass",
                                   .packages = c("egcm","stringr")) %dopar%{
                                     lapply(pares_trading_20$Pares, function(x) {
                                       egcm(dados_per_trading[1:l,str_trim(str_sub(x, start = -6))],
                                            dados_per_trading[1:l,str_trim(str_sub(x,end = -7))])}
                                     )
                                   }
  } else if(window_est == "mov") {
    pares_coint_trading <- list(NULL)
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)
    pares_coint_trading <- foreach(l=nrow(dados_per_form):nrow(dados_per_trading),
                                   j=1:(nrow(dados_per_trading)-nrow(dados_per_form)+1),
                                   .errorhandling = "pass",
                                   .packages = c("egcm","stringr")) %dopar%{
                                     lapply(pares_trading_20$Pares, function(x) {
                                       egcm(dados_per_trading[j:l,str_trim(str_sub(x, start = -6))],
                                            dados_per_trading[j:l,str_trim(str_sub(x,end = -7))])}
                                     )
                                   }
  } else {
    stop("Faltando método de estimação")
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
  portret_trading$Sharp <- (portret_trading$Retorno)/100/portret_trading$Desvio
  portret_trading$Beta_voL <- sapply(betas, function(x) x$DP)
  portret_trading <- as_tibble(portret_trading)
  
  aux <- paste0("Periodo de Trading ",
                date(Dados_2008_2018)[sem_ini[i]]+months(formationp),"/",
                date(Dados_2008_2018)[sem_ini[i]]+months(formationp)+months(tradep)-1) 
  resultados2[[aux]][["Sumario"]] <- portret_trading
  resultados2[[aux]][["Trades"]] <- resultados_trading
  pares_trade[[aux]][["ParesT"]] <- pares_coint_trading
  
}

resultados[["Periodo de Formação"]] <- resultados1
resultados[["Periodo de Trading"]] <- resultados2
pares_estimados[["Periodo de Formação"]] <- pares_form
pares_estimados[["Periodo de Trading"]] <- pares_trade

saveRDS(resultados,paste0(getwd(),"/resultados/resultados_ci/resultados_ci_",
                          pares_sele_crit,"_",
                          formationp,"f_",
                          tradep,"t_",
                          window_est,".rds"))

saveRDS(pares_estimados,paste0(getwd(),"/resultados/resultados_ci/pares_ci_",
                               pares_sele_crit,"_",
                               formationp,"f_",
                               tradep,"t_",
                               window_est,".rds"))

return(resultados)
}

