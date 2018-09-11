for(ii in c(1,3)){
  if(ii == 1){
    cat("\n","Trading Period top 20 return")
  } else {cat("\n","Trading Period top 20 sharp")}
  portsel <- row.names(ret_port[[p]][order(-ret_port[[p]][,ii]),])[1:20] ## Seleect the top 20 sharp's
  portsel <- as.character(na.omit(portsel))
  select_port[[p]] <- portsel # testing if the window is complete
  trading_period <- window(ibrx_2008_2017_70, # Select the data
                           start = time(test_period)[1],
                           end = time(test_period)[nrow(test_period)]+trading_days[1])
  trading_window <- nrow(trading_period) - nrow(test_period)
  betas_trading <- betas_formation %>% 
    select(Pares, betas) %>% 
    dplyr::filter(Pares %in% portsel)
  Zm_trading <- Zm_fornation %>% select(portsel)
  ############# Estimating the pairs ################
  cat("\n","Estimating the pairs")
  parestrade <-as.list(NULL)
  for(j in 1:length(portsel)){
    parestrade[[j]] <- cbind(trading_period[,grep(str_sub(portsel[j],end=6),names(trading_period))],
                             trading_period[,grep(str_sub(portsel[j],start=-6),names(trading_period))])
    names(parestrade)[j] <- portsel[j]
  }
  
  ### Estimating pairs
  cl <- makeCluster(no_cores)
  for(i in 1:trading_window){
    cat("\r", i, "of", trading_window,"\r")
    parestrade_est <- lapply(parestrade, function(x) x[1:(nrow(test_period)+i),])
    clusterExport(cl, "parestrade_est")
    clusterEvalQ(cl, library(partialCI))
    pares <- parLapply(cl,parestrade_est, function(x) fit.pci(x[,1],x[,2]))
    
    pares <- pares[!sapply(pares,is.null)]
    pares <- pares[!sapply(pares, function(x) is.na(x$rho.se))]
    
    #### Estimating ocult states
    #print(paste0("Estimation of ocult states. Portfolio ",p))
    paresRtested <- list(NULL)
    paresRtested <- pares
    paresRtestedM <- lapply(paresRtested, function(x) statehistory.pci(x))
    #rm(paresRtested)
    
    ### Norm M's
    #print(paste0("Normalizing the M. Portfolio",p))
    Z_norm <- lapply(paresRtestedM, function(x) x$M/sd(x$M))
    Z_norm <- as.data.frame(Z_norm)
    colnames(Z_norm) <- gsub("\\."," ",names(Z_norm))
    Zm_trading[nrow(test_period)+i,] <- Z_norm[nrow(Z_norm),]
  }
  stopCluster(cl)
  ### sign of operations
  sinal <- matrix(data = rep(0,ncol(Zm_trading)*nrow(Zm_trading)),ncol = ncol(Zm_trading),nrow = nrow(Zm_trading))
  sinal[1,1:ncol(sinal)] <- "Fora"
  cat("\n",paste0("Sign for operations - threshold[",tr[1],",",tr[2],"]. Portolio ",p))
  sinal <- sncalc(ncol(Zm_trading),nrow(Zm_trading),as.matrix(Zm_trading), tr=tr, sinal=sinal)
  sinal <- as.data.frame(sinal) 
  colnames(sinal) <- names(Zm_trading)
  sinal %>% mutate_if(is.factor,as.character) -> sinal
  
  ############# Return Calc
  invest_t <- data.frame(matrix(data = rep(1,ncol(Zm_trading)*nrow(Zm_trading)),ncol = ncol(Zm_trading),nrow = nrow(Zm_trading)))
  retorno_t <- data.frame(matrix(data = rep(0,ncol(Zm_trading)*nrow(Zm_trading)),ncol = ncol(Zm_trading),nrow = nrow(Zm_trading)))
  tt2 <- data.frame(matrix(data = rep(0,ncol(Zm_trading)*nrow(Zm_trading)),ncol = ncol(Zm_trading),nrow = nrow(Zm_trading)))
  results <- NULL
  par_est <- data.frame(NULL)
  for(j in 1:length(pares)){
    par_est <- parestrade[[j]]
    results <- returcalc(as.matrix(sinal[,j]),
                         as.matrix(par_est),betas = betas_trading$betas[j],invest = invest_t[,j])
    invest_t[,j] <- results$invest
    retorno_t[,j] <- results$retorno
    tt2[,j] <- results$tt
  }
  tt2[1,1:ncol(tt2)] <- "Fora"
  colnames(invest_t) <- names(pares)
  colnames(retorno_t) <- names(pares)
  colnames(tt2) <- names(pares)
  
  
  ################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
  cat("\n",paste0("Calculating return and sharpe. Portfolio ",p))
  portret <- as.data.frame(matrix(data = rep(0,ncol(Zm_trading)*3),ncol = ncol(Zm_trading),nrow = 3))
  for(f in 1:length(invest_t)){
    for(i in (formation_windown[pp]+2):nrow(tt2)){
      if(tt2[i,f] == "Abriu"){
        portret[1,f] <- (tail(cumprod(retorno_t[(formation_windown[pp]+2):nrow(tt2),f]+1),1)-1)*100
        portret[2,f] <- sd(invest_t[i:nrow(invest_t),f], na.rm = T)
        portret[3,f] <- portret[1,f]/portret[2,f]
        colnames(portret)[f] <- names(parestrade)[f]
        break
      } else{
        portret[1,f] <- 0
        portret[2,f] <- 0
        portret[3,f] <- 0
        colnames(portret)[f] <- names(paresRtestedM)[f]
        next
      }
    }
  }
  portret <- t(portret)
  colnames(portret) <- c("Retorno Total","Desvio Padrão","Sharpe")
  if(ii == 1){
    ret_aux[[1]] <- portret ## Retornos Totais
    trades[[1]] <- retorno_t
    names(ret_aux)[1] <- paste0("Return Trading Period ",p, ". The top 20 Return")
    names(trades)[1] <- paste0("Return Trading Period ",p, ". The top 20 Return")
  } else{
    ret_aux[[2]] <- portret ## Retornos Totais
    trades[[2]] <- retorno_t
    names(ret_aux)[2] <- paste0("Return Trading Period ",p, ". The top 20 Sharp")
    names(trades)[2] <- paste0("Return Trading Period ",p, ". The top 20 Sharp")
  }
  trading_return[[p]] <- ret_aux
  returns[[p]] <- trades
} 







