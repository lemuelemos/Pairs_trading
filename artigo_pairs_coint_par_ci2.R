library(readxl)
library(xts)
library(stringr)
library(dplyr)
library(timeSeries)
library(egcm)
library(parallel)
library(Rcpp)

##### Import data and cleaning NA's
sourceCpp("cpp_codes.cpp")
ibrx_2007_2018 <- read_excel("ibrx last price 2007 até 2018.xlsx", sheet = "ibrx") #### Reading the data

ibrx_2007_2018$Dates <- as.Date(ibrx_2007_2018$Dates) ## Setting the format of the dates column
ibrx_2007_2018 <- as.data.frame(ibrx_2007_2018) ## Transform in data_frame to easy handling
ibrx_2007_2018[,2:101] <- apply(ibrx_2007_2018[,2:101],2,as.numeric) ## Transform the data form char type to numeric type
ibrx_2007_2018 <- xts(ibrx_2007_2018[,-1],order.by = ibrx_2007_2018$Dates) ## transform in xts to easy handling with time series
ibrx_2007_2018_integridade <- apply(ibrx_2007_2018,2,
                                    function(x) sum(is.na(x))/nrow(ibrx_2007_2018))*100 ## Calculate the percentage of missing data

ibrx_2007_2018_70 <- ibrx_2007_2018[,names(ibrx_2007_2018_integridade[which(ibrx_2007_2018_integridade<10)])] ## Taking the coluns eith more than 90% of integrite
ibrx_2008_2017_70 <- ibrx_2007_2018_70["2008/2017"] ## subsetting to eliminate more missing data. 
ibrx_2008_2017_70 <- na.spline(ibrx_2008_2017_70) ## remove "NA's" spline method Missing values (NAs) are replaced by linear interpolation via approx or cubic spline interpolation via spline, respectively.
rm(list=c("ibrx_2007_2018","ibrx_2007_2018_70")) ## remove objects that will not be use 
Nomes <- colnames(ibrx_2008_2017_70)
Nomes <- str_sub(Nomes, 1,6)
colnames(ibrx_2008_2017_70) <- str_trim(Nomes)
no_cores <- detectCores()

### Set de window estimation - rolling regressions
save_data <- list(NULL)
formation_windown <- c(42,63,126,252)
trading_days <- c(60,120,180,360)
for(pp in 1:length(formation_windown)){
  ret_aux <- list(NULL)
  ret_port <- list(NULL)
  trades <- list(NULL)
  trading_return <- list(NULL)
  returns <- list(NULL)
  portfolios <- list(NULL)
  window_test <- seq(1,nrow(ibrx_2008_2017_70),by=formation_windown[pp])
  for(p in seq_along(window_test)){
  test_period <- window(ibrx_2008_2017_70,
                        start=time(ibrx_2008_2017_70)[window_test[p]],
                        end=if(is.na(time(ibrx_2008_2017_70)[window_test[p]+formation_windown[pp]])){time(ibrx_2008_2017_70)[nrow(ibrx_2008_2017_70)]}
                        else{time(ibrx_2008_2017_70)[window_test[p]+formation_windown[pp]]})

datas <- time(test_period)
#test_period <- test_period[,1:30]

##### Estimating
cl <- makeCluster(no_cores)
clusterExport(cl, "test_period")
clusterEvalQ(cl, library(egcm))
cat("\n","Estimating Pairs",append = T)

pares_pp <- parLapply(cl,data.frame(test_period),function(x) apply(test_period,2, 
                                                    function(y) if(x!=y){egcm(y, x, urtest = "pp", p.value = 0.05)}))

pares_pgff <- parLapply(cl,data.frame(test_period),function(x) apply(test_period,2, 
                                                       function(y) if(x!=y){egcm(y, x, urtest = "pgff",p.value = 0.05)}))



pares_pp <- unlist(pares_pp, recursive = F)
pares_pp <- pares_pp[!sapply(pares_pp,is.null)]
pares_pgff <- unlist(pares_pgff, recursive = F)
pares_pgff <- pares_pgff[!sapply(pares_pgff,is.null)]

stopCluster(cl)

############# Testing for cointegration #####################
cat("\n","Pairs Testing","\r",append = T)
pares_coint_pp <- pares_pp[sapply(pares_pp, is.cointegrated)]
pares_coint_pp <- pares_coint_pp[sapply(pares_coint_pp,is.ar1)]
pares_coint_pgff <- pares_pgff[sapply(pares_pgff,is.cointegrated)]
pares_coint_pgff <- pares_coint_pgff[sapply(pares_coint_pgff,is.ar1)]


rm(pares_pp)
rm(pares_pgff)


############################################
##### Comparing the cointegrationg on the 2 diferent's test's

nomes_pp <- names(pares_coint_pp)
#nomes_pp <- gsub(" ", "", nomes_pp)
nomes_pp <- gsub("\\.", " vs ", nomes_pp)
nomes_pp <- data_frame(nomes_pp)
names(pares_coint_pp) <- nomes_pp$nomes_pp

nomes_pgff <- names(pares_coint_pgff)
#nomes_pgff <- gsub(" ", "", nomes_pgff)
nomes_pgff <- gsub("\\.", " vs ", nomes_pgff)
nomes_pgff <- data_frame(nomes_pgff)
names(pares_coint_pgff) <- nomes_pgff$nomes_pgff

pares_coint_ci2 <- nomes_pp %>% dplyr::filter(nomes_pp %in% nomes_pgff$nomes_pgff)
pares_coint_ci2s <- pares_coint_pp[pares_coint_ci2$nomes_pp]


################################################################
### Signal Calc
cat("\r","Signal Calc","\r",append = T)

Zm_ci2 <- sapply(pares_coint_ci2s, function(x) x$residuals/x$residuals.sd)
Zm_ci2 <- data.frame(Zm_ci2)
colnames(Zm_ci2) <- gsub("\\.", " ", colnames(Zm_ci2))

sinal_t_ci2 <- matrix(data = rep(0,ncol(Zm_ci2)*nrow(Zm_ci2)),ncol = ncol(Zm_ci2),nrow = nrow(Zm_ci2))
sinal_t_ci2[1,1:ncol(sinal_t_ci2)] <- "Fora"
tr <- c(1,0)
sinal_t_ci2 <- sncalc(ncol(Zm_ci2),nrow(Zm_ci2),as.matrix(Zm_ci2), tr=tr, sinal=sinal_t_ci2)
colnames(sinal_t_ci2) <- names(Zm_ci2)

################################################################
### Agruping the data by pair

cat("\r","Agruping the data by pair","\r",append = T)
parestrade_ci2 <- list(NULL)

for(j in 1:ncol(sinal_t_ci2)){
  parestrade_ci2[[j]] <- cbind(test_period[,which(str_detect(colnames(test_period),
                                                             str_trim(str_sub(colnames(sinal_t_ci2)[j],
                                                                              end=6))) == T)],
                               test_period[,which(str_detect(colnames(test_period),
                                                             str_sub(colnames(sinal_t_ci2)[j],
                                                                     start = 10)) == T)])
  names(parestrade_ci2)[j] <- colnames(sinal_t_ci2)[j]
}


###################################################
### Return Calc
cat("\r","Return Calc","\r",append = T)

betas_ci2 <- plyr::ldply(pares_coint_ci2s, function(x) x$beta)
invest_f_ci2 <- data.frame(matrix(data = rep(1,ncol(Zm_ci2)*nrow(Zm_ci2)),ncol = ncol(Zm_ci2),nrow = nrow(Zm_ci2)))
retorno_f_ci2 <- data.frame(matrix(data = rep(0,ncol(Zm_ci2)*nrow(Zm_ci2)),ncol = ncol(Zm_ci2),nrow = nrow(Zm_ci2)))
ttf_ci2 <- data.frame(matrix(data = rep(0,ncol(Zm_ci2)*nrow(Zm_ci2)),ncol = ncol(Zm_ci2),nrow = nrow(Zm_ci2)))
results <- NULL
par_est <- data.frame(NULL)
for(j in 1:length(parestrade_ci2)){
  par_est <- parestrade_ci2[[j]]
  results <- returcalc(as.matrix(sinal_t_ci2[,j]),
                       as.matrix(par_est),betas = betas_ci2$V1[j],invest = invest_f_ci2[,j])
  invest_f_ci2[,j] <- results[[1]]
  retorno_f_ci2[,j] <- results[[2]]
  ttf_ci2[,j] <- results[[3]]
}
colnames(invest_f_ci2) <- names(parestrade_ci2)
colnames(retorno_f_ci2) <- names(parestrade_ci2)
colnames(ttf_ci2) <- names(parestrade_ci2)

  
################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
#####################################################################
cat("\r",paste0("Calculating return and sharpe. Portfolio ",p),"\r",append = T)
portret_t_ci2 <- as.data.frame(matrix(data = rep(0,ncol(Zm_ci2)*3),
                                      ncol = ncol(Zm_ci2),nrow = 3))
for(f in 1:length(invest_f_ci2)){
  portret_t_ci2[1,f] <- ((invest_f_ci2[nrow(invest_f_ci2),f]/invest_f_ci2[1,f])-1)*100
  portret_t_ci2[2,f] <- sd(invest_f_ci2[,f])
  portret_t_ci2[3,f] <- portret_t_ci2[1,f]/portret_t_ci2[2,f]
  colnames(portret_t_ci2)[f] <- names(parestrade_ci2)[f]
}

portret_t_ci2 <- t(portret_t_ci2) ## Retornos Totais
colnames(portret_t_ci2) <- c("Retorno Total","Desvio Padrão","Sharpe")
ret_port[[p]] <- portret_t_ci2 ## Retornos Totais
names(ret_port)[p] <- paste0("Return Formation Period ",p)

#####################################################
############### Periodo de Trading ##################
#####################################################
cat("\r","Periodo de Trading","\r",append = T)
select_port <- list(NULL)
for(ii in c(1,3)){
  if(ii == 1){
    cat("\n","Trading Period top 20 return","\r",append = T)
  } else {cat("\n","Trading Period top 20 sharp","\r",append = T)}
  portsel <- row.names(ret_port[[p]][order(ret_port[[p]][,ii], decreasing = T),])[1:20] ## Seleect the top 20 sharp's
  portsel <- as.character(na.omit(portsel))
  select_port[[p]] <- portsel # testing if the window is complete
  trading_period <- window(ibrx_2008_2017_70, # Select the data
                           start = time(test_period)[1],
                           end = time(test_period)[nrow(test_period)]+trading_days[pp])
  trading_window <- nrow(trading_period) - nrow(test_period)
  Zm_ci2_t <- Zm_ci2 %>% select(portsel)
  
  ############# Estimating the pairs ################
  cat("\n","Estimating the pairs Trading","\r",append = T)
  parestrade_ci1_t <-as.list(NULL)
  for(j in 1:length(portsel)){
    parestrade_ci1_t[[j]] <- cbind(trading_period[,grep(str_trim(str_sub(portsel[j],
                                                                         end=6)),
                                                        names(trading_period))],
                                   trading_period[,grep(str_trim(str_sub(portsel[j],
                                                                         start=-6)),
                                                        names(trading_period))])
    names(parestrade_ci1_t)[j] <- portsel[j]
  }
  ### Estimating pairs
  pares_trading <- list(NULL)
  bt_t <- data.frame(NULL)
  coint <- list(NULL)
  cl <- makeCluster(no_cores)
  for(i in 1:trading_window){
    parestrade_est <- lapply(parestrade_ci1_t, function(x) x[1:(nrow(test_period)+i),])
    cat("\r", i, "of", trading_window,"\r",append = T)
    clusterExport(cl, "parestrade_est")
    clusterEvalQ(cl, library(egcm))
    pares_trading <- parLapply(cl,parestrade_est, function(x) egcm(x[,2],x[,1]))
    bt_t <- data.frame(c(bt_t,
                         data.frame(beta=unlist(lapply(pares_trading, 
                                                       function(x) x$beta)))))
    pares_trading <- pares_trading[!sapply(pares_trading,is.null)]
    coint[[i]] <- lapply(pares_trading,is.cointegrated)
    
    ### Norm M's
    #cat(paste0("Normalizing the M. Portfolio",p))
    Z_norm <- lapply(pares_trading, function(x) x$residuals/x$residuals.sd)
    Z_norm <- as.data.frame(Z_norm)
    colnames(Z_norm) <- gsub("\\."," ",names(Z_norm))
    Zm_ci2_t[nrow(test_period)+i,] <- Z_norm[nrow(Z_norm),]
  }
  
  ### sign of operations
  #Zm_ci2_t <- Zm_ci2_t[(formation_windown[pp]+2):nrow(trading_period),]
  sinal_t_ci2 <- matrix(data = rep(0,ncol(Zm_ci2_t)*nrow(Zm_ci2_t)),ncol = ncol(Zm_ci2_t),nrow = nrow(Zm_ci2_t))
  sinal_t_ci2[1,1:ncol(sinal_t_ci2)] <- "Fora"
  cat("\n",paste0("Sign for operations - threshold[",tr[1],",",tr[2],"]. Portolio ",p),"\r",append = T)
  sinal_t_ci2 <- sncalc(ncol(Zm_ci2_t),nrow(Zm_ci2_t),as.matrix(Zm_ci2_t), tr=tr, sinal=sinal_t_ci2)
  sinal_t_ci2 <- as.data.frame(sinal_t_ci2) 
  colnames(sinal_t_ci2) <- names(Zm_ci2_t)
  sinal_t_ci2 %>% mutate_if(is.factor,as.character) -> sinal_t_ci2
  ##########################
  bt_t <- data.frame(pares = portsel,betas = apply(bt_t, 1, mean))
  ############# Return Calc
  invest_t_ci2 <- data.frame(matrix(data = rep(1,ncol(Zm_ci2_t)*nrow(Zm_ci2_t)),ncol = ncol(Zm_ci2_t),nrow = nrow(Zm_ci2_t)))
  retorno_t_ci2 <- data.frame(matrix(data = rep(0,ncol(Zm_ci2_t)*nrow(Zm_ci2_t)),ncol = ncol(Zm_ci2_t),nrow = nrow(Zm_ci2_t)))
  tt2 <- data.frame(matrix(data = rep(0,ncol(Zm_ci2_t)*nrow(Zm_ci2_t)),ncol = ncol(Zm_ci2_t),nrow = nrow(Zm_ci2_t)))
  results <- NULL
  par_est <- data.frame(NULL)
  for(j in 1:length(pares_trading)){
    par_est <- parestrade_ci1_t[[j]]
    bt <- bt_t %>% dplyr::filter(pares == portsel[j]) %>% select(betas)
    results <- returcalc(as.matrix(sinal_t_ci2[,j]),
                         as.matrix(par_est),betas = as.numeric(bt),
                         invest = invest_t_ci2[,j])
    invest_t_ci2[,j] <- results$invest
    retorno_t_ci2[,j] <- results$retorno
    tt2[,j] <- results$tt
  }
  tt2[1,1:ncol(tt2)] <- "Fora"
  colnames(invest_t_ci2) <- names(pares_trading)
  colnames(retorno_t_ci2) <- names(pares_trading)
  colnames(tt2) <- names(pares_trading)
  
  
  ################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
  cat("\n",paste0("Calculating return and sharpe. Portfolio ",p),"\r",append = T)
  portret_t <- as.data.frame(matrix(data = rep(0,ncol(Zm_ci2_t)*3),ncol = ncol(Zm_ci2_t),nrow = 3))
  for(f in 1:ncol(invest_t_ci2)){
    for(i in nrow(ttf_ci2):nrow(tt2)){
      if(tt2[i,f] == "Abriu"){
        portret_t[1,f] <- (tail(cumprod(retorno_t_ci2[i:nrow(retorno_t_ci2),f]+1),1)-1)*100
        portret_t[2,f] <- sd(invest_t_ci2[i:nrow(retorno_t_ci2),f])
        portret_t[3,f] <- portret_t[1,f]/portret_t[2,f]
        colnames(portret_t)[f] <- names(pares_trading)[f]
        break
      } else{
        portret_t[1,f] <- 0
        portret_t[2,f] <- 0
        portret_t[3,f] <- 0
        colnames(portret_t)[f] <- names(pares_trading)[f]
        next
      }
    }
  }
  portret_t <- t(portret_t)
  colnames(portret_t) <- c("Retorno Total","Desvio Padrão","Sharpe")
  
  if(ii == 1){
    ret_aux[[1]] <- portret_t ## Retornos Totais
    trades[[1]] <- retorno_t_ci2
    names(ret_aux)[1] <- paste0("Return Trading Period ",p, ". The top 20 Return")
    names(trades)[1] <- paste0("Return Trading Period ",p, ". The top 20 Return")
  } else{
    ret_aux[[2]] <- portret_t ## Retornos Totais
    trades[[2]] <- retorno_t_ci2
    names(ret_aux)[2] <- paste0("Return Trading Period ",p, ". The top 20 Sharp")
    names(trades)[2] <- paste0("Return Trading Period ",p, ". The top 20 Sharp")
  }
  trading_return[[p]] <- ret_aux
  returns[[p]] <- trades
  portfolios[[p]] <- invest_t_ci2
}  
  }
  save_data[[pp]][[1]] <- trading_return
  save_data[[pp]][[2]] <- returns
  save_data[[pp]][[3]] <- portfolios
  names(save_data[[pp]]) <- paste0("Formation Window",formation_windown[pp]," dias uteis")
  saveRDS(save_data,file=paste0(getwd(),"/resultados/pairsci2_fw_",
                                names(formation_windown)[pp]))
}
  
  
  
  