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
formation_windown <- c(41,62,125,251)
trading_days <- c(60,120,180,360)
for(pp in 1:length(formation_windown)){
  ret_aux <- list(NULL)
  ret_port <- list(NULL)
  trades <- list(NULL)
  trading_return <- list(NULL)
  returns <- list(NULL)
  portfolios <- list(NULL)
  window_test <- seq(1,nrow(ibrx_2008_2017_70),by=(formation_windown[pp]+1))
  for(p in seq_along(window_test)){
  test_period <- window(ibrx_2008_2017_70,
                          start=time(ibrx_2008_2017_70)[window_test[p]],
                          end=if(is.na(time(ibrx_2008_2017_70)[window_test[p]+formation_windown[pp]])){break}
                          else{time(ibrx_2008_2017_70)[window_test[p]+formation_windown[pp]]})
    
  datas <- time(test_period)
  
    
##### Estimating
cl <- makeCluster(no_cores)
clusterExport(cl, "test_period")
clusterEvalQ(cl, library(egcm))
print("Estimating Pairs")

pares_adf <- parLapply(cl,
                       data.frame(test_period),
                       function(x) apply(data.frame(test_period),2,
                                         function(y) if(x!=y){egcm(y, x, urtest = "adf",p.value = 0.05)}))

pares_jo <- parLapply(cl,
                      data.frame(test_period),
                      function(x) apply(data.frame(test_period),2, 
                                        function(y) if(x!=y){egcm(y, x, urtest = "jo-e",p.value = 0.05)}))

pares_jo <- unlist(pares_jo, recursive = F)
pares_jo <- pares_jo[!sapply(pares_jo,is.null)]
pares_adf <- unlist(pares_adf, recursive = F)
pares_adf <- pares_adf[!sapply(pares_adf,is.null)]
    
stopCluster(cl)   

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
#nomes_adf <- gsub(" ", "", nomes_adf)
nomes_adf <- gsub("\\.", " vs ", nomes_adf)
nomes_adf <- data_frame(nomes_adf)
names(pares_coint_adf) <- nomes_adf$nomes_adf

nomes_jo <- names(pares_coint_jo)
#nomes_jo <- gsub(" ", "", nomes_jo)
nomes_jo <- gsub("\\.", " vs ", nomes_jo)
nomes_jo <- data_frame(nomes_jo)
names(pares_coint_jo) <- nomes_jo$nomes_jo

pares_coint_ci1 <- nomes_adf %>% dplyr::filter(nomes_adf %in% nomes_jo$nomes_jo)
pares_coint_ci1s <- pares_coint_adf[pares_coint_ci1$nomes_adf]

################################################################
### Signal Calc
print("Signal Calc")

Zm_ci1 <- sapply(pares_coint_ci1s, function(x) x$residuals/x$residuals.sd)
Zm_ci1 <- data.frame(Zm_ci1)
colnames(Zm_ci1) <- gsub("\\.", " ", colnames(Zm_ci1))

sinal_f_ci1 <- matrix(data = rep(0,ncol(Zm_ci1)*nrow(Zm_ci1)),ncol = ncol(Zm_ci1),nrow = nrow(Zm_ci1))
sinal_f_ci1[1,1:ncol(sinal_f_ci1)] <- "Fora"
tr <- c(1,0)
sinal_f_ci1 <- sncalc(ncol(Zm_ci1),nrow(Zm_ci1),as.matrix(Zm_ci1), tr=tr, sinal=sinal_f_ci1)
colnames(sinal_f_ci1) <- names(Zm_ci1)

################################################################
### Agruping the data by pair
print("Agruping the data by pair")

parestrade_ci1 <- list(NULL)
for(j in 1:ncol(sinal_f_ci1)){
  parestrade_ci1[[j]] <- cbind(test_period[,which(str_detect(colnames(test_period),
                                                             str_trim(str_sub(colnames(sinal_f_ci1)[j],
                                                                              end=6))) == T)],
                               test_period[,which(str_detect(colnames(test_period),
                                                             str_trim(str_sub(colnames(sinal_f_ci1)[j], 
                                                                              start=10))) == T)])
  names(parestrade_ci1)[j] <- colnames(sinal_f_ci1)[j]
}



###################################################
### Return Calc
print("Return Calc")

betas_ci1 <- plyr::ldply(pares_coint_ci1s, function(x) x$beta)
invest_f_ci1 <- data.frame(matrix(data = rep(1,ncol(Zm_ci1)*nrow(Zm_ci1)),ncol = ncol(Zm_ci1),nrow = nrow(Zm_ci1)))
retorno_f_ci1 <- data.frame(matrix(data = rep(0,ncol(Zm_ci1)*nrow(Zm_ci1)),ncol = ncol(Zm_ci1),nrow = nrow(Zm_ci1)))
ttf_ci1 <- data.frame(matrix(data = rep(0,ncol(Zm_ci1)*nrow(Zm_ci1)),ncol = ncol(Zm_ci1),nrow = nrow(Zm_ci1)))
results <- NULL
par_est <- data.frame(NULL)
for(j in 1:length(parestrade_ci1)){
  par_est <- parestrade_ci1[[j]]
  results <- returcalc(as.matrix(sinal_f_ci1[,j]),
                       as.matrix(par_est),betas = betas_ci1$V1[j],invest = invest_f_ci1[,j])
  invest_f_ci1[,j] <- results[[1]]
  retorno_f_ci1[,j] <- results[[2]]
  ttf_ci1[,j] <- results[[3]]
}
colnames(invest_f_ci1) <- names(parestrade_ci1)
colnames(retorno_f_ci1) <- names(parestrade_ci1)
colnames(ttf_ci1) <- names(parestrade_ci1)

################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
print(paste0("Calculating return and sharpe. Portfolio ",p))
portret_t_ci1 <- as.data.frame(matrix(data = rep(0,ncol(Zm_ci1)*3),
                                      ncol = ncol(Zm_ci1),nrow = 3))
for(f in 1:length(invest_f_ci1)){
  portret_t_ci1[1,f] <- ((invest_f_ci1[nrow(invest_f_ci1),f]/invest_f_ci1[1,f])-1)*100
  portret_t_ci1[2,f] <- sd(invest_f_ci1[,f])
  portret_t_ci1[3,f] <- portret_t_ci1[1,f]/portret_t_ci1[2,f]
  colnames(portret_t_ci1)[f] <- names(parestrade_ci1)[f]
}

portret_t_ci1 <- t(portret_t_ci1) ## Retornos Totais
colnames(portret_t_ci1) <- c("Retorno Total","Desvio Padrão","Sharpe")
ret_port[[p]] <- portret_t_ci1 ## Retornos Totais
names(ret_port)[p] <- paste0("Return Formation Period ",p)

#####################################################
############### Periodo de Trading ##################
#####################################################
print("Periodo de Trading")
select_port <- list(NULL)
for(ii in c(1,3)){
  if(ii == 1){
    print("Trading Period top 20 return")
  } else {print("Trading Period top 20 sharp")}
  portsel <- row.names(ret_port[[p]][order(ret_port[[p]][,ii], decreasing = T),])[1:20] ## Seleect the top 20 sharp's
  portsel <- as.character(na.omit(portsel))
  select_port[[p]] <- portsel # testing if the window is complete
  trading_period <- window(ibrx_2008_2017_70, # Select the data
                           start = time(test_period)[1],
                           end = time(test_period)[nrow(test_period)]+trading_days[pp])
  trading_window <- nrow(trading_period) - nrow(test_period)
  Zm_ci1_t <- Zm_ci1 %>% select(portsel)
  
############# Estimating the pairs ################
print("Estimating the pairs T")
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
  cat("\r", i, "of", trading_window,"window",nrow(parestrade_est[[1]]),"\r")
  clusterExport(cl, "parestrade_est")
  clusterEvalQ(cl, library(egcm))
  pares_trading <- parLapply(cl,parestrade_est, function(x) egcm(x[,2],x[,1]))
  bt_t <- data.frame(c(bt_t,
                       data.frame(beta=unlist(lapply(pares_trading, 
                                                     function(x) x$beta)))))
  pares_trading <- pares_trading[!sapply(pares_trading,is.null)]
  coint[[i]] <- lapply(pares_trading,is.cointegrated)
  
  ### Norm M's
  #print(paste0("Normalizing the M. Portfolio",p))
  Z_norm <- lapply(pares_trading, function(x) x$residuals/x$residuals.sd)
  Z_norm <- as.data.frame(Z_norm)
  colnames(Z_norm) <- gsub("\\."," ",names(Z_norm))
  Zm_ci1_t[nrow(test_period)+i,] <- Z_norm[nrow(Z_norm),]
}
stopCluster(cl) 
### sign of operations
#Zm_ci1_t <- Zm_ci1_t[(formation_windown[pp]+2):nrow(trading_period),]
sinal_t_ci1 <- matrix(data = rep(0,ncol(Zm_ci1_t)*nrow(Zm_ci1_t)),ncol = ncol(Zm_ci1_t),nrow = nrow(Zm_ci1_t))
sinal_t_ci1[1,1:ncol(sinal_t_ci1)] <- "Fora"
print(paste0("Sign for operations - threshold[",tr[1],",",tr[2],"]. Portolio ",p))
sinal_t_ci1 <- sncalc(ncol(Zm_ci1_t),nrow(Zm_ci1_t),as.matrix(Zm_ci1_t), tr=tr, sinal=sinal_t_ci1)
sinal_t_ci1 <- as.data.frame(sinal_t_ci1) 
colnames(sinal_t_ci1) <- names(Zm_ci1_t)
sinal_t_ci1 %>% mutate_if(is.factor,as.character) -> sinal_t_ci1
##########################
bt_t <- data.frame(pares = portsel,betas = apply(bt_t, 1, mean))
############# Return Calc
invest_t_ci1 <- data.frame(matrix(data = rep(1,ncol(Zm_ci1_t)*nrow(Zm_ci1_t)),ncol = ncol(Zm_ci1_t),nrow = nrow(Zm_ci1_t)))
retorno_t_ci1 <- data.frame(matrix(data = rep(0,ncol(Zm_ci1_t)*nrow(Zm_ci1_t)),ncol = ncol(Zm_ci1_t),nrow = nrow(Zm_ci1_t)))
tt2 <- data.frame(matrix(data = rep(0,ncol(Zm_ci1_t)*nrow(Zm_ci1_t)),ncol = ncol(Zm_ci1_t),nrow = nrow(Zm_ci1_t)))
results <- NULL
par_est <- data.frame(NULL)
for(j in 1:length(pares_trading)){
  par_est <- parestrade_ci1_t[[j]]
  bt <- bt_t %>% dplyr::filter(pares == portsel[j]) %>% select(betas)
  results <- returcalc(as.matrix(sinal_t_ci1[,j]),
                       as.matrix(par_est),betas = as.numeric(bt),
                       invest = invest_t_ci1[,j])
  invest_t_ci1[,j] <- results$invest
  retorno_t_ci1[,j] <- results$retorno
  tt2[,j] <- results$tt
}
tt2[1,1:ncol(tt2)] <- "Fora"
colnames(invest_t_ci1) <- names(pares_trading)
colnames(retorno_t_ci1) <- names(pares_trading)
colnames(tt2) <- names(pares_trading)


################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
print(paste0("Calculating return and sharpe. Portfolio ",p))
portret_t <- as.data.frame(matrix(data = rep(0,ncol(Zm_ci1_t)*3),ncol = ncol(Zm_ci1_t),nrow = 3))
for(f in 1:ncol(invest_t_ci1)){
  for(i in nrow(ttf_ci1):nrow(tt2)){
    if(tt2[i,f] == "Abriu"){
  portret_t[1,f] <- (tail(cumprod(retorno_t_ci1[i:nrow(retorno_t_ci1),f]+1),1)-1)*100
  portret_t[2,f] <- sd(invest_t_ci1[i:nrow(retorno_t_ci1),f])
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
  trades[[1]] <- retorno_t_ci1
  names(ret_aux)[1] <- paste0("Return Trading Period ",p, ". The top 20 Return")
  names(trades)[1] <- paste0("Return Trading Period ",p, ". The top 20 Return")
} else{
  ret_aux[[2]] <- portret_t ## Retornos Totais
  trades[[2]] <- retorno_t_ci1
  names(ret_aux)[2] <- paste0("Return Trading Period ",p, ". The top 20 Sharp")
  names(trades)[2] <- paste0("Return Trading Period ",p, ". The top 20 Sharp")
}
trading_return[[p]] <- ret_aux
returns[[p]] <- trades
portfolios[[p]] <- invest_t_ci1
    }  
  }
 save_data[[pp]][[1]] <- trading_return
 save_data[[pp]][[2]] <- returns
 save_data[[pp]][[3]] <- portfolios
 names(save_data[[pp]]) <- paste0("Formatrion Window",formation_windown[pp]," dias uteis")
 saveRDS(save_data,file=paste0(getwd(),"/resultados/pairsci1_fw_",
                               names(formation_windown)[pp]))
}


