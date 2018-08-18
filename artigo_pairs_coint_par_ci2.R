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
colnames(ibrx_2008_2017_70) <- Nomes


ret_port <- list(NULL)
### Set de window estimation - rolling regressions
window_test <- seq(1,nrow(ibrx_2008_2017_70),by=126)
formation_windown <- c(251,503,1007)
no_cores <- detectCores()
cl <- makeCluster(no_cores)
for(pp in 1){
  for(p in 1){
  test_period <- window(ibrx_2008_2017_70,
                        start=time(ibrx_2008_2017_70)[window_test[p]],
                        end=if(is.na(time(ibrx_2008_2017_70)[window_test[p]+formation_windown[pp]])){time(ibrx_2008_2017_70)[nrow(ibrx_2008_2017_70)]}
                        else{time(ibrx_2008_2017_70)[window_test[p]+formation_windown[pp]]})

datas <- time(test_period)
test_period <- as.data.frame(test_period)

##### Estimating

clusterExport(cl, "test_period")
clusterEvalQ(cl, library(egcm))
print("Estimating Pairs")
pares_pp <- parLapply(cl,test_period,function(x) apply(test_period,2, 
                                                    function(y) if(x!=y){egcm(x, y, urtest = "pp", p.value = 0.01)}))

pares_pgff <- parLapply(cl,test_period,function(x) apply(test_period,2, 
                                                       function(y) if(x!=y){egcm(x, y, urtest = "pgff",p.value = 0.01)}))



pares_pp <- unlist(pares_pp, recursive = F)
pares_pp <- pares_pp[!sapply(pares_pp,is.null)]
pares_pgff <- unlist(pares_pgff, recursive = F)
pares_pgff <- pares_pgff[!sapply(pares_pgff,is.null)]

stopCluster(cl)

############# Testing for cointegration #####################
print("Pairs Testing")
pares_coint_pp <- pares_pp[sapply(pares_pp, is.cointegrated)]
pares_coint_pp <- pares_coint_pp[sapply(pares_coint_pp,is.ar1)]
pares_coint_pgff <- pares_pgff[sapply(pares_pgff,is.cointegrated)]
pares_coint_pgff <- pares_coint_pgff[sapply(pares_coint_pgff,is.ar1)]


rm(pares_pp)
rm(pares_pgff)


############################################
##### Comparing the cointegrationg on the 2 diferent's test's

nomes_pp <- names(pares_coint_pp)
nomes_pp <- gsub(" ", "", nomes_pp)
nomes_pp <- gsub("\\.", " vs ", nomes_pp)
nomes_pp <- data_frame(nomes_pp)
names(pares_coint_pp) <- nomes_pp$nomes_pp

nomes_pgff <- names(pares_coint_pgff)
nomes_pgff <- gsub(" ", "", nomes_pgff)
nomes_pgff <- gsub("\\.", " vs ", nomes_pgff)
nomes_pgff <- data_frame(nomes_pgff)
names(pares_coint_pgff) <- nomes_pgff$nomes_pgff

pares_coint_ci2 <- nomes_pp %>% dplyr::filter(nomes_pp %in% nomes_pgff$nomes_pgff)
pares_coint_ci2s <- pares_coint_pp[pares_coint_ci2$nomes_pp]


################################################################
### Signal Calc
print("Signal Calc")

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

print("Agruping the data by pair")
parestrade_ci2 <- list(NULL)

for(j in 1:ncol(sinal_t_ci2)){
  parestrade_ci2[[j]] <- cbind(test_period[,which(str_detect(colnames(test_period),
                                                             str_trim(str_sub(colnames(sinal_t_ci2)[j],
                                                                              start=10))) == T)],
                               test_period[,which(str_detect(colnames(test_period),
                                                             str_sub(colnames(sinal_t_ci2)[j],
                                                                     end=6)) == T)])
  names(parestrade_ci2)[j] <- colnames(sinal_t_ci2)[j]
}


###################################################
### Return Calc
print("Return Calc")

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
for(ii in c(1,3)){
  if(ii == 1){
    print("Trading Period top 20 return")
  } else {print("Trading Period top 20 sharp")}
  portsel <- row.names(ret_port[[p]][order(-ret_port[[p]][,ii]),])[1:20] ## Seleect the top 20 sharp's
  portsel <- as.character(na.omit(portsel))
  select_port[[p]] <- portsel # testing if the window is complete
  trading_period <- window(ibrx_2008_2017_70, # Select the data
                           start = time(test_period)[1],
                           end = time(test_period)[nrow(test_period)]+180)
  trading_window <- nrow(trading_period) - nrow(test_period)
  betas_trading <- betas_formation %>% 
    select(Pares, betas) %>% 
    dplyr::filter(Pares %in% portsel)
  Zm_trading <- Zm_fornation %>% select(portsel)
  ############# Estimating the pairs ################
  print("Estimating the pairs")
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

  }
 }
}



