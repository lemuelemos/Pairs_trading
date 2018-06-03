library(doParallel)
library(partialCI)
library(readxl)
library(xts)
library(stringr)
library(dplyr)
library(timeSeries)
library(plyr)
library(Rcpp)
##### Import data and cleaning NA's
#source('cpp_codes.R')
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
Nomes <- colnames(ibrx_2008_2017_70) ## Taking the names of equity's
Nomes <- str_sub(Nomes, 1,6)
colnames(ibrx_2008_2017_70) <- Nomes
threshold <- matrix(c(1,1.5,1,1.5,0.5,0.5,1,1),4,2)

### Setting the window of estimation
window_test <- seq(1,nrow(ibrx_2008_2017_70),by=126)
for(kk in 1){
  tr <- threshold[kk,]
no_cores <- detectCores()
cl <- makeCluster(no_cores)
for(p in 1){
  test_period <- window(ibrx_2008_2017_70,
                        start=time(ibrx_2008_2017_70)[window_test[p]],
                        end=if(is.na(time(ibrx_2008_2017_70)[window_test[p]+1007])){time(ibrx_2008_2017_70)[nrow(ibrx_2008_2017_70)]}
                        else{time(ibrx_2008_2017_70)[window_test[p]+1007]})
  period <- time(test_period)
  test_period <- as.data.frame(test_period)
  
  ### Estimating pairs
  clusterExport(cl, "test_period")
  clusterEvalQ(cl, library(partialCI))
  pares <- parLapply(cl,test_period,function(x) apply(test_period,2, 
                                                      function(y) if(x!=y){fit.pci(x,y)}))
  stopCluster(cl)
  
  pares <- unlist(pares, recursive = F)
  pares <- pares[!sapply(pares,is.null)]
  pares <- pares[!sapply(pares, function(x) is.na(x$rho.se))]
  names(pares) <- gsub("TIET11vs","TIET11 vs",
                       paste0(str_sub(names(pares), 1,6),"vs ", 
                              str_sub(names(pares), 8,13)))
}
pares <- pares[!sapply(pares,is.null)] ### Retirando os valores vazios
pares <- pares[!sapply(pares, function(x) is.na(x$rho.se))] ### Retirando os pares com problemas de estimação
#### Taking the pairs with R square greater than 0.5
paresR <- pares[sapply(pares,function(x) x$pvmr > 0.5)]
rm(pares)
### Testing partial Cointegration
cl <- makeCluster(no_cores)
clusterExport(cl, "paresR")
clusterEvalQ(cl, library(partialCI))
paresRtested <- paresR[parSapply(cl,paresR, 
                                 FUN = function(x) which.hypothesis.pcitest(test.pci(x))=="PCI")]
stopCluster(cl)
rm(paresR)
### Estimation of ocult states
paresRtestedM <- lapply(paresRtested, function(x) statehistory.pci(x))
betas <- ldply(paresRtested, function(x) x$beta)
#paresRtested[[j]]$beta
#rm(paresRtested)
# Variável paresRtestedM já são os pares para teste backtest
############### Normalizando O M
Zm <- lapply(paresRtestedM, function(x) x$M/sd(x$M))
Zm <- as.data.frame(Zm)
colnames(Zm) <- gsub("\\."," ",names(Zm))
rm(paresRtestedM)
### sign of operations
sinal <- matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))
sinal[1,1:ncol(sinal)] <- "Fora"
sinal <- sncalc(ncol(Zm),nrow(Zm),as.matrix(Zm), tr=tr, sinal=sinal)
sinal<- as.data.frame(sinal) 
colnames(sinal) <- names(Zm)
sinal %>% mutate_if(is.factor,as.character) -> sinal
#as.xts(sinal, order.by = time(test_period))
############# Return Calc

parestrade <- list(NULL)
for(j in 1:length(sinal)){
  parestrade[[j]] <- cbind(test_period[,str_sub(names(sinal)[j],end=6)],
                           test_period[,str_sub(names(sinal)[j],start=-6)])
  names(parestrade)[j] <- names(sinal)[j]
  colnames(parestrade[[j]]) <- cbind(str_sub(names(sinal)[j],end=6),
                                     str_sub(names(sinal)[j],start=-6))
}
invest <- data.frame(matrix(data = rep(1,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
par_est <- data.frame(NULL)
for(j in 1){
  par_est <- parestrade[[j]]
  invest[,j] <- returcalc(as.matrix(sinal[,j]),
            as.matrix(par_est),betas = betas$beta_[j],invest = invest[,j])
}
head(invest[,j],50)
#returcalc(as.matrix(sinal),as.matrix(par_est))

}


