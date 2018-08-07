library(doParallel)
library(plyr)
library(partialCI)
library(readxl)
library(xts)
library(stringr)
library(dplyr)
library(timeSeries)
library(Rcpp)
##### Import data and cleaning NA's
#source('cpp_codes.R')
sourceCpp("cpp_codes.cpp")

ibrx_2007_2018 <- read_excel("ibrx last price 2007 até 2018_2.xlsx") #### Reading the data
ibrx_2007_2018$Dates <- as.Date(ibrx_2007_2018$Dates) ## Setting the format of the dates column
ibrx_2007_2018 <- as.data.frame(ibrx_2007_2018) ## Transform in data_frame to easy handling
ibrx_2007_2018[,2:ncol(ibrx_2007_2018)] <- apply(ibrx_2007_2018[,2:ncol(ibrx_2007_2018)],2,as.numeric) ## Transform the data form char type to numeric type
ibrx_2007_2018 <- xts(ibrx_2007_2018[,-1],order.by = ibrx_2007_2018$Dates) ## transform in xts to easy handling with time series
ibrx_2007_2018_integridade <- apply(ibrx_2007_2018,2,
                                    function(x) sum(is.na(x))/nrow(ibrx_2007_2018))*100 ## Calculate the percentage of missing data
ibrx_2007_2018_70 <- ibrx_2007_2018[,names(ibrx_2007_2018_integridade[which(ibrx_2007_2018_integridade<10)])] ## Taking the coluns eith more than 90% of integrite
ibrx_2008_2017_70 <- ibrx_2007_2018_70["2008/2017"] ## subsetting to eliminate more missing data. 
ibrx_2008_2017_70 <- na.spline(ibrx_2008_2017_70) ## remove "NA's" spline method Missing values (NAs) are replaced by linear interpolation via approx or cubic spline interpolation via spline, respectively.
rm(list=c("ibrx_2007_2018","ibrx_2007_2018_70")) ## remove objects that will not be use 
Nomes <- colnames(ibrx_2008_2017_70) ## Taking the names of equity's
Nomes <- str_sub(Nomes, 1,6)
colnames(ibrx_2008_2017_70) <- gsub(" ","",Nomes)


window_test <- seq(1,nrow(ibrx_2008_2017_70),by=126)
ret_port <- as.list(NULL)
trading_return <- as.list(NULL)
select_port <- as.list(NULL)
retornos <- as.list(NULL)
time_window <- as.list(NULL)
ret_aux <- as.list(NULL)
threshold <- matrix(c(1,1.5,1,1.5,0.5,0.5,1,1),4,2)
formation_windown <- c(251,503,1007)
names(formation_windown) <- c("1Y","2Y","4Y")
for(pp in 1){
    for(p in 1){
      test_period <- window(ibrx_2008_2017_70,
                            start=time(ibrx_2008_2017_70)[window_test[p]],
                            end=if(is.na(time(ibrx_2008_2017_70)[window_test[p]+formation_windown[pp]])){break}
                            else{time(ibrx_2008_2017_70)[window_test[p]+formation_windown[pp]]})
      time_window[[p]] <- time(test_period)
      test_period_est <- as.data.frame(test_period)
      ### Estimating pairs
      print(paste0("Estimating the pairs from portfolio "
                   ,p,". Period from ",
                   min(time_window[[p]]), " to ",max(time_window[[p]])))
      
      no_cores <- detectCores() 
      cl <- makeCluster(no_cores)
      clusterExport(cl, "test_period_est")
      pares <- parLapply(cl,test_period_est,function(x) apply(test_period_est,2, 
                                                              function(y) if(x!=y){lm(x~y)}))
      stopCluster(cl)
      pares <- unlist(pares, recursive = F)
      pares <- pares[!sapply(pares,is.null)]
    }
}

no_cores <- detectCores() 
cl <- makeCluster(no_cores)
clusterExport(cl, "pares")
clusterEvalQ(cl, library(tseries))
pares_coint_test <- parLapply(cl,pares, 
                              function(x) if(tseries::adf.test(resid(x))$p.value < 0.05){x})
stopCluster(cl)

pares_coint_test <- pares_coint_test[!sapply(pares_coint_test,is.null)]
pairs_names <- gsub("\\.","vs ",names(pares_coint_test))
pairs_names <- gsub(" ","",pairs_names)
pairs_names <- gsub("vs"," vs ",pairs_names)
names(pares_coint_test) <- pairs_names

pares_fadf <- list(NULL)
for(i in 1:length(pairs_names)){
  pares_fadf[[i]] <- cbind(ibrx_2008_2017_70[,gsub(" ","",str_sub(pairs_names[i],end = 6))],
                   ibrx_2008_2017_70[,gsub(" ","",str_sub(pairs_names[i],start = -6))])
}
names(pares_fadf) <- pairs_names

pares_fcajo <- lapply(pares_fadf,function(x) tryCatch(summary(urca::ca.jo(x)), 
                                                      error=function(z) return(NULL)))
pares_fcajo <- pares_fcajo[!sapply(pares_fcajo,is.null)]







