library(readxl)
library(xts)
library(stringr)
library(dplyr)
library(timeSeries)
library(egcm)
library(parallel)

##### Import data and cleaning NA's

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

window_test <- seq(1,nrow(ibrx_2008_2017_70),by=126)
no_cores <- detectCores()
cl <- makeCluster(no_cores)
for(p in 1){
  test_period <- window(ibrx_2008_2017_70,
                        start=time(ibrx_2008_2017_70)[window_test[p]],
                        end=if(is.na(time(ibrx_2008_2017_70)[window_test[p]+1007])){time(ibrx_2008_2017_70)[nrow(ibrx_2008_2017_70)]}
                        else{time(ibrx_2008_2017_70)[window_test[p]+1007]})
test_period <- as.data.frame(test_period)
clusterExport(cl, "test_period")
clusterEvalQ(cl, library(egcm))
####################################
print("Estimating Pairs")
pares_pp <- parLapply(cl,test_period,function(x) apply(test_period,2, 
                                                    function(y) if(x!=y){egcm(x, y, urtest = "pp")}))

pares_pgff <- parLapply(cl,test_period,function(x) apply(test_period,2, 
                                                       function(y) if(x!=y){egcm(x, y, urtest = "pgff")}))

pares_pp <- unlist(pares_pp, recursive = F)
pares_pp <- pares_pp[!sapply(pares_pp,is.null)]
pares_pgff <- unlist(pares_pgff, recursive = F)
pares_pgff <- pares_pgff[!sapply(pares_pgff,is.null)]

}
stopCluster(cl)

############# Teste de Pares #####################
print("Pairs Testing")
pares_coint_pp <- pares_pp[sapply(pares_pp, is.cointegrated)]
pares_coint_pgff <- pares_pgff[sapply(pares_pgff,is.cointegrated)]

rm(pares_pp)
rm(pares_pgff)

nomes_pp <- names(pares_coint_pp)
nomes_pp <- gsub(" ", "", nomes_pp)
nomes_pp <- gsub("\\.", " vs ", nomes_pp)
nomes_pp <- data_frame(nomes_pp)

nomes_pgff <- names(pares_coint_pgff)
nomes_pgff <- gsub(" ", "", nomes_pgff)
nomes_pgff <- gsub("\\.", " vs ", nomes_pgff)
nomes_pgff <- data_frame(nomes_pgff)


pares_coint <- nomes_pp %>% dplyr::filter(nomes_pp %in% nomes_pgff$nomes_pgff)
names(pares_coint_pgff) <- nomes_pgff$nomes_pgff
