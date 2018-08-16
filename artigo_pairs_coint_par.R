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

window_test <- seq(1,nrow(ibrx_2008_2017_70),by=126)
no_cores <- detectCores()
cl <- makeCluster(no_cores)
for(p in 1){
  test_period <- window(ibrx_2008_2017_70,
                        start=time(ibrx_2008_2017_70)[window_test[p]],
                        end=if(is.na(time(ibrx_2008_2017_70)[window_test[p]+1007])){time(ibrx_2008_2017_70)[nrow(ibrx_2008_2017_70)]}
                        else{time(ibrx_2008_2017_70)[window_test[p]+1007]})
test_period <- as.data.frame(test_period)
#colnames(test_period) <- gsub(" ", "", Nomes)
clusterExport(cl, "test_period")
clusterEvalQ(cl, library(egcm))
####################################
print("Estimating Pairs")
pares_pp <- parLapply(cl,test_period,function(x) apply(test_period,2, 
                                                    function(y) if(x!=y){egcm(x, y, urtest = "pp", p.value = 0.01)}))

pares_pgff <- parLapply(cl,test_period,function(x) apply(test_period,2, 
                                                       function(y) if(x!=y){egcm(x, y, urtest = "pgff",p.value = 0.01)}))

pares_jo <- parLapply(cl,test_period,function(x) apply(test_period,2, 
                                                         function(y) if(x!=y){egcm(x, y, urtest = "jo-e",p.value = 0.01)}))

pares_adf <- parLapply(cl,test_period,function(x) apply(test_period,2, 
                                                       function(y) if(x!=y){egcm(x, y, urtest = "adf",p.value = 0.01)}))

pares_pp <- unlist(pares_pp, recursive = F)
pares_pp <- pares_pp[!sapply(pares_pp,is.null)]
pares_pgff <- unlist(pares_pgff, recursive = F)
pares_pgff <- pares_pgff[!sapply(pares_pgff,is.null)]
pares_jo <- unlist(pares_jo, recursive = F)
pares_jo <- pares_jo[!sapply(pares_jo,is.null)]
pares_adf <- unlist(pares_adf, recursive = F)
pares_adf <- pares_adf[!sapply(pares_adf,is.null)]

}
stopCluster(cl)

############# Teste de Pares #####################
print("Pairs Testing")
pares_coint_adf <- pares_adf[sapply(pares_adf,is.cointegrated)]
pares_coint_jo <- pares_jo[sapply(pares_jo,is.cointegrated)]
pares_coint_pp <- pares_pp[sapply(pares_pp, is.cointegrated)]
pares_coint_pgff <- pares_pgff[sapply(pares_pgff,is.cointegrated)]



rm(pares_pp)
rm(pares_pgff)
rm(pares_jo)
rm(pares_adf)

nomes_adf <- names(pares_coint_adf)
nomes_adf <- gsub(" ", "", nomes_adf)
nomes_adf <- gsub("\\.", " vs ", nomes_adf)
nomes_adf <- data_frame(nomes_adf)
names(pares_coint_adf) <- nomes_adf$nomes_adf

nomes_jo <- names(pares_coint_jo)
nomes_jo <- gsub(" ", "", nomes_jo)
nomes_jo <- gsub("\\.", " vs ", nomes_jo)
nomes_jo <- data_frame(nomes_jo)
names(pares_coint_jo) <- nomes_jo$nomes_jo

nomes_pp <- names(pares_coint_pp)
nomes_pp <- gsub(" ", "", nomes_pp)
nomes_pp <- gsub("\\.", " vs ", nomes_pp)
nomes_pp <- data_frame(nomes_pp)
names(pares_coint_pp) <- nomes_pp$nomes_pp

nomes_pgff <- names(pares_coint_pgff)
nomes_pgff <- gsub(" ", "", nomes_pgff)
nomes_pgff <- gsub("\\.", " ", nomes_pgff)
nomes_pgff <- data_frame(nomes_pgff)
names(pares_coint_pgff) <- nomes_pgff$nomes_pgff


pares_coint_ci1 <- nomes_adf %>% dplyr::filter(nomes_adf %in% nomes_jo$nomes_jo)
pares_coint_ci2 <- nomes_pp %>% dplyr::filter(nomes_pp %in% nomes_pgff$nomes_pgff)

pares_coint_ci1s <- pares_coint_adf[pares_coint_ci1$nomes_adf]
pares_coint_ci2s <- pares_coint_pp[pares_coint_ci2$nomes_pp]

Zm_ci1 <- sapply(pares_coint_ci1s, function(x) x$residuals/x$residuals.sd)
Zm_ci1 <- data.frame(Zm_ci1)
colnames(Zm_ci1) <- gsub("\\.", " ", colnames(Zm_ci1))
Zm_ci2 <- sapply(pares_coint_ci2s, function(x) x$residuals/x$residuals.sd)
Zm_ci2 <- data.frame(Zm_ci2)
colnames(Zm_ci2) <- gsub("\\.", " ", colnames(Zm_ci2))

sinal_t_ci1 <- matrix(data = rep(0,ncol(Zm_ci1)*nrow(Zm_ci1)),ncol = ncol(Zm_ci1),nrow = nrow(Zm_ci1))
sinal_t_ci1[1,1:ncol(sinal_t_ci1)] <- "Fora"
tr <- c(1,0)
sinal_t_ci1 <- sncalc(ncol(Zm_ci1),nrow(Zm_ci1),as.matrix(Zm_ci1), tr=tr, sinal=sinal_t_ci1)
colnames(sinal_t_ci1) <- names(Zm_ci1)

sinal_t_ci2 <- matrix(data = rep(0,ncol(Zm_ci2)*nrow(Zm_ci2)),ncol = ncol(Zm_ci2),nrow = nrow(Zm_ci2))
sinal_t_ci2[1,1:ncol(sinal_t_ci2)] <- "Fora"
tr <- c(1,0)
sinal_t_ci2 <- sncalc(ncol(Zm_ci2),nrow(Zm_ci2),as.matrix(Zm_ci2), tr=tr, sinal=sinal_t_ci2)
colnames(sinal_t_ci2) <- names(Zm_ci2)

parestrade <- list(NULL)
for(j in 1:ncol(sinal_t_ci1)){
  parestrade[[j]] <- cbind(test_period[,str_sub(colnames(sinal_t_ci1)[j],
                                                       end=5)],
                           test_period[,str_sub(colnames(sinal_t_ci1)[j],
                                                                 start=10)])
  names(parestrade)[j] <- names(sinal_t_ci1)[j]
}








