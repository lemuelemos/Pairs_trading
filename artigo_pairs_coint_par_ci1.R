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

pares_adf <- parLapply(cl,
                       test_period,
                       function(x) apply(test_period,2,
                                         function(y) if(x!=y){egcm(x, y, urtest = "adf",p.value = 0.01)}))

pares_jo <- parLapply(cl,
                      test_period,
                      function(x) apply(test_period,2, 
                                        function(y) if(x!=y){egcm(x, y, urtest = "jo-e",p.value = 0.01)}))

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
nomes_adf <- gsub(" ", "", nomes_adf)
nomes_adf <- gsub("\\.", " vs ", nomes_adf)
nomes_adf <- data_frame(nomes_adf)
names(pares_coint_adf) <- nomes_adf$nomes_adf

nomes_jo <- names(pares_coint_jo)
nomes_jo <- gsub(" ", "", nomes_jo)
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

sinal_t_ci1 <- matrix(data = rep(0,ncol(Zm_ci1)*nrow(Zm_ci1)),ncol = ncol(Zm_ci1),nrow = nrow(Zm_ci1))
sinal_t_ci1[1,1:ncol(sinal_t_ci1)] <- "Fora"
tr <- c(1,0)
sinal_t_ci1 <- sncalc(ncol(Zm_ci1),nrow(Zm_ci1),as.matrix(Zm_ci1), tr=tr, sinal=sinal_t_ci1)
colnames(sinal_t_ci1) <- names(Zm_ci1)

################################################################
### Agruping the data by pair
print("Agruping the data by pair")

parestrade_ci1 <- list(NULL)
for(j in 1:ncol(sinal_t_ci1)){
  parestrade_ci1[[j]] <- cbind(test_period[,which(str_detect(colnames(test_period),
                                                             str_sub(colnames(sinal_t_ci1)[j],
                                                                     start=10)) == T)],
                               test_period[,which(str_detect(colnames(test_period),
                                                             str_sub(colnames(sinal_t_ci1)[j],
                                                                     end=6)) == T)])
  names(parestrade_ci1)[j] <- colnames(sinal_t_ci1)[j]
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
  results <- returcalc(as.matrix(sinal_t_ci1[,j]),
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

    }
}



















