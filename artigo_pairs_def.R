library(doParallel)
library(partialCI)
library(readxl)
library(xts)
library(stringr)
library(dplyr)
library(plyr)
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
colnames(ibrx_2008_2017_70) <- Nomes

#ibrx_2008_2017_70 <- ibrx_2008_2017_70[,1:20]

### Setting the window of estimation
estimation_method <- "fixed"
dir.create(file.path(getwd(), "resultados"))
###
<<<<<<< HEAD
resultados_por_tr <- list(NULL)
threshold <- matrix(c(1,1,0.5,0),2,2)
formation_windown <- c(125,251,503,1007)
names(formation_windown) <- c("6m","1Y","2Y","4Y")
trading_days <- c(180,360,720,1440)
for(pp in 1:length(formation_windown)){
=======

window_test <- seq(1,nrow(ibrx_2008_2017_70),by=126)
threshold <- matrix(c(1,1,0.5,0),2,2)
formation_windown <- c(251,503,1007)
names(formation_windown) <- c("4Y","2Y","1Y")
for(pp in 1:3){
>>>>>>> c17347e0da0d8941a874491409a9500d18a4dba8
  ret_port <- as.list(NULL)
  pairs_est <- list(NULL)
  trading_return <- as.list(NULL)
  select_port <- as.list(NULL)
  retornos <- as.list(NULL)
  time_window <- as.list(NULL)
  ret_aux <- as.list(NULL)
  trades <- list(NULL)
  returns <- list(NULL)
<<<<<<< HEAD
  window_test <- seq(1,nrow(ibrx_2008_2017_70),by=(formation_windown[pp]+1))
=======
  resultados_por_tr <- list(NULL)
>>>>>>> c17347e0da0d8941a874491409a9500d18a4dba8
  for(kk in 1:nrow(threshold)){
    tr <- threshold[kk,]
    for(p in seq_along(window_test)){
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
  clusterEvalQ(cl, library(partialCI))
  pares <- parLapply(cl,test_period_est,
                     function(x) apply(test_period_est,2, 
                                                      function(y) if(x!=y){fit.pci(x,y)}))
  stopCluster(cl)
  
  pares <- unlist(pares, recursive = F)
  pares <- pares[!sapply(pares,is.null)]
  pares <- pares[!sapply(pares, function(x) is.na(x$rho.se))]
  names(pares) <- gsub("TIET11vs","TIET11 vs",
                       paste0(str_sub(names(pares), 1,6),"vs ", 
                              str_sub(names(pares), 8,13)))
pares <- pares[!sapply(pares,is.null)] ### Retirando os valores vazios
pares <- pares[!sapply(pares, function(x) is.na(x$rho.se))] ### Retirando os pares com problemas de estimação

#saveRDS(pares,file=paste0(getwd(),"/resultados/pair_",
                          #min(time_window[[p]]),"_to_",
                          #max(time_window[[p]]),"_portfolio",p,"_fmw_",
                          #names(formation_windown)[pp],"_tr_(",tr[1],",",tr[2],")"))

pairs_est[[p]][[1]] <- pares
#### Taking the pairs with R square greater than 0.5
print(paste0("Taking the pais with R2>0.5. Portfolio ",p))
paresR <- pares[sapply(pares,function(x) x$pvmr > 0.5)]
paresR <- paresR[sapply(paresR,function(x) x$rho > 0.5)]
rm(pares)

### Testing partial Cointegration
print(paste0("Testing for partial coitegration. Portfolio ",p))
cl <- makeCluster(no_cores)
clusterExport(cl, "paresR")
clusterEvalQ(cl, library(partialCI))
paresRtested <- paresR[parSapply(cl,paresR, 
                                 FUN = function(x) which.hypothesis.pcitest(test.pci(x))=="PCI")]
stopCluster(cl)
rm(paresR)

#saveRDS(paresRtested,file=paste0(getwd(),"/resultados/pairRtested_",
                          #min(time_window[[p]]),"_to_",
                          #max(time_window[[p]]),"_portfolio",p,"_fmw_",
                          #names(formation_windown)[pp],"_tr_(",tr[1],",",tr[2],")"))

pairs_est[[p]][[2]] <- paresRtested
### Estimation of ocult states
print(paste0("Estimation of ocult states. Portfolio ",p))
paresRtestedM <- lapply(paresRtested, function(x) statehistory.pci(x))
betas_formation <- ldply(paresRtested, function(x) x$beta)
colnames(betas_formation) <- c("Pares","betas")
#rm(paresRtested)

############### Normalizando O M
print(paste0("Normalizing the M. Portfolio",p))
Zm_fornation <- lapply(paresRtestedM, function(x) x$M/sd(x$M))
Zm_fornation <- as.data.frame(Zm_fornation)
colnames(Zm_fornation) <- gsub("\\."," ",names(Zm_fornation))
rm(paresRtestedM)

### sign of operations
print(paste0("Sign for operations - threshold[",tr[1],",",tr[2],"]. Portolio ",p))
sinal <- matrix(data = rep(0,ncol(Zm_fornation)*nrow(Zm_fornation)),ncol = ncol(Zm_fornation),nrow = nrow(Zm_fornation))
sinal[1,1:ncol(sinal)] <- "Fora"
sinal <- sncalc(ncol(Zm_fornation),nrow(Zm_fornation),as.matrix(Zm_fornation), tr=tr, sinal=sinal)
sinal<- as.data.frame(sinal) 
colnames(sinal) <- names(Zm_fornation)
sinal %>% mutate_if(is.factor,as.character) -> sinal
#as.xts(sinal, order.by = time(test_period))

############# Return Calc
print(paste0("Return Calc. Portfolio",p))
parestrade <- list(NULL)
for(j in 1:length(sinal)){
  parestrade[[j]] <- cbind(test_period[,str_sub(names(sinal)[j],end=6)],
                           test_period[,str_sub(names(sinal)[j],start=-6)])
  names(parestrade)[j] <- names(sinal)[j]
  colnames(parestrade[[j]]) <- cbind(str_sub(names(sinal)[j],end=6),
                                     str_sub(names(sinal)[j],start=-6))
}

invest <- data.frame(matrix(data = rep(1,ncol(Zm_fornation)*nrow(Zm_fornation)),ncol = ncol(Zm_fornation),nrow = nrow(Zm_fornation)))
retorno <- data.frame(matrix(data = rep(0,ncol(Zm_fornation)*nrow(Zm_fornation)),ncol = ncol(Zm_fornation),nrow = nrow(Zm_fornation)))
ttf <- data.frame(matrix(data = rep(0,ncol(Zm_fornation)*nrow(Zm_fornation)),ncol = ncol(Zm_fornation),nrow = nrow(Zm_fornation)))
results <- NULL
par_est <- data.frame(NULL)
for(j in 1:length(parestrade)){
  par_est <- parestrade[[j]]
  results <- returcalc(as.matrix(sinal[,j]),
            as.matrix(par_est),betas = betas_formation$betas[j],invest = invest[,j])
  invest[,j] <- results[[1]]
  retorno[,j] <- results[[2]]
  ttf[,j] <- results[[2]]
}
colnames(invest) <- names(parestrade)
colnames(retorno) <- names(parestrade)
colnames(ttf) <- names(parestrade)

################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
print(paste0("Calculating return and sharpe. Portfolio ",p))
portret <- as.data.frame(matrix(data = rep(0,ncol(Zm_fornation)*3),ncol = ncol(Zm_fornation),nrow = 3))
for(f in 1:length(invest)){
  portret[1,f] <- ((invest[nrow(invest),f]/invest[1,f])-1)*100
  portret[2,f] <- sd(invest[,f])
  portret[3,f] <- portret[1,f]/portret[2,f]
  colnames(portret)[f] <- names(parestrade)[f]
}

portret <- t(portret) ## Retornos Totais
colnames(portret) <- c("Retorno Total","Desvio Padrão","Sharpe")
ret_port[[p]] <- portret ## Retornos Totais
names(ret_port)[p] <- paste0("Return Formation Period ",p)


#####################################################
############### Periodo de Trading ##################
#####################################################
if(estimation_method == "fixed"){
  source("trading_period_fixed_window.R")
} else {source("trading_period_rolling_window.R")}
}
#### Salvando Dados Importantes
    source('res_data_est.R')
    saveRDS(pairs_est,file = paste0(getwd(),"/resultados/pairs_fmw_",
                                    names(formation_windown)[pp],"_tr(",tr[1],",",tr[2],")"))
    }
}

