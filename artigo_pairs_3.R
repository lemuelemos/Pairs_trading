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
colnames(ibrx_2008_2017_70) <- Nomes

### Setting the window of estimation
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
for(pp in 1:3){
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
  pares <- parLapply(cl,test_period_est,function(x) apply(test_period_est,2, 
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

#### Taking the pairs with R square greater than 0.5
print(paste0("Taking the pais with R2>0.5. Portfolio ",p))
paresR <- pares[sapply(pares,function(x) x$pvmr > 0.5)]
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

### Estimation of ocult states
print(paste0("Estimation of ocult states. Portfolio ",p))
paresRtestedM <- lapply(paresRtested, function(x) statehistory.pci(x))
betas <- ldply(paresRtested, function(x) x$beta)
#rm(paresRtested)

############### Normalizando O M
print(paste0("Normalizing the M. Portfolio",p))
Zm <- lapply(paresRtestedM, function(x) x$M/sd(x$M))
Zm <- as.data.frame(Zm)
colnames(Zm) <- gsub("\\."," ",names(Zm))
rm(paresRtestedM)

### sign of operations
print(paste0("Sign for operations - threshold[",tr[1],",",tr[2],"]. Portolio ",p))
sinal <- matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))
sinal[1,1:ncol(sinal)] <- "Fora"
sinal <- sncalc(ncol(Zm),nrow(Zm),as.matrix(Zm), tr=tr, sinal=sinal)
sinal<- as.data.frame(sinal) 
colnames(sinal) <- names(Zm)
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

invest <- data.frame(matrix(data = rep(1,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
retorno <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
ttf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
results <- NULL
par_est <- data.frame(NULL)
for(j in 1:length(parestrade)){
  par_est <- parestrade[[j]]
  results <- returcalc(as.matrix(sinal[,j]),
            as.matrix(par_est),betas = betas$beta_[j],invest = invest[,j])
  invest[,j] <- results[[1]]
  retorno[,j] <- results[[2]]
  ttf[,j] <- results[[2]]
}
colnames(invest) <- names(parestrade)
colnames(retorno) <- names(parestrade)
colnames(ttf) <- names(parestrade)

################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
print(paste0("Calculating return and sharpe. Portfolio ",p))
portret <- as.data.frame(matrix(data = rep(0,ncol(Zm)*3),ncol = ncol(Zm),nrow = 3))
for(f in 1:length(invest)){
  portret[1,f] <- ((invest[nrow(invest),f]/invest[1,f])-1)*100
  portret[2,f] <- sd(invest[,f])
  portret[3,f] <- portret[1,f]/portret[2,f]
  colnames(portret)[f] <- names(parestrade)[f]
}

portret <- t(portret) ## Retornos Totais
colnames(portret) <- c("Retorno Total","Desvio Padrão","Sharpe")
ret_port[[length(ret_port)+1]] <- portret ## Retornos Totais
names(ret_port)[p] <- paste0("Return Formation Period ",p)


ret_port[[length(ret_port)+1]] <- portret ## Retornos Totais
names(ret_port)[p] <- paste0("Return Formation Period ",p)

#####################################################
############### Periodo de Trading ##################
#####################################################
for(ii in c(1,3)){
  print("Trading Period")
  portsel <- sort(ret_port[[p]][,ii], decreasing = T)[1:20] ## Seleect the top 20 sharp's
  select_port[[p]] <- names(portsel) # testing if the window is complete
  trading_period <- window(ibrx_2008_2017_70, # Select the data
                            start = time(test_period)[1],
                            end = time(test_period)[nrow(test_period)]+175)
    
############# Estimating the pairs ################
print("Estimating the pairs")
parestrade <-as.list(NULL)
for(j in 1:length(portsel)){
  parestrade[[j]] <- cbind(trading_period[,grep(str_sub(names(portsel)[j],end=6),names(trading_period))],
                             trading_period[,grep(str_sub(names(portsel)[j],start=-6),names(trading_period))])
  names(parestrade)[j] <- names(portsel)[j]
}

### Estimating pairs
cl <- makeCluster(no_cores)
clusterExport(cl, "parestrade")
clusterEvalQ(cl, library(partialCI))
pares <- parLapply(cl,parestrade,function(x) fit.pci(x[,1],x[,2]))
stopCluster(cl)

pares <- pares[!sapply(pares,is.null)]
pares <- pares[!sapply(pares, function(x) is.na(x$rho.se))]

#### Estimating ocult states
print(paste0("Estimation of ocult states. Portfolio ",p))
paresRtested <- list(NULL)
paresRtested <- pares
paresRtestedM <- lapply(paresRtested, function(x) statehistory.pci(x))
betas <- ldply(paresRtested, function(x) x$beta)
#rm(paresRtested)

### Norm M's
print(paste0("Normalizing the M. Portfolio",p))
Zm <- lapply(paresRtestedM, function(x) x$M/sd(x$M))
Zm <- as.data.frame(Zm)

### sign of operations
print(paste0("Sign for operations - threshold[",tr[1],",",tr[2],"]. Portolio ",p))
sinal <- matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))
sinal[1,1:ncol(sinal)] <- "Fora"
sinal <- sncalc(ncol(Zm),nrow(Zm),as.matrix(Zm), tr=tr, sinal=sinal)
sinal<- as.data.frame(sinal) 
colnames(sinal) <- names(Zm)
sinal %>% mutate_if(is.factor,as.character) -> sinal

############# Return Calc
invest_t <- data.frame(matrix(data = rep(1,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
retorno_t <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
tt2 <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
results <- NULL
par_est <- data.frame(NULL)
for(j in 1:length(pares)){
  par_est <- parestrade[[j]]
  results <- returcalc(as.matrix(sinal[,j]),
                       as.matrix(par_est),betas = betas$beta_[j],invest = invest_t[,j])
  invest_t[,j] <- results$invest
  retorno_t[,j] <- results$retorno
  tt2[,j] <- results$tt
}
tt2[1,1:ncol(tt2)] <- "Fora"
colnames(invest_t) <- names(pares)
colnames(retorno_t) <- names(pares)
colnames(tt2) <- names(pares)



################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
print(paste0("Calculating return and sharpe. Portfolio ",p))
portret <- as.data.frame(matrix(data = rep(0,ncol(Zm)*3),ncol = ncol(Zm),nrow = 3))
for(f in 1:length(invest_t)){
  for(i in (formation_windown[pp]+2):nrow(tt2)){
    if(tt2[i,f] == "Abriu"){
  portret[1,f] <- ((invest_t[nrow(invest_t),f]/invest_t[i,f])-1)*100
  portret[2,f] <- sd(invest_t[i:nrow(invest_t),f])
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
  names(ret_aux)[1] <- paste0("Return Trading Period ",p, ". The top 20 Sharp")
} else{
  ret_aux[[2]] <- portret ## Retornos Totais
  names(ret_aux)[2] <- paste0("Return Trading Period ",p, ". The top 20 Return")
}
trading_return[[p]] <- ret_aux
  } 
}
#### Salvando Dados Importantes
    source('res_data_est.R')
 }
}

