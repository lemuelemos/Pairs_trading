library(partialCI)
library(readxl)
library(timeSeries)
library(zoo)
library(xts)
library(dplyr)
library(stringr)
library(BatchGetSymbols)
library(quantmod)
library(doParallel)
library(plyr)
library(timetk)
library(Rcpp)
############ Comentários sobre andamento das modificações####################
# Lembrar que o código responsável pela captura dos preços está correto, porém, ao calcular os retornos, 
# o sinal de operação obtemos uma situação em que está a abertura de uma operação. Duas alternativas:
# 1- Modificar o gerador de sinal para a situação de estar no último dado setar para "fora"
# 2- Comtemplar essa situação no pŕoprio algorítimo de pegar os preços.

###########################################################################
sourceCpp("cpp_codes.cpp")

##### Taking tickers that compound IBOV 
print("Check the ibov asset's")
Ativos <- paste0(BatchGetSymbols::GetIbovStocks()$tickers, '.SA')

# Time Window to download the data
first.date <- Sys.Date() - 540 # Dados de 4 anos mais 6 meses para o período de trading
last.date <- Sys.Date() - 1

#set folder for cache system
pasta_dos_dados <- 'BGS_CACHE'

## Downloading data from yahoo 
print("Downloading data")
ativosl <- BatchGetSymbols(tickers = Ativos, first.date, last.date,
                           cache.folder = pasta_dos_dados, do.cache = TRUE,thresh.bad.data = 0.9)

ativosw <- reshape.wide(ativosl$df.tickers) #### Changing the arrangement of the data to wide format
dados_estimacao <- xts(ativosw$price.close[,-1], order.by = ativosw$price.adjusted$ref.date) ## Transform in xts 
dados_estimacao <-  dados_estimacao[,apply(!is.na(dados_estimacao),2,all)] ## Removing Missing Data
#dados_estimacao <- dados_estimacao[,!names(dados_estimacao)=="ENBR3.SA",drop=F]
periodo_teste <- c(time(dados_estimacao)[1],time(dados_estimacao)[nrow(dados_estimacao)]-180) ### Setting the períod 
dados_estimacao_teste <- window(dados_estimacao, start=periodo_teste[1], end=periodo_teste[2]) ### The formation períod
dados_estimacao_teste <- tk_tbl(dados_estimacao_teste)
dados_estimacao <-  tk_tbl(dados_estimacao)
Nomes <- colnames(dados_estimacao_teste)
pares <- list(NULL)

print("Estimating the pairs")
no_cores <- detectCores() 
cl <- makeCluster(no_cores)
clusterExport(cl, "dados_estimacao_teste")
clusterEvalQ(cl, library(partialCI))
pares <- parLapply(cl,dados_estimacao_teste[,-1],function(x) apply(dados_estimacao_teste[,-1],2, 
                                                        function(y) if(x!=y){fit.pci(x,y)}))
stopCluster(cl)

pares <- unlist(pares, recursive = F)
pares <- pares[!sapply(pares,is.null)]
pares <- pares[!sapply(pares, function(x) is.na(x$rho.se))]

################# Retirando os pares com o R superior a 0.5 
print("Taking the pais with R2>0.5. Portfolio ")
paresR <- pares[sapply(pares,function(x) x$pvmr > 0.7)]
paresR <- paresR[sapply(paresR,function(x) x$rho > 0.5)]
paresR <- paresR[!sapply(paresR,is.null)] ### Retirando os valores vazios

### Testing partial Cointegration
print("Testing for partial coitegration. Portfolio ")
cl <- makeCluster(no_cores)
clusterExport(cl, "paresR")
clusterEvalQ(cl, library(partialCI))
paresRtested <- paresR[parSapply(cl,paresR, 
                                 FUN = function(x) which.hypothesis.pcitest(test.pci(x))=="PCI")]
stopCluster(cl)
############## Estimando os Estados Ocultos
print("Estimation of ocult states. Portfolio ")
paresRtestedM <- lapply(paresRtested, function(x) statehistory.pci(x))
betas <- ldply(paresRtested, function(x) x$beta)

# Variável paresRtestedM já são os pares para teste backtest
############### Normalizando O M
print("Normalizing the M. Portfolio")
Zm <- lapply(paresRtestedM, function(x) x$M/sd(x$M))
Zm <- as.data.frame(Zm)
colnames(Zm) <- gsub("\\."," ",names(Zm))

############# Sinal Para as Operações
## Openright/OutRight = Operações em que o valor do resíduo é positivo
## OpenLeft/OutLeft = Operações em que o valor de resídou é negativo
## threshold's = [1,0.5]
print("Generating Signal")
sinal <- matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))
sinal[1,1:ncol(sinal)] <- "Fora"
tr <- c(1,0.5)
sinal <- sncalc(ncol(Zm),nrow(Zm),as.matrix(Zm), tr=tr, sinal=sinal)
sinal<- as.data.frame(sinal) 
colnames(sinal) <- names(Zm)
sinal %>% mutate_if(is.factor,as.character) -> sinal

############# Return Calc
print("Return Calc. Portfolio")
parestrade <- list(NULL)
for(j in 1:length(sinal)){
  parestrade[[j]] <- cbind(dados_estimacao_teste[,paste0(str_sub(names(sinal)[j],end=5),".SA")],
                           dados_estimacao_teste[,paste0(str_sub(names(sinal)[j],
                                                                 start=10,end = -4),".SA")])
  names(parestrade)[j] <- names(sinal)[j]
}

invest_f <- data.frame(matrix(data = rep(1,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
retorno_f <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
ttf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
results <- NULL
par_est <- data.frame(NULL)
for(j in 1:length(parestrade)){
  par_est <- parestrade[[j]]
  results <- returcalc(as.matrix(sinal[,j]),
                       as.matrix(par_est),betas = betas$beta_[j],invest = invest_f[,j])
  invest_f[,j] <- results[[1]]
  retorno_f[,j] <- results[[2]]
  ttf[,j] <- results[[3]]
}
colnames(invest_f) <- names(parestrade)
colnames(retorno_f) <- names(parestrade)
colnames(ttf) <- names(parestrade)

################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
print("Calculating return and sharpe. Portfolio ")
portret_f <- as.data.frame(matrix(data = rep(0,ncol(invest_f)*3),ncol = ncol(invest_f),nrow = 3))
for(f in 1:length(invest_f)){
  portret_f[1,f] <- (tail(invest_f[,f],1)-1)*100
  portret_f[2,f] <- sd(invest_f[,f])
  portret_f[3,f] <- portret_f[1,f]/portret_f[2,f]
  colnames(portret_f)[f] <- names(parestrade)[f]
}

portret_f <- t(portret_f) ## Retornos Totais
colnames(portret_f) <- c("Retorno Total","Desvio Padrão","Sharpe")
portsel <- sort(portret_f[,3],decreasing = T)[1:20]

#########################################################
################## Trading Period #######################
#########################################################
print("Trading Period")
parestrade_t <- list(NULL)
for(j in 1:length(portsel)){
  parestrade_t[[j]] <- cbind(dados_estimacao[,paste0(str_sub(names(portsel)[j],end = 5),".SA")],
                           dados_estimacao[,paste0(str_sub(names(portsel)[j],start=10,end = -4),".SA")])
  names(parestrade_t)[j] <- names(portsel)[j]
}

pares_t <- list(NULL)
Zm_t <- Zm %>% select(names(portsel))

print("Estimating the pairs")
for(i in (nrow(dados_estimacao_teste)+1):nrow(dados_estimacao)){
cl <- makeCluster(no_cores)
clusterExport(cl, "parestrade_t")
clusterExport(cl, "i")
clusterEvalQ(cl, library(partialCI))
 pares_t  <- parLapply(cl,parestrade_t,function(x) fit.pci(x[1:i,1],x[1:i,2]))

 pares_t <- pares_t[!sapply(pares_t,is.null)]
 pares_t <- pares_t[!sapply(pares_t, function(x) is.na(x$rho.se))]
 
 cat("\r", i, "of", nrow(dados_estimacao))
 pares_tM <- lapply(pares_t, function(x) statehistory.pci(x))
 Z_norm <- lapply(pares_tM, function(x) x$M/sd(x$M))
 Z_norm <- as.data.frame(Z_norm)
 Zm_t[i,] <- Z_norm[nrow(Z_norm),]
 stopCluster(cl)
}

############# Sinal Para as Operações
## Openright/OutRight = Operações em que o valor do resíduo é positivo
## OpenLeft/OutLeft = Operações em que o valor de resídou é negativo
## threshold's = [1,0.5]
print("Generating Signal")
sinal_t <- matrix(data = rep(0,ncol(Zm_t)*nrow(Zm_t)),ncol = ncol(Zm_t),nrow = nrow(Zm_t))
sinal_t[1,1:ncol(sinal_t)] <- "Fora"
tr <- c(1,0.5)
sinal_t <- sncalc(ncol(Zm_t),nrow(Zm_t),as.matrix(Zm_t), tr=tr, sinal=sinal_t)
sinal_t<- as.data.frame(sinal_t) 
colnames(sinal_t) <- names(Zm_t)
sinal_t %>% mutate_if(is.factor,as.character) -> sinal_t

########## Return ############
print("Return Calc. Portfolio")
invest_t <- data.frame(matrix(data = rep(1,ncol(Zm_t)*nrow(Zm_t)),ncol = ncol(Zm_t),nrow = nrow(Zm_t)))
retorno_t <- data.frame(matrix(data = rep(0,ncol(Zm_t)*nrow(Zm_t)),ncol = ncol(Zm_t),nrow = nrow(Zm_t)))
ttt <- data.frame(matrix(data = rep(0,ncol(Zm_t)*nrow(Zm_t)),ncol = ncol(Zm_t),nrow = nrow(Zm_t)))
results <- NULL
par_est <- data.frame(NULL)
for(j in 1:length(parestrade_t)){
  par_est <- parestrade_t[[j]]
  results <- returcalc(as.matrix(sinal_t[,j]),
                       as.matrix(par_est),betas = betas$beta_[j],invest = invest_t[,j])
  invest_t[,j] <- results[[1]]
  retorno_t[,j] <- results[[2]]
  ttt[,j] <- results[[3]]
}
colnames(invest_t) <- names(parestrade_t)
colnames(retorno_t) <- names(parestrade_t)
colnames(ttt) <- names(parestrade_t)

################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
print("Calculating return and sharpe. Portfolio ")
portret_t <- as.data.frame(matrix(data = rep(0,ncol(Zm_t)*3),ncol = ncol(Zm_t),nrow = 3))
for(f in 1:length(invest_t)){
  portret_t[1,f] <- ((invest_t[nrow(dados_estimacao),f]/invest_t[nrow(dados_estimacao_teste)+1,f])-1)*100
  portret_t[2,f] <- sd(invest_t[(nrow(dados_estimacao_teste)+1):nrow(dados_estimacao),f])
  portret_t[3,f] <- portret_t[1,f]/portret_t[2,f]
  colnames(portret_t)[f] <- names(parestrade_t)[f]
}

portret_t <- t(portret_t) ## Retornos Totais
colnames(portret_t) <- c("Retorno Total","Desvio Padrão","Sharpe")







