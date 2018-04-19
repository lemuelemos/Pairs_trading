library(readxl)
library(xts)
library(partialCI)
library(stringr)
library(dplyr)
library(timeSeries)

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

window_test <- seq(1,nrow(ibrx_2008_2017_70),by=132)

for(p in seq_along(window_test)){
test_period <- window(ibrx_2008_2017_70,
                      start=time(ibrx_2008_2017_70)[window_test[p]],
                      end=if(is.na(time(ibrx_2008_2017_70)[window_test[p]+1007])){time(ibrx_2008_2017_70)[nrow(ibrx_2008_2017_70)]}
                      else{time(ibrx_2008_2017_70)[window_test[p]+1007]})
nport <- ncol(ibrx_2008_2017_70)*(ncol(ibrx_2008_2017_70)-1)
### Estimando modelo
portpairs <- list(NULL) ## List the will contain the pairs estimated
for(k in 1:ncol(test_period)){
  for(j in 1:ncol(test_period)){
    if(k != j){
      portpairs[[length(portpairs)+1]] <- fit.pci(test_period[,k], test_period[,j], 
                                          pci_opt_method=c("jp"),
                                          par_model=c("par"),
                                          lambda=0,robust=FALSE,nu=5,include_alpha=F)
      names(portpairs)[length(portpairs)] <- paste0(Nomes[k],"vs ",Nomes[j])
      print(paste0("Carteira ",length(portpairs)," de ",nport,". Período de Testes ",p))
     }
   }
 }
portpairs <- portpairs[!sapply(portpairs,is.null)] ### Retirando os valores vazios
portpairs <- portpairs[!sapply(portpairs, function(x) is.na(x$rho.se))] ### Retirando os pares com problemas de estimação
################# Retirando os pares com o R superior a 0.5 
print("Retirando os pares com o R superior a 0.5")
paresR <- list(NULL)
for(l in 1:length(portpairs)){
  if(portpairs[[l]]$pvmr > 0.5){
    paresR[l] <- portpairs[l]
    names(paresR)[l] <- names(portpairs)[l]
  }
}
paresR <- paresR[!sapply(paresR,is.null)] ### Retirando os valores vazios

################ Realizando o teste de significância para cointegração parcial
print("Realizando o teste de significância para cointegração parcial")
testepci <- list(NULL)
paresRtested <- list(NULL)
for(m in 1:length(paresR)){
  testepci[[m]] <- test.pci(paresR[[m]],alpha = 0.05, 
                            null_hyp = c("rw", "ar1"),
                            robust = FALSE, 
                            pci_opt_method = c("jp", "twostep"))
  names(testepci[m]) <- names(paresR)[m]
  print(paste0("Testando par",length(testepci)," de ",length(paresR)))
  if(testepci[[m]]$p.value[3] <= 0.05){
    paresRtested[m] <- paresR[m]
    names(paresRtested)[m] <- names(paresR)[m]
  }
}
paresRtested <- paresRtested[!sapply(paresRtested,is.null)] ### Retirando os valores vazios

##############################################################################

############# Estimando os Estados Ocultos
print("Estimando os Estados Ocultos")
paresRtestedM <- list(NULL)
for(n in 1: length(paresRtested)){
  paresRtestedM[[n]] <- statehistory.pci(paresRtested[[n]])
  names(paresRtestedM)[n] <- names(paresRtested)[n]
}

# Variável paresRtestedM já são os pares para teste backtest
############### Normalizando O M
print("Normalizando O M")
Zm <- as.list(NULL)
for(o in 1:length(paresRtestedM)){
  Zm[[o]] <- paresRtestedM[[o]]$M/paresRtested[[o]]$sigma_M.se
  names(Zm)[o] <- names(paresRtestedM)[o]
}
Zm <- as.data.frame(Zm) ### Tos os M's normalizados

############# Sinal Para as Operações
## Openright/OutRight = Operações em que o valor do resíduo é positivo
## OpenLeft/OutLeft = Operações em que o valor de resídou é negativo
## threshold's = [1,0.5]
print("Sinal Para as Operações - threshold's = [1,0.5]")
sinal <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
t <- c(1,0.5)
sinal[1,1:length(sinal)] <- "Fora"
colnames(sinal) <- names(Zm)
for(z in 1:length(Zm)){
  for(x in 2:nrow(Zm)){
    if(Zm[x,z] > t[1] && sinal[x-1,z] != "OpenLeft" || sinal[x-1,z] == "OpenRight" && Zm[x,z] > -t[2]){
      sinal[x,j] <- "OpenRight"
    } else if(Zm[x,z] < -t[1] && sinal[x-1,z] != "OpenRight" || sinal[x-1,z] == "OpenLeft" && Zm[x,z] < t[2]){
      sinal[x,z] <- "OpenLeft"
    } else if(Zm[x,z] < -t[2] && sinal[x-1,z] == "OpenRight"){
      sinal[x,z] <- "OutRight"
    } else if(Zm[x,z] > t[2] && sinal[x-1,z] == "OpenLeft"){
      sinal[x,z] <- "OutLeft"
    } else{
      sinal[x,z] <- "Fora"
    }
  }
}


llongi <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## llongi = Left Long Inicial
lshorti <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## lshorti = Left Short Inicial
llongf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## llongi = Left Long Final
lshortf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))  ## lshortf = Left Short Final
rlongi <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rlongi = Right Long Incial
rshorti <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rshorti = Right Short Incial
rlongf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rlongf = Right Long Final
rshortf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rshorti = Right Short Final
tt <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))


########## Loop para Pegar Preços de Entrada e Saída
print("Coletando Preços de Entrada e Saída. Portfólio",p)
for(j in 1:length(Zm)){
  for(i in 2:nrow(sinal)){
    if(sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "Fora" 
       || sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "OutLeft"
       || sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "OutRight"){
      rlongi[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],start = -6),
                                                  colnames(test_period))]
      rshorti[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],end = 6),
                                                   colnames(test_period))]
      colnames(rlongi)[j] <- paste0(names(paresRtestedM)[j],"RL")
      colnames(rshorti)[j] <- paste0(names(paresRtestedM)[j],"RS")
    } else if(sinal[i,j] == "OutRight" 
              && sinal[i-1,j] == "OpenRight"){
      rlongf[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],start = -6),
                                                  colnames(test_period))]
      rshortf[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],end = 6),
                                                   colnames(test_period))]
      colnames(rlongf)[j] <- paste0(names(paresRtestedM)[j],"RL")
      colnames(rshortf)[j] <- paste0(names(paresRtestedM)[j],"RS")
    } else if(sinal[i,j] == "OpenLeft" 
              && sinal[i-1,j] == "Fora" 
              || sinal[i,j] == "OpenLeft" 
              && sinal[i-1,j] == "OutRight"
              || sinal[i,j] == "OpenLeft"
              && sinal[i-1,j] == "OutLeft"){
      llongi[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],end = 6),
                                                  colnames(test_period))]
      lshorti[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],start = -6),
                                                   colnames(test_period))]
      colnames(llongi)[j] <- paste0(names(paresRtestedM)[j],"LL")
      colnames(lshorti)[j] <- paste0(names(paresRtestedM)[j],"LS")
    } else if(sinal[i,j] == "OutLeft" 
              && sinal[i-1,j] == "OpenLeft"){
      llongf[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],end = 6),
                                                  colnames(test_period))]
      lshortf[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],start = -6),
                                                   colnames(test_period))]
      colnames(llongf)[j] <- paste0(names(paresRtestedM)[j],"LL")
      colnames(lshortf)[j] <- paste0(names(paresRtestedM)[j],"LS")
    } else{
      tt[i,j] <- sinal[i,j]
    }
  }
}

##### Cálculo do Retorno Considerando o investimento de 1 Real.###################
print("Cálculo do Retorno Considerando o investimento de 1 Real. Portfólio",p)
invest <- data.frame(matrix(data = rep(1,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
retorno <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
portl <- as.vector(NULL)
ports <- as.vector(NULL)
porti <- as.vector(NULL)
portf <- as.vector(NULL)
longi <- as.vector(NULL)
shorti <- as.vector(NULL)
longf <- as.vector(NULL)
shortf <- as.vector(NULL) 
tt <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
for(j in 1:length(sinal)){
  for(i in 2:nrow(sinal)){
    #invest[i,j] <- invest[k-1,j]
    if(sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "Fora" 
       || sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "OutLeft"
       || sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "OutRight"){
      if(rlongi[i,j]*paresRtested[[j]]$beta/rshorti[i,j] < 1){
        portl <- -((rlongi[i,j]*paresRtested[[j]]$beta*invest[i-1,j])/rshorti[i,j]) 
        ports <- invest[i-1,j]
        longi <- rlongi[i,j]
        shorti <- rshorti[i,j]
        porti <- portl+ports
        tt[i,j] <- "Aberto"
        for(k in i:nrow(sinal)){
          if(sinal[k,j] == "OutRight" 
             && sinal[k-1,j] == "OpenRight"){
            longf <- rlongf[k,j]
            shortf <- rshortf[k,j]
            longf <- ((longf/longi)-1)+1
            shortf <- 1+((shortf/shorti)-1)
            portf <- -portl*longf - ports*shortf
            retorno[k,j] <- (porti+portf)/invest[k-1,j]
            invest[k,j] <- (((porti+portf)/invest[k-1,j])+1)*invest[k-1,j]
          }
        }
      } else{
        portl <- -invest[i-1,j]
        ports <- (rshorti[i,j]/(paresRtested[[j]]$beta*rlongi[i,j]))*invest[i-1,j]
        longi <- rlongi[i,j]
        shorti <- rshorti[i,j]
        porti <- portl+ports
        tt[i,j] <- "Aberto"
        for(k in i:nrow(sinal)){
          if(sinal[k,j] == "OutRight" 
             && sinal[k-1,j] == "OpenRight"){
            longf <- rlongf[k,j]
            shortf <- rshortf[k,j]
            longf <- ((longf/longi)-1)+1
            shortf <- 1+((shortf/shorti)-1)
            portf <- -portl*longf - ports*shortf
            retorno[k,j] <- (porti+portf)/invest[k-1,j]
            invest[k,j] <- (((porti+portf)/invest[k-1,j])+1)*invest[k-1,j]
          }
        } 
      } 
    } else if(sinal[i,j] == "OpenLeft" 
              && sinal[i-1,j] == "Fora" 
              || sinal[i,j] == "OpenLeft" 
              && sinal[i-1,j] == "OutRight"
              || sinal[i,j] == "OpenLeft"
              && sinal[i-1,j] == "OutLeft"){
      if(lshorti[i,j]*paresRtested[[j]]$beta/llongi[i,j] < 1){
        portl <- -invest[i-1,j]
        ports <- ((lshorti[i,j]*paresRtested[[j]]$beta)/llongi[i,j])*invest[i-1,j]
        longi <- llongi[i,j]
        shorti <- lshorti[i,j]
        porti <- portl+ports
        tt[i,j] <- "Aberto"
        for(k in i:nrow(sinal)){
          if(sinal[k,j] == "OutLeft" 
             && sinal[k-1,j] == "OpenLeft"){
            longf <- llongf[k,j]
            shortf <- lshortf[k,j]
            longf <- ((longf/longi)-1)+1
            shortf <- 1+((shortf/shorti)-1)
            portf <- -portl*longf - ports*shortf
            retorno[k,j] <- (porti+portf)/invest[k-1,j]
            invest[k,j] <- (((porti+portf)/invest[k-1,j])+1)*invest[k-1,j]
          }
        } 
      } else{
        portl <- -(llongi[i,j]/(paresRtested[[j]]$beta*lshorti[i,j]))*invest[i-1,j]
        ports <- invest[i-1,j]
        longi <- llongi[i,j]
        shorti <- lshorti[i,j]
        porti <- portl+ports
        tt[i,j] <- "Aberto"
        for(k in i:nrow(sinal)){
          if(sinal[k,j] == "OutLeft" 
             && sinal[k-1,j] == "OpenLeft"){
            longf <- llongf[k,j]
            shortf <- lshortf[k,j]
            longf <- ((longf/longi)-1)+1
            shortf <- 1+((shortf/shorti)-1)
            portf <- -portl*longf - ports*shortf
            retorno[k,j] <- (porti+portf)/invest[k-1,j]
            invest[k,j] <- (((porti+portf)/invest[k-1,j])+1)*invest[k-1,j]
          }
        } 
      } 
    } else{
      tt[i,j] <- "Aberto"
    }
  }
}


names(invest) <- names(paresRtested) ### Nomeando os Pares

################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
print("Calculando os Retornos Totais")
portret <- as.data.frame(matrix(data = rep(0,ncol(Zm)*3),ncol = ncol(Zm),nrow = 3))
for(f in 1:length(invest)){
  portret[1,f] <- (cumprod(invest[,f])[nrow(invest)]-1)*100
  portret[2,f] <- sd(cumprod(invest[,f]))
  portret[3,f] <- portret[1,j]/portret[2,f]
  colnames(portret)[f] <- names(paresRtestedM)[f]
}

colnames(portret) <- c("Retorno Total","Desvio Padrão","Sharpe")
ret_port <- as.list(NULL)
ret_port[[length(portpairs)+1]] <- t(portret) ## Retornos Totais
}

