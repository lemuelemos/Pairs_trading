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

window_test <- seq(1,nrow(ibrx_2008_2017_70),by=180)

for(i in seq_along(window_test)){
test_period <- window(ibrx_2008_2017_70,
                      start=time(ibrx_2008_2017_70)[window_test[i]],
                      end=if(is.na(time(ibrx_2008_2017_70)[window_test[i]+1439])){time(ibrx_2008_2017_70)[nrow(ibrx_2008_2017_70)]}
                      else{time(ibrx_2008_2017_70)[window_test[i]+1439]})

### Estimando modelo
portpairs <- list(NULL) ## List the will contain the pairs estimated
for(i in 1:ncol(test_period)){
  for(j in 1:ncol(test_period)){
    if(i != j){
      portpairs[[length(portpairs)+1]] <- fit.pci(test_period[,i], test_period[,j], 
                                          pci_opt_method=c("jp"),
                                          par_model=c("par"),
                                          lambda=0,robust=FALSE,nu=5,include_alpha=F)
      names(portpairs)[length(portpairs)] <- paste0(Nomes[i],"vs",Nomes[j])
     }
   }
 }

portpairs <- portpairs[!sapply(portpairs,is.null)] ### Retirando os valores vazios

################# Retirando os pares com o R superior a 0.5 

paresR <- list(NULL)
for(i in 1:length(portpairs)){
  if(portpairs[[i]]$pvmr > 0.5){
    paresR[i] <- portpairs[i]
    names(paresR)[i] <- names(portpairs)[i]
  }
}
paresR <- paresR[!sapply(paresR,is.null)] ### Retirando os valores vazios

################ Realizando o teste de significância para cointegração parcial

testepci <- list(NULL)
paresRtested <- list(NULL)
for(i in 1:length(paresR)){
  testepci[[i]] <- test.pci(paresR[[i]],alpha = 0.05, 
                            null_hyp = c("rw", "ar1"),
                            robust = FALSE, 
                            pci_opt_method = c("jp", "twostep"))
  names(testepci[i]) <- names(paresR)[i]
  if(testepci[[i]]$p.value[3] <= 0.05){
    paresRtested[i] <- paresR[i]
    names(paresRtested)[i] <- names(paresR)[i]
  }
}

paresRtested <- paresRtested[!sapply(paresRtested,is.null)] ### Retirando os valores vazios

##############################################################################

############# Estimando os Estados Ocultos

paresRtestedM <- list(NULL)
for(i in 1: length(paresRtested)){
  paresRtestedM[[i]] <- statehistory.pci(paresRtested[[i]])
  names(paresRtestedM)[i] <- names(paresRtested)[i]
}

# Variável paresRtestedM já são os pares para teste backtest
############### Normalizando O M

Zm <- as.list(NULL)
for(i in 1:length(paresRtestedM)){
  Zm[[i]] <- paresRtestedM[[i]]$M/paresRtested[[i]]$sigma_M.se
  names(Zm)[i] <- names(paresRtestedM)[i]
}

Zm <- as.data.frame(Zm) ### Tos os M's normalizados

############# Sinal Para as Operações
## Openright/OutRight = Operações em que o valor do resíduo é positivo
## OpenLeft/OutLeft = Operações em que o valor de resídou é negativo
## threshold's = [1,0.5]

sinal <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
t <- c(1,0.5)
sinal[1,1:length(sinal)] <- "Fora"
colnames(sinal) <- names(Zm)
for(j in 1:length(Zm)){
  for(i in 2:nrow(Zm)){
    if(Zm[i,j] > t[1] && sinal[i-1,j] != "OpenLeft" || sinal[i-1,j] == "OpenRight" && Zm[i,j] > -t[2]){
      sinal[i,j] <- "OpenRight"
    } else if(Zm[i,j] < -t[1] && sinal[i-1,j] != "OpenRight" || sinal[i-1,j] == "OpenLeft" && Zm[i,j] < t[2]){
      sinal[i,j] <- "OpenLeft"
    } else if(Zm[i,j] < -t[2] && sinal[i-1,j] == "OpenRight"){
      sinal[i,j] <- "OutRight"
    } else if(Zm[i,j] > t[2] && sinal[i-1,j] == "OpenLeft"){
      sinal[i,j] <- "OutLeft"
    } else{
      sinal[i,j] <- "Fora"
    }
  }
 }
########## Loop para Pegar Preços de Entrada e Saída
for(j in 1:length(Zm)){
  for(i in 2:nrow(sinal)){
    if(sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "Fora" 
       || sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "OutLeft"
       || sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "OutRight"){
      rlongi[i,j] <- parestrade[[j]][i,2]
      rshorti[i,j] <- parestrade[[j]][i,1]
      colnames(rlongi)[j] <- paste0(names(paresRtestedM)[j],"RL")
      colnames(rshorti)[j] <- paste0(names(paresRtestedM)[j],"RS")
    } else if(sinal[i,j] == "OutRight" 
              && sinal[i-1,j] == "OpenRight"){
      rlongf[i,j] <- parestrade[[j]][i,2]
      rshortf[i,j] <- parestrade[[j]][i,1]
      colnames(rlongf)[j] <- paste0(names(paresRtestedM)[j],"RL")
      colnames(rshortf)[j] <- paste0(names(paresRtestedM)[j],"RS")
    } else if(sinal[i,j] == "OpenLeft" 
              && sinal[i-1,j] == "Fora" 
              || sinal[i,j] == "OpenLeft" 
              && sinal[i-1,j] == "OutRight"
              || sinal[i,j] == "OpenLeft"
              && sinal[i-1,j] == "OutLeft"){
      llongi[i,j] <- parestrade[[j]][i,1]
      lshorti[i,j] <- parestrade[[j]][i,2]
      colnames(llongi)[j] <- paste0(names(paresRtestedM)[j],"LL")
      colnames(lshorti)[j] <- paste0(names(paresRtestedM)[j],"LS")
    } else if(sinal[i,j] == "OutLeft" 
              && sinal[i-1,j] == "OpenLeft"){
      llongf[i,j] <- parestrade[[j]][i,1]
      lshortf[i,j] <- parestrade[[j]][i,2]
      colnames(llongf)[j] <- paste0(names(paresRtestedM)[j],"LL")
      colnames(lshortf)[j] <- paste0(names(paresRtestedM)[j],"LS")
    } else{
      tt[i,j] <- sinal[i,j]
    }
  }
}

##### Cálculo do Retorno Considerando o investimento de 1 Real.###################

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

portret <- as.data.frame(matrix(data = rep(0,60),ncol = ncol(Zm),nrow = 3))
for(j in 1:length(invest)){
  portret[1,j] <- (cumprod(invest[,j])[nrow(invest)]-1)*100
  portret[2,j] <- sd(cumprod(invest[,j]))
  portret[3,j] <- portret[1,j]/portret[2,j]
  colnames(portret)[j] <- names(paresRtestedM)[j]
}

colnames(portret) <- c("Retorno Total","Desvio Padrão","Sharpe")
ret_port <- as.list(NULL)
ret_port[[length(portpairs)+1]] <- t(portret) ## Retornos Totais
}

