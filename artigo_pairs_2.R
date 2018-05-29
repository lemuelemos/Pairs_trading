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
  nport <- ncol(ibrx_2008_2017_70)*(ncol(ibrx_2008_2017_70)-1)
  time_window[[p]] <- time(test_period)
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
        #print(paste0("Carteira ",length(portpairs)," de ",nport,". Período de Testes ",p))
      }
    }
  }
portpairs <- portpairs[!sapply(portpairs,is.null)] ### Retirando os valores vazios
portpairs <- portpairs[!sapply(portpairs, function(x) is.na(x$rho.se))] ### Retirando os pares com problemas de estimação
################# Retirando os pares com o R superior a 0.5 
print(paste0("Retirando os pares com o R superior a 0.5. Portfólio ",p))
paresR <- list(NULL)
  for(l in 1:length(portpairs)){
    if(portpairs[[l]]$pvmr > 0.5){
      paresR[l] <- portpairs[l]
      names(paresR)[l] <- names(portpairs)[l]
    }
  }
  paresR <- paresR[!sapply(paresR,is.null)] ### Retirando os valores vazios
  
  ################ Realizando o teste de significância para cointegração parcial
print(paste0("Realizando o teste de significância para cointegração parcial. Portfólio ",p))
testepci <- list(NULL)
paresRtested <- list(NULL)
for(i in 1:length(paresR)){
  testepci[[i]] <- which.hypothesis.pcitest(test.pci(paresR[[i]]))
  names(testepci[i]) <- names(paresR)[i]
  if(testepci[[i]] == "PCI"){
    paresRtested[i] <- paresR[i]
    names(paresRtested)[i] <- names(paresR)[i]
  }
}
paresRtested <- paresRtested[!sapply(paresRtested,is.null)] ### Retirando os valores vazios
  
##############################################################################
  
############# Estimando os Estados Ocultos
print(paste0("Estimando os Estados Ocultos. Portfólio ",p))
paresRtestedM <- list(NULL)
  for(n in 1: length(paresRtested)){
    paresRtestedM[[n]] <- statehistory.pci(paresRtested[[n]])
    names(paresRtestedM)[n] <- names(paresRtested)[n]
}
  
  # Variável paresRtestedM já são os pares para teste backtest
  ############### Normalizando O M
  print(paste0("Normalizando O M. Portfólio",p))
  Zm <- as.list(NULL)
  for(o in 1:length(paresRtestedM)){
    Zm[[o]] <- paresRtestedM[[o]]$M/sd(paresRtested[[o]]$M)
    names(Zm)[o] <- names(paresRtestedM)[o]
  }
  Zm <- as.data.frame(Zm) ### Tos os M's normalizados
  
  ############# Sinal Para as Operações
  ## Openright/OutRight = Operações em que o valor do resíduo é positivo
  ## OpenLeft/OutLeft = Operações em que o valor de resídou é negativo
  ## threshold's = [1,0.5]
  print(paste0("Sinal Para as Operações - threshold[",tr[1],",",tr[2],"]. Portólio ",p))
  print(tr)
  sinal <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
  #tr <- c(1,0.5)
  sinal[1,1:length(sinal)] <- "Fora"
  colnames(sinal) <- names(Zm)
  for(j in 1:length(Zm)){
    for(i in 2:nrow(Zm)){
      if(Zm[i,j] > tr[1] && sinal[i-1,j] != "OpenLeft" || sinal[i-1,j] == "OpenRight" && Zm[i,j] > -tr[2]){
        sinal[i,j] <- "OpenRight"
      } else if(Zm[i,j] < -tr[1] && sinal[i-1,j] != "OpenRight" || sinal[i-1,j] == "OpenLeft" && Zm[i,j] < tr[2]){
        sinal[i,j] <- "OpenLeft"
      } else if(Zm[i,j] < -tr[2] && sinal[i-1,j] == "OpenRight"){
        sinal[i,j] <- "OutRight"
      } else if(Zm[i,j] > tr[2] && sinal[i-1,j] == "OpenLeft"){
        sinal[i,j] <- "OutLeft"
      } else{
        sinal[i,j] <- "Fora"
      }
    }
  }
  
  
  
  
########## Loop para Pegar Preços de Entrada e Saída
llongi <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## llongi = Left Long Inicial
lshorti <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## lshorti = Left Short Inicial
llongf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## llongi = Left Long Final
lshortf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))  ## lshortf = Left Short Final
rlongi <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rlongi = Right Long Incial
rshorti <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rshorti = Right Short Incial
rlongf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rlongf = Right Long Final
rshortf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rshorti = Right Short Final
tt <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
##################################################################################
print(paste0("Coletando Preços de Entrada e Saída. Portfólio",p))
for(j in 1:length(Zm)){
  for(i in 2:nrow(sinal)){
    if(sinal[i,j] == "OpenRight" 
         && sinal[i-1,j] == "Fora"
         && i != nrow(sinal)
         || sinal[i,j] == "OpenRight" 
         && sinal[i-1,j] == "OutLeft"
         && i != nrow(sinal)
         || sinal[i,j] == "OpenRight" 
         && sinal[i-1,j] == "OutRight"
         && i != nrow(sinal)){
         rlongi[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],start = -6),
                                        colnames(test_period))]
         rshorti[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],end = 6),
                                         colnames(test_period))]
         colnames(rlongi)[j] <- paste0(names(paresRtestedM)[j],"RL")
         colnames(rshorti)[j] <- paste0(names(paresRtestedM)[j],"RS")
      } else if(sinal[i,j] == "OutRight" 
                && sinal[i-1,j] == "OpenRight"
                || i == nrow(sinal)
                && sinal[i-1,j] == "OpenRight"){
        rlongf[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],start = -6),
                                          colnames(test_period))]
        rshortf[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],end = 6),
                                           colnames(test_period))]
        colnames(rlongf)[j] <- paste0(names(paresRtestedM)[j],"RL")
        colnames(rshortf)[j] <- paste0(names(paresRtestedM)[j],"RS")
      } else if(sinal[i,j] == "OpenLeft" 
                && sinal[i-1,j] == "Fora"
                && i != nrow(sinal)
                || sinal[i,j] == "OpenLeft" 
                && sinal[i-1,j] == "OutRight"
                && i != nrow(sinal)
                || sinal[i,j] == "OpenLeft"
                && sinal[i-1,j] == "OutLeft"
                && i != nrow(sinal)){
        llongi[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],end = 6),
                                          colnames(test_period))]
        lshorti[i,j] <- test_period[i,grep(str_sub(names(paresRtestedM)[j],start = -6),
                                           colnames(test_period))]
        colnames(llongi)[j] <- paste0(names(paresRtestedM)[j],"LL")
        colnames(lshorti)[j] <- paste0(names(paresRtestedM)[j],"LS")
      } else if(sinal[i,j] == "OutLeft" 
                && sinal[i-1,j] == "OpenLeft"
                || i == nrow(sinal)
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
  print(paste0("Cálculo do Retorno Considerando o investimento de 1 Real. Portfólio",p))
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
  ttf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
  for(j in 1:length(sinal)){
    for(i in 2:nrow(sinal)){
      #invest[i,j] <- invest[k,j]
      if(sinal[i,j] == "OpenRight" 
         && sinal[i-1,j] == "Fora"
         && i != nrow(sinal)
         || sinal[i,j] == "OpenRight" 
         && sinal[i-1,j] == "OutLeft"
         && i != nrow(sinal)
         || sinal[i,j] == "OpenRight" 
         && sinal[i-1,j] == "OutRight"
         && i != nrow(sinal)){
        if(rlongi[i,j]*paresRtested[[j]]$beta/rshorti[i,j] < 1){
          portl <- -((rlongi[i,j]*paresRtested[[j]]$beta*invest[i-1,j])/rshorti[i,j]) 
          ports <- invest[i-1,j]
          longi <- rlongi[i,j]
          shorti <- rshorti[i,j]
          porti <- portl+ports
          ttf[i,j] <- "Abriu"
          for(k in i:nrow(sinal)){
            if(sinal[k,j] == "OutRight" 
               && sinal[k-1,j] == "OpenRight"
               || k == nrow(sinal)
               && sinal[k-1,j] == "OpenRight"){
              longf <- rlongf[k,j]
              shortf <- rshortf[k,j]
              longf <- ((longf/longi)-1)+1
              shortf <- 1+((shortf/shorti)-1)
              portf <- -portl*longf - ports*shortf
              retorno[k,j] <- (porti+portf)/invest[k-1,j]
              invest[k,j] <- (((porti+portf)/invest[k-1,j])+1)*invest[k-1,j]
              ttf[k,j] <- "Saiu"
            }
          }
        } else{
          portl <- -invest[i-1,j]
          ports <- (rshorti[i,j]/(paresRtested[[j]]$beta*rlongi[i,j]))*invest[i-1,j]
          longi <- rlongi[i,j]
          shorti <- rshorti[i,j]
          porti <- portl+ports
          ttf[i,j] <- "Abriu"
          for(k in i:nrow(sinal)){
            if(sinal[k,j] == "OutRight" 
               && sinal[k-1,j] == "OpenRight"
               || k == nrow(sinal)
               && sinal[k-1,j] == "OpenRight"){
              longf <- rlongf[k,j]
              shortf <- rshortf[k,j]
              longf <- ((longf/longi)-1)+1
              shortf <- 1+((shortf/shorti)-1)
              portf <- -portl*longf - ports*shortf
              retorno[k,j] <- (porti+portf)/invest[k-1,j]
              invest[k,j] <- (((porti+portf)/invest[k-1,j])+1)*invest[k-1,j]
              ttf[k,j] <- "Saiu"
            }
          } 
        } 
      } else if(sinal[i,j] == "OpenLeft" 
                && sinal[i-1,j] == "Fora"
                && i != nrow(sinal)
                || sinal[i,j] == "OpenLeft" 
                && sinal[i-1,j] == "OutRight"
                && i != nrow(sinal)
                || sinal[i,j] == "OpenLeft"
                && sinal[i-1,j] == "OutLeft"
                && i != nrow(sinal)){
        if(lshorti[i,j]*paresRtested[[j]]$beta/llongi[i,j] < 1){
          portl <- -invest[i-1,j]
          ports <- ((lshorti[i,j]*paresRtested[[j]]$beta)/llongi[i,j])*invest[i-1,j]
          longi <- llongi[i,j]
          shorti <- lshorti[i,j]
          porti <- portl+ports
          ttf[i,j] <- "Abriu"
          for(k in i:nrow(sinal)){
            if(sinal[k,j] == "OutLeft" 
               && sinal[k-1,j] == "OpenLeft"
               || k == nrow(sinal)
               && sinal[k-1,j] == "OpenLeft"){
              longf <- llongf[k,j]
              shortf <- lshortf[k,j]
              longf <- ((longf/longi)-1)+1
              shortf <- 1+((shortf/shorti)-1)
              portf <- -portl*longf - ports*shortf
              retorno[k,j] <- (porti+portf)/invest[k-1,j]
              invest[k,j] <- (((porti+portf)/invest[k-1,j])+1)*invest[k-1,j]
              ttf[k,j] <- "Saiu"
            }
          } 
        } else{
          portl <- -(llongi[i,j]/(paresRtested[[j]]$beta*lshorti[i,j]))*invest[i-1,j]
          ports <- invest[i-1,j]
          longi <- llongi[i,j]
          shorti <- lshorti[i,j]
          porti <- portl+ports
          ttf[i,j] <- "Abriu"
          for(k in i:nrow(sinal)){
            if(sinal[k,j] == "OutLeft" 
               && sinal[k-1,j] == "OpenLeft"
               || k == nrow(sinal)
               && sinal[k-1,j] == "OpenLeft"){
              longf <- llongf[k,j]
              shortf <- lshortf[k,j]
              longf <- ((longf/longi)-1)+1
              shortf <- 1+((shortf/shorti)-1)
              portf <- -portl*longf - ports*shortf
              retorno[k,j] <- (porti+portf)/invest[k-1,j]
              invest[k,j] <- (((porti+portf)/invest[k-1,j])+1)*invest[k-1,j]
              ttf[k,j] <- "Saiu"
            }
          } 
        } 
      } else{
        if(i == k){          ### Mudar teste de condição. Problemático pelo o k só ser declarado dentro do laço for. 
          ttf[i,j] <- "Saiu" ### Tentar sinal[i,j] == sina[i-1,j], então ttf[i,j] <- "Aberto", senão, ttf[i,j] <- "Saiu"
        } else{
          ttf[i,j] <- "Aberto"
        }
      }
    }
  }
  
  names(invest) <- names(paresRtested) ### Nomeando os Pares
  names(retorno) <- names(paresRtested)
  
  ################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
  print(paste0("Calculando os Retornos Totais. Portfólio ",p))
  portret <- as.data.frame(matrix(data = rep(0,ncol(Zm)*3),ncol = ncol(Zm),nrow = 3))
  for(f in 1:length(invest)){
    portret[1,f] <- (cumprod(invest[,f])[nrow(invest)]-1)*100
    portret[2,f] <- sd(cumprod(invest[,f]))
    portret[3,f] <- portret[1,f]/portret[2,f]
    colnames(portret)[f] <- names(paresRtestedM)[f]
  }
  
  portret <- t(portret) ## Retornos Totais
  colnames(portret) <- c("Retorno Total","Desvio Padrão","Sharpe")
  ret_port[[length(ret_port)+1]] <- portret ## Retornos Totais
  names(ret_port)[p] <- paste0("Return Formation Period ",p)
  
  #####################################################
  ############### Periodo de Trading ##################
  #####################################################
  print(tr)
  for(ii in c(1,3)){
  print("Período de Trading")
  portsel <- sort(ret_port[[p]][,ii], decreasing = T)[1:20] ## Seleect the top 20 sharp's
  select_port[[p]] <- names(portsel)
  if(nrow(test_period) ==  formation_windown[pp]+1){ # testing if the window is complete
  trading_period <- window(ibrx_2008_2017_70, # Select the data
                           start = time(test_period)[1],
                           end = time(test_period)[nrow(test_period)]+178) 
  
  
  ############# Estimating the pairs ################
  print("Estimando os Pares")
  parestrade <-as.list(NULL)
  for(j in 1:length(portsel)){
  parestrade[[j]] <- cbind(trading_period[,grep(str_sub(names(portsel)[j],end=6),names(trading_period))],
                           trading_period[,grep(str_sub(names(portsel)[j],start=-6),names(trading_period))])
  names(parestrade)[j] <- names(portsel)[j]
  }
  
  pares2 <- list(NULL)
  for(j in 1:length(parestrade)){
  pares2[[j]] <- fit.pci(parestrade[[j]][,1],parestrade[[j]][,2], 
                         pci_opt_method=c("jp"),
                         par_model=c("par","ar1","rw"),
                         lambda=0,robust=FALSE,nu=5,include_alpha=FALSE)
  names(pares2)[length(pares2)] <- names(portsel)[j]
}
  
  pares2 <- pares2[!sapply(pares2,is.null)] ### Retirando os valores vazios
  pares2 <- pares2[!sapply(pares2, function(x) is.na(x$rho.se))]
  paresRtested <- list(NULL)
  paresRtested <- pares2
  
  ############## Estimando os Estados Ocultos

paresRtestedM <- list(NULL)
for(i in 1: length(paresRtested)){
  paresRtestedM[[i]] <- statehistory.pci(paresRtested[[i]])
  names(paresRtestedM)[i] <- names(paresRtested)[i]
}

# Variável paresRtestedM já são os pares para teste backtest
############### Normalizando O M

Zm <- as.list(NULL)
for(i in 1:length(paresRtestedM)){
  Zm[[i]] <- paresRtestedM[[i]]$M/sd(paresRtested[[i]]$M)
  names(Zm)[i] <- names(paresRtestedM)[i]
}

Zm <- as.data.frame(Zm) ### Tos os M's normalizados
############# Sinal Para as Operações
## Openright/OutRight = Operações em que o valor do resíduo é positivo
## OpenLeft/OutLeft = Operações em que o valor de resídou é negativo
## threshold's = [1,0.5]
print(tr)
print(paste0("Sinal Para as Operações - threshold[",tr[1],",",tr[2],"]. Portólio ",p))
sinal <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
sinal[1,1:length(sinal)] <- "Fora"
colnames(sinal) <- names(Zm)
for(j in 1:length(Zm)){
  for(i in 2:nrow(Zm)){
    if(Zm[i,j] > tr[1] && sinal[i-1,j] != "OpenLeft" || sinal[i-1,j] == "OpenRight" && Zm[i,j] > -tr[2]){
      sinal[i,j] <- "OpenRight"
    } else if(Zm[i,j] < -tr[1] && sinal[i-1,j] != "OpenRight" || sinal[i-1,j] == "OpenLeft" && Zm[i,j] < tr[2]) {
      sinal[i,j] = "OpenLeft"
    } else if(Zm[i,j] < -tr[2] && sinal[i-1,j] == "OpenRight"){
      sinal[i,j] <- "OutRight"
    } else if(Zm[i,j] > tr[2] && sinal[i-1,j] == "OpenLeft"){
      sinal[i,j] <- "OutLeft"
    } else{
      sinal[i,j] <- "Fora"
    }
  }
}

###### Definição das Variáveis para Pontos de Entrada e Saída###############

llongi <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## llongi = Left Long Inicial
lshorti <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## lshorti = Left Short Inicial
llongf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## llongi = Left Long Final
lshortf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))  ## lshortf = Left Short Final
rlongi <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rlongi = Right Long Incial
rshorti <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rshorti = Right Short Incial
rlongf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rlongf = Right Long Final
rshortf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm))) ## rshorti = Right Short Final
tt1 <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))

########## Loop para Pegar Preços de Entrada e Saída
print(paste0("Coletando Preços de Entrada e Saída. Portfólio",p))
for(j in 1:length(Zm)){
  for(i in 2:nrow(sinal)){
    if(sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "Fora"
       && i != nrow(sinal)
       || sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "OutLeft"
       && i != nrow(sinal)
       || sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "OutRight"
       && i != nrow(sinal)){
      rlongi[i,j] <- parestrade[[j]][i,2]
      rshorti[i,j] <- parestrade[[j]][i,1]
      colnames(rlongi)[j] <- paste0(names(paresRtestedM)[j]," RL")
      colnames(rshorti)[j] <- paste0(names(paresRtestedM)[j]," RS")
    } else if(sinal[i,j] == "OutRight" 
              && sinal[i-1,j] == "OpenRight"
              || i == nrow(sinal)
              && tt1[i-1,j] == "OpenRight"){
      rlongf[i,j] <- parestrade[[j]][i,2]
      rshortf[i,j] <- parestrade[[j]][i,1]
      colnames(rlongf)[j] <- paste0(names(paresRtestedM)[j]," RL")
      colnames(rshortf)[j] <- paste0(names(paresRtestedM)[j]," RS")
    } else if(sinal[i,j] == "OpenLeft" 
              && sinal[i-1,j] == "Fora"
              && i != nrow(sinal)
              || sinal[i,j] == "OpenLeft" 
              && sinal[i-1,j] == "OutRight"
              && i != nrow(sinal)
              || sinal[i,j] == "OpenLeft"
              && sinal[i-1,j] == "OutLeft"
              && i != nrow(sinal)){
      llongi[i,j] <- parestrade[[j]][i,1]
      lshorti[i,j] <- parestrade[[j]][i,2]
      colnames(llongi)[j] <- paste0(names(paresRtestedM)[j]," LL")
      colnames(lshorti)[j] <- paste0(names(paresRtestedM)[j]," LS")
    } else if(sinal[i,j] == "OutLeft" 
              && sinal[i-1,j] == "OpenLeft"
              || i == nrow(sinal)
              && tt1[i-1,j] == "OpenLeft"){
      llongf[i,j] <- parestrade[[j]][i,1]
      lshortf[i,j] <- parestrade[[j]][i,2]
      colnames(llongf)[j] <- paste0(names(paresRtestedM)[j]," LL")
      colnames(lshortf)[j] <- paste0(names(paresRtestedM)[j]," LS")
    } else{
      tt1[i,j] <- sinal[i,j]
    }
  }
}

##### Cálculo do Retorno Considerando o investimento de 1 Real.###################
print(paste0("Cálculo do Retorno Considerando o investimento de 1 Real. Portfólio",p))
invest_t <- data.frame(matrix(data = rep(1,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
retorno_t <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
portl <- as.vector(NULL)
ports <- as.vector(NULL)
porti <- as.vector(NULL)
portf <- as.vector(NULL)
longi <- as.vector(NULL)
shorti <- as.vector(NULL)
longf <- as.vector(NULL)
shortf <- as.vector(NULL)
tt2 <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
for(j in 1:length(sinal)){
  for(i in 2:nrow(sinal)){
    #invest_t[i,j] <- invest_t[i-1,j]
    if(sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "Fora"
       && i != nrow(sinal)
       || sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "OutLeft"
       && i != nrow(sinal)
       || sinal[i,j] == "OpenRight" 
       && sinal[i-1,j] == "OutRight"
       && i != nrow(sinal)){
      if(rlongi[i,j]*paresRtested[[j]]$beta/rshorti[i,j] < 1){
        portl <- -((rlongi[i,j]*paresRtested[[j]]$beta*invest_t[i-1,j])/rshorti[i,j]) 
        ports <- invest_t[i-1,j]
        longi <- rlongi[i,j]
        shorti <- rshorti[i,j]
        porti <- portl+ports
        tt2[i,j] <- "Abriu"
      } else{
        portl <- -invest_t[i-1,j]
        ports <- (rshorti[i,j]/(paresRtested[[j]]$beta*rlongi[i,j]))*invest_t[i-1,j]
        longi <- rlongi[i,j]
        shorti <- rshorti[i,j]
        porti <- portl+ports
        tt2[i,j] <- "Abriu"
      }
    } else if(sinal[i,j] == "OutRight" 
              && sinal[i-1,j] == "OpenRight"
              || i == nrow(sinal)
              && tt2[i-1,j] == "Aberto"
              && sinal[i-1,j] == "OpenRight"){
      longf <- rlongf[i,j]
      shortf <- rshortf[i,j]
      longf <- ((longf/longi)-1)+1
      shortf <- 1+((shortf/shorti)-1)
      portf <- -portl*longf - ports*shortf
      retorno_t[i,j] <- (porti+portf)/invest_t[i-1,j]
      invest_t[i,j] <- (((porti+portf)/invest_t[i-1,j])+1)*invest_t[i-1,j]
      tt2[i,j] <- "Saiu"
    } else if(sinal[i,j] == "OpenLeft" 
              && sinal[i-1,j] == "Fora"
              && i != nrow(sinal)
              || sinal[i,j] == "OpenLeft" 
              && sinal[i-1,j] == "OutRight"
              && i != nrow(sinal)
              || sinal[i,j] == "OpenLeft"
              && sinal[i-1,j] == "OutLeft"
              && i != nrow(sinal)){
      if(lshorti[i,j]*paresRtested[[j]]$beta/llongi[i,j] < 1){
        portl <- -invest_t[i-1,j]
        ports <- ((lshorti[i,j]*paresRtested[[j]]$beta)/llongi[i,j])*invest_t[i-1,j]
        longi <- llongi[i,j]
        shorti <- lshorti[i,j]
        porti <- portl+ports
        tt2[i,j] <- "Abriu"
      } else{
        portl <- -(llongi[i,j]/(paresRtested[[j]]$beta*lshorti[i,j]))*invest_t[i-1,j]
        ports <- invest_t[i-1,j]
        longi <- llongi[i,j]
        shorti <- lshorti[i,j]
        porti <- portl+ports
        tt2[i,j] <- "Abriu"
      }
    } else if(sinal[i,j] == "OutLeft" 
              && sinal[i-1,j] == "OpenLeft"
              || i == nrow(sinal) 
              && tt2[i-1,j] == "Aberto"
              && sinal[i-1,j] == "OpenLeft"){
      longf <- llongf[i,j]
      shortf <- lshortf[i,j]
      longf <- ((longf/longi)-1)+1
      shortf <- 1+((shortf/shorti)-1)
      portf <- -portl*longf - ports*shortf
      retorno_t[i,j] <- (porti+portf)/invest_t[i-1,j]
      invest_t[i,j] <- (((porti+portf)/invest_t[i-1,j])+1)*invest_t[i-1,j]
      tt2[i,j] <- "Saiu"
    } else{
      tt2[i,j] <- if(sinal[i-1,j] != "Fora"
                     && i != nrow(sinal)
                     && sinal[i,j] != "Fora"){
        "Aberto"
      } else{
        "Fora"
      }
    }
  }
}
retornos[[p]] <- retorno_t
names(retornos)[p] <- paste0("Retornos periodo de trading ",p)
names(invest) <- names(paresRtested) ### Nomeando os Pares

################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
portret <- data.frame(matrix(rep(0,(lenght(pares2)*3),nrow = lenght(pares2)),ncol = 3))
colnames(portret) <- c("Retorno Total","Desvio Padrão","Sharpe")
for(j in 1:ncol(tt2)){
  for(i in (formation_windown[pp]+2):nrow(tt2)){
    if(tt2[i,j] == "Abriu"){
      portret[j,1] <- (tail(cumprod(invest_t[i:nrow(invest_t),j]),1)-1)*100
      portret[j,2] <- sd(cumprod(invest_t[i:nrow(invest_t),j]))
      portret[j,3] <- portret[j,1]/portret[j,2]
      rownames(portret)[j] <- names(paresRtestedM)[j]
      break
    } else{
      portret[j,1] <- 0
      portret[j,2] <- 0
      portret[j,3] <- 0
      rownames(portret)[j] <- names(paresRtestedM)[j]
      next
    }
  }
}
  
  if(ii == 1){
  ret_aux[[1]] <- portret ## Retornos Totais
  names(ret_aux)[1] <- paste0("Return Trading Period ",p, ". The top 20 Sharp")
  } else{
  ret_aux[[2]] <- portret ## Retornos Totais
  names(ret_aux)[2] <- paste0("Return Trading Period ",p, ". The top 20 Return")
  }
  trading_return[[p]] <- ret_aux
  } else{
    print("Finish")
    break
  }
 }
}


#### Salvando Dados Importantes
source('res_data_est.R')
  }
}






