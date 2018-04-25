library(partialCI)
library(readxl)
library(timeSeries)
library(zoo)
library(xts)
library(dplyr)
library(stringr)
library(BatchGetSymbols)
library(quantmod)

############ Comentários sobre andamento das modificações####################
# Lembrar que o código responsável pela captura dos preços está correto, porém, ao calcular os retornos, 
# o sinal de operação obtemos uma situação em que está a abertura de uma operação. Duas alternativas:
# 1- Modificar o gerador de sinal para a situação de estar no último dado setar para "fora"
# 2- Comtemplar essa situação no pŕoprio algorítimo de pegar os preços.

###########################################################################


##### Taking tickers that compound IBOV 
Ativos <- paste0(BatchGetSymbols::GetIbovStocks()$tickers, '.SA')

# Time Window to download the data
first.date <- Sys.Date() - 1620 # Dados de 4 anos mais 6 meses para o período de trading
last.date <- Sys.Date() - 1

#set folder for cache system
pasta_dos_dados <- 'BGS_CACHE'

## Downloading data from yahoo 
ativosl <- BatchGetSymbols(tickers = Ativos, first.date, last.date,
                           cache.folder = pasta_dos_dados, do.cache = TRUE,thresh.bad.data = 0.9)


ativosw <- reshape.wide(ativosl$df.tickers) #### Changing the arrangement of the data to wide format
dados_estimacao <- xts(ativosw$price.adjusted[,-1], order.by = ativosw$price.adjusted$ref.date) ## Transform in xts 
dados_estimacao <-  na.omit(dados_estimacao) ## Removing Missing Data
periodo_teste <- c(time(dados_estimacao)[1],time(dados_estimacao)[nrow(dados_estimacao)]-180) ### Setting the períod 
dados_estimacao_teste <- window(dados_estimacao, start=periodo_teste[1], end=periodo_teste[2]) ### The formation períod
Nomes <- colnames(dados_estimacao_teste)
pares <- list(NULL)
for(i in 1:ncol(dados_estimacao_teste)){
  for(j in 1:ncol(dados_estimacao_teste)){
    if(i != j){
      pares[[length(pares)+1]] <- fit.pci(dados_estimacao_teste[,i], dados_estimacao_teste[,j], 
                                          pci_opt_method=c("jp"),
                                          par_model=c("par"),
                                          lambda=0,robust=FALSE,nu=5,include_alpha=FALSE)
      names(pares)[length(pares)] <- paste0(Nomes[i],"vs",Nomes[j])
    }
  }
}

pares <- pares[!sapply(pares,is.null)] ### Retirando os valores vazios
################# Retirando os pares com o R superior a 0.5 

paresR <- list(NULL)
for(i in 1:length(pares)){
  if(pares[[i]]$pvmr > 0.5){
    paresR[i] <- pares[i]
    names(paresR)[i] <- names(pares)[i]
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
      rlongi[i,j] <- dados_estimacao_teste[i,grep(str_sub(names(paresRtestedM)[j],start = -8),
                                                  colnames(dados_estimacao_teste))]
      rshorti[i,j] <- dados_estimacao_teste[i,grep(str_sub(names(paresRtestedM)[j],end = 8),
                                                   colnames(dados_estimacao_teste))]
      colnames(rlongi)[j] <- paste0(names(paresRtestedM)[j],"RL")
      colnames(rshorti)[j] <- paste0(names(paresRtestedM)[j],"RS")
    } else if(sinal[i,j] == "OutRight" 
              && sinal[i-1,j] == "OpenRight"
              || i == nrow(sinal)
              && sinal[i-1,j] == "OpenRight"){
      rlongf[i,j] <- dados_estimacao_teste[i,grep(str_sub(names(paresRtestedM)[j],start = -8),
                                                  colnames(dados_estimacao_teste))]
      rshortf[i,j] <- dados_estimacao_teste[i,grep(str_sub(names(paresRtestedM)[j],end = 8),
                                                   colnames(dados_estimacao_teste))]
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
      llongi[i,j] <- dados_estimacao_teste[i,grep(str_sub(names(paresRtestedM)[j],end = 8),
                                                  colnames(dados_estimacao_teste))]
      lshorti[i,j] <- dados_estimacao_teste[i,grep(str_sub(names(paresRtestedM)[j],start = -8),
                                                   colnames(dados_estimacao_teste))]
      colnames(llongi)[j] <- paste0(names(paresRtestedM)[j],"LL")
      colnames(lshorti)[j] <- paste0(names(paresRtestedM)[j],"LS")
    } else if(sinal[i,j] == "OutLeft" 
              && sinal[i-1,j] == "OpenLeft"
              || i == nrow(sinal)
              && sinal[i-1,j] == "OpenLeft"){
      llongf[i,j] <- dados_estimacao_teste[i,grep(str_sub(names(paresRtestedM)[j],end = 8),
                                                  colnames(dados_estimacao_teste))]
      lshortf[i,j] <- dados_estimacao_teste[i,grep(str_sub(names(paresRtestedM)[j],start = -8),
                                                   colnames(dados_estimacao_teste))]
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
ttf <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
for(j in 1:length(sinal)){
  for(i in 2:nrow(sinal)){
    #invest[i,j] <- invest[k-1,j]
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
        tt[i,j] <- "Abriu"
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
      tt[i,j] <- "Aberto"
    }
  }
}

names(invest) <- names(paresRtested) ### Nomeando os Pares
names(retorno) <- names(paresRtested)
################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.
portret <- as.data.frame(matrix(data = rep(0,length(Zm)*3),ncol = length(Zm),nrow = 3))
for(f in 1:length(invest)){
  portret[1,f] <- (cumprod(invest[,f])[nrow(invest)]-1)*100
  portret[2,f] <- sd(cumprod(invest[,f]))
  portret[3,f] <- portret[1,f]/portret[2,f]
  colnames(portret)[f] <- names(paresRtestedM)[f]
}

portret <- t(portret) ## Retornos Totais
portsel <- sort(portret[,1], decreasing = T)[1:20] ### Selecionando os 20 melhores Sharpes
colnames(portret) <- c("Retorno Total","Desvio Padrão","Sharpe")
portret1 <- portret

###################### trading Period ###############
periodo_trading <- c(time(dados_estimacao)[nrow(dados_estimacao)-180],time(dados_estimacao)[nrow(dados_estimacao)])
dados_brutos <- window(dados_estimacao, start=periodo_trading[1], end=periodo_trading[2])
parestrade <-as.list(NULL)
for(j in 1:length(portsel)){
  parestrade[[j]] <- cbind(dados_brutos[,grep(str_sub(names(portsel)[j],end=8),names(dados_brutos))],
                           dados_brutos[,grep(str_sub(names(portsel)[j],start=-8),names(dados_brutos))])
  names(parestrade)[j] <- names(portsel)[j]
}

################# Estimação ##################
pares2 <- list(NULL)
for(j in 1:length(parestrade)){
  pares2[[j]] <- fit.pci(parestrade[[j]][,1],parestrade[[j]][,2], 
                         pci_opt_method=c("jp"),
                         par_model=c("par","ar1","rw"),
                         lambda=0,robust=FALSE,nu=5,include_alpha=FALSE)
  names(pares2)[length(pares2)] <- names(portsel)[j]
}

pares2 <- pares2[!sapply(pares2,is.null)] ### Retirando os valores vazios

paresRtested <- NULL
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
  Zm[[i]] <- paresRtestedM[[i]]$M/paresRtested[[i]]$sigma_M.se
  names(Zm)[i] <- names(paresRtestedM)[i]
}

Zm <- as.data.frame(Zm) ### Tos os M's normalizados
############# Sinal Para as Operações
## Openright/OutRight = Operações em que o valor do resíduo é positivo
## OpenLeft/OutLeft = Operações em que o valor de resídou é negativo
## threshold's = [1,0.5]

sinal <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
sinal[1,1:length(sinal)] <- "Fora"
colnames(sinal) <- names(Zm)
for(j in 1:length(Zm)){
  for(i in 2:nrow(Zm)){
    if(Zm[i,j] > t[1] && sinal[i-1,j] != "OpenLeft" || sinal[i-1,j] == "OpenRight" && Zm[i,j] > -t[2]){
      sinal[i,j] <- "OpenRight"
    } else if(Zm[i,j] < -t[1] && sinal[i-1,j] != "OpenRight" || sinal[i-1,j] == "OpenLeft" && Zm[i,j] < t[2]) {
      sinal[i,j] = "OpenLeft"
    } else if(Zm[i,j] < -t[2] && sinal[i-1,j] == "OpenRight"){
      sinal[i,j] <- "OutRight"
    } else if(Zm[i,j] > t[2] && sinal[i-1,j] == "OpenLeft"){
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
tt2 <- data.frame(matrix(data = rep(0,ncol(Zm)*nrow(Zm)),ncol = ncol(Zm),nrow = nrow(Zm)))
for(j in 1:length(sinal)){
  for(i in 2:nrow(sinal)){
    invest[i,j] <- invest[i-1,j]
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
        tt2[i,j] <- "Abriu"
      } else{
        portl <- -invest[i-1,j]
        ports <- (rshorti[i,j]/(paresRtested[[j]]$beta*rlongi[i,j]))*invest[i-1,j]
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
      retorno[i,j] <- (porti+portf)/invest[i-1,j]
      invest[i,j] <- (((porti+portf)/invest[i-1,j])+1)*invest[i-1,j]
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
        portl <- -invest[i-1,j]
        ports <- ((lshorti[i,j]*paresRtested[[j]]$beta)/llongi[i,j])*invest[i-1,j]
        longi <- llongi[i,j]
        shorti <- lshorti[i,j]
        porti <- portl+ports
        tt2[i,j] <- "Abriu"
      } else{
        portl <- -(llongi[i,j]/(paresRtested[[j]]$beta*lshorti[i,j]))*invest[i-1,j]
        ports <- invest[i-1,j]
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
      retorno[i,j] <- (porti+portf)/invest[i-1,j]
      invest[i,j] <- (((porti+portf)/invest[i-1,j])+1)*invest[i-1,j]
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

names(invest) <- names(paresRtested) ### Nomeando os Pares

################ Cáculo dos Retornos Totais, Desvios Padrões e Sharpe.

portret <- as.data.frame(matrix(data = rep(0,60),ncol = ncol(Zm),nrow = 3))
for(j in 1:length(invest)){
  portret[1,j] <- (invest[nrow(invest),j]-1)*100
  portret[2,j] <- sd(invest[,j])
  portret[3,j] <- portret[1,j]/portret[2,j]
  colnames(portret)[j] <- names(paresRtestedM)[j]
}

portret <- t(portret) ## Retornos Totais
colnames(portret) <- c("Retorno Total","Desvio Padrão","Sharpe")

