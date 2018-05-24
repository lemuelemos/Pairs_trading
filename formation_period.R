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
      if(sinal[i,j] == sinal[i-1,j] && sinal[i-1,j] !=  "Fora" 
         || sinal[i,j] == sinal[i-1,j] && sinal[i-1,j] !=  "Saiu"){          ### Mudar teste de condição. Problemático pelo o k só ser declarado dentro do laço for. 
        ttf[i,j] <- "Aberto" ### Tentar sinal[i,j] == sina[i-1,j], então ttf[i,j] <- "Aberto", senão, ttf[i,j] <- "Saiu"
      } else{
        ttf[i,j] <- "Saiu"
      }
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