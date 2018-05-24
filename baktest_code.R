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
last.date <- Sys.Date() 
test_date <- as.Date("2018-05-23")
window_date <- test_date+182

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
##########################################################################
if(Sys.Date() == window_date){
  source('formation_period.R')
  test_date <- Sys.Date()
} else{
  source('trading_period.R')
  }






