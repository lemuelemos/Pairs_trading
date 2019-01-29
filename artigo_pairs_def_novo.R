library(readxl)
library(timetk)
library(purrr)
library(dplyr)
library(doParallel)
library(xts)
library(stringr)
library(partialCI)

##### Organizando os Dados

Dados_2000_2019 <- read_excel("Dados_2000_2019.xlsx")
Dados_2000_2019$Dates <- as.Date(Dados_2000_2019$Dates)
Dados_2000_2019 %>%
  map_if(is.character,as.numeric) %>%
  tk_tbl(timetk_idx = T) %>%
  tk_xts-> Dados_2000_2019

Dados_2008_2018 <- Dados_2000_2019["2008/2018"]
rm(Dados_2000_2019)
Dados_2008_2018[,
                apply(Dados_2008_2018,
                      2,
                      function(x) any(is.na(x)) == F), 
                drop = F] -> Dados_2008_2018

Nomes <- colnames(Dados_2008_2018) ## Taking the names of equity's
Nomes <- str_remove(str_sub(Nomes, 1,6),"\\.")
colnames(Dados_2008_2018) <- Nomes
#rm(Nomes)
##### Estimando as combinações de pares
## Ano de 360 dias. 4 anos 1460 dias. 6 meses 180 dias
sem_ini <- endpoints(Dados_2008_2018,"months",k=6)+1
for(i in sem_ini[15:16]){
  if(date(Dados_2008_2018)[i]+1460 <= date(Dados_2008_2018)[nrow(Dados_2008_2018)]){
  datas <- paste0(date(Dados_2008_2018)[i],"/",date(Dados_2008_2018)[i]+1460)
  dados_per_form <- Dados_2008_2018[datas]
  print(paste0("Periodo de Formação ",datas))
  
  no_cores <- detectCores()-1
  pares <- gtools::permutations(n=ncol(dados_per_form),
                                2,
                                colnames(dados_per_form))
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  pares_coint <- foreach(i=1:nrow(pares),
                         .errorhandling = "pass", 
                         .packages = "partialCI") %dopar%{
                           fit.pci(dados_per_form[,pares[i,1]],dados_per_form[,pares[i,2]], 
                                   pci_opt_method = "twostep")
                         }
  stopCluster(cl)
} else{break}
  
  pares_coint <- pares_coint[!sapply(pares_coint, function(x) is.na(x$rho.se))]
  print("Retirando apenas os pares com rho e R² acima de 0.5")
  paresR <- pares_coint[sapply(pares_coint,function(x) x$pvmr > 0.5)]
  paresR <- paresR[sapply(paresR,function(x) x$rho > 0.5)]
  pares_nomes <- sapply(paresR,
                        function(x) paste0(x$target_name," ",x$factor_names))
  
  print("Teste de Cointegração Parcial")
  cl <- makeCluster(no_cores)
  clusterExport(cl, "paresR")
  clusterEvalQ(cl, library(partialCI))
  pci_teste <- parLapply(cl,paresR, function(x) test.pci(x))
  stopCluster(cl)
  
  pci_teste <- pci_teste[!sapply(pci_teste,function(x) x$p.value[1] > 0.05)]
  pci_teste <- pci_teste[!sapply(pci_teste,function(x) x$p.value[2] > 0.05)]
  pares_parcial_coint <- lapply(pci_teste, function(x) x$data.name)
  pares_parcial_coint <- str_replace(pares_parcial_coint,"/","")
  pares_parcial_coint <- str_replace(pares_parcial_coint," ","")
  pares_parcial_coint <- paresR[which(pares_nomes %in% pares_parcial_coint)]
  
}





