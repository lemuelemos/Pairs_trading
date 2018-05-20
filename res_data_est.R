#trading_return <- readRDS("trading_return.rds")

resum_results <- list(NULL)
resum_results[[1]] <- t(sapply(trading_return, function(x) x[[1]] %>% apply(.,2,mean)))
resum_results[[2]] <- t(sapply(trading_return, function(x) x[[2]] %>% apply(.,2,mean)))
names(resum_results)[1] <- "Top 20 Sharp"
names(resum_results)[2] <- "Top 20 Retur"

resum_results[[1]] <- t(sapply(trading_return, function(x) x[[1]] %>% apply(.,2,mean)))
resum_results[[2]] <- t(sapply(trading_return, function(x) x[[2]] %>% apply(.,2,mean)))

r_names_sharp <- str_sub(sapply(trading_return, function(x) names(x)[1]), end = -19)
r_names_retur <- str_sub(sapply(trading_return, function(x) names(x)[2]), end = -20)
rownames(resum_results[[1]]) <- r_names_retur
rownames(resum_results[[2]]) <- r_names_sharp


par_estim <- list(NULL)
par_estim[[1]] <- resum_results
par_estim[[2]] <- trading_return
par_estim[[3]] <- select_port
saveRDS(par_estim,paste0("par_estim threshold[",tr[1],",",tr[2],"].rds"))

#resum_results

