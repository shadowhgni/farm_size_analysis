# Compile results of RF optimization

ff <- list.files("output/RFoptim", pattern="\\.Rds$", full=TRUE)
res <- lapply(ff, \(f) readRDS(f)$results)

res <- data.frame(filename=basename(ff), do.call(rbind, res))
res <- res[order(-res$Rsquared, res$RMSE, res$MAE), ]
saveRDS(res, "output/tables/RF_optim_summarized_table.rds")
