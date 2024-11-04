# Quantile Random forest to see what explanatory variables are needed to understand variability in farm size across SSA

# load packages
require(tidyverse)

# Clean environment
rm(list=ls())

# Set working directory
setwd('~/Harare 2023/Farm sizes across Africa/scripts')

fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# load lsms_spatial
load('../data/processed/my_lsms_africa.Rdata')
lsms_spatial <- my_lsms[, 1:20] |>  # if it confuses with SF object, use my_lsms[, 1:20]
  filter(year > 2007) |>
  select(farm_area_ha, cropland, cattle, population, 
         sand, elevation, market, rainfall, maizeyield) |>
  na.omit()

# # running  quantile regression forest for Africa takes more than 10 hours, better use the output
# # QUANTILE FOREST REGRESSION model
# deb <- Sys.time()
# train_control <- caret::trainControl(method = 'cv', number = 10, seeds = 2024)
# tune_grid <- expand.grid(mtry = 4,                                                # I first tested 4:6, 4 was best
#                          splitrule = 'extratrees',
#                          min.node.size = c(45, 50, 55, 60))                       # I first tested c(5, 50, 100) 50 was best
# qf_model <- caret::train(farm_area_ha ~ .,
#                           data = lsms_spatial,
#                           method = 'ranger',
#                           trainControl = train_control,
#                           tuneGrid = tune_grid,
#                           importance  = 'permutation',
#                           num.trees = 1500,
#                           quantreg = T,
#                           keep.inbag = T
# )
# tree_info <- ranger::treeInfo(qf_model$finalModel, tree = 1)
# fin <- Sys.time() - deb; print(fin)
# 
# save(qf_model, file = '../data/processed/qf_model_africa.Rdata')
load('../data/processed/qf_model_africa.Rdata')

# lsms_spatial$pred_q05 <- qf_log_model$predicted

# predict various quantiles
# print(predict(qf_log_model, what = function(x) sample(x, 1))) 
# print(predict(qf_log_model, what = function(x) ecdf(x) (1:20)))
# print(predict(qf_log_model, interval = 'confidence', se = 'boot'))
# nine_quantiles <- exp(predict(qf_log_model, what = c(0.1*seq(1:9), 0.025, 0.975)))   # quantiles 0.1 -0.9 + 95%CI
nine_quantiles <- predict(qf_model, what = c(0.1*seq(1:9), 0.025, 0.975))   # quantiles 0.1 -0.9 + 95%CI

# bind predicted quantiles to lsms_spatial
lsms_spatial <- cbind.data.frame(lsms_spatial, nine_quantiles)
names(lsms_spatial)[(ncol(lsms_spatial)-8):ncol(lsms_spatial)-2] <- paste0('pred_q',sprintf('%02.0f',1:9))
names(lsms_spatial)[ncol(lsms_spatial)-1] <- 'pred_q95_lower_limit'
names(lsms_spatial)[ncol(lsms_spatial)] <- 'pred_q95_upper_limit'
P_lsms <- lsms_spatial |> 
  pivot_longer(cols = starts_with('pred_q'), 
               names_prefix = 'pred_', names_to = 'quantile', 
               values_to = 'val'  )
# M_lsms <- terra::as.data.frame(qf_log_model_pred)

# PM_lsms <- P_lsms |>
#   select(quantile, val) |>
#   filter(quantile %in% c('q01', 'q05', 'q09')) |>
#   bind_rows(M_lsms |>
#               mutate(quantile= 'mean') |>
#               rename(val = farm_area_ha_pred) )
# 
# P01_quantile_Africa <- ggplot(PM_lsms, aes(val, linetype = quantile, linewidth = quantile)) +
#   geom_line(stat = 'ecdf') +
#   coord_cartesian(xlim = c(0, 10)) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   scale_linetype_manual(values = c('solid', 'dotted', 'dashed', 'dotted')) +
#   scale_linewidth_manual(values = c(0.8 ,0.3, 0.8, 0.3)) +
#   labs(x= 'Quantile predicted farm size, ha', y = 'ECDF', title = 'SSA',
#        linetype = 'Prediction', linewidth = 'Prediction') +
#   theme_test()
# 
# png('../output/graphs/ECDF_predicted_qf_africa.png', width = 10, height = 7.5, units = 'cm', res = 600)
# P01_quantile_Africa
# ggsave('../output/graphs/ECDF_predicted_qf_africa.png')
# dev.off()
# 
# png("../output/graphs/africa_pred_obs.png", units="in", width=5.5, height=5.5, res=1000)
# par(mar=c(5,5,1,1), cex.axis=1.3, cex.lab=1.4)
# plot(lsms_spatial$farm_area_ha, lsms_spatial$pred_q05, xlim=c(0, 15), ylim=c(0, 15),
#      main = 'Africa', ylab='Predicted farm size (ha)', xlab='Reported farm size (ha)') 
# abline(a=0, b=1, col=2, lwd=2)
# abline(a=0, b=0.5, col=2, lwd=3, lty=2)
# abline(a=0, b=2, col=2, lwd=3, lty=2)
# text(x = 14, y = 14, expression(R^2==r_sq))
# dev.off()

r2 <- round(cor(lsms_spatial$farm_area_ha, lsms_spatial$pred_q05)^2, 2)
rmse <- round(100 * sqrt(mean((lsms_spatial$farm_area_ha-lsms_spatial$pred_q05)^2, na.rm=T)) / mean(lsms_spatial$farm_area_ha, na.rm=T), 1) 

# variable importance
qf_log_model$importance %>%
  as_tibble() %>%
  mutate(Variable = rownames(qf_log_model$importance)) %>%
  arrange(-IncNodePurity) %>%
  print()
# for(i in 2:(ncol(lsms_spatial)-1)){
#   print(paste('------------------------', names(lsms_spatial)[i], '----------------------------'))
#   vi <- data.frame(qf_log_model$importance, variable=names(lsms_spatial)[i])
#   png(paste0("../output/graphs/africa",'_',names(lsms_spatial)[i],'_v_importance.png'), units="in", width=5.5, height=5.5, res=1000)
#   print(randomForest::partialPlot(qf_log_model, lsms_spatial, xlab = names(lsms_spatial)[i], x.var = names(lsms_spatial)[i],
#                                   main = paste("Partial Dependence on", names(lsms_spatial)[i]), "yes"))
#   dev.off()
# }