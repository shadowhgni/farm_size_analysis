# Random forest to see what explanatory variables are needed to understand variability in farm size across SSA

# load packages
require(tidyverse)

# Clean environment
rm(list=ls())

# Set working directory
setwd(paste0(here::here(), '/scripts'))

# ------------------------------------------------------------------------------
fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')

# ------------------------------------------------------------------------------
# Prepare data: load lsms data as my_lsms (lsms with geometry as data.frame)
lsms_region <- terra::vect('../data/processed/lsms_trimmed_africa.shp') # this was retrieved from '02.customize_spatial_data.r'
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')

# fetching the LSMS data with only needed variables
lsms_spatial <- lsms_region |>
  terra::as.data.frame(geom = 'xy') |>
  rename(farm_area_ha = farm_area_)
lsms_spatial <- lsms_spatial |>
  bind_cols(
    terra::extract(stacked,  lsms_spatial |> 
                     select(x, y))
  ) |>
  select(x, y, 
         country, gadm_0,# omit country_name, region, and farm_id
         cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, 
         market, maizeyield, farm_area_ha) |>     # maize_yield should be removed  for colinearity issue with temperature # GDP  remove for granularity isssues (country-level)
  na.omit()

# ------------------------------------------------------------------------------
# prepare data for a random-effect model to partition variance across scales (only 2 years to exclude temporal variability)
lsms_lmer <-  lsms_region |>
  terra::as.data.frame(geom = 'xy') |>
  rename(farm_area_ha = farm_area_) |>  
  select(farm_area_ha, gadm_0, gadm_1, gadm_2, gadm_3, gadm_4, x, y) |>
  mutate(gadm_3 = case_when(grepl('n.a', gadm_3) ~ NA, .default = gadm_3),
         region = case_when(gadm_0 %in% c('BEN', 'BFA', 'TGO', 'NER', 'MLI', 'NGA', 'CIV', 'SEN', 'GNB' ) ~ 'West',
                            gadm_0 %in% c('ETH', 'TZA', 'UGA') ~ 'East',
                            gadm_0 %in% c('ZMB', 'MWI') ~ 'South', .default = NA)) 

# random-effect model for all SSA
# Note that not all countries have up to 4 GADM levels; therefore, better look at res01 rather than res00
res00 <- lmerTest::lmer(farm_area_ha ~ 1 + (1|region/gadm_0/gadm_1/gadm_2/gadm_3/gadm_4),
                        lsms_lmer)

res00 

res01 <- lmerTest::lmer(farm_area_ha ~ 1 + (1|region/gadm_0/gadm_1/gadm_2),
                        lsms_lmer)

res01
# random-effect model per country
for(i in sort(unique(lsms_lmer$gadm_0)[!is.na(unique(lsms_lmer$gadm_0))])){
  print(paste0('==============', i, '==============='))
  
  if(all(is.na(lsms_lmer$gadm_3[lsms_lmer$gadm_0 == i])))
    res02 <- lmerTest::lmer(farm_area_ha ~ 1 + (1|gadm_1/gadm_2),
                            lsms_lmer |> filter(gadm_0 == i))
  else if(all(is.na(lsms_lmer$gadm_4[lsms_lmer$gadm_0 == i])))
    res02 <- lmerTest::lmer(farm_area_ha ~ 1 + (1|gadm_1/gadm_2/gadm_3),
                            lsms_lmer |> filter(gadm_0 == i))
  else res02 <- lmerTest::lmer(farm_area_ha ~ 1 + (1|gadm_1/gadm_2/gadm_3/gadm_4),
                               lsms_lmer |> filter(gadm_0 == i))
  
  print(res02)
}
# think of a semi-variogram instead!
my_points <- lsms_spatial|> filter(country != 'Zambia') |> select(x, y, farm_area_ha) 
sp::coordinates(my_points) <- ~x+y
g <- gstat::gstat(id = "farm_area_ha", formula = farm_area_ha ~ 1, data = my_points)
v <- gstat::variogram(g)
plot(v)
P00 <- ggplot(as.data.frame(v), aes(dist, gamma)) +
  geom_line() + 
  geom_point(shape = 19) +
  labs(x= 'distance, decimal degree', y = expression('semi-variance, ' * ha^2)) +
  theme_bw()
png(paste0('../output/graphs/semivariogram.png'), height = 5, width = 7.5, units = 'in', res = 600)
P00
ggsave(paste0('../output/graphs/semivariogram.png'))
dev.off()

# ------------------------------------------------------------------------------
# Understanding variable importance in pooled data model trained with RF
# Random-forest model for all Africa. Re-arranging column order as y ~ x
lsms_spatial <- lsms_spatial |>
  select(!c(gadm_0)) |> # x, y, country
  select(farm_area_ha, everything())


# trying the average farm size per grid cell
lsms_check <- lsms_spatial |>
  group_by(country, x, y, cropland, cattle, pop, cropland_per_capita, sand, rainfall, 
           slope, temperature, market, maizeyield) |>
  summarize(farm_area_ha = mean(farm_area_ha, na.rm = T)) |>
  na.omit() |>
  ungroup()


boxplot(lsms_check$cattle~lsms_check$country, ylim=c(0, 15000))

eth <- subset(lsms_check, country=='Ethiopia')
eth <- select(eth, !c('x', 'y', 'country'))


plot(eth$rainfall, eth$farm_area_ha)

# aa <- lm(farm_area_ha~slope, data=eth)
# summary(aa)
# car::vif(aa)

nga <- subset(lsms_check, country=='Nigeria')
nga <- select(nga, !c('x', 'y', 'country'))

my_predictors <- c('cropland', 'cattle', 'pop', 'cropland_per_capita', 'sand', 'rainfall', 
                   'slope', 'temperature', 'market', 'maizeyield')
for(i in my_predictors){
  mod <- randomForest::randomForest(nga[['farm_area_ha']] ~ nga[[i]] , data=nga)
  mod
  
  nga$pred <- predict(mod, nga)
  plot(nga$farm_area_ha, nga$pred)
  plot(nga$pred, nga$sand)
  print('--------------------------------------')
  print(paste('only ', i, 'in Nigeria has a R2 of ', cor(nga$farm_area_ha, nga$pred)^2))
  
  
  eth$pred <- predict(mod, eth)
  plot(eth$farm_area_ha, eth$pred)
  print(paste('but in Ethiopia, using Nigerian model, it crashes to ',cor(eth$farm_area_ha, eth$pred)^2))
}



# Remove one or more variable(s) at time, and check model's performances against full model
comp_var <- tibble()
train_control <- caret::trainControl(method = 'cv', number = 10, savePredictions = 'all', seeds = 2024)
tune_grid <- expand.grid(mtry = 4:5,                          # in principle, I should start with 4:6       
                         splitrule = 'extratrees',            # in principle, I should start with c('extratrees', 'variance')       
                         min.node.size = c(50, 55, 60))       # in principle, I should start with c(45, 50, 55, 60)        

sel_variables <- names(lsms_spatial)[2:ncol(lsms_spatial)]
pairs_list <- split(combn(sel_variables, 2), col(combn(sel_variables, 2)))
triplets_list <- split(combn(sel_variables, 3), col(combn(sel_variables, 3)))
quadruplets_list <- split(combn(sel_variables, 4), col(combn(sel_variables, 4)))
quintuplets_list <- split(combn(sel_variables, 5), col(combn(sel_variables, 5)))
sextuplets_list <- split(combn(sel_variables, 6), col(combn(sel_variables, 6)))
list_vars <-c(sel_variables, pairs_list, triplets_list, quadruplets_list, quintuplets_list)
# list_vars <- sextuplets_list
# not feasible to run all the above-mentioned combinations. Below, the 5 most important variables are combined
deb <- Sys.time()
cores <- parallel::detectCores() - 4
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)
rf_full <- caret::train(
  farm_area_ha ~ .,
  data = lsms_spatial,
  method = 'ranger',
  trainControl = train_control,
  # tuneGrid = tune_grid,
  # num.trees = 1500,
  importance  = 'permutation'
)
parallel::stopCluster(cl)
fin <- Sys.time() - deb; print(fin)

# prioritize the variables to remove for running models
print(caret::varImp(rf_full, scale = F)) # take a decision about the 5 most important variables
sel_variables <- c('temperature', 'maizeyield', 'sand', 'cropland', 'rainfall') # the 5 most important, in decreasing order
pairs_list <- split(combn(sel_variables, 2), col(combn(sel_variables, 2)))
triplets_list <- split(combn(sel_variables, 3), col(combn(sel_variables, 3)))
quadruplets_list <- split(combn(sel_variables, 4), col(combn(sel_variables, 4)))
quintuplets_list <- split(combn(sel_variables, 5), col(combn(sel_variables, 5)))
list_vars <-c(quintuplets_list, sel_variables, quadruplets_list, triplets_list, pairs_list)

deb <- Sys.time()
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)
for (i in list_vars){
  print(paste0('-----------------', paste0(i, collapse = ', '), '--------------------'))
  reduced_lsms <- lsms_spatial |>
    select(!i)
  
  rf_reduced <- caret::train(farm_area_ha ~ .,
                             data = reduced_lsms,
                             method = 'ranger',
                             trainControl = train_control,
                             # tuneGrid = tune_grid, # to be fast
                             # num.trees = 1500,     # to be fast
                             importance  = 'permutation'
  )
  rsq <- caret::postResample(predict(rf_reduced, reduced_lsms), reduced_lsms$farm_area_ha)[2]
  rmse <- caret::postResample(predict(rf_reduced, reduced_lsms), reduced_lsms$farm_area_ha)[1]
  rsq_cv <- max(rf_reduced$results$Rsquared)
  rsq_oob <- rf_reduced$finalModel$r.squared
  one_row <- c(model =  paste0(i, collapse = ', '), rsq = rsq, rsq_oob = rsq_oob, rsq_cv = rsq_cv, rmse = rmse)
  comp_var <- bind_rows(comp_var, one_row)
  print(comp_var)
  assign(paste0('rf_reduced_', paste0(i, collapse = '_')), rf_reduced, envir = .GlobalEnv)
}
parallel::stopCluster(cl)

rsq <- caret::postResample(predict(rf_full, lsms_spatial), lsms_spatial$farm_area_ha)[2]
rmse <- caret::postResample(predict(rf_full, lsms_spatial), lsms_spatial$farm_area_ha)[1]
rsq_cv <- max(rf_full$results$Rsquared)
rsq_oob <- rf_full$finalModel$r.squared
one_row <- c(model = 'full', rsq = rsq, rmse = rmse)
comp_var <- bind_rows(comp_var, one_row)

comp_var <- comp_var |>
  mutate(rsq = as.numeric(rsq.Rsquared), 
         rmse = as.numeric(rmse.RMSE)) |>
  arrange(desc(rsq))
save(rf_reduced_cropland, rf_reduced_cattle, rf_reduced_population, 
     rf_reduced_sand, rf_reduced_elevation, rf_reduced_market, 
     rf_reduced_rainfall, rf_reduced_maizeyield,
     comp_var, rf_full,file = '../data/processed/compare_7variables.rdata')
fin <- Sys.time() - deb; print(fin)
# ------------------------------------------------------------------------------
# All models, starting with the overall random forest
# Basic model
deb <- Sys.time()
train_control <- caret::trainControl(method = 'cv', number = 10, savePredictions = 'all', seeds = 2024)
tune_grid <- expand.grid(mtry = 4,                        # I first tried 1:8 and 4 was the best; then I tried 4:5, 4 was still the best. I think of it as a local optimum pb
                         splitrule = 'extratrees',        # I tried c('variance', 'extratrees'); extratrees was the best
                         min.node.size = 55)              # I first tried c(5, 50, 100, 200). I then tried c(45, 50, 55, 60). 55 was the best
rf_model <- caret::train(farm_area_ha ~ .,
                         data = lsms_spatial,
                         method = 'ranger',
                         trainControl = train_control,
                         keep.inbag = T,  # check this, just like savePredictions in the control
                         tuneGrid = tune_grid,
                         importance  = 'permutation',
                         num.trees = 1500
                         )
tree_info <- ranger::treeInfo(rf_model$finalModel, tree = 1)
fin <- Sys.time() - deb; print(fin)

# Log model
deb <- Sys.time()
train_control <- caret::trainControl(method = 'cv', number = 10, seeds = 2024)
tune_grid <- expand.grid(mtry = 4,
                         splitrule = 'extratrees',
                         min.node.size = c(55, 60, 65))
rf_log_model <- caret::train(log(farm_area_ha) ~ .,
                             data = lsms_spatial,
                             method = 'ranger',
                             trainControl = train_control,
                             tuneGrid = tune_grid,
                             importance  = 'permutation',
                             num.trees = 1500
)
fin <- Sys.time() - deb; print(fin)

# Square root model
deb <- Sys.time()
train_control <- caret::trainControl(method = 'cv', number = 10, seeds = 2024)
tune_grid <- expand.grid(mtry = 5,
                         splitrule = 'extratrees',
                         min.node.size = c(50, 55, 60))
rf_sqrt_model <- caret::train(sqrt(farm_area_ha) ~ .,
                              data = lsms_spatial,
                              method = 'ranger',
                              trainControl = train_control,
                              tuneGrid = tune_grid,
                              importance  = 'permutation',
                              num.trees = 1500
)
fin <- Sys.time() - deb; print(fin)

# Third order square root model
deb <- Sys.time()
train_control <- caret::trainControl(method = 'cv', number = 10, seeds = 2024)
tune_grid <- expand.grid(mtry = 4,
                         splitrule = 'extratrees',
                         min.node.size = c(50, 55, 60))
rf_sq3_model <- caret::train(farm_area_ha^(1/3) ~ .,
                             data = lsms_spatial,
                             method = 'ranger',
                             trainControl = train_control,
                             tuneGrid = tune_grid,
                             importance  = 'permutation',
                             num.trees = 1500
)
fin <- Sys.time() - deb; print(fin)

# Overall model performance of original RF model
lsms_spatial$pred_oob <- predict(rf_model, lsms_spatial)
rf_perf <- round(caret::postResample(lsms_spatial$pred_oob, lsms_spatial$farm_area_ha), 2)
r_sq <- rf_perf[2]
rmse <- rf_perf[1]
mae <- rf_perf[3]
lbl <- expression(R^2== r_sq)
rf_perf
print(paste0('RF model square error= ', r_sq))
print(paste0('RF Model RMSE = ', rmse))
print(paste0('RF Model MAE = ', mae))

# Overall performance of the other models
lsms_spatial$pred_oob_log <- predict(rf_log_model, lsms_spatial)
lsms_spatial$pred_oob_sqrt <- predict(rf_sqrt_model, lsms_spatial)
lsms_spatial$pred_oob_sq3 <- predict(rf_sq3_model, lsms_spatial)
rf_log_perf <- round(caret::postResample(predict(rf_log_model, lsms_spatial), lsms_spatial$farm_area_ha), 2)
rf_sqrt_perf <- round(caret::postResample(predict(rf_sqrt_model, lsms_spatial), lsms_spatial$farm_area_ha), 2)
rf_sq3_perf <- round(caret::postResample(predict(rf_sq3_model, lsms_spatial), lsms_spatial$farm_area_ha), 2)

# Compare the R2 of different models
r2 <- rf_perf[2]
r2_log <- rf_log_perf[2]
r2_sqrt <- rf_sqrt_perf[2]
r2_sq3 <- rf_sq3_perf[2]
print(' ==== R square ====')
cat(paste0('RF = ', r2, '\n', 'RF_log = ', r2_log, '\n', 'RF_sq = ', r2_sqrt, '\n',
           'RF_sq3 = ', r2_sq3, '\n'))

rmse_log <- rf_log_perf[1]
rmse_sqrt <- rf_sqrt_perf[1]
rmse_sq3 <- rf_sq3_perf[1]
cat(paste0('RF = ', rmse, '\n', 'RF_log = ', rmse_log, '\n', 'RF_sq = ', rmse_sqrt, '\n',
           'RF_sq3 = ', rmse_sq3, '\n'))


mae_log <- rf_log_perf[3]
mae_sqrt <- rf_sqrt_perf[3]
mae_sq3 <- rf_sq3_perf[3]
cat(paste0('RF = ', mae, '\n', 'RF_log = ', mae_log, '\n', 'RF_sq = ', mae_sqrt, '\n',
           'RF_sq3 = ', mae_sq3, '\n'))

rf_model_SSA_performances <- cbind.data.frame(country = 'SSA',
                                              indicator = rep(c('r_sq', 'rmse', 'mae'), each = 4),
                                              model = rep(c('RF', 'RF_log', 'RF_sq', 'RF_sq3'), 3),
                                              value = c(r2, r2_log, r2_sqrt, r2_sq3, rmse, 
                                                        rmse_log, rmse_sqrt, rmse_sq3,
                                                        mae, mae_log, mae_sqrt, mae_sq3))
png("../output/graphs/africa_pred_obs.png", units="in", width=5.5, height=5.5, res=1000)

# Scatter-plot  of observed and predicted farm sizes in training dataset for Africa
par(mar=c(5,5,1,1), cex.axis=1.3, cex.lab=1.4)
plot(lsms_spatial$farm_area_ha, lsms_spatial$pred_oob, xlim=c(0, 15), ylim=c(0, 15),
     main = 'Africa', ylab='Predicted farm size (ha)', xlab='Reported farm size (ha)') 
abline(a=0, b=1, col=2, lwd=2)
abline(a=0, b=0.5, col=2, lwd=3, lty=2)
abline(a=0, b=2, col=2, lwd=3, lty=2)
text(x = 14, y = 14, bquote(R^2== .(r2)))
dev.off()

# density plot of observed vs predicted farm sizes in training dataset for Africa
pal <- colorRampPalette(c('grey98', 'purple4'))
P00 <- ggplot(lsms_spatial, aes(farm_area_ha, pred_oob)) +
  geom_density_2d_filled(bins = 10) +
  geom_abline(slope = 1, linewidth = 0.8) +
  geom_abline(slope = 0.5, linewidth = 0.8, linetype = 2) +
  geom_abline(slope = 2, linewidth = 0.8, linetype = 2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_fill_manual(values = pal(10)) +
  labs(x= 'Reported farm size (ha)', y = 'Predicted farm size (ha)',
       title = 'SSA', fill = 'Density of datapoints') +
  annotate('text', x = 3, y = 4, label = bquote(R^2== .(r2)), colour = 'red') +
  theme_test()

P00 <- ggplot(lsms_spatial, aes(farm_area_ha, pred_oob)) +
  geom_density_2d_filled(bins = 9) +
  geom_abline(slope = 1, linewidth = 0.8) +
  geom_abline(slope = 0.5, linewidth = 0.8, linetype = 2) +
  geom_abline(slope = 2, linewidth = 0.8, linetype = 2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_fill_brewer() +
  labs(x= 'Reported farm size (ha)', y = 'Predicted farm size (ha)',
       title = 'SSA', fill = 'Density of points') +
  annotate('text', x = 1.8, y = 1.95, label = bquote(R^2== .(r2)) ) +
  annotate('text', x = 0.6, y = 1.5, label = 'y = 2x' ) +
  annotate('text', x = 1.4, y = 1.5, label ='y = x' ) +
  annotate('text', x = 1.4, y = 0.8, label ='y = 0.5 x' ) +
  theme_test()

png(paste0('../output/graphs/africa_pred_obs.png'), height = 5, width = 7.5, units = 'in', res = 600)
P00
ggsave(paste0('../output/graphs/africa_pred_obs.png'))
dev.off()

P01 <- ggplot(lsms_spatial, aes(farm_area_ha, exp(pred_oob_log))) +
  geom_density_2d_filled(bins = 9) +
  geom_abline(slope = 1, linewidth = 0.8) +
  geom_abline(slope = 0.5, linewidth = 0.8, linetype = 2) +
  geom_abline(slope = 2, linewidth = 0.8, linetype = 2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_fill_brewer() +
  labs(x= 'Reported farm size (ha)', y = 'Predicted farm size (ha)',
       title = 'SSA', fill = 'Density of points') +
  annotate('text', x = 1.8, y = 1.95, label = bquote(R^2== .(r2_log)) ) +
  annotate('text', x = 0.6, y = 1.5, label = 'y = 2x' ) +
  annotate('text', x = 1.4, y = 1.5, label ='y = x' ) +
  annotate('text', x = 1.4, y = 0.8, label ='y = 0.5 x' ) +
  theme_test()

png(paste0('../output/graphs/africa_log_pred_obs.png'), height = 5, width = 7.5, units = 'in', res = 600)
P01
ggsave(paste0('../output/graphs/africa_log_pred_obs.png'))
dev.off()

P02 <- ggplot(lsms_spatial, aes(farm_area_ha, pred_oob_sqrt^2)) +
  geom_density_2d_filled(bins = 9) +
  geom_abline(slope = 1, linewidth = 0.8) +
  geom_abline(slope = 0.5, linewidth = 0.8, linetype = 2) +
  geom_abline(slope = 2, linewidth = 0.8, linetype = 2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_fill_brewer() +
  labs(x= 'Reported farm size (ha)', y = 'Predicted farm size (ha)',
       title = 'SSA', fill = 'Density of points') +
  annotate('text', x = 1.8, y = 1.95, label = bquote(R^2== .(r2_sqrt)) ) +
  annotate('text', x = 0.6, y = 1.5, label = 'y = 2x' ) +
  annotate('text', x = 1.4, y = 1.5, label ='y = x' ) +
  annotate('text', x = 1.4, y = 0.8, label ='y = 0.5 x' ) +
  theme_test()

png(paste0('../output/graphs/africa_sq_pred_obs.png'), height = 5, width = 7.5, units = 'in', res = 600)
P02
ggsave(paste0('../output/graphs/africa_sq_pred_obs.png'))
dev.off()

P03 <- ggplot(lsms_spatial, aes(farm_area_ha, pred_oob_sq3^3)) +
  geom_density_2d_filled(bins = 9) +
  geom_abline(slope = 1, linewidth = 0.8) +
  geom_abline(slope = 0.5, linewidth = 0.8, linetype = 2) +
  geom_abline(slope = 2, linewidth = 0.8, linetype = 2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_fill_brewer() +
  labs(x= 'Reported farm size (ha)', y = 'Predicted farm size (ha)',
       title = 'SSA', fill = 'Density of points') +
  annotate('text', x = 1.8, y = 1.95, label = bquote(R^2== .(r2_sq3)) ) +
  annotate('text', x = 0.6, y = 1.5, label = 'y = 2x' ) +
  annotate('text', x = 1.4, y = 1.5, label ='y = x' ) +
  annotate('text', x = 1.4, y = 0.8, label ='y = 0.5 x' ) +
  theme_test()

png(paste0('../output/graphs/africa_sq_pred_obs.png'), height = 5, width = 7.5, units = 'in', res = 600)
P03
ggsave(paste0('../output/graphs/africa_sq_pred_obs.png'))
dev.off()

# variable importance
rf_model$finalModel$variable.importance |>
  as_tibble() |>
  mutate(Variable = names(rf_model$finalModel$variable.importance)) %>%
  select(Variable, value) |>
  arrange(-value) |>
  print()



# ------------------------------------------------------------------------------
# This works, but I'm suspicious that results of all chunk differ from that of whole large dataset
# disk.frame::nchunks(lsms_spatial_d)
# # Collect the list of chunks into memory
# chunk_list <- disk.frame::collect_list(lsms_spatial_d)# Collect the list of chunks into memory
# 
# # Partition cores for parallel processing
# cores <- parallel::detectCores() - 2
# cl <- parallel::makeCluster(cores)
# doParallel::registerDoParallel(cl)
# 
# # Function to process each chunk
# process_chunk <- function(chunk) {
#   # Perform any pre-processing
#   preprocessed_data <- chunk
#   
#   # Train a model using caret on this chunk of data
#   model <- caret::train(farm_area_ha ~ ., data = preprocessed_data, 
#                         method = "ranger", 
#                         trControl = caret::trainControl(method = "cv"))
#   
#   # Perform any other necessary computations
#   additional_computation <- print(model$results)
#   
#   # Return results for this chunk
#   return(list(model = model, additional = additional_computation))
# }
# 
# # Iterate over the chunks using purrr::map()
# results <- purrr::map(chunk_list, process_chunk)
