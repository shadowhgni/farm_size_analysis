# Evaluate robustness of RF model
# main question: are there regional patterns? can a country better predict its neighbours?

# load packages
require(tidyverse)

# Clean environment
rm(list=ls())

# Set working directory
setwd(paste0(here::here(), '/scripts'))

# ------------------------------------------------------------------------------
# Preparation for functions and mapping
input_path <- 'C:/Users/DHOUGNI/OneDrive - CIMMYT/Documents/Harare 2023/Spatial_data_repository'
country <- geodata::world(path=input_path, resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='Réunion' & NAME!='Saint Helena' & NAME!='São Tomé and Príncipe' & NAME!='Seychelles') # keep the mainland + Madagascar only, remove islands
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
pal <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))
pal2 <- colorRampPalette(c('lightsteelblue1', 'steelblue3'))

#define the countries for which LSMS data are available
sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# Prepare data: load lsms data and stacked (raster of drivers)
lsms_spatial <- readRDS('../data/processed/lsms_trimmed_95th_africa.rds') # this was retrieved from '03.1.pooled_data_for_analysis.r'
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')

# # just to test script (runs faster) 
# lsms_spatial <- lsms_spatial |>
#   group_by(country) |>
#   mutate(farm_area_ha = farm_area_ha,
#          x = x, y = y,
#          cropland = cropland, cattle =cattle, pop = pop, cropland_per_capita = cropland_per_capita,
#          sand = sand, slope = slope, temperature = temperature, rainfall = rainfall, maizeyield = maizeyield, market = market,
#          n_row = 1:n()) |>
#   filter(n_row < 1001) |>
#   select(!n_row) |>
#   ungroup()
  

# keep only variables needed in the models
lsms_spatial <- lsms_spatial |>
  select(x, y, country, 
         farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>
  na.omit() 


lsms_mean_farm <- lsms_spatial |>
  group_by(country, x, y) |>
  summarize(
    n = n(),
    farm_area_ha = mean(farm_area_ha, na.rm = T)) |>
  ungroup()

lsms_mean_farm <- lsms_spatial |>
  select(!farm_area_ha) |>
  distinct() |>
  inner_join(lsms_mean_farm) |>
  filter(n > 9) |> 
  select(!n)
# -------------------------------------------------------------------
# Country-pairwise comparison to detect regional trends
compare_countries_in_pairs <- function(my_country){
  print(paste0('--------------------', my_country, '---------------------------'))
  set.seed(2024) # just for reproducibility!
  
  # caret control parms
  ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = F)
  
  # running a loop over the other countries
  for(other_country in sixteen_countries){
    
    # defining training - test sets: use the other country as test set 
    training_set <- lsms_spatial |>
      filter(country == my_country) |>
      select(!c(x, y, country))
    test_set <- lsms_spatial |>
      filter(country == other_country) |>
      select(!c(x, y, country))
    
    # train RF model using my_country
    mod1 <- caret::train(
      farm_area_ha ~ .,
      data = training_set,
      method = 'ranger',
      # preProcess = c('center', 'scale', 'spatialSign'),
      trControl = ctrl,
      metric = 'Rsquared'
    )
    
    # define r2 for my_country
    rf1_cv_rsq <- mod1$results |>
      as.data.frame() |>
      select(Rsquared) |>
      pull() |>
      mean() |>
      round(2)
    rf1_oob_rsq <- round(mod1$finalModel$r.squared, 2)
    # calculate r2 for the other country
    test_set$pred1_cty <- predict(mod1, test_set)
    rf1_test_rsq <- round(cor(test_set$farm_area_ha, test_set$pred1_cty)^2, 2)
    
    ##############
    # doing the same with mean farm_area_ha
    training_set <- lsms_mean_farm |>
      filter(country == my_country) |>
      select(!c(x, y, country))
    test_set <- lsms_mean_farm |>
      filter(country == other_country) |>
      select(!c(x, y, country))
    
    # train RF model using my_country
    mod2 <- caret::train(
      farm_area_ha ~ .,
      data = training_set,
      method = 'ranger',
      # preProcess = c('center', 'scale', 'spatialSign'),
      trControl = ctrl,
      metric = 'Rsquared'
    )
    
    # define r2 for my_country
    rf2_cv_rsq <- mod2$results |>
      as.data.frame() |>
      select(Rsquared) |>
      pull() |>
      mean() |>
      round(2)
    rf2_oob_rsq <- round(mod2$finalModel$r.squared, 2)
    # calculate r2 for the other country
    test_set$pred2_cty <- predict(mod2, test_set)
    rf2_test_rsq <- round(cor(test_set$farm_area_ha, test_set$pred2_cty)^2, 2)
    
    # compiling data for the selected country
    one_row <- c(
      train_country = my_country, test_country = other_country,
      rf1_cv_rsq = rf1_cv_rsq, rf1_oob_rsq = rf1_oob_rsq, rf1_test_rsq = rf1_test_rsq,
      rf2_cv_rsq = rf2_cv_rsq, rf2_oob_rsq = rf2_oob_rsq, rf2_test_rsq = rf2_test_rsq
    )
    all_rows <- bind_rows(all_rows, one_row)
  }
  print(all_rows)
  assign(paste0('results_pairs_', my_country), all_rows, envir = .GlobalEnv)
  return(all_rows)
}

# Initialization and function application (run this chunk of 4 lines at once)
deb <- Sys.time()
all_rows <- data.frame()
mult_rsq <- do.call(bind_rows, lapply(sixteen_countries, compare_countries_in_pairs))
fin <- Sys.time() - deb
print(fin)

write.csv(mult_rsq, '../output/tables/country_pairwise_point_based_cross_validation.csv')
#-----------------------------------------------------------------
# Cross-validation R squares for the country itself
country_autoevaluation_rsq <- function(my_country){
  print(paste0('--------------------', my_country, '---------------------------'))
  set.seed(2024) # just for reproducibility!
  
  # caret control parms
  ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = F)
  
  # defining training - test sets: country as training and as test set 
  training_set <- lsms_spatial |>
    filter(country == my_country) |>
    select(!c(x, y, country))
  test_set <- training_set
  
  # train RF model using my_country
  mod1 <- caret::train(
    farm_area_ha ~ .,
    data = training_set,
    method = 'ranger',
    # preProcess = c('center', 'scale', 'spatialSign'),
    importance = 'permutation',
    trControl = ctrl,
    metric = 'Rsquared'
  )
  
  # define r2 for my_country
  rf_cv_rsq <- mod1$results |>
    as.data.frame() |>
    select(Rsquared) |>
    max() |>
    round(2)
  rf_cv_sd <- mod1$results |>
    as.data.frame() |>
    select(Rsquared) |>
    pull() |>
    sd() |>
    round(4)
  # rf_oob_rsq <- 
  
  # calculate r2 for the other country
  test_set$pred1_cty <- predict(mod1, test_set)
  rf_test_rsq <- round(cor(test_set$farm_area_ha, test_set$pred1_cty)^2, 2)
  
  #retrieve the variable importance for the best model(largest CV r square)
  variable_imp <- caret::varImp(mod1, scale = F)
  variable_importance_one <- cbind.data.frame(country = my_country, 
                                              var = rownames(variable_imp$importance),
                                              importance = variable_imp$importance$Overall) |>
    mutate(rank = 11 - rank(importance))
  # # train over the rest of the country with GAM
  # mod2 <- caret::train(
  #   farm_area_ha ~ .,
  #   data = training_set,
  #   method = gam_model,
  #   # preProcess = c('center', 'scale', 'spatialSign'),
  #   trControl = ctrl,
  #   metric = 'Rsquared'
  # )
  # # define r2 for the rest of the country
  # gam_cv_rsq <- mod2$results |>
  #   as.data.frame() |>
  #   select(Rsquared) |>
  #   max() |>
  #   round(2)
  # # calculate r2 for the selected country
  # test_set$pred2_cty <- predict(mod2, test_set)
  # gam_oob_rsq <- round(cor(test_set$farm_area_ha, test_set$pred2_cty)^2, 2)
  
  # compiling data for the selected country
  one_row <- c(country = my_country,
               rf_cv_rsq = rf_cv_rsq, rf_test_rsq = rf_test_rsq #,
               # gam_cv_rsq = gam_cv_rsq, gam_oob_rsq = gam_oob_rsq,
               # knn_cv_rsq = knn_cv_rsq, knn_oob_rsq = knn_oob_rsq
  )
  all_rows <- bind_rows(all_rows, one_row)
  
  variable_importance_table <- bind_rows(variable_importance_table, variable_importance_one)
  
  print(all_rows)
  print(rf_cv_sd); print(mod1$finalModel$r.squared)
  assign(paste0('results_pairs_', my_country), all_rows, envir = .GlobalEnv)
  assign(paste0('var_importance_', my_country), variable_importance_table, envir = .GlobalEnv)
  return(all_rows)
}

# Initialization and function application (run this chunk of 4 lines at once)
deb <- Sys.time()
all_rows <- variable_importance_table <- data.frame()
mult_rsq <- do.call(bind_rows, lapply(sixteen_countries, country_autoevaluation_rsq))
fin <- Sys.time() - deb
print(fin)

variable_importance_table <- data.frame()
for(i in sixteen_countries){
  variable_importance_table <-bind_rows(variable_importance_table, get(ls(pattern = paste0('var_importance_', i))[1]))
}
write.csv(mult_rsq, '../output/tables/country_auto_evaluation_rsquares.csv', row.names = F)
write.csv(variable_importance_table, '../output/tables/country_variable_importance.csv', row.names = F)

# ------------------------------------------------------------------------
# Using a training set and a test set to evaluate model performance: one country left out

compare_countries_leave_out_one <- function(my_country){
  print(paste0('--------------------', my_country, '---------------------------'))
  set.seed(2024) # just for reproducibility!
  # defining training - test sets: leave out the selected country and predict from the rest of SSA
  training_set <- lsms_spatial |>
    filter(country != my_country) |>
    select(!c(x, y, country))
  test_set <- lsms_spatial |>
    filter(country == my_country) |>
    select(!c(x, y, country))
  
  training_xy_set <- lsms_spatial |>
    filter(country != my_country) 
  test_xy_set <- lsms_spatial |>
    filter(country == my_country) 
  
  # defining training - test sets: leave out the selected country and predict from the rest of SSA
  training_mean_set <- lsms_mean_farm |>
    filter(country != my_country) |>
    select(!c(x, y, country))
  test_mean_set <- lsms_mean_farm |>
    filter(country == my_country) |>
    select(!c(x, y, country))
  
  training_xy_mean_set <- lsms_mean_farm |>
    filter(country != my_country) |>
    select(!country)
  test_xy_mean_set <- lsms_mean_farm |>
    filter(country == my_country) |>
    select(!country)
  
  # if(my_country == 'Rwanda') {
  #   lsms_mean_f2 <- lsms_spatial |>
  #     group_by(country, x, y) |>
  #     summarize(
  #       n = n(),
  #       farm_area_ha = mean(farm_area_ha, na.rm = T)) |>
  #     ungroup()
  #   lsms_mean_f2 <- lsms_spatial |>
  #     select(!farm_area_ha) |>
  #     distinct() |>
  #     inner_join(lsms_mean_f2) |>
  #     filter(n > 5) |>
  #     select(!n)
  #   test_mean_set <- lsms_mean_f2 |>
  #     filter(country == my_country) |>
  #     select(!c(x, y, country))
  # }
  
  # caret control parms
  ctrl <- caret::trainControl(method = 'cv', number = 10, verboseIter = F)
  
  # point-based TPS
  cty_fit1 <- fields::Tps(
    x = as.matrix(test_xy_set[, c('x', 'y')]),
    test_xy_set$farm_area_ha, 
    Z = as.matrix(test_xy_set[, c('cropland', 'cattle', 'pop', 'cropland_per_capita', 'sand', 'slope', 'temperature', 'rainfall', 'market', 'maizeyield')]), 
    lon.lat = T
  )
  test_xy_set$tps1_cty <- predict(
    cty_fit1, test_xy_set[, c('x', 'y')], 
    Z = as.matrix(test_set[, c('cropland', 'cattle', 'pop', 'cropland_per_capita', 'sand', 'slope', 'temperature', 'rainfall', 'market', 'maizeyield')])
  )
  tps1_test_rsq <- round(cor(test_xy_set$farm_area_ha, test_xy_set$tps1_cty)^2, 2)
  
  # average-based TPS
  cty_fit2 <- fields::Tps(
    x = as.matrix(test_xy_mean_set[, c('x', 'y')]),
    test_xy_mean_set$farm_area_ha, 
    Z = as.matrix(test_xy_mean_set[, c('cropland', 'cattle', 'pop', 'cropland_per_capita', 'sand', 'slope', 'temperature', 'rainfall', 'market', 'maizeyield')]), 
    lon.lat = T
  )
  test_xy_mean_set$tps2_cty <- predict(
    cty_fit2, test_xy_mean_set[, c('x', 'y')], 
    Z = as.matrix(test_xy_mean_set[, c('cropland', 'cattle', 'pop', 'cropland_per_capita', 'sand', 'slope', 'temperature', 'rainfall', 'market', 'maizeyield')])
  )
  tps2_test_rsq <- round(cor(test_xy_mean_set$farm_area_ha, test_xy_mean_set$tps2_cty)^2, 2)
  
  
  # point-based RF model train over the country
  mod1 <- caret::train(
    farm_area_ha ~ .,
    data = test_set,
    method = 'ranger',
    # preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  # find the point-based r2 for the country (CV and OOB)
  rf1_cv_rsq <- mod1$results |>
    as.data.frame() |>
    select(Rsquared) |>
    pull() |>
    mean() |>
    round(2)
  rf1_cv_rsq_sd <- mod1$results |>
    as.data.frame() |>
    select(Rsquared) |>
    pull() |>
    sd() |>
    round(2)
  rf1_oob_rsq <- round(mod1$finalModel$r.squared, 2)
  test_set$pred1_cty <- predict(mod1, test_set)
  rf1_test_rsq <- round(cor(test_set$farm_area_ha, test_set$pred1_cty)^2, 2)
  
  # average farm size-based RF model train over the country
  mod2 <- caret::train(
    farm_area_ha ~ .,
    data = test_mean_set,
    method = 'ranger',
    # preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  # find the average-fsize based r2 for the rest of SSA (CV and OOB)
  rf2_cv_rsq <- mod2$results |>
    as.data.frame() |>
    select(Rsquared) |>
    pull() |>
    mean() |>
    round(2)
  rf2_cv_rsq_sd <- mod2$results |>
    as.data.frame() |>
    select(Rsquared) |>
    pull() |>
    sd() |>
    round(2)
  rf2_oob_rsq <- round(mod2$finalModel$r.squared, 2)
  test_mean_set$pred2_cty <- predict(mod2, test_mean_set)
  rf2_test_rsq <- round(cor(test_mean_set$farm_area_ha, test_mean_set$pred2_cty)^2, 2)
  
  # point-based farm size-based RF model train over the rest of SSA
  mod3 <- caret::train(
    farm_area_ha ~ .,
    data = training_set,
    method = 'ranger',
    # preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  # find the point-based r2 for the country (calculated)
  test_set$pred3_cty <- predict(mod3, test_set)
  rf3_test_rsq <- round(cor(test_set$farm_area_ha, test_set$pred3_cty)^2, 2)
  cor_tps1_rf3 <- round(cor(test_xy_set$tps1_cty,  test_set$pred3_cty, use = 'pairwise.complete.obs'))
  cor_rf1_rf3 <- round(cor(test_set$pred1_cty,  test_set$pred3_cty, use = 'pairwise.complete.obs'))
  # average farm size-based RF model train over the rest of SSA
  mod4 <- caret::train(
    farm_area_ha ~ .,
    data = training_mean_set,
    method = 'ranger',
    # preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  # find the average-fsize XY  based r2 for the country (CV and OOB)
  test_mean_set$pred4_cty <- predict(mod4, test_mean_set)
  rf4_test_rsq <- round(cor(test_mean_set$farm_area_ha, test_mean_set$pred4_cty)^2, 2)
  cor_tps2_rf4 <- round(cor(test_xy_mean_set$tps2_cty,  test_mean_set$pred4_cty, use = 'pairwise.complete.obs'))
  cor_rf2_rf4 <- round(cor(test_mean_set$pred2_cty,  test_mean_set$pred4_cty, use = 'pairwise.complete.obs'))
  
  # compiling data for the selected
  one_row <- c(
    country = my_country,  
    tps1_test_rsq = tps1_test_rsq,
    rf1_cv_rsq = rf1_cv_rsq, rf1_cv_rsq_sd = rf1_cv_rsq_sd, rf1_oob_rsq = rf1_oob_rsq,
    rf1_test_rsq = rf1_test_rsq, rf3_test_rsq = rf3_test_rsq, 
    cor_tps1_rf3 = cor_tps1_rf3, cor_rf1_rf3 = cor_rf1_rf3,
    tps2_test_rsq = tps2_test_rsq,
    rf2_cv_rsq = rf2_cv_rsq, rf2_cv_rsq_sd = rf2_cv_rsq_sd, rf2_oob_rsq = rf2_oob_rsq,
    rf2_test_rsq = rf2_test_rsq, rf4_test_rsq = rf4_test_rsq,
    cor_tps2_rf4 = cor_tps2_rf4, cor_rf2_rf4 = cor_rf2_rf4
  )
  all_rows <- bind_rows(all_rows, one_row)
  
  print(all_rows)
  
  assign(paste0('results_', my_country), all_rows, envir = .GlobalEnv)
  return(all_rows)
}

# Initialization and function application (run this chunk of 4 lines at once)
deb <- Sys.time()
all_rows <- data.frame()
mult_rsq <- do.call(bind_rows, lapply(sixteen_countries, compare_countries_leave_out_one))
write.csv(mult_rsq, '../data/processed/point_and_unconsolidated_means_models_TPS_RF_leave_one_out.csv', row.names = F)
fin <- Sys.time() - deb
print(fin)

# visualize 
summary_mult_rsq <- mult_rsq |>
  select(country, 
         tps1_test_rsq, rf1_test_rsq, rf3_test_rsq, cor_tps1_rf3, cor_rf1_rf3,
         tps2_test_rsq, rf2_test_rsq, rf4_test_rsq, cor_tps2_rf4, cor_rf2_rf4) |>
  rename(point_country_TPS_rsq = tps1_test_rsq, point_country_RF_rsq = rf1_test_rsq, point_SSA_RF_rsq = rf3_test_rsq, 
         point_cor_TPS = cor_tps1_rf3, point_cor_RF = cor_rf1_rf3,
         mean_country_TPS_rsq = tps2_test_rsq, mean_country_RF_rsq = rf2_test_rsq, mean_SSA_RF_rsq = rf4_test_rsq, 
         mean_cor_TPS = cor_tps2_rf4, mean_cor_RF = cor_rf2_rf4) |>
  pivot_longer(!country, names_to = 'model', values_to = 'rsq') |>
  mutate(type = case_when(grepl('point', model) ~ 'all points',
                          grepl('mean', model) ~ 'average'),
         model = gsub('point_|mean_','', model)) |>
  group_by(country, model, type) 
summary_mult_rsq$model = factor(summary_mult_rsq$model,
                                levels = c('country_TPS_rsq', 'country_RF_rsq', 'SSA_RF_rsq', 
                                           'cor_TPS', 'cor_RF' ))

# long_mult_rsq <- mult_rsq |>
#   select(country, rf0_oob_rsq, rf2_oob_rsq, rf3_test_rsq) |>
#   rename(point_country_OOB_rsq = rf0_oob_rsq, mean_country_OOB_rsq = rf2_oob_rsq, mean_SSA_OOB_rsq = rf3_test_rsq) |>
#   pivot_longer(!country, names_to = 'model', values_to = 'rsq')

P00 <- ggplot(summary_mult_rsq, 
              aes(country, rsq, fill = model)) +
  geom_bar(stat = 'identity', position = position_dodge(0.5)) +
  labs(x = 'country', y = 'r square/cor. coef.', fill = 'model') +
  facet_grid(~ type) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
P00
png('../output/graphs/point_and_mean_based_cross_validation.png', height = 15, width = 30, units = 'cm', res = 1000)
P00
ggsave('../output/graphs/point_and_mean_based_cross_validation.png')
dev.off()


P01 <- ggplot(summary_mult_rsq |>
                filter(type == 'average'), 
              aes(country, rsq, fill = model)) + 
  geom_bar(stat = 'identity', position = position_dodge(0.8), colour = 'black') + 
  # geom_errorbar(aes(ymin = mean_rsq, ymax = mean_rsq + sd_rsq), 
  #               position = position_dodge(0.8), width = 0.3) +
  labs(x = 'country', y = 'r_square or cor_coef', fill = 'model') + 
  scale_fill_manual(values = pal2(5)) +
  theme_bw() + 
  theme(axis.text = element_text(angle = 45, hjust = 1))
P01

png('../output/graphs/mean_based_cross_validation.png', height = 15, width = 25, units = 'cm', res = 1000)
P01
ggsave('../output/graphs/mean_based_cross_validation.png')
dev.off()