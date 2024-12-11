# Evaluate model framework, leaving out 1 country and compare the predictions with interpolation


#-------------------------------------------------------------------------------
# load packages
require(tidyverse) # sorry dear Robert, I felt lazy to recode in basic R
require(fields)
require(caret)
require(ranger)

# Clean environment
rm(list=ls())

# Set working directory
setwd(here::here())
# ------------------------------------------------------------------------------
# create directories if needed
if(!dir.exists('../data'))
  dir.create('../data') # drop the input files here
if(!dir.exists('../output'))
  dir.create('../output') 

# define input and ouptut foders and files # please modify accordingly!
input_path <- '../data'
output_path <- '../output'
model_result_file1 <- '../output/model_evaluation_leave_one_country_out.Rdata'

# Prepare data
load('../data/lsms_trimmed_95th_africa.rdata') 
stacked <- terra::rast('../data/stacked_rasters_africa.tif')

# ------------------------------------------------------------------------------
# data wrangling
# list of countries
fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')

# keep only variables needed in the model
df <- lsms_spatial |>
  select(x, y, country, farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>
  na.omit() 

# Using a training set (all other countries) and a test set (country of interest) to evaluate model performance
leave_one_country_models <- function(my_country){
  set.seed(2024) # just for reproducibility!
  
  # caret control parms
  ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = F)
  
  # subsetting df: training - test split (point-based)
  training_set <- df |>  
    filter(country != my_country) |>
    select(!country) |>
    na.omit()
  test_set <- df |>  
    filter(country == my_country) |>
    select(!country) |>
    na.omit()
  
  #training - test split (consolidated mean-based => exclude all points with less than 10 records)
  training_set_mean <- training_set |>
    group_by(x, y) |>
    summarize(across(where(is.numeric), \(x) mean(x, na.rm = T)),
              n_obs = n()) |>
    filter(n_obs > 9) |>
    select(!n_obs) |>
    ungroup()
  test_set_mean <- test_set |>
    group_by(x, y) |>
    summarize(across(where(is.numeric), \(x) mean(x, na.rm = T)),
              n_obs = n()) |>
    filter(n_obs > 9) |>
    select(!n_obs) |>
    ungroup()
  
  print(paste0('--------------- Model evaluation in ', my_country, ' (point-based) -------------'))
  #-------- Thin plate spline (only the coordinates)-----------
  
  # Fit a TPS model (trained with  my_country), predict farm sizes over the country grid and map it 
  cty_fit0 <- fields::Tps(cbind(test_set$x, test_set$y),  # with X and Y only
                          test_set$farm_area_ha, lon.lat = T)
  
  cty_fit1 <- fields::Tps(
    x = as.matrix(test_set[, c('x', 'y')]),
    test_set$farm_area_ha, 
    Z = as.matrix(test_set[, c('cropland', 'cattle', 'pop', 'cropland_per_capita',
                                  'sand', 'slope', 'temperature',
                                  'rainfall', 'market', 'maizeyield')]),
    lon.lat = T
  )
  
  # # Construct a country grid over which prediction will be made
  # cty_vect <- subset(ssa, ssa$GID_0 == fourteen_country_codes[which(fourteen_countries == my_country)])
  # cty_grid <- terra::rast(cty_vect, nrow = 100, ncol = 100, nlyrs = 1)
  # cty_rast <- terra::rasterize(cty_vect, cty_grid)
  # cty_coords <- terra::xyFromCell(cty_rast, cell = terra::cells(cty_rast))
  
  # predict the TPS on the coordinates of observed data
  test_set$pred_tps  <- as.numeric(
    predict(
      cty_fit1, test_set[, c('x', 'y')],
      Z = as.matrix(test_set[, c('cropland', 'cattle', 'pop', 'cropland_per_capita',
                                    'sand', 'slope', 'temperature',
                                    'rainfall', 'market', 'maizeyield')]
      )
    )
  )
  # assign to the new column of test_set the prediction made over the grid
  pt_rsq_tps <- with(test_set, round(cor(farm_area_ha, pred_tps)^2, 2))   # Get the r2
  print(paste0('TPS_calculated_pt_rsq_with_covariate = ', round(pt_rsq_tps, 2)))
  
  # # plot it (the plot failed with covariates, I'm only showing Tps based on x and y)
  # cty_rast1 <- terra::interpolate(terra::rast(cty_rast), cty_fit0)
  # cty_rast2 <- terra::mask(cty_rast1, cty_rast)
  # cty_rast2[cty_rast2 <= 0] <- NA
  # png(paste0('../output/ML_model_comparison_', my_country, '_Tps.png'), units="in", width=5.5, height=5.5, res=1000)
  # M01 <- {
  #   terra::plot(cty_vect, main = paste0(my_country, ' - Tps only'))
  #   terra::plot(cty_rast2, col = terrain.colors(100), add = T)
  # }
  # dev.off()
  
  # Random forest with my_country (only the covariates). This serves as reference
  rf_country_ref <- caret::train(
    farm_area_ha ~ .,
    data = test_set |>
      select(!c(x, y)),
    method = 'ranger',
    # preProcess = c('center', 'scale', 'spatialSign'),
    # trControl = ctrl,
    metric = 'Rsquared'
  )
  test_set$pred_reference <- as.numeric(predict(rf_country_ref, test_set)  )
  pt_rsq_reference <- round(cor(test_set$farm_area_ha, test_set$pred_reference)^2, 2)
  print(paste0('pt_rsq_reference = ', round(pt_rsq_reference, 2)))
  pt_rsq_ref_cv <- rf_country_ref$results |>
    as.data.frame() |>
    select(Rsquared) |>
    pull() |>
    mean() |>
    round(2)
  print(paste0('pt_rsq_reference_cv = ', pt_rsq_ref_cv))
  # Random forest with other countries (only the covariates)
  rf_country_model <- caret::train(
    farm_area_ha ~ .,
    data = training_set |>
      select(!c(x, y)),
    method = 'ranger',
    # preProcess = c('center', 'scale', 'spatialSign'),
    # trControl = ctrl,
    importance = 'permutation',
    metric = 'Rsquared'
  )
  print(rf_country_model)
  test_set$pred_other <- as.numeric(predict(rf_country_model, test_set)  )
  pt_rsq_other <- round(cor(test_set$farm_area_ha, test_set$pred_other)^2, 2)
  print(paste0('pt_rsq_other_countries = ', round(pt_rsq_other, 2)))
  pt_rsq_other_cv <- rf_country_model$results |>
    as.data.frame() |>
    select(Rsquared) |>
    pull() |>
    mean() |>
    round(2)
  print(paste0('pt_rsq_other_cv = ', pt_rsq_other_cv))
  # calculate Rsquare showing agreement between predictions from in-country TPS and RF_model from other countries
  pt_cor_other_vs_tps <- with(test_set, round(cor(pred_tps, pred_other, use = 'complete.obs'), 2))   # Get the cor coef
  pt_rsq_other_vs_tps <- with(test_set, round(cor(pred_tps, pred_other, use = 'complete.obs')^2, 2))   # Get the r2
  print(paste0('pt_rsq_TPS_vs_other_countries = ', round(pt_rsq_other_vs_tps, 2)))
  
  
  
  
  print(paste0('--------------- Model evaluation in ', my_country, ' (consolidated means) -------------'))
  #-------- Thin plate spline (only the coordinates)-----------
  
  # Fit a TPS model (trained with  my_country), predict farm sizes over the country grid and map it 
  cty_fit0 <- fields::Tps(cbind(test_set_mean$x, test_set_mean$y),  # with X and Y only
                          test_set_mean$farm_area_ha, lon.lat = T)
  
  cty_fit1 <- fields::Tps(
    x = as.matrix(test_set_mean[, c('x', 'y')]),
    test_set_mean$farm_area_ha, 
    Z = as.matrix(test_set_mean[, c('cropland', 'cattle', 'pop', 'cropland_per_capita',
                               'sand', 'slope', 'temperature',
                               'rainfall', 'market', 'maizeyield')]),
    lon.lat = T
  )
  
  
  # predict the TPS on the coordinates of observed data
  test_set_mean$pred_tps  <- as.numeric(
    predict(
      cty_fit1, test_set_mean[, c('x', 'y')],
      Z = as.matrix(test_set_mean[, c('cropland', 'cattle', 'pop', 'cropland_per_capita',
                                 'sand', 'slope', 'temperature',
                                 'rainfall', 'market', 'maizeyield')]
      )
    )
  )
  # assign to the new column of test_set_mean the prediction made over the grid
  mn_rsq_tps <- with(test_set_mean, round(cor(farm_area_ha, pred_tps)^2, 2))   # Get the r2
  print(paste0('TPS_calculated_mn_rsq_with_covariate = ', round(mn_rsq_tps, 2)))
  
  # # plot it (the plot failed with covariates, I'm only showing Tps based on x and y)
  # cty_rast1 <- terra::interpolate(terra::rast(cty_rast), cty_fit0)
  # cty_rast2 <- terra::mask(cty_rast1, cty_rast)
  # cty_rast2[cty_rast2 <= 0] <- NA
  # png(paste0('../output/ML_model_comparison_', my_country, '_Tps.png'), units="in", width=5.5, height=5.5, res=1000)
  # M01 <- {
  #   terra::plot(cty_vect, main = paste0(my_country, ' - Tps only'))
  #   terra::plot(cty_rast2, col = terrain.colors(100), add = T)
  # }
  # dev.off()
  
  # Random forest with my_country (only the covariates). This serves as reference
  rf_country_ref <- caret::train(
    farm_area_ha ~ .,
    data = test_set_mean |>
      select(!c(x, y)),
    method = 'ranger',
    # preProcess = c('center', 'scale', 'spatialSign'),
    # trControl = ctrl,
    metric = 'Rsquared'
  )
  test_set_mean$pred_reference <- as.numeric(predict(rf_country_ref, test_set_mean)  )
  mn_rsq_reference <- round(cor(test_set_mean$farm_area_ha, test_set_mean$pred_reference)^2, 2)
  print(paste0('mn_rsq_reference = ', round(mn_rsq_reference, 2)))
  mn_rsq_ref_cv <- rf_country_ref$results |>
    as.data.frame() |>
    select(Rsquared) |>
    pull() |>
    mean() |>
    round(2)
  print(paste0('mn_rsq_reference_cv = ', mn_rsq_ref_cv))
  # Random forest with other countries (only the covariates)
  rf_country_model <- caret::train(
    farm_area_ha ~ .,
    data = training_set_mean |>
      select(!c(x, y)),
    method = 'ranger',
    # preProcess = c('center', 'scale', 'spatialSign'),
    # trControl = ctrl,
    importance = 'permutation',
    metric = 'Rsquared'
  )
  print(rf_country_model)
  test_set_mean$pred_other <- as.numeric(predict(rf_country_model, test_set_mean)  )
  mn_rsq_other <- round(cor(test_set_mean$farm_area_ha, test_set_mean$pred_other)^2, 2)
  print(paste0('mn_rsq_other_countries = ', round(mn_rsq_other, 2)))
  mn_rsq_other_cv <- rf_country_model$results |>
    as.data.frame() |>
    select(Rsquared) |>
    pull() |>
    mean() |>
    round(2)
  print(paste0('mn_rsq_other_cv = ', mn_rsq_other_cv))
  # calculate Rsquare showing agreement between predictions from in-country TPS and RF_model from other countries
  mn_cor_other_vs_tps <- with(test_set_mean, round(cor(pred_tps, pred_other, use = 'complete.obs'), 2))   # Get the cor coef
  mn_rsq_other_vs_tps <- with(test_set_mean, round(cor(pred_tps, pred_other, use = 'complete.obs')^2, 2))   # Get the r2
  print(paste0('mn_rsq_TPS_vs_other_countries = ', round(mn_rsq_other_vs_tps, 2)))
  
  
  
  # compile results
  one_rsq <- cbind.data.frame(
    country = my_country, 
    # point-basedmodel
    pt_rsq_tps = pt_rsq_tps, 
    pt_rsq_reference = pt_rsq_reference,        # RF trained and tested on my_country, calculated R2_oob
    pt_rsq_ref_cv = pt_rsq_ref_cv,              # RF trained and tested on my_country, cross-validated R2
    pt_rsq_other = pt_rsq_other,                # RF trained on other countries and tested on my_country, R2_oob
    pt_rsq_other_cv = pt_rsq_other_cv,          # RF trained on other countries and tested on my_country, R2_cv
    pt_rsq_other_vs_tps  = pt_rsq_other_vs_tps, # R2 for predictions from Tps vs RF trained on other countries, R2_oob
    pt_cor_other_vs_tps = pt_cor_other_vs_tps,        # correlation coef for predictions from RF vs Tps, cor coef
    
    # consolidated means-based model
    mn_rsq_tps = mn_rsq_tps, 
    mn_rsq_reference = mn_rsq_reference,        # RF trained and tested on my_country, calculated R2_oob
    mn_rsq_ref_cv = mn_rsq_ref_cv,              # RF trained and tested on my_country, cross-validated R2
    mn_rsq_other = mn_rsq_other,                # RF trained on other countries and tested on my_country, R2_oob
    mn_rsq_other_cv = mn_rsq_other_cv,          # RF trained on other countries and tested on my_country, R2_cv
    mn_rsq_other_vs_tps  = mn_rsq_other_vs_tps, # R2 for predictions from Tps vs RF trained on other countries, R2_oob
    mn_cor_other_vs_tps = mn_cor_other_vs_tps         # correlation coef for predictions from RF vs Tps, cor coef
  )
  mult_rsq <- rbind.data.frame(mult_rsq, one_rsq)
  
  results <- list(mult_rsq, 
                  cty_fit0, cty_fit1,
                  rf_country_ref, rf_country_model
                  )
  assign(paste0('rsq_table_for_', my_country), mult_rsq, envir = .GlobalEnv)
  assign(paste0('results_', my_country), results, envir = .GlobalEnv)
  
  write.csv(mult_rsq, file = paste0('../output/model_eval_', my_country, '.csv'), row.names = F)
  write.csv(mult_rsq, file = model_result_file1, row.names = F)
  save(results, file = paste0('../output/results_', my_country, '.rdata'))
  return(mult_rsq)
}

# Initialization and function application
# deb <- Sys.time()
# mult_rsq <- data.frame()
# sapply(fourteen_countries, leave_one_country_models)
# fin <- Sys.time() - deb
# print(fin)

deb <- Sys.time()
mult_rsq <- data.frame()
for(i in fourteen_countries){
  leave_one_country_models(i)
}
fin <- Sys.time() - deb
print(fin)

