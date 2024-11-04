# Compare ML models,
# namely check how random-forest perform vis-a-vis Thin plate spline, GBM, SVM


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

fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# I think an overall thin plate spline model (at continental scale) is not meaningful as it would be interpolation over large distances between countries
# define a TPS method for caret
tps_model <- list(
  type = "Regression",
  label = 'Thin plate spline',
  library = "fields",
  loop = NULL,
  parameters = data.frame(parameter = "lambda", class = "numeric", label = "Smoothing Parameter"),
  grid = function(x, y, len = 3, search = "grid") { #len was NULL
    data.frame(lambda = 10^seq(-3, 3, length = len))
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    fields::Tps(x, y, lambda = param$lambda, ...)
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    predict(modelFit, newdata)
  },
  prob = NULL
)
# Prepare data: load lsms data as my_lsms (lsms with geometry as data.frame)
load('../data/processed/lsms_spatial_raw.rdata') # this was retrieved from '02.customize_spatial_data.r'

# Using a training set and a test set to evaluate model performance
compare_country_models <- function(my_country){
  set.seed(2024) # just for reproducibility!
  # caret control parms
  ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = F)
  
  print(paste0('--------------- Thin plate spline in ', my_country, '-------------'))
  my_lsms_cty <- lsms_spatial_raw |>
    filter(country == my_country) |>
    select(x, y,                                                                              # omit country_name, region, and farm_id
           cropland, cattle, pop, cropland_per_capita,
           sand, slope, temperature, rainfall, 
           market, maizeyield, farm_area_ha) |>     # maize_yield removed  for colinearity issue in TPS_covariates # omit GDP because it is not relevant at country-level
    na.omit()
  
  #training - test split (70-30 rule)
  training_set_xy <- my_lsms_cty |>  
    sample_n(round(0.7 * nrow(my_lsms_cty)) )
  test_set_xy <- my_lsms_cty |>
    anti_join(training_set_xy)
  
  # Thin plate spline (only the coordinates, then coordinates and  covariates)
  
  tps_country_model_xy <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy |>
      select(farm_area_ha, x, y),
    method = tps_model,
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  
  # print R2, first for the 10-fold CV, then for the 70-30 split # I don't know what to present
  print(tps_country_model_xy)
  my_grid <- with(test_set_xy, cbind(x, y)) |> as.matrix()                      # Construct a grid over which prediction will be made:do use the test set!
  test_set_xy$pred_tps_xy  <- as.numeric(predict(tps_country_model_xy, my_grid))# assign to the new column of test_set the prediction made over the grid
  rsq_tps_xy <- with(test_set_xy, round(cor(farm_area_ha, pred_tps_xy)^2, 2))   # Get the r2
  
  print(paste0('TPS_rsq_without_covariate = ', round(rsq_tps_xy, 2)))
  
  # plot TPS inpterpolation, but not for Zambia (computational power is a constraint)
  if(my_country != 'Zambia') {
    P01 <- ggplot(test_set_xy, aes(farm_area_ha, pred_tps_xy)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, colour  = 'red4', linewidth =0.8) +
      labs(title = my_country) +
      annotate('text', x = 10, y = 13, label = bquote(R^2== .(rsq_tps_xy)) ) +
      theme_minimal()
    
    # Construct a country grid over which prediction will be made
    cty_vect <- subset(ssa, ssa$GID_0 == fourteen_country_codes[which(fourteen_countries == my_country)])
    cty_grid <- terra::rast(cty_vect, nrow = 100, ncol = 100, nlyrs = 1)
    cty_rast <- terra::rasterize(cty_vect, cty_grid)
    cty_coords <- terra::xyFromCell(cty_rast, cell = terra::cells(cty_rast))
    
    # Predict farm sizes over the country grid and map it : do not use the training or test set only, but both
    # the basic plot (true interpolation)
    cty_fit <- fields::Tps(cbind(my_lsms_cty$x, my_lsms_cty$y),
                           my_lsms_cty$farm_area_ha, lon.lat = T)
    cty_rast2 <- terra::interpolate(terra::rast(cty_rast), cty_fit)
    cty_rast2 <- terra::mask(cty_rast2, cty_rast)
    cty_rast2[cty_rast2 <= 0] <- NA
    png(paste0('../output/maps/ML_model_comparison_', my_country, '_Tps_xy.png'), units="in", width=5.5, height=5.5, res=1000)
    M01 <- {
      terra::plot(cty_vect, main = paste0(my_country, ' - Tps_xy only'))
      terra::plot(cty_rast2, col = terrain.colors(100), add = T)
    }
    dev.off()
  } else {
    print(' The Zambian TPS would take too long to run.')
  }
  
  # Thin plate spline (with covariates)
  # tps_country_model_xyz <- fields::Tps(
  #   with(training_set_xy, cbind(x, y)),
  #   training_set_xy[, 'farm_area_ha'],
  #   Z = as.matrix(training_set_xy[, c('cropland', 'cattle', 'population', 'sand', 'elevation',
  #                                     'market', 'rainfall', 'maizeyield')]), lon.lat = T )
  tps_country_model_xyz <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy,
    method = tps_model,
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  # Print the R2 for 10-fold CV, and then for the 70-30 split
  print(tps_country_model_xyz)
  test_set_xy$pred_tps_xyz <- predict(tps_country_model_xyz,
                                      as.matrix(test_set_xy) )
  rsq_tps_xyz <- with(test_set_xy, round(cor(farm_area_ha, pred_tps_xyz)^2, 2)) 
  print(paste0('TPS_rsq_with_covariate = ', round(rsq_tps_xyz, 2)))
  
  # Gradient boosting machines (only the covariates)
  # gbm_country_model <- gbm::gbm(farm_area_ha ~ ., data = training_set_xy |> select(!c(x, y)),
  #                               n.trees = 1500, cv.folds = 5) # removing n.trees gives an optimal nb of iterations around 100 (using gbm::perf)

  gbm_country_model <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy |>
      select(!c(x, y)),
    method = 'xgbTree',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  print(gbm_country_model)
  test_set_xy$pred_gbm <- as.numeric(predict(gbm_country_model, test_set_xy)  )
  rsq_gbm <- round(cor(test_set_xy$farm_area_ha, test_set_xy$pred_gbm)^2, 2)
  # additionally print GBM performances with 2 different  methods
  # gbm::gbm.perf(gbm_country_model, method = 'OOB')
  # gbm::gbm.perf(gbm_country_model, method = 'cv')
  print(paste0('rsq_gbm = ', round(rsq_gbm, 2)))
  P04 <- ggplot(test_set_xy, aes(farm_area_ha, pred_gbm)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour  = 'red4', size =0.8) +
    labs(title = my_country) +
    annotate('text', x = 10, y = 13, label = bquote(R^2== .(rsq_gbm)) ) +
    theme_minimal()
  
  # Gradient boosting machines (only the coordinates)
  # gbm_country_model_xy <- gbm::gbm(farm_area_ha ~ ., 
  #                                  data = training_set_xy |> select(x, y, farm_area_ha),
  #                                  n.trees = 1500, cv.folds = 5) # removing n.trees gives an optimal nb of iterations around 100 (using gbm::perf)
  gbm_country_model_xy <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy |>
      select(farm_area_ha, x, y),
    method = 'xgbTree',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  print(gbm_country_model_xy)
  test_set_xy$pred_gbm_xy <- as.numeric(predict(gbm_country_model_xy, test_set_xy)  )
  rsq_gbm_xy <- round(cor(test_set_xy$farm_area_ha, test_set_xy$pred_gbm_xy)^2, 2)
  # additionally print GBM performances with 2 different  methods
  # print(gbm::gbm.perf(gbm_country_model_xy, method = 'OOB'))
  # print(gbm::gbm.perf(gbm_country_model_xy, method = 'cv'))
  print(paste0('rsq_gbm_xy = ', round(rsq_gbm_xy, 2)))
  P05 <- ggplot(test_set_xy, aes(farm_area_ha, pred_gbm_xy)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour  = 'red4', size =0.8) +
    labs(title = my_country) +
    annotate('text', x = 10, y = 13, label = bquote(R^2== .(rsq_gbm_xy)) ) +
    theme_minimal()
  
  # Gradient boosting machines (coordinates and covariates) # I do have some doubts about the approach!
  # gbm_country_model_xyz <- gbm::gbm(farm_area_ha ~ ., 
  #                                  data = training_set_xy,
  #                                  n.trees = 1500, cv.folds = 5) # removing n.trees gives an optimal nb of iterations around 100 (using gbm::perf)
  gbm_country_model_xyz <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy,
    method = 'xgbTree',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  print(gbm_country_model_xyz)
  test_set_xy$pred_gbm_xyz <- as.numeric(predict(gbm_country_model_xyz, test_set_xy)  )
  rsq_gbm_xyz <- round(cor(test_set_xy$farm_area_ha, test_set_xy$pred_gbm_xyz)^2, 2)
  # additionally print GBM performances with 2 different  methods
  # gbm::gbm.perf(gbm_country_model_xyz, method = 'OOB')
  # gbm::gbm.perf(gbm_country_model_xyz, method = 'cv')
  print(paste0('rsq_gbm_xyz = ', round(rsq_gbm_xyz, 2)))
  P06 <- ggplot(test_set_xy, aes(farm_area_ha, pred_gbm_xyz)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour  = 'red4', size =0.8) +
    labs(title = my_country) +
    annotate('text', x = 10, y = 13, label = bquote(R^2== .(rsq_gbm_xyz)) ) +
    theme_minimal()
  
  # Support vector machines (only the covariates)
  # svm_country_model <- e1071::svm(farm_area_ha ~ ., 
  #                                 data = training_set_xy |> select(!c(x, y)),
  #                                 n.trees = 1500, cross = 5) # tune.svm() suggested  cross = 10
  svm_country_model <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy |>
      select(!c(x, y)),
    method = 'svmRadial',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  print(svm_country_model)
  test_set_xy$pred_svm <- as.numeric(predict(svm_country_model, test_set_xy)  )
  rsq_svm <- round(cor(test_set_xy$farm_area_ha, test_set_xy$pred_svm)^2, 2)
  print(paste0('rsq_svm = ', round(rsq_svm, 2)))
  P07 <- ggplot(test_set_xy, aes(farm_area_ha, pred_svm)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour  = 'red4', size =0.8) +
    labs(title = my_country) +
    annotate('text', x = 10, y = 13, label = bquote(R^2== .(rsq_svm)) ) +
    theme_minimal()
  
  # Support vector machines (only the coordinates)
  # svm_country_model_xy <- e1071::svm(farm_area_ha ~ ., 
  #                                 data = training_set_xy |> select(x, y, farm_area_ha),
  #                                 n.trees = 1500, cross = 5) 
  svm_country_model_xy <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy |>
      select(farm_area_ha, x, y),
    method = 'svmRadial',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  print(svm_country_model_xy)
  test_set_xy$pred_svm_xy <- as.numeric(predict(svm_country_model_xy, test_set_xy)  )
  rsq_svm_xy <- round(cor(test_set_xy$farm_area_ha, test_set_xy$pred_svm_xy)^2, 2)
  print(paste0('rsq_svm_xy = ', round(rsq_svm_xy, 2)))
  P08 <- ggplot(test_set_xy, aes(farm_area_ha, pred_svm_xy)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour  = 'red4', size =0.8) +
    labs(title = my_country) +
    annotate('text', x = 10, y = 13, label = bquote(R^2== .(rsq_svm_xy)) ) +
    theme_minimal()
  
  # Support vector machines (coordinates and covariates)
  # svm_country_model_xyz <- e1071::svm(farm_area_ha ~ ., data = training_set_xy,
  #                                 n.trees = 1500, cross = 5) 
  svm_country_model_xyz <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy,
    method = 'svmRadial',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  print(svm_country_model_xyz)
  test_set_xy$pred_svm_xyz <- as.numeric(predict(svm_country_model_xyz, test_set_xy)  )
  rsq_svm_xyz <- round(cor(test_set_xy$farm_area_ha, test_set_xy$pred_svm_xyz)^2, 2)
  print(paste0('rsq_svm_xyz = ', round(rsq_svm_xyz, 2)))
  P09 <- ggplot(test_set_xy, aes(farm_area_ha, pred_svm_xyz)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour  = 'red4', size =0.8) +
    labs(title = my_country) +
    annotate('text', x = 10, y = 13, label = bquote(R^2== .(rsq_svm_xyz)) ) +
    theme_minimal()
  
  # Random forest (only the covariates)
  # rf_country_model <- randomForest::randomForest(farm_area_ha ~ ., data = training_set_xy |> select(!c(x, y)),
  #                                                n.trees = 1500, cross = 5)
  rf_country_model <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy |>
      select(!c(x, y)),
    method = 'ranger',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  print(rf_country_model)
  test_set_xy$pred_rf <- as.numeric(predict(rf_country_model, test_set_xy)  )
  rsq_rf <- round(cor(test_set_xy$farm_area_ha, test_set_xy$pred_rf)^2, 2)
  print(paste0('rsq_rf = ', round(rsq_rf, 2)))
  P10 <- ggplot(test_set_xy, aes(farm_area_ha, pred_rf)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour  = 'red4', size =0.8) +
    labs(title = my_country) +
    annotate('text', x = 10, y = 13, label = bquote(R^2== .(rsq_rf)) ) +
    theme_minimal() 
  # Plot the map with RF
  # if(my_country != 'Zambia'){
  #   # refit the model using the whole dataset (training  + test sets)
  #   stacked_cty <- terra::rast(paste0('../data/processed/stacked_cty_rasters_', my_country,'.tif'))
  #   cty_fit <- randomForest::randomForest(farm_area_ha ~ ., data = my_lsms_cty |> select(!c(x, y)),
  #                                         n.trees = 1500, cross = 5)
  #   cty_rast3 <- terra::predict(stacked_cty, cty_fit, type = 'response', na.rm = T)
  #   png(paste0('../output/maps/ML_model_comparison_', my_country, '_RF_covariates_only.png'), units="in", width=5.5, height=5.5, res=1000)
  #   M10 <- {
  #     terra::plot(cty_vect, main = paste0(my_country, ' - RF_covariates only'))
  #     terra::plot(cty_rast3, col = terrain.colors(100), add = T)
  #   }
  #   dev.off()
  # } else {
  #   print('')
  # }
  
  # Random forest (only the coordinates)
  # rf_country_model_xy <- randomForest::randomForest(farm_area_ha ~ .,
  #                                                data = training_set_xy |> select(x, y, farm_area_ha),
  #                                                n.trees = 1500, cross = 5)
  rf_country_model_xy <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy |>
      select(farm_area_ha, x, y),
    method = 'ranger',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  print(rf_country_model_xy)
  test_set_xy$pred_rf_xy <- as.numeric(predict(rf_country_model_xy, test_set_xy)  )
  rsq_rf_xy <- round(cor(test_set_xy$farm_area_ha, test_set_xy$pred_rf_xy)^2, 2)
  print(paste0('rsq_rf_xy = ', round(rsq_rf_xy, 2)))
  P11 <- ggplot(test_set_xy, aes(farm_area_ha, pred_rf_xy)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour  = 'red4', size =0.8) +
    labs(title = my_country) +
    annotate('text', x = 10, y = 13, label = bquote(R^2== .(rsq_rf_xy)) ) +
    theme_minimal()
  
  # Random forest (coordinates and covariates)
  # rf_country_model_xyz <- randomForest::randomForest(farm_area_ha ~ ., data = training_set_xy,
  #                                                n.trees = 1500, cross = 5)
  rf_country_model_xyz <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy,
    method = 'ranger',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  print(rf_country_model_xyz)
  test_set_xy$pred_rf_xyz <- as.numeric(predict(rf_country_model_xyz, test_set_xy)  )
  rsq_rf_xyz <- round(cor(test_set_xy$farm_area_ha, test_set_xy$pred_rf_xyz)^2, 2)
  print(paste0('rsq_rf_xyz = ', round(rsq_rf_xyz, 2)))
  P12 <- ggplot(test_set_xy, aes(farm_area_ha, pred_rf_xyz)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour  = 'red4', size =0.8) +
    labs(title = my_country) +
    annotate('text', x = 10, y = 13, label = bquote(R^2== .(rsq_rf_xyz)) ) +
    theme_minimal()
  
  one_rsq <- cbind.data.frame(country = my_country, 
                              rsq_tps_xy = rsq_tps_xy, rsq_tps_xyz = rsq_tps_xyz,
                              rsq_gbm = rsq_gbm, rsq_gbm_xy = rsq_gbm_xy, rsq_gbm_xyz = rsq_gbm_xyz, 
                              rsq_svm = rsq_svm, rsq_svm_xy = rsq_svm_xy, rsq_svm_xyz = rsq_svm_xyz, 
                              rsq_rf = rsq_rf, rsq_rf_xy = rsq_rf_xy, rsq_rf_xyz = rsq_rf_xyz  #,
                              # rmse_tps_xy = rmse_tps_xy, rmse_tps_cov = rmse_tps_xyz,
                              # rmse_gbm = rmse_gbm, rmse_svm = rmse_svm, rmse_rf = rmse_rf
                              )
  mult_rsq <- rbind(mult_rsq, one_rsq)
  results <- list(mult_rsq, P01, # P02, P03, 
                  P04, P05, P06,
                  # P07, P08, P09, 
                  P10, P11, P12,
                  # M01, M10,
                  tps_country_model_xy, # tps_country_model_xyz,
                  gbm_country_model, gbm_country_model_xy, gbm_country_model_xyz,
                  svm_country_model, svm_country_model_xy, svm_country_model_xyz,
                  rf_country_model, rf_country_model_xy, rf_country_model_xyz
                  )
  assign(paste0('results_', my_country), results, envir = .GlobalEnv)
}

# Initialization and function application (run this chunk of 4 lines at once)
deb <- Sys.time()
mult_rsq <- data.frame()
sapply(fourteen_countries, compare_country_models)
fin <- Sys.time() - deb
print(fin)

# Compiling the R squares per model per country
mult_rsq <- bind_rows(results_Benin[[1]], results_Burkina[[1]], results_Cote_d_Ivoire[[1]],
                      results_Ethiopia[[1]], results_Guinea_Bissau[[1]], results_Malawi[[1]], results_Mali[[1]], 
                      results_Niger[[1]], results_Nigeria[[1]], results_Senegal[[1]], results_Tanzania[[1]], 
                      results_Togo[[1]], results_Uganda[[1]], results_Zambia[[1]] )

model_perf <- mult_rsq |>
  pivot_longer(cols=starts_with('rsq_'),
               names_prefix = 'rsq_',
               names_to = 'model',
               values_to = 'r_sq') |>
  arrange(country, desc(r_sq))
# model_perf_wide <- reshape2::dcast(model_perf, model ~ country, value.var='r_sq')
model_perf_wide <-model_perf |>
  pivot_wider(id_cols = model, names_from = country, values_from = r_sq)
save(mult_rsq, model_perf, model_perf_wide,
     results_Benin, results_Burkina, results_Cote_d_Ivoire, 
     results_Ethiopia, results_Guinea_Bissau, results_Malawi,results_Mali,
      results_Niger, results_Nigeria, results_Senegal, results_Tanzania, 
     results_Togo, results_Uganda, results_Zambia, 
     file = '../data/processed/compare_country_models.Rdata')
save(model_perf_wide, file = '../output/tables/comparison_ML_models_per_country.Rdata')
write.csv(model_perf_wide, file = '../output/tables/comparison_ML_models_per_country.csv')