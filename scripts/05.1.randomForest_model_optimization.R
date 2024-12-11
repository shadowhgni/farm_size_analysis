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
# Prepare data: load lsms data and stacked (raster of drivers)
load('../data/processed/lsms_trimmed_95th_africa.rdata') # this was retrieved from '03.1.pooled_data_for_analysis.r'
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')

# cheap clustering of datapoints in RALS- Zambia
lsms_spatial <- lsms_spatial |>
  mutate(x = case_when(country == 'Zambia' ~ round(x, 1),
                       .default = x),
         y = case_when(country == 'Zambia' ~ round(y, 1),
                       .default = y))

# keep only variables needed in the models
lsms_spatial <- lsms_spatial |>
  select(
    farm_area_ha, cropland, cattle, pop, cropland_per_capita,
    sand, slope, temperature, rainfall, maizeyield, market
  ) |>
  na.omit() 

# ------------------------------------------------------------------------------
# # Two models for stacking/ensembling, starting with the overall quantile random forest with ranger
# 
# lsms_spatial0 <- lsms_spatial
# lsms_spatial1 <- lsms_spatial |>
#   select(farm_area_ha, cropland, cattle, pop, cropland_per_capita,
#          sand, slope, temperature, rainfall, market, maizeyield)
# 
# #- fast optimization of hyperparameters
# task1 <- mlr::makeRegrTask(data = lsms_spatial1, target = 'farm_area_ha')
# hyperparms <- tuneRanger::tuneRanger(
#   task = task1,
#   num.trees = 1500,
#   tune.parameters = c('mtry', 'min.node.size', 'split.rule'),
#   build.final.model = T
# )
# check approx(x = probs, y = quantiles, xout = sort(runif(pop_farms)), method = 'linear', rule = 2) to generate virtual farm sizes
#- regular ranger used in caret
deb <- Sys.time()

train_control <- caret::trainControl(method = 'cv', number = 10, savePredictions = 'all', seeds = 2024)
tune_grid <- expand.grid(
  mtry = 3:10,                     
  splitrule = 'extratrees',                       
  min.node.size = c(5, 45:55, 60, 100, )
)

rf_full_model <- caret::train(
  farm_area_ha ~ .,
  data = lsms_spatial,
  method = 'ranger',
  preProcess = c('center', 'scale', 'spatialSign'),
  trainControl = train_control,
  # quantreg = T,
  keep.inbag = T,
  tuneGrid = tune_grid,
  importance  = 'permutation',
  num.trees = 1500
)
tree_info <- ranger::treeInfo(qrf_best_model$finalModel, tree = 1)
save(rf_full_model, file = '../data/processed/rf_full_model_with_95th_trimmed_data.rdata')
fin <- Sys.time() - deb; print(fin)


# old school
qrf_best_model <- quantregForest::quantregForest(
  x = lsms_spatial[, c('cropland', 'cattle', 'pop', 'cropland_per_capita',
                       'sand', 'slope', 'temperature', 'rainfall', 
                       'maizeyield', 'market')],
  y = lsms_spatial[, 'farm_area_ha'],
  ntree = 1500,
  keep.inbag = T,
  mtry = 3,
  splitrule = 'extratrees',
  nodesize = 50,
  importance = T
)

# deb <- Sys.time()
# cores <- parallel::detectCores() - 4
# cl <- parallel::makeCluster(cores)
# doParallel::registerDoParallel(cl)
# 
# train_control <- caret::trainControl(method = 'cv', number = 10, savePredictions = 'all', seeds = 2024)
# tune_grid <- expand.grid(
#   mtry = 1:8,                                     # I first tried 1:8 and 4 was the best; then I tried 4:5, 4 was still the best. I think of it as a local optimum pb
#   splitrule = 'extratrees',                       # I tried c('variance', 'extratrees'); extratrees was the best
#   min.node.size = c(5, 50, 100, 200)
# )
# 
# qrf_full_model <- caret::train(
#   farm_area_ha ~ .,
#   data = lsms_spatial1,
#   method = 'ranger',
#   preProcess = c('center', 'scale', 'spatialSign'),
#   trainControl = train_control,
#   quantreg = T,
#   keep.inbag = T,
#   tuneGrid = tune_grid,
#   importance  = 'permutation',
#   num.trees = 1500
# )
# tree_info <- ranger::treeInfo(qrf_full_model$finalModel, tree = 1)
# parallel::stopCluster(cl)
# save(qrf_full_model, file = '../data/processed/2024-11-11.qrf_full_model.rdata')
# fin <- Sys.time() - deb; print(fin)

# -------------------------------------------------------
# set.seed(2024)
# seeds <- vector(mode = 'list', length = 11)
# for(i in 1:10) seeds[[i]] <- sample.int(1000, 729)
# seeds[[11]] <- sample.int(1000, 1)
# 
# train_control <- caret::trainControl(
#   method = 'cv', 
#   number = 10, 
#   savePredictions = 'all', 
#   seeds = seeds
# )
# 
# tune_grid_xgbTree <- expand.grid(
#   nrounds = c(100, 200),
#   max_depth = c(3, 6, 9),
#   eta = c(0.01, 0.1, 0.3),
#   gamma = c(0, 0.1, 0.2),
#   colsample_bytree = c(0.6, 0.8, 1.0),
#   min_child_weight = c(1, 3, 5),
#   subsample = c(0.6, 0.8, 1.0)
# )
# 
# deb <- Sys.time()
# xgbTree_full_model <- caret::train(
#   farm_area_ha ~ .,
#   data = lsms_spatial1,
#   method = 'xgbTree',
#   preProcess = c('center', 'scale', 'spatialSign'),
#   trControl = train_control,
#   tuneGrid = tune_grid_xgbTree
# )
# range(xgbTree_full_model$results$Rsquared)
# xgbTree_full_model$finalModel
# save(xbgTree_full_model, file = '../data/processed/2024-10-26.xgbTree_full_model.rdata')
# fin <- Sys.time() - deb
# print(fin)

# -----------------------------------------------------------
# # Trying out spatialML package
# grf_full_model <- SpatialML::grf.bw(
#   farm_area_ha ~ cropland + cattle + pop + cropland_per_capita + 
#     sand + slope + temperature + rainfall + market + maizeyield,
#   lsms_spatial1,
#   coords = as.matrix(lsms_spatial |> select(x, y)),
#   trees = 1500,
#   importance = 'permutation',
#   forests = T
# )