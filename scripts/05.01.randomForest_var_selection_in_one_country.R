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
# Understanding variable importance in pooled data model trained with RF
# Random-forest model for all Africa. Re-arranging column order as y ~ x
lsms_spatial <- lsms_spatial |>
  select(!c(x, y, country, gadm_0)) |>
  select(farm_area_ha, everything())

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
septuplets_list <- split(combn(sel_variables, 7), col(combn(sel_variables, 7)))
octuplets_list <- split(combn(sel_variables, 8), col(combn(sel_variables, 8)))
list_vars <-c(sel_variables, pairs_list, triplets_list, quadruplets_list, quintuplets_list, sextuplets_list, septuplets_list, octuplets_list)
# not feasible to run all the above-mentioned combinations. Below, the 5 most important variables are combined
list_vars <-c(octuplets_list, pairs_list, sel_variables)
#
# one predictor
deb <- Sys.time()
for (i in sel_variables){
  print(paste0('-----------------', paste0(i, collapse = ', '), '--------------------'))
  reduced_lsms <- lsms_spatial |>
    select(farm_area_ha, i)
  
  rf_reduced <- randomForest::randomForest(farm_area_ha ~ .,
                                           data = reduced_lsms
  )
  pp <- terra::plot(terra::predict(stacked, rf_reduced, main = i))
  
  print(rf_reduced)
  assign(paste0('rf_reduced_', paste0(i, collapse = '_')), rf_reduced, envir = .GlobalEnv)
  assign(paste0('pp_', i), pp, envir = .GlobalEnv)
}

# all_plots_of_signle_var <- patchwork::wrap_plots()
fin <- Sys.time() - deb; print(fin)



deb <- Sys.time()
cores <- parallel::detectCores() - 4
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)
rf_full <- caret::train(
  farm_area_ha ~ .,
  data = lsms_spatial,
  method = 'ranger',
  trainControl = train_control,
  preProcess = c('center', 'scale', 'spatialSign'),
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
                             preProcess = c('center', 'scale', 'spatialSign'),
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
# Use distances to 4 reference points as proxies of lon-lat and run a fake model 
# Define the reference points
reference_points <- data.frame(
  name = c('north', 'west', 'east', 'south', ),
  x = c(0, -20, 130, 0),
  y = c(50, 0, -10, -89.9)
)
lsms_spatial$dist_north <- terra::distance(with(lsms_spatial, cbind(x, y)), 
                                           with(reference_points[reference_points$name == 'north',], 
                                                cbind(x, y)))
lsms_spatial$dist_west <- terra::distance(with(lsms_spatial, cbind(x, y)), 
                                           with(reference_points[reference_points$name == 'west',], 
                                                cbind(x, y)))
lsms_spatial$dist_east <- terra::distance(with(lsms_spatial, cbind(x, y)), 
                                           with(reference_points[reference_points$name == 'east',], 
                                                cbind(x, y)))
lsms_spatial$dist_south <- terra::distance(with(lsms_spatial, cbind(x, y)), 
                                           with(reference_points[reference_points$name == 'south',], 
                                                cbind(x, y)))

names(lsms_spatial) <- gsub('\\[*', '', names(lsms_spatial))

deb <- Sys.time()
cores <- parallel::detectCores() - 4
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)
rf_ref_distance_and_rainfall <-  caret::train(
  farm_area_ha ~ dist_north + dist_south + dist_east + dist_west + rainfall,
  data = lsms_spatial,
  method = 'ranger',
  trainControl = train_control,
  preProcess = c('center', 'scale', 'spatialSign'),
  # tuneGrid = tune_grid,
  # num.trees = 1500,
  importance  = 'permutation'
)
parallel::stopCluster(cl)
fin <- Sys.time() - deb; print(fin)
