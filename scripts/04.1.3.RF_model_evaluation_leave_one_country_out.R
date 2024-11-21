# Evaluate model framework, leaving out 1 country and compare the predictions with interpolation


#-------------------------------------------------------------------------------
# load packages
require(tidyverse)
require(fields)
require(terra)
require(geodata)

# Clean environment
rm(list=ls())

# Set working directory
setwd(paste0(here::here(), '/scripts'))

# define input and ouptut foders and files # please modify accordingly!
input_path <- 'C:/Users/DHOUGNI/OneDrive - CIMMYT/Documents/Harare 2023/Spatial_data_repository'
output_file <- '../output/tables/comparison_ML_models_per_country.csv'
model_result_file <- '../data/processed/compare_country_models.Rdata'

# Prepare data: load lsms data and stacked (raster of drivers)
load('../data/processed/lsms_trimmed_95th_africa.rdata') # this was retrieved from '03.1.pooled_data_for_analysis.r'
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')

# ------------------------------------------------------------------------------
# Preparation for functions and mapping
country <- geodata::world(path=input_path, resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='Réunion' & NAME!='Saint Helena' & NAME!='São Tomé and Príncipe' & NAME!='Seychelles') # keep the mainland + Madagascar only, remove islands
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
pal <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))

fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# # define a TPS method for caret  (only if TPS takes too long to run, use this as a shortcut)
# tps_model <- list(
#   type = "Regression",
#   label = 'Thin plate spline',
#   library = "fields",
#   loop = NULL,
#   parameters = data.frame(parameter = "lambda", class = "numeric", label = "Smoothing Parameter"),
#   grid = function(x, y, len = 3, search = "grid") { #len was NULL
#     data.frame(lambda = 10^seq(-3, 3, length = len))
#   },
#   fit = function(x, y, wts, param, lev, last, classProbs, ...) {
#     fields::Tps(x, y, lambda = param$lambda, ...)
#   },
#   predict = function(modelFit, newdata, submodels = NULL) {
#     predict(modelFit, newdata)
#   },
#   prob = NULL
# )


# cheap clustering of datapoints in RALS- Zambia
lsms_spatial <- lsms_spatial |>
  mutate(x = case_when(country == 'Zambia' ~ round(x, 1),
                       .default = x),
         y = case_when(country == 'Zambia' ~ round(y, 1),
                       .default = y))

# keep only variables needed in the models
lsms_spatial <- lsms_spatial |>
  select(x, y, country, farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>
  na.omit() 

# Using a training set (all other countries) and a test set (country of interest) to evaluate model performance
leave_one_country_models <- function(my_country){
  set.seed(2024) # just for reproducibility!
  
  # caret control parms
  ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = F)
  
  print(paste0('--------------- Thin plate spline in ', my_country, '-------------'))
  # rename dataset (just to backup original lsms_spatial)
  my_lsms_cty <- lsms_spatial
  
  #training - test split (leave one country out)
  training_set_xy <- my_lsms_cty |>  
    filter(country != my_country) |>
    select(!country) |>
    na.omit()
  test_set_xy <- my_lsms_cty |>  
    filter(country == my_country) |>
    select(!country) |>
    na.omit()
  
  #-------- Thin plate spline (only the coordinates)-----------
  
  # Fit a TPS model (trained with  other countries), predict farm sizes over the country grid and map it 
  cty_fit <- fields::Tps(cbind(test_set_xy$x, test_set_xy$y),
                         test_set_xy$farm_area_ha, lon.lat = T)
  
  # Construct a country grid over which prediction will be made
  cty_vect <- subset(ssa, ssa$GID_0 == fourteen_country_codes[which(fourteen_countries == my_country)])
  cty_grid <- terra::rast(cty_vect, nrow = 100, ncol = 100, nlyrs = 1)
  cty_rast <- terra::rasterize(cty_vect, cty_grid)
  cty_coords <- terra::xyFromCell(cty_rast, cell = terra::cells(cty_rast))
  
  # predict the TPS on the coordinates of observed data
  test_set_xy$pred_tps_xy  <- as.numeric(predict(cty_fit, test_set_xy[, c('x', 'y')]))# assign to the new column of test_set the prediction made over the grid
  rsq_tps_xy <- with(test_set_xy, round(cor(farm_area_ha, pred_tps_xy)^2, 2))   # Get the r2
  print(paste0('TPS_calculated_rsq_without_covariate = ', round(rsq_tps_xy, 2)))
  
  # # plot it
  # cty_rast1 <- terra::interpolate(terra::rast(cty_rast), cty_fit)
  # cty_rast2 <- terra::mask(cty_rast1, cty_rast)
  # cty_rast2[cty_rast2 <= 0] <- NA
  # png(paste0('../output/maps/ML_model_comparison_', my_country, '_Tps_xy.png'), units="in", width=5.5, height=5.5, res=1000)
  # M01 <- {
  #   terra::plot(cty_vect, main = paste0(my_country, ' - Tps_xy only'))
  #   terra::plot(cty_rast2, col = terrain.colors(100), add = T)
  # }
  # dev.off()
  
  # Random forest (only the covariates)
  rf_country_model <- caret::train(
    farm_area_ha ~ .,
    data = training_set_xy |>
      select(!c(x, y)),
    method = 'ranger',
    preProcess = c('center', 'scale', 'spatialSign'),
    # trControl = ctrl,
    metric = 'Rsquared'
  )
  print(rf_country_model)
  test_set_xy$pred_rf <- as.numeric(predict(rf_country_model, test_set_xy)  )
  rsq_rf <- round(cor(test_set_xy$farm_area_ha, test_set_xy$pred_rf)^2, 2)
  print(paste0('rsq_rf = ', round(rsq_rf, 2)))
  
  # calculate Rsquare showing agreement between in-country TPS and RF_model from other countries
  rsq_rf_vs_tps <- with(test_set_xy, round(cor(pred_tps_xy, pred_rf, use = 'complete.obs')^2, 2))   # Get the r2
  
  # compile results
  one_rsq <- cbind.data.frame(
    country = my_country, 
    rsq_tps_xy = rsq_tps_xy, 
    rsq_rf = rsq_rf, 
    rsq_rf_vs_tps  = rsq_rf_vs_tps
  )
  mult_rsq <- rbind(mult_rsq, one_rsq)
  results <- list(mult_rsq, 
                  cty_fit,
                  rf_country_model
                  )
  assign(paste0('rsq_table_for_', my_country), mult_rsq, envir = .GlobalEnv)
  assign(paste0('results_', my_country), results, envir = .GlobalEnv)
}

# Initialization and function application (run this chunk of 4 lines at once)
deb <- Sys.time()
mult_rsq <- data.frame()
sapply(fourteen_countries, leave_one_country_models)
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
     file = model_result_file)
write.csv(model_perf_wide, file = ouput_file)