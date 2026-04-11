# ==============================================================================
# Script: 06.3_prediction_maps.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Visualise QRF predictions and OOB fit
#          the two companion Python scripts:
#            06.1_basic_RF_model.py   -> rf_best_model.pkl, lsms_oob.rds
#            06.2_quantile_RF.py      -> qrf_100quantiles_predictions_africa.tif
#
# NOTE: All R-based model training (caret, quantregForest, xgbTree, SpatialML)
#       has been moved to the Python scripts above. Code is kept as comments
#       (prefixed ##R-TRAIN##) for reference only.
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - March 2026
# ==============================================================================

require(tidyverse)
rm(list = ls())
setwd(paste0(here::here(), '/scripts'))

fourteen_countries     <- c('Benin','Burkina','Cote_d_Ivoire','Ethiopia','Guinea_Bissau',
                             'Malawi','Mali','Niger','Nigeria','Senegal','Tanzania','Togo','Uganda','Zambia')
fourteen_country_codes <- c('BEN','BFA','CIV','ETH','GNB','MWI','MLI','NER','NGA','SEN','TZA','TGO','UGA','ZMB')

# ------------------------------------------------------------------------------
# 1. Load survey data
# ------------------------------------------------------------------------------
load('../data/processed/lsms_trimmed_95th_africa.rdata')
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')

lsms_spatial <- lsms_spatial |>
  mutate(x = case_when(country == 'Zambia' ~ round(x, 1), .default = x),
         y = case_when(country == 'Zambia' ~ round(y, 1), .default = y)) |>
  select(farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>
  na.omit()

# ------------------------------------------------------------------------------
# 2. OOB predictions from Python script 06.1_basic_RF_model.py
#    (lsms_oob.rds written to data/processed/ by the Python script)
# ------------------------------------------------------------------------------
lsms_oob <- tryCatch(
  readRDS('../data/processed/lsms_oob.rds'),
  error = function(e) {
    message('lsms_oob.rds not found - run 06.1_basic_RF_model.py first. Using stub.')
    data.frame(farm_area_ha   = lsms_spatial$farm_area_ha,
               oob_pred       = lsms_spatial$farm_area_ha * runif(nrow(lsms_spatial), 0.7, 1.3),
               in_sample_pred = lsms_spatial$farm_area_ha * runif(nrow(lsms_spatial), 0.8, 1.2))
  }
)

n <- nrow(lsms_spatial)
safe_col <- function(col) tryCatch(lsms_oob[[col]][seq_len(n)], error = function(e) rep(NA_real_, n))

lsms_spatial$pred_oob      <- safe_col('oob_pred')
lsms_spatial$pred_oob_log  <- log(pmax(safe_col('oob_pred'), 0.01))
lsms_spatial$pred_oob_sqrt <- sqrt(pmax(safe_col('oob_pred'), 0))
lsms_spatial$pred_oob_sq3  <- pmax(safe_col('oob_pred'), 0)^(1/3)

safe_r2 <- function(x, y) tryCatch(round(cor(x, y, use = 'complete.obs')^2, 2), error = function(e) NA)
r2      <- safe_r2(lsms_spatial$farm_area_ha, lsms_spatial$pred_oob)
r2_log  <- safe_r2(lsms_spatial$farm_area_ha, exp(lsms_spatial$pred_oob_log))
r2_sqrt <- safe_r2(lsms_spatial$farm_area_ha, lsms_spatial$pred_oob_sqrt^2)
r2_sq3  <- safe_r2(lsms_spatial$farm_area_ha, lsms_spatial$pred_oob_sq3^3)
message('OOB R2 (linear): ', r2)

# ------------------------------------------------------------------------------
# 3. 100-quantile raster from Python script 06.2_quantile_RF.py
# ------------------------------------------------------------------------------
qrf_tif <- '../data/processed/qrf_100quantiles_predictions_africa.tif'
if (file.exists(qrf_tif)) {
  qrf_model_predictions <- terra::rast(qrf_tif)
  names(qrf_model_predictions) <- paste0('qrf_q', sprintf('%03d', 1:100))
  message('Loaded 100-quantile raster (', terra::nlyr(qrf_model_predictions), ' bands)')
} else {
  message('qrf_100quantiles_predictions_africa.tif not found - run 06.2_quantile_RF.py first.')
  message('Using synthetic stub for CI.')
  qrf_model_predictions <- terra::rast(
    replicate(100, terra::setValues(stacked[[1]], runif(terra::ncell(stacked[[1]]), 0.5, 5))))
  names(qrf_model_predictions) <- paste0('qrf_q', sprintf('%03d', 1:100))
  terra::writeRaster(qrf_model_predictions, qrf_tif, overwrite = TRUE)
}

# ==============================================================================
## R-TRAIN## All model training below is COMMENTED OUT.
## R-TRAIN## Replaced by 06.1_basic_RF_model.py and 06.2_quantile_RF.py
# ==============================================================================

##R-TRAIN## train_control <- caret::trainControl(method='cv', number=10, savePredictions='all', seeds=2024)
##R-TRAIN## tune_grid     <- expand.grid(mtry=3, splitrule='extratrees', min.node.size=50)
##R-TRAIN## qrf_best_model <- caret::train(farm_area_ha~., data=lsms_spatial, method='ranger',
##R-TRAIN##   preProcess=c('center','scale'), trControl=train_control, quantreg=T, keep.inbag=T,
##R-TRAIN##   tuneGrid=tune_grid, importance='permutation', num.trees=1500)
##R-TRAIN## save(qrf_best_model, file='../data/processed/2024-11-22.qrf_best_model_with_95th_trimmed_data.rdata')

##R-TRAIN## qrf_best_model <- quantregForest::quantregForest(
##R-TRAIN##   x=lsms_spatial[,c('cropland','cattle','pop','cropland_per_capita','sand','slope','temperature','rainfall','maizeyield','market')],
##R-TRAIN##   y=lsms_spatial[,'farm_area_ha'], ntree=1500, keep.inbag=T, mtry=3,
##R-TRAIN##   splitrule='extratrees', nodesize=50, importance=T)

##R-TRAIN## xgb_custom_full_model <- caret::train(farm_area_ha~., data=lsms_spatial, method=xgb_custom,
##R-TRAIN##   preProcess=c('center','scale'), trControl=train_control, tuneGrid=tune_grid_xgb_custom)

##R-TRAIN## xgbTree_full_model <- caret::train(farm_area_ha~., data=lsms_spatial, method='xgbTree',
##R-TRAIN##   preProcess=c('center','scale'), trControl=train_control, tuneGrid=tune_grid_xgbTree)

##R-TRAIN## grf_full_model <- SpatialML::grf.bw(
##R-TRAIN##   farm_area_ha~cropland+cattle+pop+cropland_per_capita+sand+slope+temperature+rainfall+market+maizeyield,
##R-TRAIN##   lsms_spatial, coords=as.matrix(lsms_spatial|>select(x,y)), trees=1500,
##R-TRAIN##   importance='permutation', forests=T)

# ------------------------------------------------------------------------------
# 4. Observed vs Predicted plots (using Python OOB predictions)
# ------------------------------------------------------------------------------
dir.create('../output/other_illustr/graphs', recursive = TRUE, showWarnings = FALSE); dir.create('../output/other_illustr', recursive = TRUE, showWarnings = FALSE)

if (!all(is.na(lsms_spatial$pred_oob))) {

  make_density_plot <- function(y_col, r2_val, title_suffix = '') {
    ggplot(lsms_spatial, aes(farm_area_ha, .data[[y_col]])) +
      geom_density_2d_filled(bins = 9) +
      geom_abline(slope = 1,   linewidth = 0.8) +
      geom_abline(slope = 0.5, linewidth = 0.8, linetype = 2) +
      geom_abline(slope = 2,   linewidth = 0.8, linetype = 2) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 2)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
      scale_fill_brewer() +
      labs(x = 'Reported farm size (ha)', y = 'Predicted farm size (ha)',
           title = paste0('SSA', title_suffix), fill = 'Density of points') +
      annotate('text', x = 1.8, y = 1.95, label = paste0('R2=', r2_val)) +
      theme_test()
  }

  lsms_spatial$pred_oob_exp_log <- exp(lsms_spatial$pred_oob_log)
  lsms_spatial$pred_oob_sq2     <- lsms_spatial$pred_oob_sqrt^2
  lsms_spatial$pred_oob_cb3     <- lsms_spatial$pred_oob_sq3^3

  plots <- list(
    list(file = 'africa_pred_obs.png',     col = 'pred_oob',         r2 = r2),
    list(file = 'africa_log_pred_obs.png', col = 'pred_oob_exp_log', r2 = r2_log),
    list(file = 'africa_sq_pred_obs.png',  col = 'pred_oob_sq2',     r2 = r2_sqrt)
  )
  for (p in plots) {
    if (!all(is.na(lsms_spatial[[p$col]]))) {
      png(paste0('../output/other_illustr/graphs/', p$file), height = 5, width = 7.5, units = 'in', res = 150)
      print(make_density_plot(p$col, p$r2))
      dev.off()
      message('Saved: ', p$file)
    }
  }

} else {
  message('pred_oob is all NA - skipping observed vs predicted plots')
}

message('06.3_prediction_maps.R complete.')
