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
# Two models for stacking/ensembling, starting with the overall quantile random forest with ranger

lsms_spatial0 <- lsms_spatial
lsms_spatial1 <- lsms_spatial |>
  select(farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, market, maizeyield)

deb <- Sys.time()
cores <- parallel::detectCores() - 4
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

train_control <- caret::trainControl(method = 'cv', number = 10, savePredictions = 'all', seeds = 2024)
tune_grid <- expand.grid(
  mtry = 1:8,                                     # I first tried 1:8 and 4 was the best; then I tried 4:5, 4 was still the best. I think of it as a local optimum pb
  splitrule = 'extratrees',                       # I tried c('variance', 'extratrees'); extratrees was the best
  min.node.size = c(5, 50, 100, 200)
)

qrf_full_model <- caret::train(
  farm_area_ha ~ .,
  data = lsms_spatial1,
  method = 'ranger',
  preProcess = c('center', 'scale', 'spatialSign'),
  trainControl = train_control,
  quantreg = T,
  keep.inbag = T,  
  tuneGrid = tune_grid,
  importance  = 'permutation',
  num.trees = 1500
) 
tree_info <- ranger::treeInfo(qrf_full_model$finalModel, tree = 1)
parallel::stopCluster(cl)
save(qrf_full_model, file = '../data/processed/2024-10-17.qrf_full_model.rdata')
fin <- Sys.time() - deb; print(fin)

# Define the custom GBM model (but it failed till now)
xgb_custom <- list(
  type = "Regression",
  library = "xgboost",
  loop = NULL,
  parameters = data.frame(parameter = c("nrounds", "max_depth", "eta"),
                          class = rep("numeric", 3),
                          label = c("nrounds", "max_depth", "eta")),
  grid = function(x, y, len = NULL, search = "grid") {
    expand.grid(nrounds = 100, max_depth = 6, eta = 0.3)
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    dtrain <- xgb.DMatrix(data = as.matrix(x), label = y)
    xgboost::xgb.train(params = list(objective = quantile_loss, max_depth = param$max_depth, eta = param$eta),
                       data = dtrain, nrounds = param$nrounds, ...)
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    predict(modelFit, newdata = as.matrix(newdata))
  },
  prob = NULL,
  tags = c("xgboost", "quantile"),
  sort = function(x) x[order(x$nrounds), ],
  levels = NULL
)

tune_grid_xgb_custom <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3) 
)

xgb_custom_full_model <- caret::train(
  farm_area_ha ~ .,
  data = lsms_spatial1[sample(1:nrow(lsms_spatial1), 1000),],
  method = xgb_custom,
  preProcess = c('center', 'scale', 'spatialSign'),
  trControl = train_control,
  tuneGrid = tune_grid_xgb_custom
)

# -------------------------------------------------------
set.seed(2024)
seeds <- vector(mode = 'list', length = 11)
for(i in 1:10) seeds[[i]] <- sample.int(1000, 729)
seeds[[11]] <- sample.int(1000, 1)

train_control <- caret::trainControl(
  method = 'cv', 
  number = 10, 
  savePredictions = 'all', 
  seeds = seeds
)

tune_grid_xgbTree <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 0.1, 0.2),
  colsample_bytree = c(0.6, 0.8, 1.0),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.6, 0.8, 1.0)
)

deb <- Sys.time()
xgbTree_full_model <- caret::train(
  farm_area_ha ~ .,
  data = lsms_spatial1,
  method = 'xgbTree',
  preProcess = c('center', 'scale', 'spatialSign'),
  trControl = train_control,
  tuneGrid = tune_grid_xgbTree
)
range(xgbTree_full_model$results$Rsquared)
xgbTree_full_model$finalModel
save(xbgTree_full_model, file = '../data/processed/2024-10-26.xgbTree_full_model.rdata')
fin <- Sys.time() - deb
print(fin)

# -----------------------------------------------------------
# Trying out spatialML package
grf_full_model <- SpatialML::grf.bw(
  farm_area_ha ~ cropland + cattle + pop + cropland_per_capita + 
    sand + slope + temperature + rainfall + market + maizeyield,
  lsms_spatial1,
  coords = as.matrix(lsms_spatial |> select(x, y)),
  trees = 1500,
  importance = 'permutation',
  forests = T
)




# 
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
