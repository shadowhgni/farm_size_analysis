# Random forest optimization


#-------------------------------------------------------------------------------
# load packages
require(tidyverse) # sorry dear Robert, I felt lazy to recode in basic R
require(caret)
require(ranger)

# Clean environment
rm(list=ls())

# Set working directory
#setwd(here::here())
# ------------------------------------------------------------------------------
# create directories if needed
if(!dir.exists('../data'))
  dir.create('../data') # drop the input files here
if(!dir.exists('../output'))
  dir.create('../output') 

# define input and ouptut foders and files # please modify accordingly!
input_path <- '../data'
output_path <- '../output'
model_result_file2 <- '../output/model_optimization.Rdata'

# ------------------------------------------------------------------------------
# Prepare data
load('../data/lsms_trimmed_95th_africa.rdata') 
stacked <- terra::rast('../data/stacked_rasters_africa.tif')

# ------------------------------------------------------------------------------
# data wrangling

# keep only variables needed in the model
df <- lsms_spatial |>
  select(farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>
  na.omit() 

# ------------------------------------------------------------------------------
# set parms

tune_grid <- expand.grid(
  mtry = 3:10,                     
  splitrule = c('variance', 'extratrees'),                       
  min.node.size = c(5, 40:60, 100, 200)
)

set.seed(2024) 
seeds_list <- vector('list', length = 11) 
for (i in 1:10) {
  seeds_list[[i]] <- sample.int(10000, size = nrow(tune_grid))  # Random integers for each fold
}
seeds_list[[11]] <- sample.int(10000, size = 1) 
ctrl <- caret::trainControl(method = 'cv', number = 10, savePredictions = 'all', seeds = seeds_list)


rf_optim <- function(ind = 1, mbuck = mbuck){
  my_grid <- tune_grid[ind, ]
  rf_full_model <- caret::train(
    farm_area_ha ~ .,
    data = df,
    method = 'ranger',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    keep.inbag = T,
    tuneGrid = my_grid,
    importance  = 'permutation',
    metric = 'RMSE',
    min.bucket = mbuck,
    num.trees = 500
  )
  print(paste0('-------- run = ', ind, '_min.bucket = ', mbuck,'----------'))
  print(rf_full_model$results)
  saveRDS(rf_full_model, file = paste0(output_path, '/RF_optim_run_', ind, '__mbucket_', mbuck, '.RDS'))
  return(rf_full_model$results)
}


# run function in a loop
deb <- Sys.time()

for(i in 1:nrow(tune_grid)){
  for(m in c(1, 5, seq(10, 100, 10), 200)){ # min.bucket
    rf_optim(ind = i, mbuck = m)
  }
}
fin <- Sys.time() - deb; print(fin)