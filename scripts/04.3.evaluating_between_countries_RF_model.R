# Evaluate robustness of RF model
# main question: how well does a country be predicted in absence of training data

# load packages
require(tidyverse)

# Clean environment
rm(list=ls())

# Set working directory
setwd(here::here())

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
# Define custom gam model with smoother for each independent variable (CopILOT generated)
gam_model <- list(
  label = "Generalized Additive Model using Splines",
  library = "mgcv",
  type = c("Regression", "Classification"),
  parameters = data.frame(parameter = c("select", "method"),
                          class = c("logical", "character"),
                          label = c("Feature Selection", "Smoothing Parameter Estimation Method")),
  grid = function(x, y, len = NULL, search = "grid") {
    if (search == "grid") {
      expand.grid(select = c(TRUE, FALSE)[1:min(2, len)], method = "GCV.Cp")
    } else {
      data.frame(select = sample(c(TRUE, FALSE), size = len, replace = TRUE),
                 method = sample(c("GCV.Cp", "ML"), size = len, replace = TRUE))
    }
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    require(mgcv)
    dat <- if (is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
    modForm <- caret:::smootherFormula(x)
    if (is.factor(y)) {
      dat$.outcome <- ifelse(y == lev, 0, 1)
      dist <- binomial()
    } else {
      dat$.outcome <- y
      dist <- gaussian()
    }
    modelArgs <- list(formula = modForm, data = dat, select = param$select, method = as.character(param$method))
    theDots <- list(...)
    if (!any(names(theDots) == "family")) modelArgs$family <- dist
    modelArgs <- c(modelArgs, theDots)
    out <- do.call(mgcv::gam, modelArgs)
    out
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
    if (modelFit$problemType == "Classification") {
      probs <- predict(modelFit, newdata, type = "response")
      out <- ifelse(probs < 0.5, modelFit$obsLevel, modelFit$obsLevel)
    } else {
      out <- predict(modelFit, newdata, type = "response")
    }
    out
  },
  prob = NULL,
  predictors = function(x, ...) {
    predictors(x$terms)
  },
  levels = function(x) x$obsLevels,
  varImp = function(object, ...) {
    smoothed <- summary(object)$s.table[, "p-value", drop = FALSE]
    linear <- summary(object)$p.table
    linear <- linear[, grepl("^Pr", colnames(linear)), drop = FALSE]
    gams <- rbind(smoothed, linear)
    gams <- gams[rownames(gams) != "(Intercept)", , drop = FALSE]
    rownames(gams) <- gsub("^s\\(", "", rownames(gams))
    rownames(gams) <- gsub("\\)$", "", rownames(gams))
    colnames(gams) <- "Overall"
    gams <- as.data.frame(gams, stringsAsFactors = TRUE)
    gams$Overall <- -log10(gams$Overall)
    allPreds <- colnames(attr(object$terms, "factors"))
    extras <- allPreds[!(allPreds %in% rownames(gams))]
    if (any(extras)) {
      tmp <- data.frame(Overall = rep(NA, length(extras)))
      rownames(tmp) <- extras
      gams <- rbind(gams, tmp)
    }
    gams
  },
  tags = c("Generalized Linear Model", "Generalized Additive Model"),
  sort = function(x) x
)

# ------------------------------------------------------------------------------
# Prepare data: load lsms data as my_lsms (lsms with geometry as data.frame)
lsms_region <- terra::vect('../data/processed/lsms_trimmed_africa.shp') # this was retrieved from '02.customize_spatial_data.r'
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')


# ------------------------------------------------------------------------
# Using a training set and a test set to evaluate model performance: one country left out
compare_countries_leave_out_one <- function(my_country){
  print(paste0('--------------------', my_country, '---------------------------'))
  set.seed(2024) # just for reproducibility!
  
  # fetching the data subset for the selected country
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
           market, maizeyield, farm_area_ha) |>     # maize_yield removed  for colinearity issue with temperature # GDP  remove for granularity isssues (country-level)
    na.omit()
  
  my_lsms_cty <- lsms_spatial |>
    filter(country == my_country, 
           gadm_0 == fourteen_country_codes[fourteen_countries == my_country]) # to exclude points outside country
  
  # caret control parms
  ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = F)
  
  # defining training - test sets: leave out the selected country and predict from the rest of SSA
  training_set <- lsms_spatial |>
    filter(country != my_country) |>
    select(!c(x, y, country, gadm_0))
  test_set <- lsms_spatial |>
    filter(country == my_country) |>
    select(!c(x, y, country, gadm_0))
  
  # train over the rest of SSA with RF
  mod1 <- caret::train(
    farm_area_ha ~ .,
    data = training_set,
    method = 'ranger',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  
  # define r2 for the rest of SSA
  rf_cv_rsq <- mod1$results |>
    as.data.frame() |>
    select(Rsquared) |>
    max() |>
    round(2)
  # calculate r2 for the selected country
  test_set$pred1_cty <- predict(mod1, test_set)
  cty_test_rf_rsq <- round(cor(test_set$farm_area_ha, test_set$pred1_cty)^2, 2)
  
  # train over the rest of the country with GAM
  mod2 <- caret::train(
    farm_area_ha ~ .,
    data = training_set,
    method = gam_model,
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  # define r2 for the rest of the country
  gam_cv_rsq <- mod2$results |>
    as.data.frame() |>
    select(Rsquared) |>
    max() |>
    round(2)
  # calculate r2 for the selected country
  test_set$pred2_cty <- predict(mod2, test_set)
  cty_test_gam_rsq <- round(cor(test_set$farm_area_ha, test_set$pred2_cty)^2, 2)
  
  # train over the rest of SSA with KNN
  mod3 <- caret::train(
    farm_area_ha ~ .,
    data = training_set,
    method = 'knn',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  # define r2 for the rest of SSA
  knn_cv_rsq <- mod3$results |>
    as.data.frame() |>
    select(Rsquared) |>
    max() |>
    round(2)
  # calculate r2 for the selected country
  test_set$pred3_cty <- predict(mod3, test_set)
  cty_test_knn_rsq <- round(cor(test_set$farm_area_ha, test_set$pred3_cty)^2, 2)
  
  # compiling data for the selected
  one_row <- c(country = my_country,  
               rf_cv_rsq = rf_cv_rsq, cty_test_rf_rsq = cty_test_rf_rsq,
               gam_cv_rsq = gam_cv_rsq, cty_test_gam_rsq = cty_test_gam_rsq,
               knn_cv_rsq = knn_cv_rsq, cty_test_knn_rsq = cty_test_knn_rsq)
  all_rows <- bind_rows(all_rows, one_row)
  
  print(all_rows)
  
  assign(paste0('results_', my_country), all_rows, envir = .GlobalEnv)
  return(all_rows)
}

# Initialization and function application (run this chunk of 4 lines at once)
deb <- Sys.time()
all_rows <- data.frame()
mult_rsq <- do.call(bind_rows, lapply(fourteen_countries, compare_countries_leave_out_one))
fin <- Sys.time() - deb
print(fin)

# visualize 
summary_mult_rsq <- mult_rsq |>
  group_by(country, model, type) |>
  summarize(mean_rsq = mean(rsq, na.rm = T), sd_rsq =sd(rsq, na.rm = T))
  
P00 <- ggplot(long_mult_rsq, 
              aes(country, rsq, fill = type)) +
  geom_bar(stat = 'identity', position = position_dodge(0.5)) +
  labs(x = 'country', y = 'r square', fill = 'type') + 
  facet_grid(~ model)
P00

P01 <- ggplot(summary_mult_rsq, 
              aes(country, mean_rsq, fill = type)) + 
  geom_bar(stat = 'identity', position = position_dodge(0.8), colour = 'black') + 
  geom_errorbar(aes(ymin = mean_rsq, ymax = mean_rsq + sd_rsq), 
                position = position_dodge(0.8), width = 0.3) +
  labs(x = 'country', y = 'r square', fill = 'type') + 
  scale_fill_manual(values = c('lightsteelblue', 'steelblue3')) +
  facet_grid(~ model) +
  theme_bw() + 
  theme(axis.text = element_text(angle = 45, hjust = 1))
P01

png('../output/graphs/country_point_based_cross_validation.png', height = 15, width = 25, units = 'cm', res = 1000)
P01
ggsave('../output/graphs/country__point_based_cross_validation.png')
dev.off()
write.csv(mult_rsq, '../output/tables/country_leave_one_out_point_based_cross_validation.csv')

# -------------------------------------------------------------------
# Country-pairwise comparison to detect regional trends
compare_countries_in_pairs <- function(my_country){
  print(paste0('--------------------', my_country, '---------------------------'))
  set.seed(2024) # just for reproducibility!
  
  # fetching the data subset for the selected country
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
           market, maizeyield, farm_area_ha) |>     # maize_yield removed  for colinearity issue with temperature # GDP  remove for granularity isssues (country-level)
    na.omit()
  
  my_lsms_cty <- lsms_spatial |>
    filter(country == my_country, 
           gadm_0 == fourteen_country_codes[fourteen_countries == my_country]) # to exclude points outside country
  
  # caret control parms
  ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = F)
  
  # running a loop over the other countries
  for(other_country in fourteen_countries){
    
    # defining training - test sets: use the other country as test set 
    training_set <- lsms_spatial |>
      filter(country == my_country) |>
      select(!c(x, y, country, gadm_0))
    test_set <- lsms_spatial |>
      filter(country == other_country) |>
      select(!c(x, y, country, gadm_0))
    
    # train RF model using my_country
    mod1 <- caret::train(
      farm_area_ha ~ .,
      data = training_set,
      method = 'ranger',
      preProcess = c('center', 'scale', 'spatialSign'),
      trControl = ctrl,
      metric = 'Rsquared'
    )
    
    # define r2 for my_country
    rf_cv_rsq <- mod1$results |>
      as.data.frame() |>
      select(Rsquared) |>
      max() |>
      round(2)
    # calculate r2 for the other country
    test_set$pred1_cty <- predict(mod1, test_set)
    cty_test_rf_rsq <- round(cor(test_set$farm_area_ha, test_set$pred1_cty)^2, 2)
    
    # compiling data for the selected country
    one_row <- c(train_country = my_country,  test_country = other_country,
                 rf_cv_rsq = rf_cv_rsq, cty_test_rf_rsq = cty_test_rf_rsq
                 # gam_cv_rsq = gam_cv_rsq, gadm_test_gam_rsq = gadm_test_gam_rsq,
                 # knn_cv_rsq = knn_cv_rsq, gadm_test_knn_rsq = gadm_test_knn_rsq
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
mult_rsq <- do.call(bind_rows, lapply(fourteen_countries, compare_countries_in_pairs))
fin <- Sys.time() - deb
print(fin)

write.csv(mult_rsq, '../output/tables/country_pairwise_point_based_cross_validation.csv')
#-----------------------------------------------------------------
# Cross-validation R squares for the country itself
country_autoevaluation_rsq <- function(my_country){
  print(paste0('--------------------', my_country, '---------------------------'))
  set.seed(2024) # just for reproducibility!
  
  # fetching the data subset for the selected country
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
           market, maizeyield, farm_area_ha) |>     # maize_yield removed  for colinearity issue with temperature # GDP  remove for granularity isssues (country-level)
    na.omit()
  
  my_lsms_cty <- lsms_spatial |>
    filter(country == my_country, 
           gadm_0 == fourteen_country_codes[fourteen_countries == my_country]) # to exclude points outside country
  
  # caret control parms
  ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = F)
  
  # defining training - test sets: country as training and as test set 
  training_set <- lsms_spatial |>
    filter(country == my_country) |>
    select(!c(x, y, country, gadm_0))
  test_set <- training_set
  
  # train RF model using my_country
  mod1 <- caret::train(
    farm_area_ha ~ .,
    data = training_set,
    method = 'ranger',
    preProcess = c('center', 'scale', 'spatialSign'),
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
  
  # calculate r2 for the other country
  test_set$pred1_cty <- predict(mod1, test_set)
  rf_oob_rsq <- round(cor(test_set$farm_area_ha, test_set$pred1_cty)^2, 2)
  
  #retrieve the variable importance for the best model(largest CV r square)
  variable_imp <- caret::varImp(mod1, scale = F)
  variable_importance_one <- cbind.data.frame(country = my_country, 
                                              var = rownames(variable_imp$importance),
                                              importance = variable_imp$importance$Overall) |>
    mutate(rank = 11 - rank(importance))
  # train over the rest of the country with GAM
  mod2 <- caret::train(
    farm_area_ha ~ .,
    data = training_set,
    method = gam_model,
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  # define r2 for the rest of the country
  gam_cv_rsq <- mod2$results |>
    as.data.frame() |>
    select(Rsquared) |>
    max() |>
    round(2)
  # calculate r2 for the selected country
  test_set$pred2_cty <- predict(mod2, test_set)
  gam_oob_rsq <- round(cor(test_set$farm_area_ha, test_set$pred2_cty)^2, 2)
  
  # train over the rest of SSA with KNN
  mod3 <- caret::train(
    farm_area_ha ~ .,
    data = training_set,
    method = 'knn',
    preProcess = c('center', 'scale', 'spatialSign'),
    trControl = ctrl,
    metric = 'Rsquared'
  )
  # define r2 for the rest of SSA
  knn_cv_rsq <- mod3$results |>
    as.data.frame() |>
    select(Rsquared) |>
    max() |>
    round(2)
  # calculate r2 for the selected country
  test_set$pred3_cty <- predict(mod3, test_set)
  knn_oob_rsq <- round(cor(test_set$farm_area_ha, test_set$pred3_cty)^2, 2)
  
  # compiling data for the selected country
  one_row <- c(country = my_country,
               rf_cv_rsq = rf_cv_rsq, rf_oob_rsq = rf_oob_rsq,
               gam_cv_rsq = gam_cv_rsq, gam_oob_rsq = gam_oob_rsq,
               knn_cv_rsq = knn_cv_rsq, knn_oob_rsq = knn_oob_rsq
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
mult_rsq <- do.call(bind_rows, lapply(fourteen_countries, country_autoevaluation_rsq))
fin <- Sys.time() - deb
print(fin)

variable_importance_table <- data.frame()
for(i in fourteen_countries){
  variable_importance_table <-bind_rows(variable_importance_table, get(ls(pattern = paste0('var_importance_', i))[1]))
}
write.csv(mult_rsq, '../output/tables/country_auto_evaluation_rsquares.csv')
write.csv(variable_importance_table, '../output/tables/country_variable_importance.csv')