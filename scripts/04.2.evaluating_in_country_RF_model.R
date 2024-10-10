# Evaluate robustness of RF model
# main question: how well can a GADM1 be predicted if it has no data?
# two case studies: Ethiopia (high R2 from script 04.1) vs Malawi (low R2) 

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

# Define the custom model for geographically weighted regression
gwr_model <- list(
  type = "Regression",
  library = "spgwr",
  loop = NULL,
  parameters = data.frame(parameter = "bandwidth", class = "numeric", label = "Bandwidth"),
  grid = function(x, y, len = 5, search = "grid") {
    expand.grid(bandwidth = seq(50, 150, length = len))
  },
  fit = function(dat, farm_area_ha, wts, param, lev, last, classProbs, ...) {
    spgwr::gwr(farm_area_ha ~ ., data = dat, coords = cbind(dat$x, dat$y), bandwidth = param$bandwidth, longlat = TRUE, ...)
  },
  predict = function(modelFit, newdata, submodels = NULL) { #prediction failed
    coords = cbind(newdata$x, newdata$y)
    predict(modelFit, newdata)
  },
  prob = NULL
)

# Define the custom model for gaussian process
gp_model <- list(
  type = "Regression",
  library = "gstat",
  loop = NULL,
  parameters = data.frame(parameter = "range", class = "numeric", label = "Range"),
  grid = function(x, y, len = 10, search = "grid") {
    expand.grid(range = seq(100, 500, length = len))
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    gstat::gstat(id = "farm_area_ha", formula = y ~ ., data = x, model = vgm(1, "Sph", param$range, 1))
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    predict(modelFit, newdata)
  },
  prob = NULL
)

# ------------------------------------------------------------------------------
# Prepare data: load lsms data as my_lsms (lsms with geometry as data.frame)
lsms_region <- terra::vect('../data/processed/lsms_trimmed_africa.shp') # this was retrieved from '02.customize_spatial_data.r'
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')

# Using a training set and a test set to evaluate model performance
compare_gadm_rf_models <- function(my_country){
  print(paste0('--------------------', my_country, '---------------------------'))
  set.seed(2024) # just for reproducibility!
  
  # fetching the data subset for the country
  my_lsms_cty <- lsms_region |>
    terra::as.data.frame(geom = 'xy') |>
    filter(country == my_country, 
           gadm_0 == fourteen_country_codes[fourteen_countries == my_country]) |># to exclude points outside country
    rename(farm_area_ha = farm_area_)
  
  my_lsms_cty <- my_lsms_cty |>
    bind_cols(
      terra::extract(stacked,  my_lsms_cty |> 
                       select(x, y))
    ) |>
    select(x, y,                                                                              # omit country_name, region, and farm_id
           gadm_1,
           cropland, cattle, pop, cropland_per_capita,
           sand, slope, temperature, rainfall, 
           market, maizeyield, farm_area_ha) |>     # maize_yield removed  for colinearity issue in TPS_covariates # omit GDP because it is not relevant at country-level
    na.omit()
  
  # caret control parms
  ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = F)
  
  # run a loop for all gadm_1 in a country to estimate r2 when the gadm is left out of the training set
  for(my_gadm in unique(my_lsms_cty$gadm_1) ){
    print(my_gadm)
    # defining training - test sets: leave one GADM 1 out and predict from the rest of the country
    training_set <- my_lsms_cty |>
      filter(gadm_1 != my_gadm) |>
      select(!c(x, y, gadm_1))
    test_set <- my_lsms_cty |>
      anti_join(training_set) |>
      select(!c(x, y, gadm_1))
    
    # train over the rest of the country with RF
    mod1 <- caret::train(
      farm_area_ha ~ .,
      data = training_set,
      method = 'ranger',
      trControl = ctrl,
      metric = 'Rsquared'
    )
    # define r2 for the rest of the country
    rf_cv_rsq <- mod1$results |>
      as.data.frame() |>
      select(Rsquared) |>
      max() |>
      round(2)
    # calculate r2 for the identified gadm_1
    test_set$pred1_gadm <- predict(mod1, test_set)
    gadm_test_rf_rsq <- round(cor(test_set$farm_area_ha, test_set$pred1_gadm)^2, 2)
    
    # train over the rest of the country with GAM
    mod2 <- caret::train(
      farm_area_ha ~ .,
      data = training_set,
      method = gam_model,
      trControl = ctrl,
      metric = 'Rsquared'
    )
    # define r2 for the rest of the country
    gam_cv_rsq <- mod2$results |>
      as.data.frame() |>
      select(Rsquared) |>
      max() |>
      round(2)
    # calculate r2 for the identified gadm_1
    test_set$pred2_gadm <- predict(mod2, test_set)
    gadm_test_gam_rsq <- round(cor(test_set$farm_area_ha, test_set$pred2_gadm)^2, 2)
    
    # train over the rest of the country with KNN
    mod3 <- caret::train(
      farm_area_ha ~ .,
      data = training_set,
      method = 'knn',
      trControl = ctrl,
      metric = 'Rsquared'
    )
    # define r2 for the rest of the country
    knn_cv_rsq <- mod3$results |>
      as.data.frame() |>
      select(Rsquared) |>
      max() |>
      round(2)
    # calculate r2 for the identified gadm_1
    test_set$pred3_gadm <- predict(mod3, test_set)
    gadm_test_knn_rsq <- round(cor(test_set$farm_area_ha, test_set$pred3_gadm)^2, 2)
    
    
    # compiling data for the identified gadm_1
    one_row <- c(country = my_country, gadm_1 = my_gadm, 
                 rf_cv_rsq = rf_cv_rsq, gadm_test_rf_rsq = gadm_test_rf_rsq,
                 gam_cv_rsq = gam_cv_rsq, gadm_test_gam_rsq = gadm_test_gam_rsq,
                 knn_cv_rsq = knn_cv_rsq, gadm_test_knn_rsq = gadm_test_knn_rsq)
    all_rows <- bind_rows(all_rows, one_row)
  }
  print(all_rows)
  assign(paste0('results_', my_country), all_rows, envir = .GlobalEnv)
  return(all_rows)
}

# Initialization and function application (run this chunk of 4 lines at once)
deb <- Sys.time()
all_rows <- data.frame()
mult_rsq <- do.call(bind_rows, lapply(fourteen_countries, compare_gadm_rf_models))
fin <- Sys.time() - deb
print(fin)

# visualize 
long_mult_rsq1 <- mult_rsq |>
  select(!starts_with('gadm_test')) |>
  pivot_longer(cols = ends_with('cv_rsq'),
               names_to = 'model',
               values_to = 'reference') |>
  mutate(model = gsub('_cv_rsq$', '', model))
long_mult_rsq2 <- mult_rsq |>
  select(!ends_with('cv_rsq')) |>
  pivot_longer(cols = starts_with('gadm_test'),
               names_to = 'model',
               values_to = 'test_gadm') |>
  mutate(model = gsub('_rsq$', '', gsub('^gadm_test_', '', model)))
long_mult_rsq <- inner_join(long_mult_rsq1, long_mult_rsq2) |>
  pivot_longer(cols = c(reference, test_gadm),
               names_to = 'type',
               values_to = 'rsq'); rm (long_mult_rsq1, long_mult_rsq2)
summary_mult_rsq <- long_mult_rsq |>
  group_by(country, model, type) |>
  summarize(mean_rsq = mean(rsq, na.rm = T), sd_rsq =sd(rsq, na.rm = T))
  
P00 <- ggplot(long_mult_rsq, 
              aes(country, rsq, fill = type)) +
  geom_bar(stat = 'identity', position = position_dodge(0.5)) +
  labs(x = 'country', y = 'rsquared', fill = 'type') + 
  facet_grid(~ model)
P00

P01 <- ggplot(summary_mult_rsq, 
              aes(country, mean_rsq, fill = type)) + 
  geom_bar(stat = 'identity', position = position_dodge(0.8), colour = 'black') + 
  geom_errorbar(aes(ymin = mean_rsq, ymax = mean_rsq + sd_rsq), 
                position = position_dodge(0.8), width = 0.3) +
  labs(x = 'country', y = 'rsquared', fill = 'type') + 
  scale_fill_manual(values = c('lightsteelblue', 'steelblue3')) +
  facet_grid(~ model) +
  theme_bw() + 
  theme(axis.text = element_text(angle = 45, hjust = 1))
P01

png('../output/graphs/gadm_1__point_based_cross_validation.png', height = 15, width = 25, units = 'cm', res = 1000)
P01
ggsave('../output/graphs/gadm_1__point_based_cross_validation.png')
dev.off()

write.csv(mult_rsq, '../output/tables/gadm_1__point_based_cross_validation.csv')