# Evaluate robustness of RF model
# first question: how well can a GADM1 be predicted if it has no data?

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
# Prepare data: load lsms data and stacked (raster of drivers)
load('../data/processed/lsms_trimmed_95th_africa.rdata') # this was retrieved from '03.1.pooled_data_for_analysis.r'
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')

# keep only variables needed in the models
lsms_spatial <- lsms_spatial |>
  select(x, y, country, gadm_0, gadm_1, gadm_2,
         farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>
  na.omit() 

# Using a training set and a test set to evaluate model performance
compare_gadm_rf_models <- function(my_country){
  print(paste0('--------------------', my_country, '---------------------------'))
  set.seed(2024) # just for reproducibility!
  
  # fetching the data subset for the country
  my_lsms_cty <- lsms_spatial |>
    filter(country == my_country, 
           gadm_0 == fourteen_country_codes[fourteen_countries == my_country]) 
  
  # caret control parms
  ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = F)
  
  # run a loop for all gadm_1 in a country to estimate r2 when the gadm is left out of the training set
  for(my_gadm in unique(my_lsms_cty$gadm_1) ){
    print(my_gadm)
    # defining training - test sets: leave one GADM 1 out and predict from the rest of the country
    training_set <- my_lsms_cty |>
      filter(gadm_1 != my_gadm) |>
      select(!c(x, y, country, starts_with('gadm')))
    training_coords <- my_lsms_cty |>
      filter(gadm_1 != my_gadm) |>
      select(x, y)
    test_set <- my_lsms_cty |>
      filter(gadm_1 == my_gadm) |>
      select(!c(x, y, country, starts_with('gadm')))
    
    # train over the rest of the country with RF
    mod1 <- caret::train(
      farm_area_ha ~ .,
      data = training_set,
      method = 'ranger',
      trControl = ctrl,
      metric = 'Rsquared'
    )
    # train over the GADM_1 # presumably failed in Guinea-Bissau
    # mod2 <- caret::train(
    #   farm_area_ha ~ .,
    #   data = test_set,
    #   method = 'ranger',
    #   trControl = ctrl,
    #   metric = 'Rsquared'
    # )
    # retrieve r2 for the rest of the country
    rf_cv_rsq <- mod1$results |>
      as.data.frame() |>
      select(Rsquared) |>
      pull() |>
      mean() |>
      round(2)
    rf_cv_rsq_sd <- mod1$results |>
      as.data.frame() |>
      select(Rsquared) |>
      pull() |>
      sd() |>
      round(2)
    # calculate r2 for the identified gadm_1
    test_set$pred1_gadm <- predict(mod1, test_set)
    gadm_test_rf_rsq <- round(cor(test_set$farm_area_ha, test_set$pred1_gadm)^2, 2)
    
    # compiling data for the identified gadm_1
    one_row <- c(
      country = my_country, gadm_1 = my_gadm, 
      rf_cv_rsq = rf_cv_rsq, rf_cv_rsq_sd = rf_cv_rsq_sd,
      gadm_test_rf_rsq = gadm_test_rf_rsq
    )
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
long_mult_rsq <- mult_rsq |>
  pivot_longer(cols = c(rf_cv_rsq, gadm_test_rf_rsq),
               names_to = 'type', values_to = 'rsq') |>
  mutate(rsq = as.numeric(rsq))
summary_mult_rsq <- long_mult_rsq |>
  group_by(country, type) |>
  summarize(mean_rsq = mean(rsq, na.rm = T),
            sd_rsq = sd(rsq, na.rm = T))
P01 <- ggplot(summary_mult_rsq, 
              aes(country, mean_rsq, fill = type)) + 
  geom_bar(stat = 'identity', position = position_dodge(0.8), colour = 'black') + 
  geom_errorbar(aes(ymin = mean_rsq, ymax = mean_rsq + sd_rsq), 
                position = position_dodge(0.8), width = 0.3) +
  labs(x = 'country', y = 'rsquared', fill = 'type') + 
  scale_fill_manual(values = c('lightsteelblue', 'steelblue3')) +
  theme_bw() + 
  theme(axis.text = element_text(angle = 45, hjust = 1))
P01

png('../output/graphs/gadm_1__point_based_cross_validation.png', height = 15, width = 25, units = 'cm', res = 1000)
P01
ggsave('../output/graphs/gadm_1__point_based_cross_validation.png')
dev.off()

write.csv(mult_rsq, '../output/tables/gadm_1__point_based_cross_validation.csv')