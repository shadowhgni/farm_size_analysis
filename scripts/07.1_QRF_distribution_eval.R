# ==============================================================================
# Script: 07.1_QRF_distribution_eval.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Analyze RF sensitivity to cropland correlation
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - February 2026
# ==============================================================================


setwd(paste0(here::here(), '/scripts'))
dir.create('../output/other_illustr/graphs', recursive = TRUE, showWarnings = FALSE)

# Clean environment
rm(list=ls())

# load packages
require(tidyverse)

#############################################################################################################
# Here are stored all the maps that will serve as predictors in the machine-learning (ML) models. Adjust accordingly
input_path <- '../data/raw/spatial'

#############################################################################################################
#define the region of interest: subSaharan Africa, excluding small islands 
country <- geodata::world(path=input_path, resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='RC)union' & NAME!='Saint Helena' & NAME!='SC#o TomC) and PrC-ncipe' & NAME!='Seychelles') # keep the mainland + Madagascar only, remove islands
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)

#define the countries for which LSMS data are available
sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')

#############################################################################################################
# retrieve all required spatial layers from input_path
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')
rf_model_predictions <- terra::rast('../data/processed/rf_model_predictions_SSA.tif')
# ------------------------------------------------------------------------------
# load the cropland rasters
all_cropland_mask <- terra::rast(paste0(input_path, '/landuse/landuse/all_cropland_mask.tif'))

ssa_grid <- terra::rast(ssa, nrow = 2000, ncol = 2000)
ssa_rast <- terra::rasterize(ssa, ssa_grid, field = 'NAME_0')
ssa_rast <- terra::resample(ssa_rast, all_cropland_mask)

all_cropland_mask <- c(all_cropland_mask, ssa_rast)

cropland_divergence <- all_cropland_mask |>
  terra::as.data.frame() |>
  group_by(NAME_0) |>
  summarize(across(contains('20'), ~ sum(., na.rm = T))) |>
  pivot_longer(cols = -NAME_0,
               names_to = 'source',
               values_to = 'cropland')
ssa_cropland <- cropland_divergence |>
  group_by(source) |>
  summarise(total = sum(cropland, na.rm = T))

top10_cropland <- cropland_divergence |>
  ungroup() |>
  group_by(source) |>
  summarize(rank = rev(rank(cropland)), NAME_0 = NAME_0) |>
  inner_join(cropland_divergence) |>
  filter(rank < 11) |>
  arrange(source, rank)

top10_cropland |> head(20)
top10_cropland |> tail(20)
# ------------------------------------------------------------------------------
# load the prediction rasters for models based on different croplands
predictions_spam2010 <- terra::rast('../data/processed/Python_SPAM2010_rf_predictions_africa.tif')
predictions_spam2017 <- terra::rast('../data/processed/Python_SPAM2017_rf_predictions_africa.tif')
predictions_spam2020 <- terra::rast('../data/processed/Python_SPAM2020_rf_predictions_africa.tif')
predictions_geosurvey2015 <- terra::rast('../data/processed/Python_Geosurvey2015_rf_predictions_africa.tif')
predictions_potapov2019 <- terra::rast('../data/processed/Python_potapov_rf_predictions_africa.tif')
predictions_esa2020 <- terra::rast('../data/processed/Python_ESA2021_rf_predictions_africa.tif')

predictions_croplands <- c(predictions_spam2010, predictions_spam2017, predictions_spam2020,
                           predictions_geosurvey2015, predictions_geosurvey2015, terra::resample(predictions_esa2020, predictions_spam2017))

names(predictions_croplands) <- c('spam2010', 'spam2017', 'spam2020', 'geosurvey2015', 'glad2019', 'esa2020')

pred_cpland_df <- predictions_croplands |> 
  terra::as.data.frame(xy = T)

# Check correlation matrix to select relevant drivers
P00 <- pred_cpland_df |>
  select(!c(x, y)) |>
  GGally::ggpairs(upper = list(continuous = GGally::wrap("cor", size = 3)), 
                  diag = list(continuous = GGally::wrap("densityDiag"))) +
  theme(
    strip.text = element_text(size = 6),
    axis.text = element_text(size = 4)  
  )
P00
png('../output/other_illustr/sensitivity_to_cropland_correlation_matrix.png', height = 15, width = 20, units = 'cm', res = 600)
P00
ggsave('../output/other_illustr/sensitivity_to_cropland_correlation_matrix.png')
dev.off()

# Compare total croplands from different sources
P01 <- ggplot(ssa_cropland, aes(source, total / 1000000)) + 
  geom_col() + 
  labs(x = 'Source', y = 'Total cropland in SSA, million ha') + 
  theme_test() + 
  theme(axis.ticks.x = element_blank())
P01
png('../output/other_illustr/comparison_total_croplands_per_source.png', height = 15, width = 20, units = 'cm', res = 600)
P01
ggsave('../output/other_illustr/comparison_total_croplands_per_source.png')
dev.off()


# Estimated nb of farms using avg. farm size from SPAM 2017
nb_farms <- all_cropland_mask / terra::resample(rf_model_predictions, all_cropland_mask)

nb_farms_df <- nb_farms |>
  terra::as.data.frame()

nb_farms_summarized <- nb_farms_df |>
  select(!NAME_0) |>
  pivot_longer(cols = contains('20'),
               names_to = 'source',
               values_to = 'nb_farms') |>
  group_by(source) |>
  summarize(nb_farms = sum(nb_farms, na.rm = T))
  
P02 <- ggplot(nb_farms_summarized, aes(source, nb_farms / 1000000)) + 
  geom_col() + 
  labs(x = 'Source', y = 'Number of farms in SSA, million ') + 
  theme_test() + 
  theme(axis.ticks.x = element_blank())
P02
png('../output/other_illustr/comparison_nb_farms_per_source.png', height = 15, width = 20, units = 'cm', res = 600)
P02
ggsave('../output/other_illustr/comparison_nb_farms_per_source.png')
dev.off()

saveRDS(list(cropland_divergence = cropland_divergence,
             ssa_cropland = ssa_cropland, top10_cropland = top10_cropland,
             pred_cpland_df = pred_cpland_df, nb_farms_summarized = nb_farms_summarized,
             P00, P01, P02), 
        file = '../output/plot_data/plot_suppl_01_effect_of_source_of_cropland_masks.rds')
################################################################################
