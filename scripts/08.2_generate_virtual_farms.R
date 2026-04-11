# ==============================================================================
# Script: 08.2_generate_virtual_farms.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Generate farm size predictions by country
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - February 2026
# ==============================================================================


require(tidyverse)

# Clean environment
rm(list=ls())

# Set working directory
setwd(paste0(here::here(), '/scripts'))
dir.create('../output/other_illustr/maps', recursive = TRUE, showWarnings = FALSE)
dir.create('../output/other_illustr/graphs', recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Preparation for functions and mapping
input_path <- '../data/raw/spatial'
country <- geodata::world(path=input_path, resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='Réunion' & NAME!='Saint Helena' & NAME!='São Tomé and Príncipe' & NAME!='Seychelles') # keep the mainland + Madagascar only, remove islands
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
pal <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))
pal2 <- colorRampPalette(c('#c6dbef','#6baed6','#3182bd', '#08519c', '#08306b'))
# force terra to use disk-based processing and 50% of RAM (Use this if R crashes because of limited memory)
terra::terraOptions(memfrac = 0.5, todisk = T, verbose = F)

# ------------------------------------------------------------------------------
#define the countries for which LSMS data are available
sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# Prepare data rasters: lsms and predictions
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')
all_cropland_mask <- terra::rast(paste0(input_path, '/landuse/landuse/all_cropland_mask.tif'))

rf_model_predictions <- terra::rast('../data/processed/rf_model_predictions_SSA.tif')
names(rf_model_predictions) <- 'pred_farm_area_ha'
qrf_model_predictions <- terra::rast('../data/processed/qrf_100quantiles_predictions_africa.tif')
names(qrf_model_predictions) <- paste0('qrf_q', sprintf('%03g', 1:100))

# Prepare lsms data
lsms_spatial <-  readRDS('../data/processed/lsms_trimmed_95th_africa.rds') # this was retrieved from '03.1.pooled_data_for_analysis.r'

# Load Sarah's data
sarah_nb_farms <- readxl::read_excel('../data/raw/web_scrapped/sarah_lowder/1-s2.0-S0305750X2100067X-mmc3.xlsx', skip = 0)
sarah_farm_size_class <- readxl::read_excel('../data/raw/web_scrapped/sarah_lowder/1-s2.0-S0305750X2100067X-mmc5.xlsx', skip = 0)
sarah_historical_farm_size_demo <- readxl::read_excel('../data/raw/web_scrapped/sarah_lowder/1-s2.0-S0305750X2100067X-mmc7.xlsx', skip = 0)
names(sarah_nb_farms) <- c('country', 'census_year', 'nb_farms', 'source', 'gadm_1', 'income_group')

# ------------------------------------------------------------------------------
# load the SSA country boundaries as well as lower admin boundaries for the 6 countries
# Country admin 

ben_distr <- geodata::gadm('Benin', level=3, path=paste0(input_path,'/gadm/Benin'))
bfa_distr <- geodata::gadm('Burkina Faso', level=3, path=paste0(input_path,'/gadm/Burkina'))
civ_distr <- geodata::gadm('CIV', level=4, path=paste0(input_path,'/gadm/Cote_d_Ivoire'))
eth_distr <- geodata::gadm('Ethiopia', level=3, path=paste0(input_path,'/gadm/Ethiopia'))
gha_distr <- geodata::gadm('Ghana', level=2, path=paste0(input_path,'/gadm/Ghana'))
gnb_distr <- geodata::gadm('GNB', level=2, path=paste0(input_path,'/gadm/Guinea_Bissau'))

mwi_distr <- geodata::gadm('Malawi', level=3, path=paste0(input_path,'/gadm/Malawi'))
mli_distr <- geodata::gadm('Mali', level=4, path=paste0(input_path,'/gadm/Mali'))
ner_distr <- geodata::gadm('Niger', level=3, path=paste0(input_path,'/gadm/Niger'))
nga_distr <- geodata::gadm('Nigeria', level=2, path=paste0(input_path,'/gadm/Nigeria'))
rwa_distr <- geodata::gadm('Rwanda', level=4, path=paste0(input_path,'/gadm/Rwanda'))
sen_distr <- geodata::gadm('Senegal', level=4, path=paste0(input_path,'/gadm/Senegal'))
tza_distr <- geodata::gadm('Tanzania', level=3, path=paste0(input_path,'/gadm/Tanzania'))
tgo_distr <- geodata::gadm('Togo', level=3, path=paste0(input_path,'/gadm/Togo'))
uga_distr <- geodata::gadm('Uganda', level=4, path=paste0(input_path,'/gadm/Uganda'))
zmb_distr <- geodata::gadm('Zambia', level=2, path=paste0(input_path,'/gadm/Zambia'))

sixteen_count_distr <- rbind(ben_distr, bfa_distr, civ_distr, eth_distr, gha_distr, gnb_distr, mwi_distr, mli_distr, ner_distr, nga_distr, rwa_distr, sen_distr,  tza_distr, tgo_distr, uga_distr, zmb_distr)

# Getting rasters from the polygons vectors
sixteen_count_grid <- terra::rast(sixteen_count_distr, nrow = 2000, ncol = 2000)
sixteen_count_rast1 <- terra::rasterize(sixteen_count_distr, sixteen_count_grid, field = 'NAME_0')
sixteen_count_rast1 <- terra::resample(sixteen_count_rast1, stacked)
names(sixteen_count_rast1) <- 'NAME_0'
sixteen_count_rast2 <- terra::rasterize(sixteen_count_distr, sixteen_count_grid, field = c('NAME_1') )
sixteen_count_rast2 <- terra::resample(sixteen_count_rast2, stacked)
sixteen_count_rast <- c(sixteen_count_rast1, sixteen_count_rast2)

ssa_grid <- terra::rast(ssa, nrow = 2000, ncol = 2000)
ssa_rast <- terra::rasterize(ssa, ssa_grid, field = 'NAME_0')
ssa_rast <- terra::resample(ssa_rast, stacked)

# correct rf_predicted with the mask of cropland based on SPAM, not geosurvey # Not needed, since cropland started with spam
rf_pred <- c(sixteen_count_rast2, ssa_rast, terra::resample(rf_model_predictions, stacked))
country_predicted <- terra::as.data.frame(rf_pred) |>
  rename(country = NAME_0, region = NAME_1)
  
summary_country <- country_predicted |>
  filter(!is.na(pred_farm_area_ha), !is.na(country)) |>
  group_by(country) |>
  summarize(mean_pred = mean(pred_farm_area_ha, na.rm = T) ) # Use quantile regression for median and variability

gadm_predicted_level1 <- country_predicted |>                # GADM level 1
  filter(!is.na(pred_farm_area_ha), !is.na(region), country %in% sixteen_countries) |>
  group_by(country, region) |>
  summarize(mean_pred = mean(pred_farm_area_ha, na.rm = T))

gadm_observed_level1 <- lsms_spatial |>
  select(country, gadm_1, farm_area_ha)

gadm_level1 <- gadm_predicted_level1 |>
  rename(gadm_1 = region) |>
  inner_join(gadm_observed_level1)

summary_gadm_1 <- gadm_level1 |>
  group_by(country, gadm_1) |>
  summarize(farm_area_ha = mean(farm_area_ha, na.rm = T), mean_pred = unique(mean_pred), n_obs = n())

summary_sixteen_country <- gadm_level1 |>
  group_by(country) |>
  summarize(farm_area_ha = mean(farm_area_ha, na.rm = T), mean_pred = mean(mean_pred, na.rm = T), n_obs = n())

r2_gadm_1 <- round(with(summary_gadm_1, cor(farm_area_ha, mean_pred)^2), 2)
r2_country <- round(with(summary_sixteen_country, cor(farm_area_ha, mean_pred)^2), 2)

P00 <- ggplot(summary_gadm_1, aes(farm_area_ha, mean_pred)) +
  geom_point(aes(colour = country, size = n_obs)) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.8) + 
  geom_abline(intercept = 0, slope = 0.5, linewidth = 0.8, linetype = 'dashed') + 
  geom_abline(intercept = 0, slope = 2, linewidth = 0.8, linetype = 'dashed') + 
  labs(x = 'Average reported farm size (ha)', y = 'Average predicted farm size (ha)',
       colour = 'Country', size = 'Nb of observ.') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 8.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8.5)) +
  annotate('text', x = 5, y = 6, label = paste0('R2=', round(r2_gadm_1, 2)) ) + 
  annotate('text', x = 4, y = 4.5, label = 'y = x' ) + 
  annotate('text', x = 1.8, y = 4.5, label = 'y = 2 x' ) + 
  annotate('text', x = 4, y = 1.6, label = 'y = 0.5 x' ) + 
  theme_test()
P00 

png('../output/other_illustr/gadm_1al_pred_obs.png', height = 5, width = 5, units = 'in', res = 600)
P00
ggsave('../output/other_illustr/gadm_1al_pred_obs.png')
dev.off()

P01 <- ggplot(summary_sixteen_country, aes(farm_area_ha, mean_pred)) +
  geom_point(aes(colour = country, size = n_obs)) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.8) + 
  geom_abline(intercept = 0, slope = 0.5, linewidth = 0.8, linetype = 'dashed') + 
  geom_abline(intercept = 0, slope = 2, linewidth = 0.8, linetype = 'dashed') + 
  labs(x = 'Average reported farm size (ha)', y = 'Average predicted farm size (ha)',
       colour = 'Country', size = 'Nb of observ.') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) +
  annotate('text', x = 4.5, y = 4.8, label = paste0('R2=', round(r2_country, 2)) ) + 
  annotate('text', x = 3.5, y = 4, label = 'y = x' ) + 
  annotate('text', x = 1.7, y = 4, label = 'y = 2 x' ) + 
  annotate('text', x = 3.5, y = 1.4, label = 'y = 0.5 x' ) + 
  theme_test()
P01 

png('../output/other_illustr/country_pred_obs.png', height = 5, width = 5, units = 'in', res = 600)
P01
ggsave('../output/other_illustr/country_pred_obs.png')
dev.off()

# ------------------------------------------------------------------------------
# Compare predicted number of farms with Sarah Lowder's data
ssa_nb_farms <- sarah_nb_farms |>
  filter(country %in% ssa$NAME_0) |>
  select(country, census_year, nb_farms) |>
  mutate(nb_farms = as.numeric(nb_farms),
         census_year = as.numeric(substr(census_year, nchar(census_year) - 3, nchar(census_year))))

# calc_nb_farms <- c(stacked$cropland, # add avg of 4 croplands
#                    terra::resample(terra::mean(all_cropland_mask$`SPAM 2010`, all_cropland_mask$`SPAM 2017`, 
#                                                all_cropland_mask$`SPAM 2020`, all_cropland_mask$`ESA 2020`),
#                                    stacked),
#                    rf_model_predictions, ssa_rast)
calc_nb_farms <- c(
  stacked$cropland,
  terra::resample(terra::mean(all_cropland_mask$`SPAM 2010`, all_cropland_mask$`SPAM 2017`,
                              all_cropland_mask$`SPAM 2020`, all_cropland_mask$`ESA 2020`,
                              all_cropland_mask$`GLAD 2019`, all_cropland_mask$`GEOSURVEY 2015`),
                 stacked),
  terra::resample(rf_model_predictions, stacked),
  terra::resample(ssa_rast, stacked)
)


names(calc_nb_farms) <- c('spam_2017', 'cropland', 'pred_farm_area_ha', 'country')
calc_nb_farms$nb_farms <- calc_nb_farms$cropland / calc_nb_farms$pred_farm_area_ha

summary_nb_farms <- terra::as.data.frame(calc_nb_farms) |>
  select(country, nb_farms) |>
  group_by(country) |>
  summarize(estim_nb_farms = sum(nb_farms, na.rm =T), n_cells = n()  )

comp_nb_farms <- summary_nb_farms |>
  select(country, estim_nb_farms) |>
  inner_join(ssa_nb_farms)

write.csv(comp_nb_farms, '../output/other_illustr/tables/estimated_number_of_farms_in_ssa.csv', row.names = F)
# comparing aggregates with Sarah
comp_nb_farms |> 
  filter(!is.na(estim_nb_farms), !is.na(nb_farms)) |> 
  summarize(across(c(estim_nb_farms, nb_farms), ~ sum(.)))

# the total number of farm is
comp_nb_farms |> 
  summarize(across(c(estim_nb_farms, nb_farms), ~ sum(., na.rm = T)))

r2_sarah <- round(with(na.omit(comp_nb_farms), cor(nb_farms, estim_nb_farms)^2), 2)
r2_sarah
round(with(na.omit(comp_nb_farms |> filter(!country %in% c('Ethiopia', 'Nigeria'))), cor(nb_farms, estim_nb_farms)^2), 2)
# When Nigeria and Ethiopia are removed, Rsquare drops to 0.5 instead of 0.9!

P02 <- ggplot(comp_nb_farms, 
              aes(nb_farms/1000000, estim_nb_farms/1000000, 
                  colour = census_year) ) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, size = 0.8) +
  geom_abline(intercept = 0, slope = 2, size = 0.8, linetype = 2) +
  geom_abline(intercept = 0, slope = 0.5, size = 0.8, linetype = 2) +
  geom_text(aes(label = substr(country, 1, 3)), vjust = -0.5, size = 3) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 24)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 24)) +
  labs(x = 'Number of farms based on census (x millions)', y = 'Estim. number of farms based on predicted farm sizes (x millions)', colour = 'Census year (end)') +
  annotate('text', x = 15, y = 18, label = paste0('R2=', round(r2_sarah, 2)) ) + 
  theme_test() +
  theme(legend.position = c(0.85, 0.55),
        legend.text = element_text(hjust = 1))

P02_zoom <- ggplot(comp_nb_farms, 
                   aes(nb_farms/1000000, estim_nb_farms/1000000, 
                       colour = census_year) ) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = 2, size = 0.8, linetype = 2) +
  geom_abline(intercept = 0, slope = 0.5, size = 0.8, linetype = 2) +
  geom_text(aes(label = country), hjust = -0.2, size = 2.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) +
  labs(x = NULL, y = NULL) +
  theme_test() + 
  theme(legend.position = 'none')

P02_final <- P02 +
  patchwork::inset_element(P02_zoom, left = 0.5, bottom = 0.01, right = 0.99, top = 0.4)

png('../output/other_illustr/country_compare_sarah.png', height = 5, width = 5, units = 'in', res = 600)
P02_final
ggsave('../output/other_illustr/country_compare_sarah.png')
dev.off()

P03 <- ggplot(comp_nb_farms, 
              aes(nb_farms/1000000, estim_nb_farms/1000000, 
                  colour = census_year) ) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_text(aes(label = country), vjust = -0.5, size = 2.5) +
  scale_x_continuous(expand = c(0.1, 0.1), limits = c(0.1, 24), trans = 'log10') +
  scale_y_continuous(expand = c(0.1, 0.1), limits = c(0.1, 24), trans = 'log10') +
  labs(x = 'Number of farms based on census, million (log scale)', y = 'Estim. nb. of farms based on predictions, million (log scale)', colour = 'Census year (end)') +
  annotate('text', x = 13, y = 24, label = paste0('R2=', round(r2_sarah, 2)) ) + 
  theme_test() +
  theme(legend.position = c(0.8, 0.25),
        legend.text = element_text(hjust = 1))
P03
png('../output/other_illustr/country_compare_sarah_log_scale.png', height = 5, width = 5, units = 'in', res = 600)
P03
ggsave('../output/other_illustr/country_compare_sarah_log_scale.png')
dev.off()

ssa_nb_farms <- ssa_rast
enf <- terra::as.data.frame(ssa_nb_farms, xy = T) |> 
  as_tibble() |>
  left_join(comp_nb_farms |>
              rename(NAME_0 = country) )
ssa_nb_farms <- terra::rast(enf, type = 'xyz', crs = 'EPSG:4326')
terra::writeRaster(ssa_nb_farms, '../data/processed/estim_nb_farms_per_country.tif', overwrite = T)

png('../output/other_illustr/maps/estim_nb_farms_per_country.png', height = 5, width = 5, units = 'in', res = 600)
M01 <-{
  terra::plot(ssa_nb_farms$estim_nb_farms, main = 'Predicted number of farms per country')
  terra::plot(ssa, add = T)
}
ggsave('../output/other_illustr/maps/estim_nb_farms_per_country.png')
dev.off()

png('../output/other_illustr/maps/estim_nb_farms_per_grid_cell.png', height = 5, width = 5, units = 'in', res = 600)
M02 <-{
  terra::plot(ssa, col = 'azure', main = 'Predicted density of farms',
              panel.first = grid(col = 'gray', lty = 'solid'), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
  terra::plot(calc_nb_farms$nb_farms)
  terra::plot(ssa, axes = F, add = T)
  
  terra::plot(calc_nb_farms$nb_farms )
  terra::plot(ssa, add = T)
}
ggsave('../output/other_illustr/maps/estim_nb_farms_per_grid_cell.png')
dev.off()

png('../output/other_illustr/maps/estim_nb_farms_per_grid_cell_classes.png', height = 5, width = 5, units = 'in', res = 600)
M03 <-{
  terra::plot(ssa, col = 'azure', main = 'Predicted farm density',
              panel.first = grid(col = 'gray', lty = 'solid'), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
  terra::plot(calc_nb_farms$nb_farms, breaks = c(0, 500, 1000, 2000, 5000, 10000, Inf), col = pal2(6), legend = F, cex = 1, axes = F, add = T)
  legend(-16.5, -10, bty = 'y', cex = 0.7, ncol = 1, box.col = 'white',
         title  =  expression('Nb. of farms per 100' ~ km^2), legend = c('≤ 500', '501 - 1000', '1001 - 2000', '2001 - 5000', '5001 - 10000', '> 10000'),
         fill = pal2(6), horiz = FALSE)
  terra::plot(ssa, axes = F, add = T)
}
ggsave('../output/other_illustr/maps/estim_nb_farms_per_grid_cell_classes.png')
dev.off()
terra::writeRaster(calc_nb_farms, file = '../data/processed/nb_farms_per_grid_cell.tif', overwrite = T)

ssa_pop <- stacked$pop
summary_pop <- terra::as.data.frame(c(ssa_rast, stacked$pop)) |>
  rename(country = NAME_0) |>
  group_by(country) |>
  summarize(pop = sum(pop, na.rm =T))

# correlation with population density
cor_farms_pop <- inner_join(summary_pop, comp_nb_farms)
cor_coef_farms_pop <- with(cor_farms_pop, cor(estim_nb_farms, pop))
cor_coef_farms_pop
