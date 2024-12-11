# Random forest predictions to derive summary values per country (SSA) and provinces/regions in the 6 countries
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
pal2 <- colorRampPalette(c('#c6dbef','#6baed6','#3182bd', '#08519c', '#08306b'))
# force terra to use disk-based processing and 50% of RAM (Use this if R crashes because of limited memory)
terra::terraOptions(memfrac = 0.5)

# ------------------------------------------------------------------------------
fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# Prepare data rasters: lsms and predictions
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')
rf_model_predictions <- terra::rast('../data/processed/rf_model_predictions_SSA.tif')
names(rf_model_predictions) <- 'pred_farm_area_ha'
qrf_model_predictions <- terra::rast('../data/processed/qrf_100quantiles_predictions_africa.tif')
names(qrf_model_predictions) <- paste0('qrf_q', sprintf('%03g', 1:100))

# Prepare lsms data
load('../data/processed/lsms_trimmed_95th_africa.rdata') # this was retrieved from '03.1.pooled_data_for_analysis.r'
# cluster datapoints in Zambia
lsms_spatial <- lsms_spatial |>
  mutate(x = case_when(country == 'Zambia' ~ round(x, 1),
                       .default = x),
         y = case_when(country == 'Zambia' ~ round(y, 1),
                       .default = y))


# Load Sarah's data
sarah_nb_farms <- readxl::read_excel('../data/raw/web_scrapped/sarah_lowder/1-s2.0-S0305750X2100067X-mmc3.xlsx', skip = 1)
sarah_farm_size_class <- readxl::read_excel('../data/raw/web_scrapped/sarah_lowder/1-s2.0-S0305750X2100067X-mmc5.xlsx', skip = 2)
sarah_historical_farm_size_demo <- readxl::read_excel('../data/raw/web_scrapped/sarah_lowder/1-s2.0-S0305750X2100067X-mmc7.xlsx', skip = 1)
names(sarah_nb_farms) <- c('country', 'census_year', 'nb_farms', 'source', 'gadm_1', 'income_group')



# ------------------------------------------------------------------------------
# Checking and reproducing farm size classes as Sarah did

# quick wrangling of Sarah's excel sheet
names(sarah_farm_size_class) <- c('NAME_0', 'year', 'nb_farms_or_area', 'total', 'fsize0_1ha', 'fsize1_2ha', 'fsize2_5ha', 'fsize5_10ha', 'fsize10_20ha', 'fsize20_50ha',
                                  'fsize50_100ha', 'fsize100_200ha', 'fsize200_500ha', 'fsize500_1000ha', 'fsize1000ha_above', 'source_code', 'income_group')
country_list <- na.omit(sarah_farm_size_class$NAME_0[!grepl('note|source', sarah_farm_size_class$NAME_0, ignore.case = T)])
year_list <- na.omit(sarah_farm_size_class$year)
sarah_farm_size_class <- sarah_farm_size_class |>
  filter(!grepl('source|note', NAME_0, ignore.case = T),
         !if_all(everything(), is.na)) |>
  mutate(NAME_0 = rep(country_list, each = 2),
         year = rep(year_list, each = 2))
sarah_ssa_fsize_class <- sarah_farm_size_class |>
  mutate(certitude =ifelse(grepl('\\*', NAME_0), 'missing_info', 'complete_info'),
         NAME_0 = gsub('\\*', '', NAME_0)) |> 
  filter(NAME_0 %in% unique(ssa$NAME_0)) |>  
  pivot_longer(cols = starts_with('fsize'),
               names_to = 'farm_class',
               values_to = 'val') |>
  filter(farm_class %in% c('fsize0_1ha', 'fsize1_2ha', 'fsize2_5ha', 'fsize5_10ha', 'fsize10_20ha', 'fsize20_50ha')) |>
  mutate(farm_class = case_when(farm_class == 'fsize0_1ha' ~ 1,
                                farm_class == 'fsize1_2ha' ~ 2,
                                farm_class == 'fsize2_5ha' ~ 5,
                                farm_class == 'fsize5_10ha' ~ 10,
                                farm_class == 'fsize10_20ha' ~ 20,
                                farm_class == 'fsize20_50ha' ~ 50,
                                .default = NA)) 

sarah_ssa_fsize_class_ha <- sarah_ssa_fsize_class |> 
  filter(nb_farms_or_area == 'A') |>
  rename(cropland_ha = val)
sarah_ssa_fsize_class_nb <- sarah_ssa_fsize_class |> 
  filter(nb_farms_or_area == 'F') |>
  rename(nb_farms = val)

sarah_ssa_fsize_cum_class_ha <- sarah_ssa_fsize_class_ha |>
  group_by(NAME_0, certitude) |>
  summarize(cum_cropland_ha = cumsum(cropland_ha)) |>
  mutate(farm_class = rep(c(1, 2, 5, 10, 20, 50), length(unique(NAME_0)))) |>
  na.omit() |>
  inner_join(sarah_ssa_fsize_class_ha |>
               select(NAME_0, farm_class, cropland_ha)) |>
  select(NAME_0, farm_class, cropland_ha, cum_cropland_ha) 

sarah_ssa_fsize_cum_class_nb <- sarah_ssa_fsize_class_nb |>
  group_by(NAME_0, certitude) |>
  summarize(cum_nb_farms= cumsum(nb_farms)) |>
  mutate(farm_class = rep(c(1, 2, 5, 10, 20, 50), length(unique(NAME_0)))) |>
  na.omit() |>
  inner_join(sarah_ssa_fsize_class_nb |>
               select(NAME_0, farm_class, nb_farms)) |>
  select(NAME_0, farm_class, nb_farms, cum_nb_farms) 

# putting predicted farm sizes per grid cell into classes to match Sarah's data format
#load('../data/processed/log_normal_distribution_nb_farms_resample_long.rdata')

cty_croplands <- theor_farms_application |>
  bind_cols(terra::extract(ssa_rast, 
                           theor_farms_application |>
                             ungroup() |>
                             select(x, y)))

cty_cropland_under_thresh <- function(thresh){
  class_for_thresh <- cty_croplands |>
    filter(individual_farm_size_ha < thresh) |>
    ungroup() |>
    group_by(NAME_0) |> 
    summarize(pred_nb_farms = n(),
              pred_cum_cropland_ha = sum(individual_farm_size_ha, na.rm = T)) |>
    mutate(farm_class = thresh)
  return(class_for_thresh)
}

six_classes_croplands <- lapply(c(1, 2, 5, 10, 20, 50), cty_cropland_under_thresh) |>
  bind_rows() |>
  select(NAME_0, farm_class, pred_nb_farms, pred_cum_cropland_ha) |> 
  group_by(NAME_0) |> 
  arrange(NAME_0, farm_class) |>
  mutate(pred_cropland_ha = c(pred_cum_cropland_ha[1], diff(pred_cum_cropland_ha)))

comp_fsize_classes_ha <- sarah_ssa_fsize_cum_class_ha |> 
  inner_join(six_classes_croplands) |>
  mutate(NAME_0 = ifelse(grepl('democratic', NAME_0, ignore.case = T), 'DRC', NAME_0))

comp_fsize_classes_nb <- sarah_ssa_fsize_cum_class_nb |> 
  inner_join(six_classes_croplands) |>
  mutate(NAME_0 = ifelse(grepl('democratic', NAME_0, ignore.case = T), 'DRC', NAME_0))

# P04 <- ggplot(comp_fsize_classes_nb) +
#   geom_col(aes(NAME_0, cum_nb_farms, group = farm_class), position = position_dodge(width = 0.8),
#            fill = 'lightskyblue1', colour = 'blue4', linewidth = 0.8, alpha = 0.2) + 
#   geom_col(aes(NAME_0, pred_cum_cropland_ha / 1000000, group = farm_class), position = position_dodge(width = 0.8), 
#            fill = 'salmon1', colour = 'red4', linewidth = 0.8, alpha = 0.2) +
#   labs(x = 'Country', y = 'Cumulative cropland area per farm size class, million ha') +
#   theme_test() + 
#   theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0.1))
# 
# png('../output/graphs/country_compare_sarah_cum_farm_size_classes.png', height = 5, width = 7.5, units = 'in', res = 600)
# P04
# ggsave('../output/graphs/country_compare_sarah_cum_farm_size_classes.png')
# dev.off()

P05 <- ggplot(comp_fsize_classes_nb, aes(NAME_0, nb_farms / 1000000, group = farm_class)) +
  geom_col(position = position_dodge(width = 0.8),
           fill = 'steelblue1', colour = 'blue4', linewidth = 0.8, alpha = 0.2) + 
  geom_col(aes(NAME_0, pred_nb_farms / 1000000, group = farm_class), position = position_dodge(width = 0.8), 
           fill = 'salmon1', colour = 'red4', linewidth = 0.8, alpha = 0.2) +
  geom_text(aes(y = 0.3 + ifelse(nb_farms < pred_nb_farms, pred_nb_farms, nb_farms) / 1000000, 
                label = farm_class),  position = position_dodge(width = 0.8), size = 1.7) +
  labs(x = 'Country', y = 'Cropland area per farm size class, million ha') +
  theme_test() + 
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0.1))

png('../output/graphs/country_compare_sarah_nb_farms_per_size_classes.png', height = 5, width = 7.5, units = 'in', res = 600)
P05
ggsave('../output/graphs/country_compare_sarah_nb_farms_per_size_classes.png')
dev.off()

P06 <- ggplot(comp_fsize_classes_ha) +
  geom_col(aes(NAME_0, cum_cropland_ha / 1000000, group = farm_class), position = position_dodge(width = 0.8),
           fill = 'lightskyblue1', colour = 'blue4', linewidth = 0.8, alpha = 0.2) + 
  geom_col(aes(NAME_0, pred_cum_cropland_ha / 1000000, group = farm_class), position = position_dodge(width = 0.8), 
           fill = 'salmon1', colour = 'red4', linewidth = 0.8, alpha = 0.2) +
  labs(x = 'Country', y = 'Cumulative cropland area per farm size class, million ha') +
  theme_test() + 
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0.1))

png('../output/graphs/country_compare_sarah_cum_farm_size_cropland_classes.png', height = 5, width = 7.5, units = 'in', res = 600)
P06
ggsave('../output/graphs/country_compare_sarah_cum_farm_size_cropland_classes.png')
dev.off()

P07 <- ggplot(comp_fsize_classes_ha, aes(NAME_0, cropland_ha / 1000000, group = farm_class)) +
  geom_col(position = position_dodge(width = 0.8),
           fill = 'steelblue1', colour = 'blue4', linewidth = 0.8, alpha = 0.2) + 
  geom_col(aes(NAME_0, pred_cropland_ha / 1000000, group = farm_class), position = position_dodge(width = 0.8), 
           fill = 'salmon1', colour = 'red4', linewidth = 0.8, alpha = 0.2) +
  geom_text(aes(y = 0.2 + ifelse(cropland_ha < pred_cropland_ha, pred_cropland_ha, cropland_ha)/1000000, 
                label = farm_class),  position = position_dodge(width = 0.8), size = 1.7) +
  labs(x = 'Country', y = 'Cropland area per farm size class, million ha') +
  theme_test() + 
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0.1))

png('../output/graphs/country_compare_sarah_farm_size_cropland_classes.png', height = 5, width = 7.5, units = 'in', res = 600)
P07
ggsave('../output/graphs/country_compare_sarah_farm_size_cropland_classes.png')
dev.off()

save(summary_country, summary_gadm_1, comp_nb_farms, comp_fsize_classes, 
     comp_fsize_classes_ha, comp_fsize_classes_nb,
     P00, P01, P02_final, P03, P04, P05, P06, P07,
     file = '../data/processed/summarized_farm_area_ha_for_validation.Rdata')