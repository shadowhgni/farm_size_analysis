# Farm size analysis
# harmonize the calculation of farm size across countries

# load packages
require(tidyverse)
# Set working directory
setwd('C:/Users/DHOUGNI/OneDrive - CIMMYT/Documents/Harare 2023/Farm sizes across Africa/scripts')

# Clean environment
rm(list=ls())

# here are stored all the maps that will serve as predictors in the machine-learning (ML) models
input_path <- 'C:/Users/DHOUGNI/OneDrive - CIMMYT/Documents/Harare 2023/Spatial_data_repository'

###########################################
# Quick summary
my_countries <- dir('../data/processed', full.names = T)[grepl('[0-9]_raw\\.csv$', dir('../data/processed'))]
all_countries <- data_frame()
all_lsms_raw_data <- data_frame()
for(i in seq_along(my_countries)){
  cty <- basename(my_countries[i])
  ppp <- read_csv(my_countries[i]) 
  nb_farms <- length(unique(ppp$farm_id))
  one_country <- cbind(country = substr(cty, 1, nchar(cty)- 13),
                       year = substr(cty, nchar(cty) - 11, nchar(cty) - 8),
                       nb_farms = nb_farms) |>
    as_tibble()
  all_countries <- bind_rows(all_countries, one_country)
  all_lsms_raw_data <- all_lsms_raw_data |>
    bind_rows(
      ppp |>
        mutate(ea_id = as.character(ea_id),
               farm_id = as.character(farm_id),
               plot_land_use = as.character(plot_land_use),
               measured_plot = as.character(measured_plot),
               report_unit = as.character(report_unit)) |>
        distinct()
    )
}
all_countries <- all_countries |>
  mutate(nb_farms = as.integer(nb_farms)) |>
  arrange(country, desc(year))

########################################################################
# Check if there is need to adjust some plot area at country level
pl <- ggplot(all_lsms_raw_data, aes(reported_area_ha, measured_plot_area_ha)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.8, colour = 'red') +
  facet_wrap( ~ paste0(country, '_', year), scales = 'free') +
  theme_classic()

png('../data/processed/check_plot_size.png', height = 15, width = 20, units = 'cm', res = 600)
pl
ggsave('../data/processed/check_plot_size.png')
dev.off()

pp <- ggplot(all_lsms_raw_data, aes(reported_area_ha, measured_plot_area_ha)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.8, colour = 'red') +
  lims(x = c(0, 30), y = c(0, 30)) +
  facet_wrap( ~ paste0(country, '_', year) ) +
  theme_classic()

png('../data/processed/check_lsms_plot_size_2.png', height = 15, width = 20, units = 'cm', res = 600)
pp
ggsave('../data/processed/check_lsms_plot_size_2.png')
dev.off()
# pp_check <- ggExtra::ggMarginal(pp, type = 'density', fill = 'grey90')
########################################################################
# Correct measured plot areas 

all_lsms_raw_data <- all_lsms_raw_data |>
  mutate(measured_plot_area_ha = case_when(measured_plot_area_ha %in% c(99, 999, 9999, 99999, 999999) ~ NA,                       # discard all the measurements that are series of 9
                                           .default = measured_plot_area_ha),
         measured_plot_area_ha = case_when(measured_plot_area_ha > 10 & reported_area_ha < 1 ~ measured_plot_area_ha / 10000,     # implicit confusion of unit (sq_meters to ha) for small plots
                                           measured_plot_area_ha > 1000 & reported_area_ha >= 1 ~ measured_plot_area_ha / 10000,  # implicit confusion of unit (sq_meters to ha) for large plots
                                           .default = measured_plot_area_ha),
         measured_plot_area_ha = case_when(measured_plot_area_ha > 50 ~ NA,                                                       # discard all measured plots of more than 50ha 
                                           .default = measured_plot_area_ha),                                                     # try the quantile approach (90th) or boxplot IQR per province for the farm size 
         reported_area_ha = case_when(reported_area_ha > 50 ~ NA,                                                                 # discard all reported plots of more than 20ha 
                                      .default = reported_area_ha)
         )

pm <- ggplot(all_lsms_raw_data, aes(reported_area_ha, measured_plot_area_ha)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.8, colour = 'red') +
  facet_wrap( ~ paste0(country, '_', year), scales = 'free') +
  theme_classic()
png('../data/processed/check_plot_size_after_correction.png', height = 15, width = 20, units = 'cm', res = 600)
pm
ggsave('../data/processed/check_plot_size_after_correction.png')
dev.off()

pn <- ggplot(all_lsms_raw_data, aes(reported_area_ha, measured_plot_area_ha)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.8, colour = 'red') +
  lims(x = c(0, 30), y = c(0, 30)) +
  facet_wrap( ~ paste0(country, '_', year), scales = 'free') +
  theme_classic()
png('../data/processed/check_plot_size_after_correction_2.png', height = 15, width = 20, units = 'cm', res = 600)
pn
ggsave('../data/processed/check_plot_size_after_correction_2.png')
dev.off()
########################################################################
# Calculate farm size, based on the plot size
# First use the measured plot area, and if there is no measurement, use the reported area.
# For plots that were measured to be 0 (due to rounding), the reported area is taken instead.
# only farms that have all their plots assessed will be included
lsms_raw_data <- all_lsms_raw_data |>
  mutate(plot_area_ha = case_when(is.na(measured_plot_area_ha) ~ reported_area_ha,
                                  measured_plot_area_ha <= 0 ~ reported_area_ha,
                                  .default = measured_plot_area_ha))

excluded_farms <- lsms_raw_data |>
  filter(is.na(plot_area_ha)) |>
  select(country, year, farm_id) |>
  distinct()

lsms_farm_size <- lsms_raw_data |>
  anti_join(excluded_farms) |>
  group_by(x, y, country, year, farm_id, hh_size) |>
  summarize(farm_area_ha = sum(plot_area_ha, na.rm = T)) |>
  ungroup()

# if we only use farms with all plots being measured
excluded_farms_strict <- all_lsms_raw_data |>
  filter(is.na(measured_plot_area_ha)) |>
  select(country, year, farm_id) |>
  distinct()

lsms_farm_size_strict <- all_lsms_raw_data |>
  anti_join(excluded_farms_strict) |>
  group_by(x, y, country, year, farm_id, hh_size) |>
  summarize(farm_area_ha = sum(measured_plot_area_ha, na.rm = T)) |>
  ungroup() # here, we lose 32% of the dataset
#######################################################################
# Get Zambian datasets as curated by Joao
zam <- read_csv('../data/raw/received/Zambia/# RALS_for_Typology.csv')

zam_raw <- zam |>
  # select(lon, lat, hh, year, cluster, prov, dist, hh_size, cultland_ha) |>
  rename(x = lon, y = lat) |>
  mutate(country = 'Zambia',
         ea_id = paste0(year, '_', prov, '_', dist, '_', cluster),
         farm_id = paste0(ea_id, '_', hh),
         farm_area_ha = round(cultland_ha, 4)) |>
  select(x, y, country, year, farm_id, hh_size, farm_area_ha)

lsms_and_zambia <- bind_rows(
  lsms_farm_size |>
    filter(!is.na(x), !is.na(y), x + 18 > 0, x - 52 < 0, y + 35 > 0, y - 38 < 0), # using approximate extent of Africa
  zam_raw
)
#######################################################################
# Cross-check against farm size as calculated in the 2021 paper
# load farm size data as received from Joao, the table is named lsms_and_geodata
load('../data/raw/received/lsms_and_geodata.rda')  

pp_2021 <- ggplot(lsms_and_geodata, aes(farm_area_ha)) +
  geom_histogram() +
  lims(x = c(0, 25)) +
  facet_wrap( ~ country_name, scales = 'free') +
  theme_minimal()
png('../data/processed/check_2021_LSMS.png', height = 15, width = 20, units = 'cm', res = 600)
pp_2021
ggsave('../data/processed/check_2021_LSMS.png')
dev.off()

pp_2024 <- ggplot(lsms_and_zambia |>
                    filter(country %in% c('Ethiopia', 'Malawi', 'Niger', 'Nigeria', 'Tanzania', 'Uganda')), 
                  aes(farm_area_ha)) +
  geom_histogram() +
  lims(x = c(0, 25)) +
  facet_wrap(country ~ year, scales = 'free') +
  theme_minimal()
png('../data/processed/check_2024_LSMS.png', height = 15, width = 20, units = 'cm', res = 600)
pp_2024
ggsave('../data/processed/check_2024_LSMS.png')
dev.off()

pp_2021_vs_2024 <- patchwork::wrap_plots(pp_2021 + pp_2024 + patchwork::plot_layout (widths = c(1, 2)))
png('../data/processed/check_2021_2024_LSMS.png', height = 15, width = 30, units = 'cm', res = 600)
pp_2021_vs_2024
ggsave('../data/processed/check_2021_2024_LSMS.png')
dev.off()

lsms_and_geodata |>
  group_by(country_name) |>
  summarize(med = median(farm_area_ha, na.rm = T), mean = mean(farm_area_ha, na.rm = T), sd = sd(farm_area_ha, na.rm = T))

lsms_and_zambia |>
  group_by(country, year) |>
  summarize(med = median(farm_area_ha, na.rm = T), mean = mean(farm_area_ha, na.rm = T), sd = sd(farm_area_ha, na.rm = T)) |>
  View()

write_csv(all_countries, file = '../data/processed/lsms_number_of_farms_all_inclusive.csv')
write_csv(all_lsms_raw_data, file = '../data/processed/lsms_raw_data.csv')
write_csv(lsms_and_zambia, file = '../data/processed/lsms_and_zambia.csv')
save(all_countries, lsms_farm_size, lsms_farm_size_strict, lsms_and_zambia, file = '../data/processed/lsms_and_zambia.rdata')