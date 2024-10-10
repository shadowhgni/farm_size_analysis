# DESCRIPTIVE STATS OF LSMS DATA

# load packages
require(tidyverse)
# Set working directory
setwd(here::here())

# Clean environment
rm(list=ls())

# ------------------------------------------------------------------------------
# for all raw files, get 'my_lsms_africa.Rdata' and pick my_lsms
load('../data/processed/lsms_spatial_raw.rdata') 

my_lsms <- lsms_spatial_raw |>
  select(country, year, farm_area_ha)

# nunber of waves per country
nb_waves <- my_lsms |>
  select(country, year) |>
  distinct() |>
  group_by(country) |>
  summarize(n_waves = n(), period = paste0(min(year), '-', max(year))) |>
  mutate(period = ifelse(substr(period, 1, 4) == substr(period, 6, 9),
                         substr(period, 1, 4), period))

nb_obs <- my_lsms |>
  select(country, year, farm_area_ha) |>
  group_by(country) |>
  summarize(n_obs = n())

farms_below_0.5 <- my_lsms |>
  select(country, farm_area_ha) |>
  filter(farm_area_ha < 0.5) |>
  group_by(country) |>
  summarize(n_0.5 = n())

farms_below_1 <- my_lsms |>
  select(country, farm_area_ha) |>
  filter(farm_area_ha < 1) |>
  group_by(country) |>
  summarize(n_1 = n())

descrip_farm_sizes <-  my_lsms |>
  select(country,  farm_area_ha) |>
  group_by(country) |>
  summarize(avg = round(mean(farm_area_ha, na.rm = T), 2),
            med = round(median(farm_area_ha, na.rm = T),2),
            q10 = round(quantile(farm_area_ha, 0.1, na.rm = T), 2),
            q90 = round(quantile(farm_area_ha, 0.9, na.rm = T), 2))

table_01 <- nb_waves |>
  inner_join(nb_obs) |>
  inner_join(farms_below_0.5) |>
  inner_join(farms_below_1) |>
  inner_join(descrip_farm_sizes) |>
  mutate(prct_below_0.5 = round(100 * n_0.5 / n_obs, 2),
         prct_below_1 = round(100 * n_1 / n_obs, 2)) 

sum_table01 <- table_01 |>
  summarize(country = 'all', n_waves = sum(n_waves), n_obs = sum(n_obs),
            prct_below_0.5 = round(100 * sum(n_0.5) / sum(n_obs), 2), 
            prct_below_1 = round(100 * sum(n_1) / sum(n_obs), 2),
            avg = round(mean(my_lsms$farm_area_ha, na.rm = T), 2),
            med = round(median(my_lsms$farm_area_ha, na.rm = T), 2),
            q10 = round(quantile(my_lsms$farm_area_ha, 0.1, na.rm = T), 2),
            q90 = round(quantile(my_lsms$farm_area_ha, 0.9, na.rm = T), 2))

table_01 <- table_01 |>
  bind_rows(sum_table01)|>
  select(country, n_waves, period, n_obs, prct_below_0.5, prct_below_1, q10, med, avg, q90) 

write_csv(table_01, file = '../output/tables/summary_descriptive_stats_survey.csv')