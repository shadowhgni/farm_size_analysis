# ==============================================================================
# Script: T01_area_production_tables.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Table 1 — Survey summary statistics by country
#          (replicates the table_01 built in 04.1, saved here as the canonical
#           output to output/main_fig/T01_summary_descriptive_stats_survey.csv)
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - March 2026
# ==============================================================================

require(tidyverse)

rm(list = ls())
setwd(paste0(here::here(), '/scripts'))
dir.create('../output/main_fig', recursive = TRUE, showWarnings = FALSE)

# ── Load data ──────────────────────────────────────────────────────────────────
processed_path <- '../data/processed'
lsms_spatial <- tryCatch(
  read.csv(file.path(processed_path, 'lsms_and_zambia.csv')),
  error = function(e) {
    message('CI: lsms_and_zambia.csv not found, using lsms_trimmed stub')
    readRDS(file.path(processed_path, 'lsms_trimmed_95th_africa.rds'))
  }
)

my_lsms <- lsms_spatial |>
  filter(!is.na(farm_area_ha), farm_area_ha > 0)

message("Total observations: ", nrow(my_lsms))
message("Countries: ", length(unique(my_lsms$country)))

# ── Build per-country summary ──────────────────────────────────────────────────
nb_waves <- my_lsms |>
  group_by(country, year) |>
  summarize(.groups = 'drop') |>
  group_by(country) |>
  summarize(
    n_waves = n(),
    period  = paste0(min(year), '-', max(year)),
    .groups = 'drop'
  ) |>
  mutate(
    period = ifelse(
      substr(period, 1, 4) == substr(period, 6, 9),
      substr(period, 1, 4),
      period
    )
  )

nb_obs <- my_lsms |>
  group_by(country) |>
  summarize(n_obs = n(), .groups = 'drop')

farms_below_0.5 <- my_lsms |>
  group_by(country) |>
  summarize(n_0.5 = sum(farm_area_ha < 0.5, na.rm = TRUE), .groups = 'drop')

farms_below_1 <- my_lsms |>
  group_by(country) |>
  summarize(n_1 = sum(farm_area_ha < 1, na.rm = TRUE), .groups = 'drop')

descrip_farm_sizes <- my_lsms |>
  group_by(country) |>
  summarize(
    avg = round(mean(farm_area_ha,             na.rm = TRUE), 2),
    med = round(median(farm_area_ha,           na.rm = TRUE), 2),
    q10 = round(quantile(farm_area_ha, 0.10,   na.rm = TRUE), 2),
    q90 = round(quantile(farm_area_ha, 0.90,   na.rm = TRUE), 2),
    .groups = 'drop'
  )

table_01 <- nb_waves |>
  left_join(nb_obs,          by = 'country') |>
  left_join(farms_below_0.5, by = 'country') |>
  left_join(farms_below_1,   by = 'country') |>
  left_join(descrip_farm_sizes, by = 'country') |>
  mutate(
    prct_below_0.5 = round(100 * n_0.5 / n_obs, 2),
    prct_below_1   = round(100 * n_1   / n_obs, 2)
  )

# TOTAL row
sum_table01 <- tibble(
  country        = 'TOTAL',
  n_waves        = sum(table_01$n_waves),
  period         = paste0(min(my_lsms$year), '-', max(my_lsms$year)),
  n_obs          = sum(table_01$n_obs),
  n_0.5          = sum(table_01$n_0.5),
  n_1            = sum(table_01$n_1),
  prct_below_0.5 = round(100 * sum(table_01$n_0.5) / sum(table_01$n_obs), 2),
  prct_below_1   = round(100 * sum(table_01$n_1)   / sum(table_01$n_obs), 2),
  avg            = round(mean(my_lsms$farm_area_ha,            na.rm = TRUE), 2),
  med            = round(median(my_lsms$farm_area_ha,          na.rm = TRUE), 2),
  q10            = round(quantile(my_lsms$farm_area_ha, 0.10,  na.rm = TRUE), 2),
  q90            = round(quantile(my_lsms$farm_area_ha, 0.90,  na.rm = TRUE), 2)
)

table_01 <- table_01 |>
  bind_rows(sum_table01) |>
  select(country, n_waves, period, n_obs, prct_below_0.5, prct_below_1,
         q10, med, avg, q90)

message("\n=== T01 Survey Summary Statistics ===")
print(table_01, n = 20)

# ── Save ───────────────────────────────────────────────────────────────────────
write.csv(table_01,
          '../output/main_fig/T01_summary_descriptive_stats_survey.csv',
          row.names = FALSE)
message("Saved: ../output/main_fig/T01_summary_descriptive_stats_survey.csv")
