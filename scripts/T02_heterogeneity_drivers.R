# ==============================================================================
# Script: T02_heterogeneity_drivers.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Table 2 — prop_area and prop_farms per size class by AEZ and region
#
#   Reads farm_classes_long from cropland_stats_per_aez.rds (built in 10.1)
#   which already contains region (from ssa GADM vect) and aez groupings.
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - March 2026
# ==============================================================================

require(tidyverse)

rm(list = ls())
setwd(paste0(here::here(), '/scripts'))
dir.create('../output/main_fig', recursive = TRUE, showWarnings = FALSE)

# ── Load ───────────────────────────────────────────────────────────────────────
xx <- readRDS('../output/other_illustr/tables/cropland_stats_per_aez.rds')
farm_classes_long <- xx$farm_classes_long
rm(xx)

thresholds <- c('< 0.5 ha', '0.5 - 1 ha', '1 - 2 ha', '2 - 5 ha', '> 5 ha')

# ── Pivot to wide ──────────────────────────────────────────────────────────────
make_wide <- function(grp) {
  farm_classes_long |>
    filter(group_col == grp, !is.na(group_val)) |>
    mutate(size_class = factor(size_class, levels = thresholds)) |>
    select(group_val, size_class, prop_area, prop_farms) |>
    pivot_wider(names_from  = size_class,
                values_from = c(prop_area, prop_farms),
                names_glue  = 'prop_{.value}_{size_class}') |>
    rename(!!grp := group_val)
}

# ── AEZ table ──────────────────────────────────────────────────────────────────
aez_order <- c('humid','sub-humid','semi-arid','tropical highlands','sub-tropical')  # arid excluded
t2_aez <- make_wide('aez') |>
  mutate(aez = factor(aez, levels = aez_order)) |>
  arrange(aez) |>
  mutate(aez = as.character(aez))

message('\n=== T02 by AEZ ==='); print(t2_aez)

# ── Region table ───────────────────────────────────────────────────────────────
t2_region <- make_wide('region') |>
  mutate(region = factor(region, levels = c('Central','Eastern','Southern','Western'))) |>
  arrange(region) |>
  mutate(region = as.character(region))

message('\n=== T02 by Region ==='); print(t2_region)

# ── Save ───────────────────────────────────────────────────────────────────────
write.csv(t2_aez,    '../output/main_fig/T02_heterogeneity_by_aez.csv',    row.names = FALSE)
write.csv(t2_region, '../output/main_fig/T02_heterogeneity_by_region.csv', row.names = FALSE)
message('Saved T02_heterogeneity_by_aez.csv and T02_heterogeneity_by_region.csv')
