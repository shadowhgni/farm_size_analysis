# ==============================================================================
# Script: 02.3_measured_vs_reported.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Analyze agreement between GPS-measured and farmer-reported plot sizes
#
# Authors: Deo, Joao, Robert, Fred 
# Documentation: Claude (Anthropic) - February 2026
#
# Description:
#   This script combines raw LSMS data from all countries, corrects measurement
#   errors, calculates farm-level area from plot-level data, and integrates
#   Zambian RALS data which uses a different survey methodology.
#
# Inputs:
#   - ../data/processed/{Country}_{Year}_raw.csv (from 02.1)
#   - ../data/raw/received/Zambia/# RALS_for_Typology.csv (Zambia RALS data)
#   - ../data/raw/received/lsms_and_geodata.rda (2021 paper reference data)
#
# Outputs:
#   - ../data/processed/lsms_number_of_farms_all_inclusive.csv
#   - ../data/processed/lsms_raw_data.csv
#   - ../data/processed/lsms_and_zambia.csv
#   - ../data/processed/lsms_and_zambia.rds
#
# Processing Steps:
#   1. Read all country CSV files
#   2. Correct measurement errors (unit confusion, outliers)
#   3. Calculate plot area (prefer measured over reported)
#   4. Aggregate plots to farm-level area
#   5. Integrate Zambia RALS data
#   6. Cross-check against previous analysis
#
# Measurement Error Corrections:
#   - Remove placeholder values (99, 999, 9999, etc.)
#   - Fix sq_meter/hectare confusion (factor of 10,000)
#   - Remove implausible values (>100 ha per plot)
#   - Prefer GPS-measured over farmer-reported when both available
#
# Dependencies:
#   - tidyverse: Data manipulation
#
# Usage:
#   # Requires 02.1 to be run first
#   source("02.2_harmonize_farm_area.R")
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------------------------
# Load packages
require(tidyverse)

# Set working directory
setwd(paste0(here::here(), '/scripts'))

# Clean environment
rm(list = ls())

# Paths
input_path <- '../data/raw/spatial'
processed_path <- '../data/processed'

# ------------------------------------------------------------------------------
# 2. READ ALL COUNTRY RAW DATA
# ------------------------------------------------------------------------------
message("=== Reading all country CSV files ===")

# Find all raw data files
my_countries <- dir(processed_path, full.names = TRUE)
my_countries <- my_countries[grepl('[0-9]_raw\\.csv$', my_countries)]
message("Found ", length(my_countries), " country-year files")

# Initialize data frames
all_countries <- data.frame()
all_lsms_raw_data <- data.frame()

# Read and combine all files
for (i in seq_along(my_countries)) {
  cty <- basename(my_countries[i])
  ppp <- read_csv(my_countries[i], show_col_types = FALSE)
  
  # Extract country and year from filename
  nb_farms <- length(unique(ppp$farm_id))
  one_country <- data.frame(
    country = substr(cty, 1, nchar(cty) - 13),
    year = substr(cty, nchar(cty) - 11, nchar(cty) - 8),
    nb_farms = nb_farms
  )
  all_countries <- bind_rows(all_countries, one_country)
  
  # Standardize column types and combine
  all_lsms_raw_data <- all_lsms_raw_data |>
    bind_rows(
      ppp |>
        mutate(
          ea_id = as.character(ea_id),
          farm_id = as.character(farm_id),
          plot_land_use = as.character(plot_land_use),
          measured_plot = as.character(measured_plot),
          report_unit = as.character(report_unit)
        ) |>
        distinct()
    )
  
  message("  ", cty, ": ", nb_farms, " farms")
}

# Sort by country and year
all_countries <- all_countries |>
  mutate(nb_farms = as.integer(nb_farms)) |>
  arrange(country, desc(year))

message("\nTotal plots: ", nrow(all_lsms_raw_data))
message("Total unique farms: ", sum(all_countries$nb_farms))

# ------------------------------------------------------------------------------
# 3. CORRECT MEASUREMENT ERRORS
# ------------------------------------------------------------------------------
message("\n=== Correcting measurement errors ===")

all_lsms_raw_data <- all_lsms_raw_data |>
  mutate(
    # Remove placeholder values (series of 9s indicating missing data)
    measured_plot_area_ha = case_when(
      measured_plot_area_ha %in% c(99, 999, 9999, 99999, 999999) ~ NA,
      .default = measured_plot_area_ha
    ),
    
    # Fix unit confusion: sq_meters recorded as hectares
    # Small plots (reported <1 ha) with large measured values (>10)
    measured_plot_area_ha = case_when(
      measured_plot_area_ha > 10 & reported_area_ha < 1 ~ 
        measured_plot_area_ha / 10000,
      # Large plots with very large measured values (>1000)
      measured_plot_area_ha > 1000 & reported_area_ha >= 1 ~ 
        measured_plot_area_ha / 10000,
      .default = measured_plot_area_ha
    ),
    
    # Remove implausible values (>100 ha per plot)
    measured_plot_area_ha = case_when(
      measured_plot_area_ha > 100 ~ NA,
      .default = measured_plot_area_ha
    )
  )

# Count corrections
n_orig <- nrow(all_lsms_raw_data)
n_measured <- sum(!is.na(all_lsms_raw_data$measured_plot_area_ha))
message("Plots with GPS measurement: ", n_measured, " (", 
        round(100 * n_measured / n_orig, 1), "%)")

# ------------------------------------------------------------------------------
# 4. CALCULATE PLOT AREA (HARMONIZED)
# ------------------------------------------------------------------------------
message("\n=== Calculating harmonized plot area ===")

# Decision rule for plot area:
# 1. Use measured area if available and valid
# 2. Fall back to reported area if measured is NA or 0
# 3. If measured >> reported (>5x) and measured > 20ha, use reported (likely error)

lsms_raw_data <- all_lsms_raw_data |>
  mutate(
    plot_area_ha = case_when(
      is.na(measured_plot_area_ha) ~ reported_area_ha,
      measured_plot_area_ha <= 0 ~ reported_area_ha,
      measured_plot_area_ha / reported_area_ha > 5 & measured_plot_area_ha > 20 ~ 
        reported_area_ha,
      .default = measured_plot_area_ha
    )
  )

# ------------------------------------------------------------------------------
# 5. AGGREGATE TO FARM LEVEL
# ------------------------------------------------------------------------------
message("\n=== Aggregating to farm level ===")

# Identify farms with any missing plot area
excluded_farms <- lsms_raw_data |>
  filter(is.na(plot_area_ha)) |>
  select(country, year, farm_id) |>
  distinct()

message("Farms excluded (missing plot data): ", nrow(excluded_farms))

# Calculate farm-level area (sum of all plots)
lsms_farm_size <- lsms_raw_data |>
  anti_join(excluded_farms, by = c("country", "year", "farm_id")) |>
  group_by(x, y, country, year, farm_id, hh_size) |>
  summarize(
    farm_area_ha = sum(plot_area_ha, na.rm = TRUE),
    .groups = 'drop'
  )

message("Farms with complete data: ", nrow(lsms_farm_size))

# Alternative: strict version (only GPS-measured plots)
excluded_farms_strict <- all_lsms_raw_data |>
  filter(is.na(measured_plot_area_ha)) |>
  select(country, year, farm_id) |>
  distinct()

lsms_farm_size_strict <- all_lsms_raw_data |>
  anti_join(excluded_farms_strict, by = c("country", "year", "farm_id")) |>
  group_by(x, y, country, year, farm_id, hh_size) |>
  summarize(
    farm_area_ha = sum(measured_plot_area_ha, na.rm = TRUE),
    .groups = 'drop'
  )

message("Farms with ALL plots measured: ", nrow(lsms_farm_size_strict),
        " (", round(100 * nrow(lsms_farm_size_strict) / nrow(lsms_farm_size), 1), 
        "% of total)")

# ------------------------------------------------------------------------------
# 6. INTEGRATE ZAMBIA RALS DATA
# ------------------------------------------------------------------------------
message("\n=== Integrating Zambia RALS data ===")

zambia_file <- '../data/raw/received/Zambia/# RALS_for_Typology.csv'

if (file.exists(zambia_file)) {
  zam <- read_csv(zambia_file, show_col_types = FALSE)
  
  zam_raw <- zam |>
    rename(x = lon, y = lat) |>
    mutate(
      country = 'Zambia',
      ea_id = paste0(year, '_', prov, '_', dist, '_', cluster),
      farm_id = paste0(ea_id, '_', hh),
      farm_area_ha = round(cultland_ha, 4)
    ) |>
    select(x, y, cluster, country, year, farm_id, hh_size, farm_area_ha)
  
  # Average coordinates by cluster (for privacy)
  zam_raw <- zam_raw |>
    select(-c(x, y)) |>
    inner_join(
      zam_raw |>
        group_by(cluster) |>
        summarize(x = mean(x, na.rm = TRUE), y = mean(y, na.rm = TRUE)),
      by = "cluster"
    ) |>
    select(-cluster)
  
  message("Zambia farms: ", nrow(zam_raw))
  
  # Combine LSMS and Zambia
  lsms_and_zambia <- bind_rows(
    lsms_farm_size |>
      # Filter to approximate Africa extent
      filter(!is.na(x), !is.na(y), 
             x + 18 > 0, x - 52 < 0, 
             y + 35 > 0, y - 38 < 0),
    zam_raw
  )
} else {
  message("WARNING: Zambia RALS file not found")
  lsms_and_zambia <- lsms_farm_size |>
    filter(!is.na(x), !is.na(y),
           x + 18 > 0, x - 52 < 0,
           y + 35 > 0, y - 38 < 0)
}

message("Total farms (LSMS + Zambia): ", nrow(lsms_and_zambia))

# ------------------------------------------------------------------------------
# 7. SUMMARY STATISTICS
# ------------------------------------------------------------------------------
message("\n=== Summary Statistics ===")

summary_stats <- lsms_and_zambia |>
  group_by(country) |>
  summarize(
    n_farms = n(),
    median_ha = round(median(farm_area_ha, na.rm = TRUE), 2),
    mean_ha = round(mean(farm_area_ha, na.rm = TRUE), 2),
    sd_ha = round(sd(farm_area_ha, na.rm = TRUE), 2),
    .groups = 'drop'
  )

print(summary_stats)

# ------------------------------------------------------------------------------
# 8. SAVE OUTPUTS
# ------------------------------------------------------------------------------
message("\n=== Saving outputs ===")

write_csv(all_countries, 
          file = file.path(processed_path, 'lsms_number_of_farms_all_inclusive.csv'))
message("Saved: lsms_number_of_farms_all_inclusive.csv")

write_csv(all_lsms_raw_data, 
          file = file.path(processed_path, 'lsms_raw_data.csv'))
message("Saved: lsms_raw_data.csv")

write_csv(lsms_and_zambia, 
          file = file.path(processed_path, 'lsms_and_zambia.csv'))
message("Saved: lsms_and_zambia.csv")

saveRDS(
  list(
    all_countries = all_countries,
    all_lsms_raw_data = all_lsms_raw_data,
    lsms_farm_size = lsms_farm_size,
    lsms_farm_size_strict = lsms_farm_size_strict,
    lsms_and_zambia = lsms_and_zambia
  ),
  file = file.path(processed_path, 'lsms_and_zambia.rds')
)
message("Saved: lsms_and_zambia.rds")

message("\n=== Processing Complete ===")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
