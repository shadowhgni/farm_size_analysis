# ==============================================================================
# Script: 01.4_prepare_spatial_layers.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Prepare all spatial predictor layers for machine learning models
#
# Authors: Deo, Joao, Robert, Fred 
# Documentation: Claude (Anthropic) - February 2026
#
# Inputs:
#   - ../data/raw/spatial/rainfall/rainfall_yearly/chirps-yearly-rainfall-*.tif
#
# Outputs:
#   - ../data/raw/spatial/rainfall/rainfall_yearly/#_long_term_rainfall_avg.tif
#   - ../data/raw/spatial/rainfall/rainfall_yearly/#_long_term_rainfall_cv.tif
#
# Dependencies:
#   - terra: Raster data handling
#
# Processing:
#   - Loads all yearly rainfall rasters (1981-2023)
#   - Calculates pixel-wise mean annual rainfall (mm/year)
#   - Calculates pixel-wise coefficient of variation (CV = SD/mean)
#
# Usage:
#   # Requires 01.2_chirps_summarize.R to be run first
#   source("01.3_chirps_trends.R")
#
# Notes:
#   - CV indicates rainfall variability (higher = more variable)
#   - Output files prefixed with '#' to sort first in directory listings
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------------------------
# Set working directory
setwd(paste0(here::here(), '/scripts'))

# Clean environment
rm(list = ls())

# Load required packages
require(terra)

# ------------------------------------------------------------------------------
# 2. CONFIGURATION
# ------------------------------------------------------------------------------
# Spatial data repository path
input_path <- '../data/raw/spatial'

# Input/output directory
yearly_dir <- file.path(input_path, 'rainfall', 'rainfall_yearly')

# ------------------------------------------------------------------------------
# 3. LOAD YEARLY RAINFALL DATA
# ------------------------------------------------------------------------------
message("=== Loading yearly rainfall data ===")

# Find all yearly rainfall rasters
# Note: Original script used underscore pattern; updated script uses hyphen
yearly_files <- Sys.glob(file.path(yearly_dir, 'chirps-yearly-rainfall-*.tif'))

# Fallback to underscore pattern if no files found
if (length(yearly_files) == 0) {
  yearly_files <- Sys.glob(file.path(yearly_dir, 'chirps_yearly_rainfall_*.tif'))
}

if (length(yearly_files) == 0) {
  stop("No yearly rainfall files found in: ", yearly_dir)
}

message("Found ", length(yearly_files), " yearly rasters")

# Load as multi-layer raster stack
years_stack <- terra::rast(yearly_files)
message("Loaded raster stack with ", terra::nlyr(years_stack), " layers")
message("Years: ", min(terra::time(years_stack)), " to ", max(terra::time(years_stack)))

# ------------------------------------------------------------------------------
# 4. CALCULATE LONG-TERM STATISTICS
# ------------------------------------------------------------------------------
message("\n=== Calculating long-term statistics ===")

# Mean annual rainfall (mm/year)
message("Calculating mean...")
longterm_avg <- terra::app(years_stack, fun = mean, na.rm = TRUE)
names(longterm_avg) <- 'rainfall_avg_mm'

# Standard deviation
message("Calculating standard deviation...")
longterm_std <- terra::app(years_stack, fun = sd, na.rm = TRUE)
names(longterm_std) <- 'rainfall_sd_mm'

# Coefficient of variation (CV = SD / Mean)
# CV > 0.3 indicates high interannual variability
message("Calculating coefficient of variation...")
longterm_cv <- longterm_std / longterm_avg
names(longterm_cv) <- 'rainfall_cv'

# ------------------------------------------------------------------------------
# 5. VISUALIZE RESULTS
# ------------------------------------------------------------------------------
message("\n=== Generating preview plots ===")

# Plot both statistics side by side
par(mfrow = c(1, 2))
terra::plot(
  longterm_avg,
  main = "Mean Annual Rainfall (mm)",
  col = rev(terrain.colors(50))
)
terra::plot(
  longterm_cv,
  main = "Rainfall CV",
  col = hcl.colors(50, "YlOrRd")
)
par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# 6. SAVE OUTPUTS
# ------------------------------------------------------------------------------
message("\n=== Saving outputs ===")

# Long-term average
avg_file <- file.path(yearly_dir, '#_long_term_rainfall_avg.tif')
terra::writeRaster(longterm_avg, avg_file, overwrite = TRUE)
message("Saved: ", avg_file)

# Coefficient of variation
cv_file <- file.path(yearly_dir, '#_long_term_rainfall_cv.tif')
terra::writeRaster(longterm_cv, cv_file, overwrite = TRUE)
message("Saved: ", cv_file)

# ------------------------------------------------------------------------------
# 7. SUMMARY STATISTICS
# ------------------------------------------------------------------------------
message("\n=== Summary Statistics ===")

# Mean rainfall stats
avg_vals <- terra::values(longterm_avg, na.rm = TRUE)
message("Mean Annual Rainfall (mm):")
message("  Min:    ", round(min(avg_vals), 1))
message("  Median: ", round(median(avg_vals), 1))
message("  Max:    ", round(max(avg_vals), 1))

# CV stats
cv_vals <- terra::values(longterm_cv, na.rm = TRUE)
message("\nRainfall CV:")
message("  Min:    ", round(min(cv_vals), 3))
message("  Median: ", round(median(cv_vals), 3))
message("  Max:    ", round(max(cv_vals), 3))

# Classify variability
pct_high_var <- mean(cv_vals > 0.3, na.rm = TRUE) * 100
message("\n  % area with high variability (CV > 0.3): ", round(pct_high_var, 1), "%")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
