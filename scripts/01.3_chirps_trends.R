# ==============================================================================
# Script: 01.3_chirps_trends.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Calculate long-term rainfall statistics from yearly CHIRPS data
#
# Authors: Deo, Joao, Robert, Fred 
# Documentation: Claude (Anthropic) - February 2026
#
# Inputs:
#   - ../data/raw/spatial/rainfall/CHIRPS/*.tif (dekadal rainfall rasters)
#
# Outputs:
#   - ../data/raw/spatial/rainfall/rainfall_monthly/chirps-monthly-rainfall-YYYY-MM.tif
#   - ../data/raw/spatial/rainfall/rainfall_yearly/chirps-yearly-rainfall-YYYY.tif
#
# Dependencies:
#   - terra: Raster data handling
#   - geodata: Country boundary data
#
# Processing:
#   - Loads dekadal (10-day) rainfall rasters
#   - Sums 3 dekads to monthly totals (mm/month)
#   - Sums 12 months to yearly totals (mm/year)
#   - Crops to Sub-Saharan Africa extent
#   - Handles negative values (sets to NA)
#
# Usage:
#   # Requires 01.1_chirps_download.R to be run first
#   source("01.2_chirps_summarize.R")
#
# Notes:
#   - Processing is memory-intensive for long time series
#   - 2024 may be incomplete (partial year)
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
require(geodata)

# ------------------------------------------------------------------------------
# 2. CONFIGURATION
# ------------------------------------------------------------------------------
# Spatial data repository path
input_path <- '../data/raw/spatial'

# Time range
years <- 1981:2024
months <- sprintf("%02d", 1:12)  # "01" to "12"

# ------------------------------------------------------------------------------
# 3. DEFINE SUB-SAHARAN AFRICA EXTENT
# ------------------------------------------------------------------------------
message("=== Loading SSA boundaries ===")

# Download world boundaries
country <- geodata::world(path = input_path, resolution = 5, level = 0)

# Get ISO country codes
isocodes <- geodata::country_codes()

# Filter to Sub-Saharan Africa (excluding small islands)
isocodes_ssa <- subset(
  isocodes,
  NAME == 'Sudan' |
  UNREGION1 == 'Middle Africa' |
  UNREGION1 == 'Western Africa' |
  UNREGION1 == 'Southern Africa' |
  UNREGION1 == 'Eastern Africa'
)

# Remove small island nations
islands_to_remove <- c(
  'Cabo Verde', 'Comoros', 'Mauritius', 'Mayotte',
  'Réunion', 'Saint Helena', 'São Tomé and Príncipe', 'Seychelles'
)
isocodes_ssa <- subset(isocodes_ssa, !(NAME %in% islands_to_remove))

# Extract SSA polygon
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
message("SSA countries loaded: ", nrow(ssa))

# ------------------------------------------------------------------------------
# 4. CREATE OUTPUT DIRECTORIES
# ------------------------------------------------------------------------------
# Monthly rainfall directory
monthly_dir <- file.path(input_path, 'rainfall', 'rainfall_monthly')
if (!dir.exists(monthly_dir)) {
  dir.create(monthly_dir, recursive = TRUE)
  message("Created: ", monthly_dir)
}

# Yearly rainfall directory
yearly_dir <- file.path(input_path, 'rainfall', 'rainfall_yearly')
if (!dir.exists(yearly_dir)) {
  dir.create(yearly_dir, recursive = TRUE)
  message("Created: ", yearly_dir)
}

# ------------------------------------------------------------------------------
# 5. AGGREGATE DEKADAL TO MONTHLY AND YEARLY
# ------------------------------------------------------------------------------
message("\n=== Processing CHIRPS data ===")

# Source directory for dekadal data
chirps_dir <- file.path(input_path, 'rainfall', 'CHIRPS')

for (year in years) {
  message("\nProcessing year: ", year)
  
  # Initialize yearly accumulator
  yearly_stack <- terra::rast()
  
  for (month in months) {
    # Initialize monthly accumulator
    monthly_stack <- terra::rast()
    
    for (dekad in 1:3) {
      # Build filename
      tif_name <- paste0('chirps-v2.0.', year, '.', month, '.', dekad, '.tif')
      tif_path <- file.path(chirps_dir, tif_name)
      
      # Check if file exists
      if (!file.exists(tif_path)) {
        message("  Missing: ", tif_name)
        next
      }
      
      # Load and process raster
      tryCatch({
        r <- terra::rast(tif_path)
        
        # Crop to SSA extent
        r <- terra::crop(r, ssa, mask = TRUE)
        
        # Set negative values to NA (data quality)
        r[r < 0] <- NA
        
        # Name the layer for tracking
        names(r) <- paste0("rain_", year, "_", month, "_", dekad)
        
        # Add to stacks
        monthly_stack <- c(monthly_stack, r)
        yearly_stack <- c(yearly_stack, r)
        
      }, error = function(e) {
        message("  Error loading: ", tif_name, " - ", e$message)
      })
    }
    
    # Sum dekads to monthly total
    if (terra::nlyr(monthly_stack) > 0) {
      monthly_total <- sum(monthly_stack, na.rm = TRUE)
      names(monthly_total) <- paste0(year, '.', month, '_mm')
      
      # Save monthly raster
      monthly_file <- file.path(
        monthly_dir,
        paste0('chirps-monthly-rainfall-', year, '-', month, '.tif')
      )
      terra::writeRaster(monthly_total, monthly_file, overwrite = TRUE)
    }
  }
  
  # Sum months to yearly total (skip incomplete years)
  if (year != max(years) && terra::nlyr(yearly_stack) == 36) {
    yearly_total <- sum(yearly_stack, na.rm = TRUE)
    names(yearly_total) <- paste0(year, '_mm')
    
    # Save yearly raster
    yearly_file <- file.path(
      yearly_dir,
      paste0('chirps-yearly-rainfall-', year, '.tif')
    )
    terra::writeRaster(yearly_total, yearly_file, overwrite = TRUE)
    message("  Saved yearly total: ", year)
  }
}

# ------------------------------------------------------------------------------
# 6. SUMMARY
# ------------------------------------------------------------------------------
message("\n=== Processing Summary ===")

monthly_files <- length(Sys.glob(file.path(monthly_dir, "*.tif")))
yearly_files <- length(Sys.glob(file.path(yearly_dir, "chirps-yearly*.tif")))

message("Monthly rasters created: ", monthly_files)
message("Yearly rasters created: ", yearly_files)
message("Expected monthly: ", length(years) * 12)
message("Expected yearly: ", length(years) - 1, " (excluding current year)")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
