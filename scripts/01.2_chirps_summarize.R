# ==============================================================================
# Script: 01.2_chirps_summarize.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Aggregate dekadal CHIRPS rainfall to monthly and yearly totals
#
# Authors: Deo, Joao, Robert, Fred 
# Documentation: Claude (Anthropic) - February 2026
#
# Inputs:
#   - Internet connection to UCSB CHIRPS server
#
# Outputs:
#   - ../data/raw/spatial/rainfall/*.gz (compressed dekadal rainfall rasters)
#   - ../data/raw/spatial/rainfall/CHIRPS/*.tif (decompressed rasters)
#
# Dependencies:
#   - curl: HTTP file downloads
#   - R.utils: File decompression (gunzip)
#
# Data Source:
#   CHIRPS (Climate Hazards Group InfraRed Precipitation with Station data)
#   URL: https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_dekad/tifs/
#   Resolution: 0.05° (~5.5 km)
#   Temporal: Dekadal (10-day periods, 3 per month)
#   Coverage: 1981-present, Africa
#
# Usage:
#   source("01.1_chirps_download.R")
#   # Downloads ~1,584 files (44 years × 12 months × 3 dekads)
#   # Estimated size: ~15 GB compressed, ~50 GB decompressed
#
# Notes:
#   - Downloads are resumable (skips existing files)
#   - Dekad 1: days 1-10, Dekad 2: days 11-20, Dekad 3: days 21-end
#   - Network errors are caught and logged (script continues)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------------------------
# Set working directory
setwd(paste0(here::here(), '/scripts'))

# Clean environment
rm(list = ls())

# Load required packages
require(curl)

# ------------------------------------------------------------------------------
# 2. CONFIGURATION
# ------------------------------------------------------------------------------
# Spatial data repository path (relative from scripts folder)
input_path <- '../data/raw/spatial'

# CHIRPS data source URL
chirps_url <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_dekad/tifs/'

# Time range for downloads
years <- seq(1981, 2024, 1)
months <- sprintf("%02d", 1:12)  # Zero-padded: "01" to "12"
dekads <- 1:3                     # Three dekads per month

# ------------------------------------------------------------------------------
# 3. GENERATE FILE LIST
# ------------------------------------------------------------------------------
# Build list of all CHIRPS filenames to download
# Format: chirps-v2.0.YYYY.MM.D.tif.gz

message("=== Generating file list ===")

filenames <- expand.grid(
  year = years,
  month = months,
  dekad = dekads,
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(
    filename = paste0("chirps-v2.0.", year, ".", month, ".", dekad, ".tif.gz")
  ) |>
  dplyr::pull(filename)

message("Total files to download: ", length(filenames))

# ------------------------------------------------------------------------------
# 4. CREATE OUTPUT DIRECTORIES
# ------------------------------------------------------------------------------
# Directory for compressed files
rainfall_dir <- file.path(input_path, 'rainfall')
if (!dir.exists(rainfall_dir)) {
  dir.create(rainfall_dir, recursive = TRUE)
  message("Created directory: ", rainfall_dir)
}

# Directory for decompressed files
chirps_dir <- file.path(input_path, 'rainfall', 'CHIRPS')
if (!dir.exists(chirps_dir)) {
  dir.create(chirps_dir, recursive = TRUE)
  message("Created directory: ", chirps_dir)
}

# ------------------------------------------------------------------------------
# 5. DOWNLOAD COMPRESSED FILES
# ------------------------------------------------------------------------------
message("\n=== Downloading CHIRPS data ===")

# Get list of already downloaded files
downloaded_gz <- basename(Sys.glob(file.path(rainfall_dir, "*.gz")))
message("Already downloaded: ", length(downloaded_gz), " files")

# Download missing files
download_count <- 0
error_count <- 0

for (filename in filenames) {
  # Skip if already downloaded
  if (filename %in% downloaded_gz) next
  
  # Attempt download
  tryCatch({
    dest_file <- file.path(rainfall_dir, filename)
    curl::curl_download(
      url = paste0(chirps_url, filename),
      destfile = dest_file,
      quiet = TRUE
    )
    download_count <- download_count + 1
    
    # Progress message every 50 files
    if (download_count %% 50 == 0) {
      message("Downloaded: ", download_count, " files...")
    }
  }, error = function(e) {
    error_count <<- error_count + 1
    message("ERROR downloading: ", filename)
  })
}

message("Downloads complete: ", download_count, " new files")
if (error_count > 0) {
  message("WARNING: ", error_count, " files failed to download")
}

# ------------------------------------------------------------------------------
# 6. DECOMPRESS FILES
# ------------------------------------------------------------------------------
message("\n=== Decompressing CHIRPS data ===")

# Get list of already decompressed files
downloaded_tif <- basename(Sys.glob(file.path(chirps_dir, "*.tif")))
message("Already decompressed: ", length(downloaded_tif), " files")

# Decompress missing files
decompress_count <- 0

for (year in years) {
  for (month in months) {
    for (dekad in dekads) {
      # Build filenames
      tif_name <- paste0('chirps-v2.0.', year, '.', month, '.', dekad, '.tif')
      gz_name <- paste0(tif_name, '.gz')
      
      # Skip if already decompressed
      if (tif_name %in% downloaded_tif) next
      
      # Check if compressed file exists
      gz_path <- file.path(rainfall_dir, gz_name)
      if (!file.exists(gz_path)) next
      
      # Attempt decompression
      tryCatch({
        R.utils::gunzip(
          filename = gz_path,
          destname = file.path(chirps_dir, tif_name),
          remove = FALSE,
          overwrite = TRUE
        )
        decompress_count <- decompress_count + 1
        
        # Progress message
        if (decompress_count %% 100 == 0) {
          message("Decompressed: ", decompress_count, " files...")
        }
      }, error = function(e) {
        message("ERROR decompressing: ", gz_name)
      })
    }
  }
}

message("Decompression complete: ", decompress_count, " new files")

# ------------------------------------------------------------------------------
# 7. SUMMARY
# ------------------------------------------------------------------------------
message("\n=== Download Summary ===")
final_gz <- length(Sys.glob(file.path(rainfall_dir, "*.gz")))
final_tif <- length(Sys.glob(file.path(chirps_dir, "*.tif")))
message("Compressed files (.gz): ", final_gz)
message("Decompressed files (.tif): ", final_tif)
message("Expected total: ", length(filenames))

if (final_tif == length(filenames)) {
  message("SUCCESS: All CHIRPS files downloaded and decompressed!")
} else {
  message("NOTE: Some files may be missing (recent dates or download errors)")
}

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
