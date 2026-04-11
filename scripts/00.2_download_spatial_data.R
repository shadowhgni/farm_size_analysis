# ==============================================================================
# Script: 00.2_download_spatial_data.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Download all spatial data layers that can be auto-downloaded
#
# Author: [Original author] is me
# Documentation: Claude (Anthropic) - February 2026
#
# Description:
#   This script downloads all spatial data layers that are available via the
#   geodata R package. Some layers require manual download (see below).
#
# Outputs (in ../data/raw/spatial/):
#   - gadm/: GADM administrative boundaries for all SSA countries
#   - spam/: SPAM 2010 and 2017 cropland area
#   - landuse/: ESA, GLAD, GeoSurvey cropland
#   - population/: GPW population density
#   - soil_world/: SoilGrids sand content
#   - wc2.1_30s/: WorldClim elevation
#   - temperature/: WorldClim temperature
#   - travel/: Travel time to cities
#
# MANUAL DOWNLOADS REQUIRED:
#   These files cannot be auto-downloaded and must be obtained manually:
#
#   1. SPAM 2020 Cropland
#      URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SWPENT
#      Files: Download all *_H_*_A.tif files
#      Place in: ../data/raw/spatial/spam/spam2020/
#
#   2. Cattle Density (GLW 2010)
#      URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GIVQ75
#      File: 5_Ct_2010_DA.tif
#      Place in: ../data/raw/spatial/cattle-density/
#
#   3. Poverty/Wealth Index
#      URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5OGWYM
#      File: poverty.zip
#      Place in: ../data/raw/spatial/poverty/
#
#   4. Maize Water-Limited Yield (Bonilla-Cedrez et al., 2021)
#      Contact authors or data repository for: watlimsummary.tif
#      Place in: ../data/raw/spatial/maize_water_lim_yield/
#
#   5. FAOSTAT GDP Per Capita
#      URL: https://www.fao.org/faostat/en/#data/MK
#      Download: GDP per capita for all SSA countries
#      Place in: ../data/raw/web_scrapped/faostat/FAOSTAT_data_GDP_per_capita.csv
#
#   6. Du et al. 2025 Annual Livestock Maps (OPTIONAL - large files)
#      DOI: 10.5281/zenodo.17128483
#      URL: https://zenodo.org/records/17128483
#      Files: 
#        - LivestockMap.zip (7.4 GB) - Annual 5km livestock density 1961-2021
#        - MapUncertainty.zip (10.2 GB) - Per-pixel uncertainty layers
#      Species: cattle, buffaloes, sheep, goats, horses, pigs, chickens, ducks
#      Resolution: 5 km, heads/km²
#      Place in: ../data/raw/spatial/livestock-du2025/
#      Citation: Du, Z., Yu, L., Zhao, Y., et al. (2025). Annual global gridded 
#                livestock mapping from 1961 to 2021. Zenodo.
#
# Usage:
#   source("00_download_spatial_data.R")
#   # Estimated download time: 30-60 minutes depending on connection
#   # Estimated disk space: ~10 GB
#
# Notes:
#   - Internet connection required
#   - Downloads are resumable (skips existing files)
#   - Some downloads may take considerable time (SPAM data especially)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------------------------
setwd(paste0(here::here(), '/scripts'))
rm(list = ls())

# Load required packages
require(geodata)
require(terra)

# Set memory options for large rasters
terra::terraOptions(memfrac = 0.3, todisk = TRUE, verbose = FALSE)

# ------------------------------------------------------------------------------
# 2. CONFIGURATION
# ------------------------------------------------------------------------------
# Spatial data repository path
input_path <- '../data/raw/spatial'

# Create directory structure
dirs_to_create <- c(
  file.path(input_path, 'gadm'),
  file.path(input_path, 'spam', 'spam2010'),
  file.path(input_path, 'spam', 'spam2017'),
  file.path(input_path, 'spam', 'spam2020'),
  file.path(input_path, 'landuse'),
  file.path(input_path, 'cattle-density'),
  file.path(input_path, 'population'),
  file.path(input_path, 'soil_world'),
  file.path(input_path, 'wc2.1_30s'),
  file.path(input_path, 'temperature'),
  file.path(input_path, 'rainfall'),
  file.path(input_path, 'travel'),
  file.path(input_path, 'maize_water_lim_yield'),
  file.path(input_path, 'FAO-GDP'),
  file.path(input_path, 'poverty'),
  file.path(input_path, 'AEZ'),
  file.path(input_path, 'livestock-du2025')
)

for (d in dirs_to_create) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created: ", d)
  }
}

# ------------------------------------------------------------------------------
# 3. DEFINE SUB-SAHARAN AFRICA COUNTRIES
# ------------------------------------------------------------------------------
message("\n=== Identifying SSA countries ===")

# Get country codes
isocodes <- geodata::country_codes()

# Filter to Sub-Saharan Africa
isocodes_ssa <- subset(
  isocodes,
  NAME == 'Sudan' |
  UNREGION1 == 'Middle Africa' |
  UNREGION1 == 'Western Africa' |
  UNREGION1 == 'Southern Africa' |
  UNREGION1 == 'Eastern Africa'
)

# Remove small islands
islands <- c('Cabo Verde', 'Comoros', 'Mauritius', 'Mayotte',
             'Réunion', 'Saint Helena', 'São Tomé and Príncipe', 'Seychelles')
isocodes_ssa <- subset(isocodes_ssa, !(NAME %in% islands))

ssa_codes <- isocodes_ssa$ISO3
message("SSA countries to download: ", length(ssa_codes))

# ------------------------------------------------------------------------------
# 4. DOWNLOAD WORLD BOUNDARIES
# ------------------------------------------------------------------------------
message("\n=== Downloading world boundaries ===")
tryCatch({
  world <- geodata::world(path = input_path, resolution = 5, level = 0)
  message("SUCCESS: World boundaries downloaded")
}, error = function(e) {
  message("ERROR: ", e$message)
})

# ------------------------------------------------------------------------------
# 5. DOWNLOAD GADM BOUNDARIES
# ------------------------------------------------------------------------------
message("\n=== Downloading GADM boundaries ===")
message("This may take 10-20 minutes...")

gadm_dir <- file.path(input_path, 'gadm')

for (code in ssa_codes) {
  country_dir <- file.path(gadm_dir, code)
  if (!dir.exists(country_dir)) dir.create(country_dir, recursive = TRUE)
  
  # Try to download each admin level
  for (level in 0:3) {
    tryCatch({
      gadm <- geodata::gadm(country = code, level = level, 
                            version = 'latest', path = country_dir)
      message("  ", code, " level ", level, " - OK")
    }, error = function(e) {
      # Level not available - normal for many countries
    })
  }
}
message("GADM download complete")

# ------------------------------------------------------------------------------
# 6. DOWNLOAD SPAM CROPLAND DATA
# ------------------------------------------------------------------------------
message("\n=== Downloading SPAM cropland data ===")
message("This may take 30+ minutes...")

all_spam_crops <- geodata::spamCrops()$code
message("SPAM crops to download: ", length(all_spam_crops))

# SPAM 2017 (Africa-specific)
message("\nDownloading SPAM 2017...")
spam2017_dir <- file.path(input_path, 'spam', 'spam2017')
for (crop in all_spam_crops) {
  tryCatch({
    geodata::crop_spam(crop, 'harv_area', path = spam2017_dir, africa = TRUE)
    message("  SPAM 2017 ", crop, " - OK")
  }, error = function(e) {
    message("  SPAM 2017 ", crop, " - SKIP")
  })
}

# SPAM 2010 (global, includes Sudan)
message("\nDownloading SPAM 2010...")
spam2010_dir <- file.path(input_path, 'spam', 'spam2010')
for (crop in all_spam_crops) {
  tryCatch({
    geodata::crop_spam(crop, 'harv_area', path = spam2010_dir, africa = FALSE)
    message("  SPAM 2010 ", crop, " - OK")
  }, error = function(e) {
    message("  SPAM 2010 ", crop, " - SKIP")
  })
}

message("SPAM download complete")
message("NOTE: SPAM 2020 requires manual download from Harvard Dataverse")

# ------------------------------------------------------------------------------
# 7. DOWNLOAD CROPLAND MASKS
# ------------------------------------------------------------------------------
message("\n=== Downloading cropland masks ===")

tryCatch({
  message("Downloading ESA WorldCover cropland...")
  esa <- geodata::cropland(source = 'WorldCover', path = input_path)
  message("  ESA WorldCover - OK")
}, error = function(e) message("  ESA WorldCover - ERROR: ", e$message))

tryCatch({
  message("Downloading GeoSurvey cropland...")
  geo <- geodata::cropland(source = 'QED', path = input_path)
  message("  GeoSurvey - OK")
}, error = function(e) message("  GeoSurvey - ERROR: ", e$message))

tryCatch({
  message("Downloading GLAD cropland...")
  glad <- geodata::cropland(source = 'GLAD', year = 2019, path = input_path)
  message("  GLAD - OK")
}, error = function(e) message("  GLAD - ERROR: ", e$message))

# ------------------------------------------------------------------------------
# 8. DOWNLOAD POPULATION DENSITY
# ------------------------------------------------------------------------------
message("\n=== Downloading population density ===")

tryCatch({
  pop <- geodata::population(2020, 0.5, path = input_path)
  message("  GPW Population 2020 - OK")
}, error = function(e) message("  GPW Population - ERROR: ", e$message))

# ------------------------------------------------------------------------------
# 9. DOWNLOAD SOIL DATA
# ------------------------------------------------------------------------------
message("\n=== Downloading soil data ===")

for (depth in c(5, 15, 30)) {
  tryCatch({
    soil <- geodata::soil_world('sand', depth, path = input_path)
    message("  Sand content ", depth, "cm - OK")
  }, error = function(e) {
    message("  Sand content ", depth, "cm - ERROR: ", e$message)
  })
}

# ------------------------------------------------------------------------------
# 10. DOWNLOAD ELEVATION
# ------------------------------------------------------------------------------
message("\n=== Downloading elevation ===")

tryCatch({
  elev <- geodata::elevation_global(0.5, path = input_path)
  message("  Elevation - OK")
}, error = function(e) message("  Elevation - ERROR: ", e$message))

# ------------------------------------------------------------------------------
# 11. DOWNLOAD TEMPERATURE
# ------------------------------------------------------------------------------
message("\n=== Downloading temperature ===")
message("Downloading by country...")

for (code in ssa_codes) {
  tryCatch({
    temp <- geodata::worldclim_country(code, 'tavg', path = input_path)
    message("  Temperature ", code, " - OK")
  }, error = function(e) {
    # Some countries may not have data
  })
}

# ------------------------------------------------------------------------------
# 12. DOWNLOAD TRAVEL TIME
# ------------------------------------------------------------------------------
message("\n=== Downloading travel time ===")

tryCatch({
  travel <- geodata::travel_time(to = 'city', size = 6, up = TRUE, path = input_path)
  message("  Travel time to cities - OK")
}, error = function(e) message("  Travel time - ERROR: ", e$message))

# ------------------------------------------------------------------------------
# 13. SUMMARY
# ------------------------------------------------------------------------------
message("\n")
message("=" |> rep(70) |> paste(collapse = ""))
message("DOWNLOAD SUMMARY")
message("=" |> rep(70) |> paste(collapse = ""))

# Count downloaded files
count_files <- function(pattern, path) {
  length(list.files(path, pattern = pattern, recursive = TRUE))
}

message("\nAuto-downloaded data:")
message("  GADM boundaries:    ", count_files("\\.rds$", file.path(input_path, 'gadm')), " files")
message("  SPAM 2017:          ", count_files("\\.tif$", file.path(input_path, 'spam', 'spam2017')), " files")
message("  SPAM 2010:          ", count_files("\\.tif$", file.path(input_path, 'spam', 'spam2010')), " files")
message("  Cropland masks:     ", count_files("\\.tif$", file.path(input_path, 'landuse')), " files")
message("  Population:         ", count_files("\\.tif$", file.path(input_path, 'population')), " files")
message("  Soil:               ", count_files("\\.tif$", file.path(input_path, 'soil_world')), " files")
message("  Elevation:          ", count_files("\\.tif$", file.path(input_path, 'wc2.1_30s')), " files")
message("  Temperature:        ", count_files("\\.tif$", file.path(input_path, 'temperature')), " files")
message("  Travel time:        ", count_files("\\.tif$", file.path(input_path, 'travel')), " files")

message("\n")
message("=" |> rep(70) |> paste(collapse = ""))
message("MANUAL DOWNLOADS REQUIRED")
message("=" |> rep(70) |> paste(collapse = ""))
message("\n1. SPAM 2020 Cropland")
message("   URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SWPENT")
message("   Place in: ", file.path(input_path, 'spam', 'spam2020'))

message("\n2. Cattle Density (GLW 2010)")
message("   URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GIVQ75")
message("   File: 5_Ct_2010_DA.tif")
message("   Place in: ", file.path(input_path, 'cattle-density'))

message("\n3. Poverty/Wealth Index")
message("   URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5OGWYM")
message("   File: poverty.zip")
message("   Place in: ", file.path(input_path, 'poverty'))

message("\n4. Maize Water-Limited Yield")
message("   Source: Bonilla-Cedrez et al., 2021")
message("   File: watlimsummary.tif")
message("   Place in: ", file.path(input_path, 'maize_water_lim_yield'))

message("\n5. FAOSTAT GDP Per Capita")
message("   URL: https://www.fao.org/faostat/en/#data/MK")
message("   Place in: ../data/raw/web_scrapped/faostat/")

message("\n6. CHIRPS Rainfall")
message("   Run script: 01.1_chirps_download.R")
message("   (Automated download from UCSB)")

message("\n7. Du et al. 2025 Annual Livestock Maps (OPTIONAL - 17.6 GB total)")
message("   DOI: 10.5281/zenodo.17128483")
message("   URL: https://zenodo.org/records/17128483")
message("   Files: LivestockMap.zip (7.4 GB), MapUncertainty.zip (10.2 GB)")
message("   Place in: ", file.path(input_path, 'livestock-du2025'))

message("\n")
message("=" |> rep(70) |> paste(collapse = ""))
message("Download script complete!")
message("=" |> rep(70) |> paste(collapse = ""))

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
