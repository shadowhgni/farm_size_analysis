# ==============================================================================
# Script: 02.1_compile_LSMS.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Compile and standardize LSMS survey data across 16 SSA countries
#
# Authors: Deo, Joao, Robert, Fred 
# Documentation: Claude (Anthropic) - February 2026
#
# Inputs (from ../data/raw/spatial/):
#   - GADM administrative boundaries (via geodata)
#   - SPAM 2010/2017/2020 cropland area (via geodata + manual download)
#   - ESA WorldCover cropland (via geodata)
#   - GLAD/GeoSurvey cropland (via geodata)
#   - Cattle density (manual download from Harvard Dataverse)
#   - Population density (via geodata)
#   - Soil sand content (via geodata)
#   - Elevation/slope (via geodata)
#   - Temperature (via geodata WorldClim)
#   - Rainfall (from 01.2/01.3 scripts)
#   - Travel time to cities (via geodata)
#   - Maize water-limited yield (manual download)
#   - GDP per capita (FAOSTAT CSV)
#   - Poverty/wealth index (manual download)
#
# Outputs:
#   - ../data/processed/all_predictors.tif (stacked predictor layers)
#   - ../data/processed/stacked_rasters_africa.tif (for ML models)
#   - ../output/maps/africa-*.png (visualization maps)
#   - Individual *_ssa.tif layers in input_path subdirectories
#
# Dependencies:
#   - terra: Raster/vector handling
#   - geodata: Data downloads
#   - tidyverse: Data manipulation
#   - afrilearndata: Land cover classification
#
# Processing Steps:
#   1. Define SSA study region (excluding small islands)
#   2. Download GADM boundaries for all SSA countries
#   3. Process cropland layers (SPAM, ESA, GLAD, GeoSurvey)
#   4. Process cattle density
#   5. Process population density
#   6. Calculate cropland per capita
#   7. Process soil properties (sand content 0-30cm)
#   8. Process elevation and slope
#   9. Process temperature (WorldClim annual mean)
#   10. Process rainfall (from CHIRPS outputs)
#   11. Process travel time to markets
#   12. Process maize yield potential
#   13. Process GDP per capita
#   14. Process poverty/wealth indices
#   15. Stack all layers and create masks
#
# Usage:
#   # Requires 01.1-01.3 scripts to be run first (for rainfall)
#   # Some data requires manual download (see DATA SOURCES below)
#   source("01.4_prepare_spatial_layers.R")
#
# DATA SOURCES REQUIRING MANUAL DOWNLOAD:
#   1. SPAM 2020: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SWPENT
#      → Place .tif files in ../data/raw/spatial/spam/spam2020/
#
#   2. Cattle density: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GIVQ75
#      → Download "5_Ct_2010_DA.tif" to ../data/raw/spatial/cattle-density/
#
#   3. Wealth/Poverty index: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5OGWYM
#      → Download poverty.zip to ../data/raw/spatial/poverty/
#
#   4. Maize water-limited yield: From Bonilla-Cedrez et al., 2021
#      → Download "watlimsummary.tif" to ../data/raw/spatial/maize_water_lim_yield/
#
#   5. FAOSTAT GDP: https://www.fao.org/faostat/en/#data/MK
#      → Download GDP per capita CSV to ../data/raw/web_scrapped/faostat/
#
# Notes:
#   - All rasters are resampled to SPAM 2017 resolution (~10km)
#   - Coordinate system: EPSG:4326 (WGS84)
#   - Processing is memory-intensive; consider terra::terraOptions(memfrac=0.2)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------------------------
# Set working directory
setwd(paste0(here::here(), '/scripts'))

# Clean environment
rm(list = ls())

# Load required packages
require(tidyverse)
require(terra)
require(geodata)

# Memory management for large rasters
terra::terraOptions(memfrac = 0.2, todisk = TRUE, verbose = FALSE)

# ------------------------------------------------------------------------------
# 2. CONFIGURATION
# ------------------------------------------------------------------------------
# Spatial data repository path (relative from scripts folder)
input_path <- '../data/raw/spatial'

# Output paths
output_maps <- '../output/maps'
output_processed <- '../data/processed'

# Ensure output directories exist
dir.create(output_maps, recursive = TRUE, showWarnings = FALSE)
dir.create(output_processed, recursive = TRUE, showWarnings = FALSE)

# Countries with LSMS data available
sixteen_countries <- c(
  'Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau',
  'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda', 'Senegal',
  'Tanzania', 'Togo', 'Uganda', 'Zambia'
)
sixteen_country_codes <- c(
  'BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB',
  'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN',
  'TZA', 'TGO', 'UGA', 'ZMB'
)

# ------------------------------------------------------------------------------
# 3. DEFINE SUB-SAHARAN AFRICA REGION
# ------------------------------------------------------------------------------
message("=== Defining SSA study region ===")

# Download world boundaries
country <- geodata::world(path = input_path, resolution = 5, level = 0)

# Get ISO country codes
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

# Remove small island nations (keep mainland + Madagascar only)
islands_to_remove <- c(
  'Cabo Verde', 'Comoros', 'Mauritius', 'Mayotte',
  'Réunion', 'Saint Helena', 'São Tomé and Príncipe', 'Seychelles'
)
isocodes_ssa <- subset(isocodes_ssa, !(NAME %in% islands_to_remove))

# Extract SSA polygon
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
message("SSA countries: ", nrow(ssa))

# ------------------------------------------------------------------------------
# 4. CLEAN PREVIOUS OUTPUT FILES
# ------------------------------------------------------------------------------
message("=== Cleaning outdated map files ===")

# List of map files that will be regenerated
map_names <- c(
  'cattle', 'cropland', 'gdp', 'lsms', 'maizeyield', 'market',
  'pop', 'poverty', 'rainfall', 'sand0_30', 'slope'
)

# Remove old country-specific maps
for (cty in sixteen_countries) {
  for (map in map_names) {
    old_file <- file.path(output_maps, paste0(cty, '-', map, '.png'))
    if (file.exists(old_file)) file.remove(old_file)
  }
}

# ------------------------------------------------------------------------------
# 5. DOWNLOAD GADM BOUNDARIES
# ------------------------------------------------------------------------------
message("\n=== Downloading GADM boundaries ===")

#' Download GADM boundaries for a country at all available levels
#' @param cty Country name
#' @return NULL (side effect: downloads GADM files)
download_gadm <- function(cty) {
  cty_dir <- file.path(input_path, 'gadm', cty)
  if (!dir.exists(cty_dir)) dir.create(cty_dir, recursive = TRUE)
  
  # Try downloading levels 1-5
  for (lvl in 1:5) {
    tryCatch({
      message("  ", cty, ": level ", lvl)
      geodata::gadm(country = cty, level = lvl, version = 'latest', path = cty_dir)
    }, error = function(e) {
      # Level not available for this country - expected for many countries
    })
  }
}

# Download GADM for all SSA countries
sapply(unique(ssa$NAME_0), download_gadm)
message("GADM download complete")
message("NOTE: Manually rename folders for Côte d'Ivoire and Guinea-Bissau if needed")

# ------------------------------------------------------------------------------
# 6. PROCESS CROPLAND LAYERS
# ------------------------------------------------------------------------------
message("\n=== Processing cropland layers ===")

# --- 6.1 SPAM Cropland (2010, 2017, 2020) ---
# SPAM provides harvested area per crop; we sum all crops for total cropland

# Helper: Get all SPAM crop area files
get_spam_crops <- function(spam_dir) {
  # Pattern matches: *_H_XXXX_A.tif (harvested area, all tech levels)
  files <- dir(spam_dir, pattern = '_H_[A-Z]+_A\\.tif$', full.names = TRUE)
  if (length(files) == 0) return(NULL)
  terra::rast(files)
}

# SPAM 2020 (manual download required)
spam2020_dir <- file.path(input_path, 'spam', 'spam2020')
if (dir.exists(spam2020_dir) && length(dir(spam2020_dir)) > 0) {
  message("Processing SPAM 2020...")
  each_2020_crop <- get_spam_crops(spam2020_dir)
  if (!is.null(each_2020_crop)) {
    each_2020_crop <- terra::crop(each_2020_crop, ssa, mask = TRUE)
    names(each_2020_crop) <- substr(names(each_2020_crop), 24, 27)
    all_2020_crops <- sum(each_2020_crop, na.rm = TRUE)
    all_2020_crops <- terra::crop(all_2020_crops, ssa, mask = TRUE)
    terra::writeRaster(
      all_2020_crops,
      file.path(input_path, 'spam', 'spam2020_cropland_ssa.tif'),
      overwrite = TRUE
    )
  }
} else {
  message("SPAM 2020 not found - skipping (requires manual download)")
  all_2020_crops <- NULL
}

# SPAM 2017 (via geodata for Africa-specific version)
message("Processing SPAM 2017...")
spam2017_dir <- file.path(input_path, 'spam', 'spam2017')
if (!dir.exists(spam2017_dir)) dir.create(spam2017_dir, recursive = TRUE)

# Download if needed
each_2017_crop <- get_spam_crops(spam2017_dir)
if (is.null(each_2017_crop)) {
  message("  Downloading SPAM 2017 via geodata...")
  all_spam_crops <- geodata::spamCrops()$code
  for (crop in all_spam_crops) {
    tryCatch({
      geodata::crop_spam(crop, 'harv_area', path = spam2017_dir, africa = TRUE)
    }, error = function(e) NULL)
  }
  each_2017_crop <- get_spam_crops(spam2017_dir)
}

if (!is.null(each_2017_crop)) {
  each_2017_crop <- terra::crop(each_2017_crop, ssa, mask = TRUE)
  names(each_2017_crop) <- substr(names(each_2017_crop), 20, 23)
  all_2017_crops <- sum(each_2017_crop, na.rm = TRUE)
}

# SPAM 2010 (for Sudan which isn't in 2017 Africa version)
message("Processing SPAM 2010 (for Sudan)...")
spam2010_dir <- file.path(input_path, 'spam', 'spam2010')
if (!dir.exists(spam2010_dir)) dir.create(spam2010_dir, recursive = TRUE)

each_2010_crop <- get_spam_crops(spam2010_dir)
if (is.null(each_2010_crop)) {
  message("  Downloading SPAM 2010 via geodata...")
  all_spam_crops <- geodata::spamCrops()$code
  for (crop in all_spam_crops) {
    tryCatch({
      geodata::crop_spam(crop, 'harv_area', path = spam2010_dir, africa = FALSE)
    }, error = function(e) NULL)
  }
  each_2010_crop <- get_spam_crops(spam2010_dir)
}

if (!is.null(each_2010_crop)) {
  names(each_2010_crop) <- substr(names(each_2010_crop), 23, 26)
  all_2010_crops <- sum(each_2010_crop, na.rm = TRUE)
  all_2010_crops <- terra::crop(all_2010_crops, ssa, mask = TRUE)
  
  # Merge Sudan from 2010 into 2017
  sudan_mask <- terra::crop(
    each_2010_crop,
    subset(ssa, ssa$NAME_0 == 'Sudan'),
    mask = TRUE
  )
  sudan_total <- sum(sudan_mask, na.rm = TRUE)
  all_2017_crops <- terra::merge(all_2017_crops, sudan_total)
}

# Save SPAM products
terra::writeRaster(
  all_2017_crops,
  file.path(input_path, 'spam', 'spam2017_cropland_ssa.tif'),
  overwrite = TRUE
)
terra::writeRaster(
  all_2010_crops,
  file.path(input_path, 'spam', 'spam2010_cropland_ssa.tif'),
  overwrite = TRUE
)

# --- 6.2 Create reference grid from SSA ---
message("Creating SSA reference grid...")
ssa_grid <- terra::rast(ssa, nrow = 2000, ncol = 2000)
ssa_rast <- terra::rasterize(ssa, ssa_grid, field = 'NAME_0')
ssa_rast <- terra::resample(ssa_rast, all_2010_crops)

# --- 6.3 Other cropland products (ESA, GLAD, GeoSurvey) ---
message("Processing ESA WorldCover cropland...")
esa_cropland <- geodata::cropland(source = 'WorldCover', path = input_path)
esa_cropland <- terra::crop(esa_cropland, ssa_rast, mask = TRUE)
esa_cropland <- esa_cropland * terra::cellSize(esa_cropland, unit = 'ha')

message("Processing GeoSurvey cropland...")
geosurvey_cropland <- geodata::cropland(source = 'QED', path = input_path)
geosurvey_cropland <- terra::crop(geosurvey_cropland, ssa_rast, mask = TRUE)
geosurvey_cropland <- geosurvey_cropland * terra::cellSize(geosurvey_cropland, unit = 'ha')

message("Processing GLAD cropland...")
potapov_cropland <- geodata::cropland(source = 'GLAD', year = 2019, path = input_path)
potapov_cropland <- terra::crop(potapov_cropland, ssa_rast, mask = TRUE)
potapov_cropland <- potapov_cropland * terra::cellSize(potapov_cropland, unit = 'ha')

# --- 6.4 Aggregate and compare cropland products ---
message("Aggregating cropland products...")
geosurvey_cropland <- terra::aggregate(geosurvey_cropland, 10, fun = 'sum', na.rm = TRUE)
esa_cropland <- terra::aggregate(esa_cropland, 10, fun = 'sum', na.rm = TRUE)
potapov_cropland <- terra::aggregate(potapov_cropland, 10, fun = 'sum', na.rm = TRUE)

# Align extents
terra::ext(geosurvey_cropland) <- terra::ext(esa_cropland) <- 
  terra::ext(potapov_cropland) <- terra::ext(all_2017_crops) <- 
  floor(terra::ext(esa_cropland))

# Stack all cropland products for comparison
all_cropland_mask <- c(
  terra::resample(all_2010_crops, all_2017_crops),
  all_2017_crops,
  terra::resample(if(exists("all_2020_crops")) all_2020_crops else all_2017_crops, all_2017_crops),
  terra::resample(esa_cropland, all_2017_crops),
  terra::resample(potapov_cropland, all_2017_crops),
  terra::resample(geosurvey_cropland, all_2017_crops)
)
names(all_cropland_mask) <- c('SPAM 2010', 'SPAM 2017', 'SPAM 2020',
                               'ESA 2020', 'GLAD 2019', 'GEOSURVEY 2015')

# Save cropland comparison
terra::writeRaster(
  all_cropland_mask,
  file.path(input_path, 'landuse', 'all_cropland_mask.tif'),
  overwrite = TRUE
)

# Save comparison map
png(file.path(output_maps, "africa_all_croplands.png"),
    units = "in", width = 8, height = 6, res = 300)
terra::plot(all_cropland_mask, range = c(0, 5000), fill_range = TRUE)
dev.off()

# --- 6.5 Select primary cropland layer ---
# SPAM 2017 chosen as default (middle year, Africa-specific)
cropland_ha <- all_2017_crops
names(cropland_ha) <- 'cropland'
terra::writeRaster(
  cropland_ha,
  file.path(input_path, 'spam', 'cropland_ssa.tif'),
  overwrite = TRUE
)
message("Primary cropland layer: SPAM 2017")

# Save cropland map
png(file.path(output_maps, "africa-cropland.png"),
    units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
terra::plot(ssa, col = 'azure', main = 'Cropland (ha)',
            panel.first = grid(col = "gray", lty = "solid"),
            pax = list(cex.axis = 1.4))
terra::plot(cropland_ha, range = c(0, 5000), fill_range = TRUE,
            cex = 1.2, axes = FALSE, add = TRUE)
terra::plot(ssa, axes = FALSE, add = TRUE)
dev.off()

# ------------------------------------------------------------------------------
# 7. PROCESS CATTLE DENSITY
# ------------------------------------------------------------------------------
message("\n=== Processing cattle density ===")

cattle_dir <- file.path(input_path, 'cattle-density')
if (!dir.exists(cattle_dir)) dir.create(cattle_dir, recursive = TRUE)

cattle_file <- file.path(cattle_dir, '5_Ct_2010_DA.tif')
if (file.exists(cattle_file)) {
  cattle <- terra::rast(cattle_file)
  cattle <- terra::crop(cattle, ssa, mask = TRUE)
  cattle <- terra::resample(cattle, cropland_ha)
  names(cattle) <- 'cattle'
  terra::writeRaster(
    cattle,
    file.path(cattle_dir, '2010_cattle_density_ssa.tif'),
    overwrite = TRUE
  )
  
  # Save map
  png(file.path(output_maps, "africa-cattle-density.png"),
      units = "in", width = 5.5, height = 5.5, res = 1000)
  par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
  terra::plot(ssa, col = 'azure', main = 'Cattle density (# /km2)',
              panel.first = grid(col = "gray", lty = "solid"))
  terra::plot(cattle, range = c(0, 2000), fill_range = TRUE,
              axes = FALSE, add = TRUE)
  terra::plot(ssa, axes = FALSE, add = TRUE)
  dev.off()
} else {
  message("WARNING: Cattle density file not found - requires manual download")
  message("  Download from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GIVQ75")
  cattle <- NULL
}

# ------------------------------------------------------------------------------
# 8. PROCESS POPULATION DENSITY
# ------------------------------------------------------------------------------
message("\n=== Processing population density ===")

pop_dir <- file.path(input_path, 'population')
if (!dir.exists(pop_dir)) dir.create(pop_dir, recursive = TRUE)

# Download population data via geodata
message("Downloading population density (GPW 2020)...")
pop <- geodata::population(2020, 0.5, path = input_path)

# Process
pop <- terra::crop(pop, ssa_rast, mask = TRUE)
pop <- terra::resample(pop, cropland_ha)
names(pop) <- 'pop'
terra::writeRaster(
  pop,
  file.path(pop_dir, '2020_population_density_ssa.tif'),
  overwrite = TRUE
)

# Save map
png(file.path(output_maps, "africa-population-density.png"),
    units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
terra::plot(ssa, col = 'azure', main = 'Population density (inhabitants/km2)',
            panel.first = grid(col = "gray", lty = "solid"))
terra::plot(pop, range = c(0, 500), fill_range = TRUE,
            axes = FALSE, add = TRUE)
terra::plot(ssa, axes = FALSE, add = TRUE)
dev.off()

# ------------------------------------------------------------------------------
# 9. CALCULATE CROPLAND PER CAPITA
# ------------------------------------------------------------------------------
message("\n=== Calculating cropland per capita ===")

cropland_per_capita <- cropland_ha / pop
cropland_per_capita[is.infinite(cropland_per_capita)] <- NA
names(cropland_per_capita) <- 'cropland_per_capita'
terra::writeRaster(
  cropland_per_capita,
  file.path(input_path, 'spam', 'cropland_per_capita_ssa.tif'),
  overwrite = TRUE
)

# Save map
png(file.path(output_maps, "africa-cropland-per-capita.png"),
    units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
terra::plot(ssa, col = 'azure', main = 'Cropland per capita (ha/person)',
            panel.first = grid(col = "gray", lty = "solid"))
terra::plot(cropland_per_capita, range = c(0, 50), fill_range = TRUE,
            axes = FALSE, add = TRUE)
terra::plot(ssa, axes = FALSE, add = TRUE)
dev.off()

# ------------------------------------------------------------------------------
# 10. PROCESS SOIL PROPERTIES (SAND CONTENT)
# ------------------------------------------------------------------------------
message("\n=== Processing soil sand content ===")

soil_dir <- file.path(input_path, 'soil_world')
if (!dir.exists(soil_dir)) dir.create(soil_dir, recursive = TRUE)

# Download sand content at three depths
message("Downloading soil sand content (SoilGrids)...")
sand05 <- geodata::soil_world('sand', 5, path = input_path)
sand15 <- geodata::soil_world('sand', 15, path = input_path)
sand30 <- geodata::soil_world('sand', 30, path = input_path)

# Calculate weighted average for 0-30cm
# Weights: 5cm (0-5), 10cm (5-15), 15cm (15-30)
sand0_30 <- (5 * sand05 + 10 * sand15 + 15 * sand30) / 30

# Process
sand0_30 <- terra::crop(sand0_30, ssa_rast, mask = TRUE)
sand0_30 <- terra::resample(sand0_30, cropland_ha)
names(sand0_30) <- 'sand'
terra::writeRaster(
  sand0_30,
  file.path(soil_dir, 'sand_content_0_30cm_ssa.tif'),
  overwrite = TRUE
)

# Save map
png(file.path(output_maps, "africa-soil.png"),
    units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
terra::plot(ssa, col = 'azure', main = 'Texture (% sand)',
            panel.first = grid(col = "gray", lty = "solid"))
terra::plot(sand0_30, range = c(0, 80), fill_range = TRUE,
            axes = FALSE, add = TRUE)
terra::plot(ssa, axes = FALSE, add = TRUE)
dev.off()

# ------------------------------------------------------------------------------
# 11. PROCESS ELEVATION AND SLOPE
# ------------------------------------------------------------------------------
message("\n=== Processing elevation and slope ===")

elev_dir <- file.path(input_path, 'wc2.1_30s')
if (!dir.exists(elev_dir)) dir.create(elev_dir, recursive = TRUE)

# Download elevation
message("Downloading elevation (WorldClim)...")
elevation <- geodata::elevation_global(0.5, path = input_path)

# Process elevation
elevation <- terra::crop(elevation, ssa_rast, mask = TRUE)
elevation <- terra::resample(elevation, cropland_ha)
names(elevation) <- 'elevation'
terra::writeRaster(
  elevation,
  file.path(elev_dir, 'elevation_ssa.tif'),
  overwrite = TRUE
)

# Calculate slope (radians)
message("Calculating slope...")
slope <- terra::terrain(elevation, 'slope', unit = "radians", neighbors = 8)
slope <- terra::crop(slope, ssa, mask = TRUE)
slope <- terra::resample(slope, cropland_ha)
names(slope) <- 'slope'
terra::writeRaster(
  slope,
  file.path(elev_dir, 'terrain_slope_ssa.tif'),
  overwrite = TRUE
)

# Save maps
png(file.path(output_maps, "africa-elevation.png"),
    units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
terra::plot(ssa, col = 'azure', main = 'Elevation (m.a.s.l)',
            panel.first = grid(col = "gray", lty = "solid"))
terra::plot(elevation, range = c(0, 2500), fill_range = TRUE,
            axes = FALSE, add = TRUE)
terra::plot(ssa, axes = FALSE, add = TRUE)
dev.off()

png(file.path(output_maps, "africa_terrain-slope.png"),
    units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
terra::plot(ssa, col = 'azure', main = 'Terrain slope (%)',
            panel.first = grid(col = "gray", lty = "solid"))
terra::plot(100 * slope, range = c(0, 5), fill_range = TRUE,
            axes = FALSE, add = TRUE)
terra::plot(ssa, axes = FALSE, add = TRUE)
dev.off()

# ------------------------------------------------------------------------------
# 12. PROCESS TEMPERATURE
# ------------------------------------------------------------------------------
message("\n=== Processing temperature ===")

temp_dir <- file.path(input_path, 'temperature')
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)

# Download temperature by country and merge
message("Downloading temperature (WorldClim)...")
temp_list <- lapply(isocodes_ssa$ISO3, function(x) {
  tryCatch({
    geodata::worldclim_country(x, 'tavg', path = input_path)
  }, error = function(e) NULL)
})
temp_list <- temp_list[!sapply(temp_list, is.null)]

# Calculate annual mean and merge
message("Merging temperature data...")
temp0 <- terra::resample(terra::rast(), cropland_ha)
for (temp_cty in temp_list) {
  temp1 <- terra::resample(terra::mean(temp_cty, na.rm = TRUE), cropland_ha)
  temp0 <- terra::merge(temp0, temp1)
}
temperature <- terra::crop(temp0, ssa, mask = TRUE)
terra::ext(temperature) <- terra::ext(cropland_ha)
names(temperature) <- 'temperature'
terra::writeRaster(
  temperature,
  file.path(temp_dir, 'avg_temperature_ssa.tif'),
  overwrite = TRUE
)

# Save map
png(file.path(output_maps, "africa-temperature.png"),
    units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
terra::plot(ssa, col = 'azure', main = 'Average annual temperature (°C)',
            panel.first = grid(col = "gray", lty = "solid"))
terra::plot(temperature, range = c(0, 30), fill_range = TRUE,
            axes = FALSE, add = TRUE)
terra::plot(ssa, axes = FALSE, add = TRUE)
dev.off()

# ------------------------------------------------------------------------------
# 13. PROCESS RAINFALL (FROM CHIRPS)
# ------------------------------------------------------------------------------
message("\n=== Processing rainfall ===")

rainfall_file <- file.path(input_path, 'rainfall', 'rainfall_yearly',
                           '#_long_term_rainfall_avg.tif')
if (file.exists(rainfall_file)) {
  rainfall <- terra::rast(rainfall_file)
  rainfall <- terra::crop(rainfall, ssa, mask = TRUE)
  rainfall <- terra::resample(rainfall, cropland_ha)
  names(rainfall) <- 'rainfall'
  terra::writeRaster(
    rainfall,
    file.path(input_path, 'rainfall', 'rainfall_ssa.tif'),
    overwrite = TRUE
  )
  
  # Save map
  png(file.path(output_maps, "africa-rainfall.png"),
      units = "in", width = 5.5, height = 5.5, res = 1000)
  par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
  terra::plot(ssa, col = 'azure', main = 'Annual rainfall (mm)',
              panel.first = grid(col = "gray", lty = "solid"))
  terra::plot(rainfall, range = c(0, 2000), fill_range = TRUE,
              axes = FALSE, add = TRUE)
  terra::plot(ssa, axes = FALSE, add = TRUE)
  dev.off()
} else {
  message("WARNING: Rainfall file not found - run 01.1-01.3 scripts first")
  rainfall <- NULL
}

# ------------------------------------------------------------------------------
# 14. PROCESS TRAVEL TIME TO MARKETS
# ------------------------------------------------------------------------------
message("\n=== Processing travel time ===")

travel_dir <- file.path(input_path, 'travel')
if (!dir.exists(travel_dir)) dir.create(travel_dir, recursive = TRUE)

# Download travel time to cities with 50,000+ inhabitants
message("Downloading travel time to cities...")
market <- geodata::travel_time(to = 'city', size = 6, up = TRUE, path = input_path)

# Process
market <- terra::crop(market, ssa, mask = TRUE)
market <- terra::resample(market, cropland_ha)
names(market) <- 'market'
terra::writeRaster(
  market,
  file.path(travel_dir, 'travel_time_to_cities_ssa.tif'),
  overwrite = TRUE
)

# Save map
png(file.path(output_maps, "africa-market.png"),
    units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
terra::plot(ssa, col = 'azure', main = 'Travel time to nearest city (min)',
            panel.first = grid(col = "gray", lty = "solid"))
terra::plot(market, range = c(0, 300), fill_range = TRUE,
            axes = FALSE, add = TRUE)
terra::plot(ssa, axes = FALSE, add = TRUE)
dev.off()

# ------------------------------------------------------------------------------
# 15. PROCESS MAIZE YIELD POTENTIAL
# ------------------------------------------------------------------------------
message("\n=== Processing maize yield potential ===")

maize_dir <- file.path(input_path, 'maize_water_lim_yield')
if (!dir.exists(maize_dir)) dir.create(maize_dir, recursive = TRUE)

maize_file <- file.path(maize_dir, 'watlimsummary.tif')
if (file.exists(maize_file)) {
  maizeyield <- terra::rast(maize_file)
  maizeyield <- maizeyield[[2]]  # Extract median layer
  maizeyield <- terra::crop(maizeyield, ssa, mask = TRUE)
  maizeyield <- terra::resample(maizeyield, cropland_ha)
  names(maizeyield) <- 'maizeyield'
  terra::writeRaster(
    maizeyield,
    file.path(maize_dir, 'maize_yield_ssa.tif'),
    overwrite = TRUE
  )
  
  # Save map
  png(file.path(output_maps, "africa-maizeyield.png"),
      units = "in", width = 5.5, height = 5.5, res = 1000)
  par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
  terra::plot(ssa, col = 'azure', main = 'Water-limited maize yield (kg/ha)',
              panel.first = grid(col = "gray", lty = "solid"))
  terra::plot(maizeyield, range = c(0, 15000), fill_range = TRUE,
              axes = FALSE, add = TRUE)
  terra::plot(ssa, axes = FALSE, add = TRUE)
  dev.off()
} else {
  message("WARNING: Maize yield file not found - requires manual download")
  maizeyield <- NULL
}

# ------------------------------------------------------------------------------
# 16. PROCESS GDP PER CAPITA
# ------------------------------------------------------------------------------
message("\n=== Processing GDP per capita ===")

gdp_dir <- file.path(input_path, 'FAO-GDP')
if (!dir.exists(gdp_dir)) dir.create(gdp_dir, recursive = TRUE)

# Look for FAOSTAT GDP file
gdp_files <- list.files(
  '../data/raw/web_scrapped/faostat',
  pattern = 'FAOSTAT.*GDP.*\\.csv$',
  full.names = TRUE
)

if (length(gdp_files) > 0) {
  gdp_csv <- read_csv(gdp_files[1], show_col_types = FALSE)
  
  gdp_ssa <- gdp_csv |>
    select(Area, Year, Item, Value) |>
    pivot_wider(id_cols = c(Area, Year), names_from = Item, values_from = Value) |>
    rename(NAME_0 = Area, year = Year, gdp = 'Gross Domestic Product') |>
    filter(NAME_0 %in% isocodes_ssa$NAME) |>
    group_by(NAME_0) |>
    summarize(gdp = mean(gdp, na.rm = TRUE)) |>
    inner_join(terra::as.data.frame(ssa), by = "NAME_0")
  
  # Rasterize GDP
  gdp_vect <- ssa
  gdp_vect$gdp <- gdp_ssa$gdp[match(gdp_vect$NAME_0, gdp_ssa$NAME_0)]
  gdp_grid <- terra::rast(gdp_vect, nrow = 1000, ncol = 1000)
  gdp <- terra::rasterize(gdp_vect, gdp_grid, field = 'gdp')
  gdp <- terra::resample(gdp, cropland_ha)
  names(gdp) <- 'gdp'
  terra::writeRaster(
    gdp,
    file.path(gdp_dir, 'gdp_ssa.tif'),
    overwrite = TRUE
  )
  
  # Save map
  png(file.path(output_maps, "africa-gdp.png"),
      units = "in", width = 5.5, height = 5.5, res = 1000)
  par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)
  terra::plot(ssa, col = 'azure', main = 'Gross Domestic Product per country, $',
              panel.first = grid(col = "gray", lty = "solid"))
  terra::plot(gdp, range = c(0, 5000), fill_range = TRUE,
              axes = FALSE, add = TRUE)
  dev.off()
} else {
  message("WARNING: GDP file not found - download from FAOSTAT")
  gdp <- NULL
}

# ------------------------------------------------------------------------------
# 17. PROCESS POVERTY/WEALTH INDEX
# ------------------------------------------------------------------------------
message("\n=== Processing poverty/wealth index ===")

poverty_dir <- file.path(input_path, 'poverty')
if (!dir.exists(poverty_dir)) dir.create(poverty_dir, recursive = TRUE)

poverty_zip <- list.files(poverty_dir, pattern = 'poverty\\.zip$', full.names = TRUE)
if (length(poverty_zip) > 0) {
  # Create temporary directory for extraction
  temp_dir <- '../data/processed/temporary'
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir)
  
  # Unzip and process
  poverty_files <- unzip(poverty_zip[1], exdir = temp_dir)
  wealth_zips <- poverty_files[grep('estimated_wealth_index\\.shp\\.zip$', poverty_files)]
  
  # Process each country's wealth index
  wealth_rasts <- lapply(wealth_zips, function(z) {
    unzip(z, exdir = temp_dir)
  })
  
  wealth_shps <- unlist(wealth_rasts)[grep('_estimated_wealth_index\\.shp$', unlist(wealth_rasts))]
  
  wealth <- terra::rast()
  for (shp in wealth_shps) {
    tryCatch({
      v <- terra::vect(shp)
      empty_rast <- terra::rast(v, res = 0.01)
      prob_poor <- terra::rasterize(v, empty_rast, field = 'img_prob_p')
      prob_poor <- terra::resample(prob_poor, cropland_ha)
      wealth <- terra::merge(wealth, prob_poor)
    }, error = function(e) NULL)
  }
  
  names(wealth) <- 'prob_poor'
  terra::writeRaster(
    wealth,
    file.path(poverty_dir, 'wealth_ssa.tif'),
    overwrite = TRUE
  )
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
} else {
  message("WARNING: Poverty data not found - requires manual download")
  wealth <- NULL
}

# ------------------------------------------------------------------------------
# 18. STACK ALL PREDICTOR LAYERS
# ------------------------------------------------------------------------------
message("\n=== Stacking predictor layers ===")

# Collect all available layers
layers_to_stack <- list(
  cropland_ha = cropland_ha,
  cattle = if(exists("cattle") && !is.null(cattle)) cattle else NULL,
  pop = pop,
  cropland_per_capita = cropland_per_capita,
  sand = sand0_30,
  elevation = elevation,
  slope = slope,
  temperature = temperature,
  rainfall = if(exists("rainfall") && !is.null(rainfall)) rainfall else NULL,
  market = market,
  maizeyield = if(exists("maizeyield") && !is.null(maizeyield)) maizeyield else NULL,
  gdp = if(exists("gdp") && !is.null(gdp)) gdp else NULL,
  wealth = if(exists("wealth") && !is.null(wealth)) wealth else NULL
)

# Remove NULL layers
layers_to_stack <- layers_to_stack[!sapply(layers_to_stack, is.null)]
message("Layers available: ", paste(names(layers_to_stack), collapse = ", "))

# Stack layers
stacked_00 <- terra::rast(layers_to_stack)
terra::writeRaster(
  stacked_00,
  file.path(output_processed, 'all_predictors.tif'),
  overwrite = TRUE
)
message("Saved: ", file.path(output_processed, 'all_predictors.tif'))

# ------------------------------------------------------------------------------
# 19. CREATE MASKS FOR POST-PROCESSING
# ------------------------------------------------------------------------------
message("\n=== Creating masks ===")

# Forest mask (from afrilearndata)
require(afrilearndata)
land_cover <- terra::rast(system.file('extdata', 'afrilandcover.grd',
                                       package = 'afrilearndata', mustWork = TRUE))
# Forest classes: 2, 4, 5, 8
forests <- land_cover == 2 | land_cover == 4 | land_cover == 5 | land_cover == 8
forests <- terra::ifel(forests, NA, 1)
mask_forest_ssa <- terra::crop(forests, ssa)

# Dryland mask (rainfall < 200 mm/year)
if (!is.null(rainfall)) {
  drylands <- terra::ifel(rainfall < 200, NA, 1)
}

# ------------------------------------------------------------------------------
# 20. SUMMARY
# ------------------------------------------------------------------------------
message("\n=== Processing Complete ===")
message("Predictor stack layers: ", terra::nlyr(stacked_00))
message("Resolution: ", terra::res(stacked_00)[1], " degrees")
message("Extent: ", paste(round(terra::ext(stacked_00)[], 2), collapse = ", "))
message("CRS: ", terra::crs(stacked_00, describe = TRUE)$name)

message("\nOutput files:")
message("  - ", file.path(output_processed, 'all_predictors.tif'))
message("  - Maps saved to: ", output_maps)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
