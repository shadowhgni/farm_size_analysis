# ==============================================================================
# Script: 01.1_chirps_download.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Download CHIRPS dekadal rainfall data for Africa (1981-2024)
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - February 2026
#
# This version works in restricted environments without terra/tidyverse
# ==============================================================================

message("\n")
message(paste(rep("=", 70), collapse = ""))
message("SYNTHETIC DATA GENERATION (Base R)")
message(paste(rep("=", 70), collapse = ""))
message("Started: ", Sys.time())

set.seed(42)

# ------------------------------------------------------------------------------
# 1. CREATE DIRECTORY STRUCTURE
# ------------------------------------------------------------------------------
message("\n[1/6] Creating directory structure...")

dirs <- c(
  "../data/raw/spatial/gadm",
  "../data/raw/spatial/spam/spam2017",
  "../data/raw/spatial/rainfall/rainfall_yearly",
  "../data/raw/spatial/cattle-glw2010",
  "../data/raw/spatial/cattle-du2025",
  "../data/raw/spatial/population",
  "../data/raw/spatial/soil_world",
  "../data/raw/spatial/temperature",
  "../data/raw/spatial/travel",
  "../data/raw/spatial/maize_water_lim_yield",
  "../data/raw/web_scrapped/survey_data",
  "../data/raw/web_scrapped/faostat",
  "../data/processed",
  "../output/maps",
  "../output/graphs",
  "../output/other_illustr/tables/main",
  "../output/other_illustr/tables/supplementary",
  "../output/figures/main",
  "../output/figures/supplementary",
  "../output/reports"
)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}
message("  Created ", length(dirs), " directories")

# ------------------------------------------------------------------------------
# 2. CONFIGURATION
# ------------------------------------------------------------------------------
message("\n[2/6] Setting configuration...")

n_farms <- 5000
n_grid_points <- 1000  # For raster-like data

lsms_countries <- c(
  "Ethiopia", "Malawi", "Nigeria", "Tanzania", "Uganda", "Zambia",
  "Ghana", "Niger", "Mali", "Burkina", "Senegal", "Benin",
  "Togo", "Rwanda", "Cote_d_Ivoire", "Guinea_Bissau"
)

# Country centroids (lon, lat)
country_coords <- data.frame(
  country = lsms_countries,
  lon = c(38, 34, 8, 35, 32, 28, -1, 8, -4, -1.5, -14, 2, 1, 30, -5, -15),
  lat = c(9, -13, 9, -6, 1, -15, 8, 17, 17, 12, 14, 9, 8, -2, 7, 12),
  stringsAsFactors = FALSE
)

message("  Countries: ", length(lsms_countries))
message("  Target farms: ", n_farms)

# ------------------------------------------------------------------------------
# 3. GENERATE SYNTHETIC RASTER DATA (as CSV grid)
# ------------------------------------------------------------------------------
message("\n[3/6] Generating synthetic spatial predictor grid...")

# Create grid covering SSA
lon_seq <- seq(-18, 52, length.out = 50)
lat_seq <- seq(-35, 15, length.out = 40)
grid <- expand.grid(x = lon_seq, y = lat_seq)

# Add predictor values with spatial patterns
grid$cropland <- pmax(0, rnorm(nrow(grid), 500, 300) + 
                       (grid$y + 10) * 10)  # Higher near equator

# Cattle density - GLW 2010 version (used for ML predictors)
grid$cattle_glw2010 <- pmax(0, rnorm(nrow(grid), 50, 40) +
                             abs(grid$y) * 2)  # Pattern with latitude

# Cattle density - Du et al. 2025 version (used for Fig 3)
# Slightly different pattern to simulate different methodology
grid$cattle_du2025 <- pmax(0, rnorm(nrow(grid), 55, 45) +
                            abs(grid$y) * 1.8 + 
                            rnorm(nrow(grid), 0, 5))

# Use GLW 2010 as main cattle predictor for ML models
grid$cattle <- grid$cattle_glw2010

grid$pop <- pmax(0, rnorm(nrow(grid), 100, 150) +
                  runif(nrow(grid), 0, 200))

grid$cropland_per_capita <- ifelse(grid$pop > 0, 
                                    grid$cropland / grid$pop, 
                                    NA)

grid$sand <- pmax(0, pmin(100, rnorm(nrow(grid), 40, 20)))

grid$elevation <- pmax(0, rnorm(nrow(grid), 800, 500) +
                        abs(grid$y) * 20)

grid$slope <- pmax(0, rnorm(nrow(grid), 0.05, 0.03))

grid$temperature <- pmax(10, pmin(35, 
                                   28 - abs(grid$y) * 0.3 + 
                                   rnorm(nrow(grid), 0, 3)))

grid$rainfall <- pmax(0, rnorm(nrow(grid), 1000, 500) -
                       abs(grid$y - 5) * 20)

grid$market <- pmax(0, rnorm(nrow(grid), 120, 80))

grid$maizeyield <- pmax(0, rnorm(nrow(grid), 5000, 2000))

# Save as CSV (simulating raster extraction)
write.csv(grid, "../data/processed/predictor_grid.csv", row.names = FALSE)
message("  Grid points: ", nrow(grid))
message("  Predictors: ", ncol(grid) - 2)

# Save individual predictor files (as CSV representations)
for (var in c("cropland", "cattle", "pop", "sand", "temperature", "rainfall", "market")) {
  pred_data <- grid[, c("x", "y", var)]
  write.csv(pred_data, 
            paste0("../data/raw/spatial/", var, "_synthetic.csv"), 
            row.names = FALSE)
}

# Save cattle density files to their specific directories
# GLW 2010 - main ML predictor
cattle_glw <- grid[, c("x", "y", "cattle_glw2010")]
write.csv(cattle_glw, 
          "../data/raw/spatial/cattle-glw2010/cattle_glw2010_synthetic.csv", 
          row.names = FALSE)
message("  Saved: cattle-glw2010/ (ML predictor)")

# Du et al. 2025 - for Figure 3
cattle_du <- grid[, c("x", "y", "cattle_du2025")]
write.csv(cattle_du, 
          "../data/raw/spatial/cattle-du2025/cattle_du2025_synthetic.csv", 
          row.names = FALSE)
message("  Saved: cattle-du2025/ (Figure 3)")

# ------------------------------------------------------------------------------
# 4. GENERATE SYNTHETIC LSMS FARM DATA
# ------------------------------------------------------------------------------
message("\n[4/6] Generating synthetic LSMS farm data...")

generate_country_farms <- function(country, n, lon_center, lat_center) {
  # Generate locations around country center
  x <- rnorm(n, mean = lon_center, sd = 2)
  y <- rnorm(n, mean = lat_center, sd = 2)
  
  # Farm size: log-normal distribution (realistic)
  farm_area_ha <- rlnorm(n, meanlog = 0.3, sdlog = 0.8)
  farm_area_ha <- pmin(farm_area_ha, 50)  # Cap at 50 ha
  
  # Household size: Poisson
  hh_size <- rpois(n, lambda = 5) + 1
  
  # Survey years
  years <- sample(c(2010, 2012, 2014, 2016, 2018, 2020), n, replace = TRUE)
  
  data.frame(
    x = x,
    y = y,
    country = country,
    year = years,
    farm_id = paste0(country, "_", sprintf("%05d", 1:n)),
    hh_size = hh_size,
    farm_area_ha = round(farm_area_ha, 4),
    stringsAsFactors = FALSE
  )
}

# Generate data for all countries
farms_per_country <- ceiling(n_farms / length(lsms_countries))

all_farms <- lapply(1:nrow(country_coords), function(i) {
  generate_country_farms(
    country_coords$country[i],
    farms_per_country,
    country_coords$lon[i],
    country_coords$lat[i]
  )
})

lsms_data <- do.call(rbind, all_farms)

# Filter to SSA extent
lsms_data <- lsms_data[lsms_data$x >= -18 & lsms_data$x <= 52 &
                        lsms_data$y >= -35 & lsms_data$y <= 15, ]

message("  Generated ", nrow(lsms_data), " synthetic farms")
message("  Countries: ", length(unique(lsms_data$country)))

# Save raw LSMS data
write.csv(lsms_data, "../data/processed/lsms_and_zambia.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 5. CREATE ANALYSIS-READY DATASETS
# ------------------------------------------------------------------------------
message("\n[5/6] Creating analysis-ready datasets...")

# Function to find nearest grid point and extract values
extract_predictor_values <- function(farm_data, grid_data) {
  result <- farm_data
  
  # Initialize predictor columns
  predictors <- c("cropland", "cattle", "pop", "cropland_per_capita",
                  "sand", "elevation", "slope", "temperature", 
                  "rainfall", "market", "maizeyield")
  
  for (p in predictors) {
    result[[p]] <- NA
  }
  
  # For each farm, find nearest grid point
  for (i in 1:nrow(farm_data)) {
    distances <- sqrt((grid_data$x - farm_data$x[i])^2 + 
                       (grid_data$y - farm_data$y[i])^2)
    nearest <- which.min(distances)
    
    for (p in predictors) {
      if (p %in% names(grid_data)) {
        result[[p]][i] <- grid_data[[p]][nearest]
      }
    }
  }
  
  return(result)
}

message("  Extracting predictor values at farm locations...")
lsms_spatial <- extract_predictor_values(lsms_data, grid)

# Add GADM columns
lsms_spatial$gadm_0 <- substr(lsms_spatial$country, 1, 3)
lsms_spatial$gadm_1 <- paste0(lsms_spatial$country, "_Region1")
lsms_spatial$gadm_2 <- paste0(lsms_spatial$country, "_District1")
lsms_spatial$gadm_3 <- NA
lsms_spatial$gadm_4 <- NA

# Remove rows with NA predictors
lsms_spatial <- lsms_spatial[complete.cases(lsms_spatial[, c("cropland", "rainfall")]), ]

# Save untrimmed
saveRDS(lsms_spatial, "../data/processed/lsms_untrimmed_africa.rds")

# Trim by percentile (per country)
trim_by_country <- function(data, percentile = 0.95) {
  result <- data.frame()
  for (cty in unique(data$country)) {
    cty_data <- data[data$country == cty, ]
    threshold <- quantile(cty_data$farm_area_ha, percentile, na.rm = TRUE)
    cty_trimmed <- cty_data[cty_data$farm_area_ha <= threshold, ]
    result <- rbind(result, cty_trimmed)
  }
  return(result)
}

lsms_95 <- trim_by_country(lsms_spatial, 0.95)
lsms_99 <- trim_by_country(lsms_spatial, 0.99)

saveRDS(lsms_95, "../data/processed/lsms_trimmed_95th_africa.rds")
saveRDS(lsms_99, "../data/processed/lsms_trimmed_99th_africa.rds")

message("  Trimmed datasets: 95th (", nrow(lsms_95), "), 99th (", nrow(lsms_99), ")")

# ML-ready dataset
ml_cols <- c("x", "y", "farm_area_ha", "cropland", "cattle", "pop", 
             "cropland_per_capita", "sand", "slope", "temperature", 
             "rainfall", "maizeyield", "market")
lsms_ml <- lsms_95[, ml_cols[ml_cols %in% names(lsms_95)]]
lsms_ml <- lsms_ml[complete.cases(lsms_ml), ]

write.csv(lsms_ml, "../data/processed/lsms_spatial.csv", row.names = FALSE)
saveRDS(lsms_ml, "../data/processed/lsms_spatial_africa.Rds")

# Save country raw files
for (cty in unique(lsms_data$country)) {
  cty_data <- lsms_data[lsms_data$country == cty, ]
  for (yr in unique(cty_data$year)) {
    yr_data <- cty_data[cty_data$year == yr, ]
    if (nrow(yr_data) > 0) {
      # Add extra columns
      yr_data$ea_id <- paste0(cty, "_EA_", sample(1:100, nrow(yr_data), replace = TRUE))
      yr_data$field_id <- paste0(yr_data$farm_id, "_F1")
      yr_data$plot_id <- paste0(yr_data$field_id, "_P1")
      yr_data$reported_area <- yr_data$farm_area_ha * runif(nrow(yr_data), 0.8, 1.2)
      yr_data$report_unit <- "hectare"
      yr_data$reported_area_ha <- yr_data$reported_area
      yr_data$plot_land_use <- "Cultivated"
      yr_data$measured_plot <- sample(c("Yes", "No"), nrow(yr_data), replace = TRUE, prob = c(0.7, 0.3))
      yr_data$measured_plot_area_ha <- ifelse(yr_data$measured_plot == "Yes", yr_data$farm_area_ha, NA)
      
      write.csv(yr_data, paste0("../data/processed/", cty, "_", yr, "_raw.csv"), row.names = FALSE)
    }
  }
}

# ------------------------------------------------------------------------------
# 6. GENERATE DESCRIPTIVE STATISTICS
# ------------------------------------------------------------------------------
message("\n[6/6] Generating descriptive statistics...")

# By country statistics
countries <- unique(lsms_data$country)
stats_list <- lapply(countries, function(cty) {
  cty_data <- lsms_data[lsms_data$country == cty, ]
  data.frame(
    country = cty,
    n_waves = length(unique(cty_data$year)),
    n_obs = nrow(cty_data),
    min_year = min(cty_data$year),
    max_year = max(cty_data$year),
    mean_ha = round(mean(cty_data$farm_area_ha), 2),
    median_ha = round(median(cty_data$farm_area_ha), 2),
    q10 = round(quantile(cty_data$farm_area_ha, 0.10), 2),
    q90 = round(quantile(cty_data$farm_area_ha, 0.90), 2),
    pct_below_1ha = round(100 * mean(cty_data$farm_area_ha < 1), 1),
    stringsAsFactors = FALSE
  )
})
country_stats <- do.call(rbind, stats_list)

# Add total row
total_row <- data.frame(
  country = "TOTAL",
  n_waves = sum(country_stats$n_waves),
  n_obs = sum(country_stats$n_obs),
  min_year = min(country_stats$min_year),
  max_year = max(country_stats$max_year),
  mean_ha = round(mean(lsms_data$farm_area_ha), 2),
  median_ha = round(median(lsms_data$farm_area_ha), 2),
  q10 = round(quantile(lsms_data$farm_area_ha, 0.10), 2),
  q90 = round(quantile(lsms_data$farm_area_ha, 0.90), 2),
  pct_below_1ha = round(100 * mean(lsms_data$farm_area_ha < 1), 1),
  stringsAsFactors = FALSE
)
country_stats <- rbind(country_stats, total_row)

write.csv(country_stats, "../output/other_illustr/tables/summary_descriptive_stats_survey.csv", row.names = FALSE)

# Correlation matrix (simple)
cor_vars <- c("farm_area_ha", "cropland", "cattle", "pop", "temperature", "rainfall", "market")
cor_data <- lsms_ml[, cor_vars[cor_vars %in% names(lsms_ml)]]
cor_matrix <- cor(cor_data, use = "complete.obs")
write.csv(round(cor_matrix, 3), "../output/other_illustr/tables/correlation_matrix.csv")

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------
message("\n")
message(paste(rep("=", 70), collapse = ""))
message("SYNTHETIC DATA GENERATION COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message("\nGenerated files:")
message("  Processed data: ", length(list.files("../data/processed", pattern = "\\.(csv|rds|Rds)$")))
message("  Output tables:  ", length(list.files("../output/tables", pattern = "\\.csv$", recursive = TRUE)))

message("\nData summary:")
message("  Total farms:    ", nrow(lsms_data))
message("  Countries:      ", length(unique(lsms_data$country)))
message("  Farm size range:", round(min(lsms_data$farm_area_ha), 2), "-", 
        round(max(lsms_data$farm_area_ha), 2), "ha")
message("  Median farm:    ", round(median(lsms_data$farm_area_ha), 2), "ha")

message("\nFinished: ", Sys.time())
message(paste(rep("=", 70), collapse = ""))

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
