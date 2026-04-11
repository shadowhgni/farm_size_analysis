# ==============================================================================
# Script: 03.1_pooled_data.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Prepare pooled LSMS dataset with spatial predictors for ML analysis
#
# Reads:  data/processed/lsms_and_zambia.csv
#         data/processed/stacked_rasters_africa.tif
# Writes: data/processed/lsms_untrimmed_africa.rds
#         data/processed/lsms_trimmed_95th_africa.rds
#         data/processed/lsms_trimmed_99th_africa.rds
#         data/processed/lsms_spatial_with_country_names.csv
#         data/processed/lsms_spatial.csv
#         data/processed/lsms_spatial_africa.Rds
#         data/processed/lsms_trimmed_95th_africa.rdata
# ==============================================================================

source("00_report_utils.R")
t0 <- proc.time()[["elapsed"]]

require(tidyverse)
require(terra)
rm(list = setdiff(ls(), c("t0","write_report","capture_output","ci_trees","ci_folds")))

setwd(paste0(here::here(), "/scripts"))
dir.create("../data/processed", recursive = TRUE, showWarnings = FALSE)

# ── Load survey data ──────────────────────────────────────────────────────────
lsms_raw <- read.csv("../data/processed/lsms_and_zambia.csv",
                     stringsAsFactors = FALSE)
message("Raw LSMS loaded: ", nrow(lsms_raw), " farms, ",
        length(unique(lsms_raw$country)), " countries")

# ── Load predictor raster and extract at farm locations ───────────────────────
stacked   <- terra::rast("../data/processed/stacked_rasters_africa.tif")
pred_cols <- names(stacked)

farm_pts  <- terra::vect(lsms_raw, geom = c("x","y"), crs = "EPSG:4326")
extracted <- as.data.frame(terra::extract(stacked, farm_pts, ID = FALSE))

lsms_spatial <- cbind(lsms_raw, extracted)

# Admin stubs if missing
if (!"gadm_0" %in% names(lsms_spatial)) {
  sixteen_countries     <- c("Benin","Burkina","Cote_d_Ivoire","Ethiopia","Ghana",
                              "Guinea_Bissau","Malawi","Mali","Niger","Nigeria",
                              "Rwanda","Senegal","Tanzania","Togo","Uganda","Zambia")
  sixteen_country_codes <- c("BEN","BFA","CIV","ETH","GHA","GNB","MWI","MLI",
                              "NER","NGA","RWA","SEN","TZA","TGO","UGA","ZMB")
  lsms_spatial$gadm_0 <- sixteen_country_codes[match(lsms_spatial$country, sixteen_countries)]
  lsms_spatial$gadm_1 <- paste0(lsms_spatial$country, "_Region1")
  lsms_spatial$gadm_2 <- paste0(lsms_spatial$country, "_District1")
  lsms_spatial$gadm_3 <- NA_character_
  lsms_spatial$gadm_4 <- NA_character_
}

# Drop incomplete predictor rows
key_cols     <- c("x","y","farm_area_ha", pred_cols)
lsms_spatial <- lsms_spatial[complete.cases(lsms_spatial[, key_cols]), ]
message("After predictor extraction: ", nrow(lsms_spatial), " farms")

# ── Trim by percentile within country ────────────────────────────────────────
trim_pct <- function(df, p) {
  do.call(rbind, lapply(split(df, df$country), function(d)
    d[d$farm_area_ha <= quantile(d$farm_area_ha, p, na.rm = TRUE), ]))
}

trim95 <- trim_pct(lsms_spatial, 0.95)
trim99 <- trim_pct(lsms_spatial, 0.99)

# ML dataset: 95th trim, key columns only
ml_cols <- c("x","y","farm_area_ha", pred_cols,
             "country","gadm_0","gadm_1","gadm_2","gadm_3","gadm_4","year","farm_id","hh_size")
lsms_ml  <- trim95[, intersect(ml_cols, names(trim95))]
lsms_ml  <- lsms_ml[complete.cases(lsms_ml[, key_cols]), ]

# ── Save outputs ──────────────────────────────────────────────────────────────
saveRDS(lsms_spatial, "../data/processed/lsms_untrimmed_africa.rds")
saveRDS(trim95,       "../data/processed/lsms_trimmed_95th_africa.rds")
saveRDS(trim99,       "../data/processed/lsms_trimmed_99th_africa.rds")

write.csv(lsms_ml, "../data/processed/lsms_spatial_with_country_names.csv", row.names = FALSE)
write.csv(lsms_ml[, c("x","y","farm_area_ha", pred_cols)],
          "../data/processed/lsms_spatial.csv", row.names = FALSE)
saveRDS(lsms_ml,  "../data/processed/lsms_spatial_africa.Rds")

lsms_spatial <- lsms_ml   # for load() compatibility
save(lsms_spatial, file = "../data/processed/lsms_trimmed_95th_africa.rdata")

# ── Summary ───────────────────────────────────────────────────────────────────
farm_summary <- lsms_ml |>
  group_by(country) |>
  summarise(n       = n(),
            mean_ha = round(mean(farm_area_ha), 2),
            med_ha  = round(median(farm_area_ha), 2),
            p10     = round(quantile(farm_area_ha, .10), 2),
            p90     = round(quantile(farm_area_ha, .90), 2),
            .groups = "drop")
print(farm_summary)

elapsed <- proc.time()[["elapsed"]] - t0

write_report(
  script_name = "03.1_pooled_data.R",
  description = "Pool LSMS surveys, extract spatial predictors, trim outliers",
  inputs  = list(
    "LSMS raw CSV"  = "../data/processed/lsms_and_zambia.csv",
    "Stacked raster" = "../data/processed/stacked_rasters_africa.tif"
  ),
  outputs = list(
    "Untrimmed RDS"  = "../data/processed/lsms_untrimmed_africa.rds",
    "95th trim RDS"  = "../data/processed/lsms_trimmed_95th_africa.rds",
    "99th trim RDS"  = "../data/processed/lsms_trimmed_99th_africa.rds",
    "ML CSV"         = "../data/processed/lsms_spatial_with_country_names.csv",
    "Spatial CSV"    = "../data/processed/lsms_spatial.csv",
    "ML RDS"         = "../data/processed/lsms_spatial_africa.Rds",
    "Rdata"          = "../data/processed/lsms_trimmed_95th_africa.rdata"
  ),
  sections = list(
    "Farm size by country" = capture_output(print(farm_summary, n = 20)),
    "Dataset dimensions"   = c(
      paste("Raw farms:", nrow(lsms_raw)),
      paste("After extraction:", nrow(lsms_spatial)),
      paste("95th trim:", nrow(trim95)),
      paste("ML dataset:", nrow(lsms_ml)),
      paste("Predictors:", length(pred_cols))
    )
  ),
  elapsed_sec = elapsed
)
message("03.1 done in ", round(elapsed, 1), "s")
