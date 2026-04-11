# ==============================================================================
# Script: S07_distribution_parameters.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Supplementary Figure 7 — Distribution parameters of truncated
#          log-normal fits to empirical farm size distributions
#          6 panels: skew, gini, adjusted_logn_mean, adjusted_logn_sd,
#                    ks_trunc_D, ks_trunc_pval
#
# PRODUCTION inputs:
#   ../data/processed/fsize_distribution_resample_long.rds ($theor_farms)
#   ../data/processed/gini_raster.tif
#   ../data/processed/back_transf_trunc_adj_mean.tif
#   ../data/processed/back_transf_trunc_adj_sd.tif
#
# CI: stubs from 00.3_synthetic_data.R
# Output: ../output/other_illustr/graphs/Suppl.Fig07.png
# ==============================================================================

source("00_report_utils.R")
t0 <- proc.time()[["elapsed"]]
require(tidyverse)
setwd(paste0(here::here(), "/scripts"))
dir.create("../output/other_illustr/graphs", recursive = TRUE, showWarnings = FALSE)

# ── SSA boundary ──────────────────────────────────────────────────────────────
input_path <- "../data/raw/spatial"
country    <- geodata::world(path = input_path, resolution = 5, level = 0)
isocodes   <- geodata::country_codes()
if (!"UNREGION1" %in% names(isocodes)) {
  ssa_i3 <- c("AGO","CMR","CAF","TCD","COD","COG","GAB","GNQ","STP",
               "BWA","LSO","MWI","MOZ","NAM","ZAF","SWZ","ZMB","ZWE",
               "BDI","COM","DJI","ERI","ETH","KEN","MDG","MUS","RWA","SDN","SSD","SOM","TZA","UGA",
               "BEN","BFA","CPV","CIV","GMB","GHA","GIN","GNB","LBR","MLI","MRT","NER","NGA",
               "SEN","SLE","TGO","SDN")
  isocodes_ssa <- subset(isocodes, ISO3 %in% ssa_i3)
} else {
  isocodes_ssa <- subset(isocodes,
    NAME == "Sudan" |
    UNREGION1 %in% c("Middle Africa","Western Africa","Southern Africa","Eastern Africa"))
  isocodes_ssa <- subset(isocodes_ssa,
    !NAME %in% c("Cabo Verde","Comoros","Mauritius","Mayotte","Reunion",
                 "Saint Helena","Sao Tome and Principe","Seychelles"))
}
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)

pal1 <- colorRampPalette(c("darkred","orange","gold","darkolivegreen3","darkgreen"))

terra::terraOptions(memfrac = 0.5, todisk = TRUE)

# ── Load data ─────────────────────────────────────────────────────────────────
xx          <- readRDS("../data/processed/fsize_distribution_resample_long.rds")
theor_farms <- xx$theor_farms; rm(xx)

theor_rast <- theor_farms |>
  ungroup() |>
  select(x, y, skew, kurt, ks_trunc_D, ks_trunc_pval,
         adjusted_logn_mean, adjusted_logn_sd) |>
  terra::rast()

gini     <- terra::rast("../data/processed/gini_raster.tif")
terra::crs(gini) <- "EPSG:4326"
gini     <- tryCatch(terra::resample(gini, theor_rast), error = function(e) gini)

back_avg <- terra::rast("../data/processed/back_transf_trunc_adj_mean.tif")
terra::crs(back_avg) <- "EPSG:4326"
back_avg <- tryCatch(terra::resample(back_avg, theor_rast), error = function(e) back_avg)

back_sd  <- terra::rast("../data/processed/back_transf_trunc_adj_sd.tif")
terra::crs(back_sd) <- "EPSG:4326"
back_sd  <- tryCatch(terra::resample(back_sd, theor_rast), error = function(e) back_sd)

selected_rast <- c(theor_rast$skew, gini,
                   back_avg, back_sd,
                   theor_rast$ks_trunc_D, theor_rast$ks_trunc_pval)
names(selected_rast) <- c("skew","gini",
                           "adjusted_logn_mean","adjusted_logn_sd",
                           "ks_trunc_D","ks_trunc_pval")

# ── Build tmap list ───────────────────────────────────────────────────────────
tmap::tmap_mode("plot")
tmap::tmap_options(component.autoscale = FALSE, asp = 1)
tmap_list <- list()

for (i in names(selected_rast)) {
  my_range <- switch(i,
    skew               = c(0, 6),
    gini               = c(0.4, 0.6),
    ks_trunc_D         = c(0, 0.16),
    ks_trunc_pval      = c(0.05, 1),
    adjusted_logn_mean = c(0, 5),
    adjusted_logn_sd   = c(1, 5),
    c(0, 1))

  my_label <- switch(i,
    skew               = "Skewness of empirical\nfarm size distribution",
    gini               = "Gini coef of empirical\nfarm size distribution",
    ks_trunc_D         = "Goodness of fit\nKolmogorov's D distance",
    ks_trunc_pval      = "Goodness of fit\nP-value",
    adjusted_logn_mean = "Average (back-transformed)\nfarm size (ha)",
    adjusted_logn_sd   = "Standard deviation (back-transformed)\nof farm size (ha)",
    i)

  my_tag <- switch(i,
    skew = "A", gini = "B",
    adjusted_logn_mean = "C", adjusted_logn_sd = "D",
    ks_trunc_D = "E", ks_trunc_pval = "F", "?")

  my_col <- switch(i,
    skew               = rev(pal1(10)),
    gini               = rev(pal1(10)),
    ks_trunc_D         = rev(pal1(10)),
    ks_trunc_pval      = pal1(10),
    adjusted_logn_mean = pal1(10),
    adjusted_logn_sd   = rev(pal1(10)),
    pal1(10))

  tmap_list[[i]] <- tmap::tm_shape(selected_rast[[i]]) +
    tmap::tm_raster(
      col.scale = tmap::tm_scale_continuous(
        values = my_col, limits = my_range,
        outliers.trunc = c(TRUE, TRUE), n = 3),
      col.legend = tmap::tm_legend(
        title = my_label, frame = FALSE,
        text.size = 1, title.size = 0.01, title.align = "left")) +
    tmap::tm_shape(sf::st_as_sf(ssa), crs = terra::crs(ssa)) +
    tmap::tm_borders(col = "black", lwd = 0.5) +
    tmap::tm_graticules(
      x = seq(-20, 60, by = 10), y = seq(-40, 20, by = 10),
      col = "gray70", lwd = 0.3, alpha = 0.7,
      labels.size = 0.6, labels.col = "gray50") +
    tmap::tm_layout(
      frame = FALSE, bg.color = "whitesmoke",
      legend.position = c("left","bottom"), legend.frame = FALSE,
      legend.bg.color = "transparent", legend.frame.lwd = 0,
      legend.width = 4.2, legend.height = 9) +
    tmap::tm_credits(my_tag,
      position = tmap::tm_pos_in("right","top"), size = 1.5)
}

combined_plot <- tmap::tmap_arrange(tmap_list, ncol = 2)
tmap::tmap_save(combined_plot, "../output/other_illustr/graphs/Suppl.Fig07.png",
                width = 7, height = 10, units = "in", dpi = 150)

# ── Report ────────────────────────────────────────────────────────────────────
elapsed <- proc.time()[["elapsed"]] - t0
write_report(
  "S07_distribution_parameters.R",
  "Supp Fig 7: distribution parameters of truncated log-normal fits (6-panel tmap)",
  inputs  = list(
    "theor_farms RDS"  = "../data/processed/fsize_distribution_resample_long.rds",
    "gini raster"      = "../data/processed/gini_raster.tif",
    "back_avg raster"  = "../data/processed/back_transf_trunc_adj_mean.tif",
    "back_sd raster"   = "../data/processed/back_transf_trunc_adj_sd.tif"),
  outputs = list("PNG" = "../output/other_illustr/graphs/Suppl.Fig07.png"),
  elapsed_sec = elapsed)
message("S07_distribution_parameters.R done in ", round(elapsed, 1), "s")
