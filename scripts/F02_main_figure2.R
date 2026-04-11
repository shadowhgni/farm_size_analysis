# ==============================================================================
# Script: F02_main_figure2.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Main Figure 2 — 4-panel: Q10 map, Q90 map, ECDF by farm class, Gini scatter
#
# PRODUCTION inputs (all in scripts/ dir):
#   fig.2a_quantile_10_fsizes.tif  — q10 predicted farm size raster
#   fig.2b_quantile_90_fsizes.tif  — q90 predicted farm size raster
#   fig2c.rds                      — theor_farms_application joined with rf predictions
#                                    (farm_size = linear_farm_size_ha, avg_size = rf pred)
#   fig.2d_mean_fsize_gini_coefs.rds — predicted & observed avg vs Gini per grid cell
#
# CI: all inputs are synthetic stubs produced by 00_synthetic_data.R
# Outputs: ../output/main_fig/Fig.02.png
# ==============================================================================

source("00_report_utils.R")
t0 <- proc.time()[["elapsed"]]
require(tidyverse)
rm(list = setdiff(ls(), c("t0","write_report","capture_output","ci_trees","ci_folds")))
setwd(paste0(here::here(), "/scripts"))
dir.create("../output/main_fig", recursive = TRUE, showWarnings = FALSE)

# ── SSA boundary ──────────────────────────────────────────────────────────────
# PRODUCTION: geodata::world downloads to input_path
# CI: same — geodata caches in ../data/raw/spatial
input_path <- "../data/raw/spatial"
country    <- geodata::world(path = input_path, resolution = 5, level = 0)
isocodes   <- geodata::country_codes()
isocodes_ssa <- subset(isocodes,
  NAME == "Sudan" | UNREGION1 == "Middle Africa" | UNREGION1 == "Western Africa" |
  UNREGION1 == "Southern Africa" | UNREGION1 == "Eastern Africa")
isocodes_ssa <- subset(isocodes_ssa,
  !NAME %in% c("Cabo Verde","Comoros","Mauritius","Mayotte","Réunion",
               "Saint Helena","São Tomé and Príncipe","Seychelles"))
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)

pal1 <- colorRampPalette(c("darkred","orange","gold","darkolivegreen3","darkgreen"))
pal4 <- colorRampPalette(c("#A1D99B","#00441B"))
terra::terraOptions(memfrac = 0.2, todisk = TRUE)

# ── Open PNG ──────────────────────────────────────────────────────────────────
png("../output/main_fig/Fig.02.png", width = 9, height = 8.8, units = "in", res = 200)
par(mfrow = c(2,2), mar = c(3.5,3.5,1,1), xaxs = "i", yaxs = "i")

# ── Panel A: Q10 farm size map ────────────────────────────────────────────────
fig2a <- terra::rast("fig.2a_quantile_10_fsizes.tif")
pal   <- colorRampPalette(c("#8B0000","#FFCB00","forestgreen"))
terra::plot(ssa, mar = c(3.5,3.5,1,1), clip = FALSE, col = "white", main = "",
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.8))
terra::plot(fig2a[[1]], breaks = c(0,0.1,0.2,0.5,1,2,Inf),
            col = pal(6), legend = FALSE, axes = FALSE, add = TRUE)
legend(-15, -5, bty = "y", bg = "white", cex = 1.1, ncol = 1, box.col = "white",
       title = expression(paste("Farm size (q"[10],")")),
       legend = c("< 0.1 ha","0.1 - 0.2 ha","0.2 - 0.5 ha","0.5 - 1 ha","1 - 2 ha","> 2 ha"),
       fill = pal(6), horiz = FALSE)
terra::plot(ssa, axes = FALSE, add = TRUE)
text(48, 26, "A)", cex = 1.5)

# ── Panel B: Q90 farm size map ────────────────────────────────────────────────
fig2b <- terra::rast("fig.2b_quantile_90_fsizes.tif")
pal   <- colorRampPalette(c("#8B0000","#FFCB00","forestgreen"))
terra::plot(ssa, mar = c(3.5,3.5,1,1), clip = FALSE, col = "white", main = "",
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.8))
terra::plot(fig2b[[1]], breaks = c(0,1,2,5,10,15,Inf),
            col = pal(6), legend = FALSE, axes = FALSE, add = TRUE)
legend(-15, -5, bty = "y", bg = "white", cex = 1.1, ncol = 1, box.col = "white",
       title = expression(paste("Farm size (q"[90],")")),
       legend = c("< 1 ha","1 - 2 ha","2 - 5 ha","5 - 10 ha","10 - 15 ha","> 15 ha"),
       fill = pal(6), horiz = FALSE)
terra::plot(ssa, axes = FALSE, add = TRUE)
text(48, 26, "B)", cex = 1.5)

# ── Panel C: ECDF by farm size class ─────────────────────────────────────────
# PRODUCTION: fig2c built from theor_farms_application joined with rf predictions
# fig2c <- theor_farms_application |>
#   select(x, y, linear_farm_size_ha) |>
#   inner_join(terra::as.data.frame(rf_model_predictions_SSA, xy=TRUE)) |>
#   rename(farm_size = linear_farm_size_ha, avg_size = rf_mean)
fig2c <- readRDS("fig2c.rds")

plot(ecdf(fig2c$farm_size[fig2c$avg_size > 5]), col = NA, verticals = FALSE,
     xlim = c(0,25), main = "",
     xlab = "Average farm size per grid cell (ha)", ylab = "Cumulative probability",
     cex.axis = 1.2, cex.lab = 1.4, las = 0, mgp = c(2,0.75,0), bg = "whitesmoke")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "whitesmoke")
grid(nx = 8, ny = 8, col = "lightgrey")
plot(ecdf(fig2c$farm_size[fig2c$avg_size <  0.5]),           col = "#8B0000",    lwd = 3, add = TRUE)
plot(ecdf(fig2c$farm_size[fig2c$avg_size >= 1 & fig2c$avg_size < 2]), col = "#FFCB00", lwd = 3, add = TRUE)
plot(ecdf(fig2c$farm_size[fig2c$avg_size >  5]),             col = "forestgreen", lwd = 3, add = TRUE)
legend("bottomright", legend = c("< 0.5 ha","1–2 ha","> 5 ha"),
       col = c("#8B0000","#FFCB00","forestgreen"), bg = NA, box.col = NA,
       lty = 1, lwd = 1.5, cex = 1.2, title = "Farm size class")
text(23, 0.93, "C)", cex = 1.5)

# ── Panel D: Gini vs average farm size ───────────────────────────────────────
fig2d <- readRDS("fig.2d_mean_fsize_gini_coefs.rds")

plot(fig2d$predicted_avg_vs_gini$avg, fig2d$predicted_avg_vs_gini$gini,
     col = "white", xlim = c(0,15), ylim = c(0.1,0.8),
     cex.axis = 1.2, cex.lab = 1.4, las = 0, mgp = c(2,0.75,0),
     xlab = "Average farm size per grid cell (ha)",
     ylab = "Gini coefficient of farm size per grid cell")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "whitesmoke")
grid(nx = 8, ny = 8, col = "lightgrey")
points(fig2d$predicted_avg_vs_gini$avg, fig2d$predicted_avg_vs_gini$gini,
       cex = 0.5, pch = 21, col = viridis::viridis(5, alpha = 0.1)[2],
       bg  = viridis::viridis(5, alpha = 0.1)[2])
points(fig2d$observed_avg_vs_gini$mean, fig2d$observed_avg_vs_gini$gini,
       cex = 0.5, pch = 21, col = viridis::viridis(5, alpha = 0.1)[4],
       bg  = viridis::viridis(5, alpha = 0.1)[4])
tryCatch({
  car::dataEllipse(fig2d$predicted_avg_vs_gini$avg, fig2d$predicted_avg_vs_gini$gini,
    col = viridis::viridis(5, alpha=0.9)[2], cex=0, levels=0.50, lwd=2, center.pch=FALSE, add=TRUE)
  car::dataEllipse(fig2d$observed_avg_vs_gini$mean, fig2d$observed_avg_vs_gini$gini,
    col = viridis::viridis(5, alpha=0.9)[4], cex=0, levels=0.50, lwd=2, center.pch=FALSE, add=TRUE)
  car::dataEllipse(fig2d$predicted_avg_vs_gini$avg, fig2d$predicted_avg_vs_gini$gini,
    col = viridis::viridis(5, alpha=0.9)[2], cex=0, levels=0.95, lty=2, lwd=2, center.pch=FALSE, add=TRUE)
  car::dataEllipse(fig2d$observed_avg_vs_gini$mean, fig2d$observed_avg_vs_gini$gini,
    col = viridis::viridis(5, alpha=0.9)[4], cex=0, levels=0.95, lty=2, lwd=2, center.pch=FALSE, add=TRUE)
}, error = function(e) message("CI: dataEllipse skipped: ", e$message))
abline(a = 0.7, b = -0.03, col = 1, lwd = 2)
abline(h = 0.2, col = 1, lty = 2, lwd = 2)
legend("topright", bty = "n", bg = "whitesmoke", cex = 1.1, ncol = 1,
       legend = c("Predicted","Reported","y=0.7-0.03x","y=0.2"),
       pch = c(21,21,NA,NA), lty = c(NA,NA,1,2), lwd = c(NA,NA,2,2),
       col = c(viridis::viridis(5, alpha=0.7)[c(2,4)], 1, 1),
       pt.bg = c(viridis::viridis(5, alpha=0.7)[c(2,4)], NA, NA), pt.cex = 1.5)
text(1, 0.75, "D)", cex = 1.5)
box()

dev.off()

# PRODUCTION: also write PDF via magick
# magick::image_write(magick::image_read("../output/main_fig/Fig.02.png"),
#                     "../output/main_fig/Fig.02.pdf", format = "pdf")

elapsed <- proc.time()[["elapsed"]] - t0
write_report("F02_main_figure2.R",
  "Main Figure 2: Q10 map | Q90 map | ECDF by farm class | Gini scatter",
  inputs = list(
    "Q10 raster"    = "fig.2a_quantile_10_fsizes.tif",
    "Q90 raster"    = "fig.2b_quantile_90_fsizes.tif",
    "fig2c RDS"     = "fig2c.rds",
    "fig2d RDS"     = "fig.2d_mean_fsize_gini_coefs.rds"
  ),
  outputs = list("Fig.02.png" = "../output/main_fig/Fig.02.png"),
  sections = list("Panel notes" = c(
    "A: Q10 farm size map — smallest predicted farm sizes",
    "B: Q90 farm size map — largest predicted farm sizes",
    "C: ECDF of individual farm sizes stratified by avg size class",
    "D: Gini coefficient vs average farm size with 50% and 95% ellipses"
  )),
  elapsed_sec = elapsed)
message("F02 done in ", round(elapsed, 1), "s")
