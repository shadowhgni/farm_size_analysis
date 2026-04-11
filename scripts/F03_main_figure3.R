# ==============================================================================
# Script: F03_main_figure3.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Main Figure 3 — 6-panel cumulative crop area / herd size by AEZ
#          layout: matrix(c(1,1,2,2, 3,4,5,6), nrow=2, byrow=TRUE)
#          Panel A: all_crops + cattle across 4 AEZ  (full-width)
#          Panel B: 6 crops across all AEZ           (full-width)
#          Panels C-F: maize/sorghum/millet/cassava per AEZ
#
# PRODUCTION input (scripts/ dir):
#   2026-01-24.CHINA_croplands_per_crop_per_aez.rds
#     $df_rel_long: pred_farm_area_ha, aez, product, value (cumulative 0→1)
#
# CI: stub produced by 00_synthetic_data.R with sigmoid-shaped cumulative values
# Output: ../output/main_fig/Fig.03.png
# ==============================================================================

source("00_report_utils.R")
t0 <- proc.time()[["elapsed"]]
require(tidyverse)
rm(list = setdiff(ls(), c("t0","write_report","capture_output","ci_trees","ci_folds")))
setwd(paste0(here::here(), "/scripts"))
dir.create("../output/main_fig", recursive = TRUE, showWarnings = FALSE)

# ── Load data ─────────────────────────────────────────────────────────────────
fig3  <- readRDS("2026-01-24.CHINA_croplands_per_crop_per_aez.rds")
fig3a <- fig3$df_rel_long |>
  filter(product %in% c("all_crops","cattle"), aez != "all_aez")
fig3b <- fig3$df_rel_long |>
  filter(product %in% c("maize","sorghum","millet","cassava","legumes","non_food"),
         aez == "all_aez") |>
  mutate(product = if_else(product == "non_food", "cash crops", product))
fig3c <- fig3$df_rel_long |>
  filter(product %in% c("maize","sorghum","millet","cassava"), aez != "all_aez")

# ── Open PNG ──────────────────────────────────────────────────────────────────
png("../output/main_fig/Fig.03.png", width = 9, height = 8.8, units = "in", res = 200)
layout(matrix(c(1,1,2,2, 3,4,5,6), nrow = 2, byrow = TRUE))
par(mar = c(3.5,3.5,1,1), xaxs = "i", yaxs = "i")

# ── Panel A: all_crops + cattle by AEZ ───────────────────────────────────────
plot(0, 0, xlim = c(0,8), ylim = c(0,100), xlab = "", ylab = "",
     cex.axis = 1.2, cex.lab = 1.4, las = 0, mgp = c(2,0.75,0), col = "white")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "whitesmoke")
grid(nx = 8, ny = 10, lty = 1, col = "lightgrey")
i <- 1
for (aez1 in c("tropical highlands","humid","sub-humid","semi-arid")) {
  sub1 <- subset(fig3a, aez == aez1)
  col  <- viridis::viridis(4, direction = 1)[i]
  for (ct in c("all_crops","cattle")) {
    sub  <- subset(sub1, product == ct)
    if (nrow(sub) > 0)
      lines(sub$pred_farm_area_ha, sub$value * 100,
            col = col, lwd = 3.5, lty = if (ct == "all_crops") 1L else 2L)
  }
  i <- i + 1
}
legend("bottomright", bty = "n", bg = "whitesmoke", cex = 1.1,
       lty = c(1,1,1,1,1,2), lwd = 3,
       legend = c("Tropical highlands","Humid","Sub-humid","Semi-arid","Cropland","Cattle"),
       col = c(viridis::viridis(4, direction=1), 1, 1))
text(0.5, 93, "A)", cex = 1.5)
title(ylab = "Cumulative cultivated area or herd size (%)", cex.lab = 1.4, line = 2)
title(xlab = "Average farm size (ha)",                       cex.lab = 1.4, line = 2)
box()

# ── Panel B: 6 crops across all AEZ ──────────────────────────────────────────
plot(0, 0, xlim = c(0,8), ylim = c(0,100), xlab = "", ylab = "",
     cex.axis = 1.2, cex.lab = 1.4, las = 0, mgp = c(2,0.75,0), col = "white")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "whitesmoke")
grid(nx = 8, ny = 10, lty = 1, col = "lightgrey")
i <- 1
for (prod1 in c("maize","sorghum","millet","cassava","legumes","cash crops")) {
  sub1 <- subset(fig3b, product == prod1)
  if (nrow(sub1) > 0)
    lines(sub1$pred_farm_area_ha, sub1$value * 100,
          col = viridis::viridis(6, direction=1)[i], lwd = 3.5, lty = 1)
  i <- i + 1
}
legend("bottomright", bty = "n", bg = "whitesmoke", cex = 1.1, lty = 1, lwd = 3,
       legend = c("Maize","Sorghum","Millet","Cassava","Legumes","Non-food crops"),
       col = viridis::viridis(6, direction=1))
text(0.5, 93, "B)", cex = 1.5)
title(ylab = "Cumulative crop area (%)", cex.lab = 1.4, line = 2)
title(xlab = "Average farm size (ha)",   cex.lab = 1.4, line = 2)
box()

# ── Panels C–F: maize/sorghum/millet/cassava per AEZ ─────────────────────────
# Panels C–F: one per AEZ with hardcoded expression() titles (bquote + \n not supported)
panel_aez    <- c("tropical highlands","humid","sub-humid","semi-arid")
panel_label  <- c("C)","D)","E)","F)")
panel_titles <- list(
  expression(bold("Tropical\nhighlands")),
  expression(bold("Humid")),
  expression(bold("Sub-humid")),
  expression(bold("Semi-arid"))
)
crops4 <- c("maize","sorghum","millet","cassava")

for (j in seq_along(panel_aez)) {
  sub_aez <- subset(fig3c, aez == panel_aez[j])
  plot(0, 0, xlim = c(0,8), ylim = c(0,100), xlab = "", ylab = "",
       cex.axis = 1.2, cex.lab = 1.4, las = 0, mgp = c(2,0.75,0), col = "white")
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "whitesmoke")
  grid(nx = 8, ny = 10, lty = 1, col = "lightgrey")
  for (k in seq_along(crops4)) {
    sub_c <- subset(sub_aez, product == crops4[k])
    if (nrow(sub_c) > 0)
      lines(sub_c$pred_farm_area_ha, sub_c$value * 100,
            col = viridis::viridis(4, direction=1)[k], lwd = 3.5, lty = 1)
  }
  legend("bottomright", bty = "n", bg = "whitesmoke", cex = 1.1, lty = 1, lwd = 3,
         title = panel_titles[[j]],
         legend = c("Maize","Sorghum","Millet","Cassava"),
         col = viridis::viridis(4, direction=1))
  text(0.8, 93, panel_label[j], cex = 1.5)
  title(ylab = "Cumulative crop area (%)", cex.lab = 1.4, line = 2)
  title(xlab = "Average farm size (ha)",   cex.lab = 1.4, line = 2)
  box()
}

dev.off()

elapsed <- proc.time()[["elapsed"]] - t0
write_report("F03_main_figure3.R",
  "Main Figure 3: 6-panel cumulative crop area / herd size by AEZ",
  inputs  = list("CHINA RDS" = "2026-01-24.CHINA_croplands_per_crop_per_aez.rds"),
  outputs = list("Fig.03.png" = "../output/main_fig/Fig.03.png"),
  sections = list("Panel layout" = c(
    "A (full-width): all_crops + cattle vs farm size, 4 AEZ",
    "B (full-width): 6 crops across all AEZ",
    "C: tropical highlands — 4 crops",
    "D: humid — 4 crops",
    "E: sub-humid — 4 crops",
    "F: semi-arid — 4 crops",
    paste("fig3a rows:", nrow(fig3a)),
    paste("fig3b rows:", nrow(fig3b)),
    paste("fig3c rows:", nrow(fig3c))
  )),
  elapsed_sec = elapsed)
message("F03 done in ", round(elapsed, 1), "s")
