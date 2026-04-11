# ==============================================================================
# Script: S02_cropland_uncertainty.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Supplementary Figure 2 — Cropland data uncertainties
#          Panel A: GGally correlation matrix of LSMS predictors
#          Panel B: GGally correlation matrix of 5 cropland sources
#          Panel C: Total cropland (million ha) per source, with nb farms overlay
#
# PRODUCTION inputs:
#   ../output/plot_data/plot_suppl_01_effect_of_source_of_cropland_masks.rds
#     $pred_cpland_df      — x, y + columns per cropland source
#     $ssa_cropland        — source, total (ha)
#     $nb_farms_summarized — source, nb_farms
#   ../data/processed/lsms_trimmed_95th_africa.rds
#
# CI: stubs from 00.3_synthetic_data.R
# Output: ../output/other_illustr/graphs/Suppl.Fig02.png
# ==============================================================================

source("00_report_utils.R")
t0 <- proc.time()[["elapsed"]]
require(tidyverse)
require(patchwork)
setwd(paste0(here::here(), "/scripts"))
dir.create("../output/other_illustr/graphs", recursive = TRUE, showWarnings = FALSE)

# ── Load data ─────────────────────────────────────────────────────────────────
xx <- readRDS("../output/plot_data/plot_suppl_01_effect_of_source_of_cropland_masks.rds")
pred_cpland_df <- xx$pred_cpland_df
ssa_cropland   <- xx$ssa_cropland
ssa_nb_farms   <- xx$nb_farms_summarized |>
  mutate(nb_rounded = round(nb_farms / 1e6, 0))
rm(xx)

lsms_spatial <- readRDS("../data/processed/lsms_trimmed_95th_africa.rds") |>
  select(x, y, country, farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>
  na.omit()

# ── Custom correlation panel with significance stars ──────────────────────────
my_cor_stars <- function(data, mapping, size = 5, digits = 2, ...) {
  x    <- GGally::eval_data_col(data, mapping$x)
  y    <- GGally::eval_data_col(data, mapping$y)
  corr <- cor(x, y, use = "complete.obs")
  pval <- cor.test(x, y)$p.value
  stars <- symnum(pval, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols   = c("***", "**", "*", ".", " "))
  GGally::ggally_text(
    label = paste0(round(corr, digits), stars),
    mapping = aes(), xP = 0.5, yP = 0.5, size = size, ...)
}

# ── Panel A: predictor correlation matrix ────────────────────────────────────
P00a <- lsms_spatial |>
  select(!c(x, y, country)) |>
  rename(Y = farm_area_ha, A = cropland, B = cattle, C = pop, D = cropland_per_capita,
         E = sand, F = slope, G = temperature, H = rainfall, I = maizeyield, J = market) |>
  GGally::ggpairs(upper = list(continuous = my_cor_stars),
                  diag  = list(continuous = GGally::wrap("densityDiag"))) +
  labs(title = "A)") +
  theme_test() +
  theme(strip.text.x.top    = element_text(size = 14),
        strip.text.y.right  = element_text(size = 14),
        axis.text  = element_blank(), axis.ticks = element_blank(),
        text = element_text(size = 10), title = element_text(size = 16),
        strip.background = element_rect(color = NA, fill = "white"))
P00a1 <- patchwork::wrap_elements(GGally::ggmatrix_gtable(P00a))
ggsave("../output/other_illustr/graphs/S02_panel_a.png",
       P00a1, width = 9, height = 4.4, dpi = 150)
P00a2 <- magick::image_read("../output/other_illustr/graphs/S02_panel_a.png")

# ── Panel B: cropland source correlation matrix ───────────────────────────────
P00b <- pred_cpland_df |>
  rename_all(toupper) |>
  rename_with(~ gsub("GEOSURVEY2015","GEOS.2015",.)) |>
  select(!c(X, Y)) |>
  GGally::ggpairs(upper = list(continuous = my_cor_stars),
                  diag  = list(continuous = GGally::wrap("densityDiag"))) +
  labs(title = "B)") +
  theme_test() +
  theme(strip.text.x.top   = element_text(size = 10),
        strip.text.y       = element_text(size = 7.3),
        axis.text  = element_blank(), axis.ticks = element_blank(),
        text = element_text(size = 10), title = element_text(size = 14),
        strip.background = element_rect(color = NA, fill = "white"))
P00b1 <- patchwork::wrap_elements(GGally::ggmatrix_gtable(P00b))
ggsave("../output/other_illustr/graphs/S02_panel_b.png",
       P00b1, width = 6, height = 4.4, dpi = 150)
P00b2 <- magick::image_read("../output/other_illustr/graphs/S02_panel_b.png")

# ── Panel C: total cropland bar chart ─────────────────────────────────────────
P00c <- ssa_cropland |>
  inner_join(ssa_nb_farms, by = "source") |>
  ggplot(aes(source, total / 1e6)) +
  geom_col() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Source", y = "Total cropland (million ha)") +
  geom_text(y = 60, aes(label = nb_rounded), size = 6, angle = -90, colour = "white") +
  geom_text(x = 1, y = max(ssa_cropland$total / 1e6) * 0.95, label = "C)", size = 7) +
  theme_test() +
  theme(axis.text.y   = element_text(size = 12),
        axis.title    = element_text(size = 12),
        axis.ticks.x  = element_blank(),
        axis.text.x   = element_text(angle = -90, hjust = 0.1, size = 12),
        plot.margin   = margin(5, 15, 5, 10, "pt"))
ggsave("../output/other_illustr/graphs/S02_panel_c.png",
       P00c, width = 3, height = 4.4, dpi = 150)
P00c2 <- magick::image_read("../output/other_illustr/graphs/S02_panel_c.png")

# ── Assemble with magick/grid ──────────────────────────────────────────────────
grob1 <- grid::rasterGrob(as.raster(P00a2), interpolate = TRUE)
grob2 <- grid::rasterGrob(as.raster(P00b2), interpolate = TRUE)
grob3 <- grid::rasterGrob(as.raster(P00c2), interpolate = TRUE)

plot1 <- patchwork::wrap_elements(full = grob1)
plot2 <- patchwork::wrap_elements(full = grob2)
plot3 <- patchwork::wrap_elements(full = grob3)

P00d <- plot2 + plot3 + patchwork::plot_layout(ncol = 2, widths = c(2, 1))
P00e <- plot1 / P00d + patchwork::plot_layout(nrow = 2)

ggsave("../output/other_illustr/graphs/Suppl.Fig02.png",
       P00e, height = 9, width = 8.8, units = "in", dpi = 150)

# ── Report ────────────────────────────────────────────────────────────────────
elapsed <- proc.time()[["elapsed"]] - t0
write_report(
  "S02_cropland_uncertainty.R",
  "Supp Fig 2: predictor correlations, cropland source correlations, cropland totals by source",
  inputs  = list(
    "plot_suppl_01 RDS" = "../output/plot_data/plot_suppl_01_effect_of_source_of_cropland_masks.rds",
    "LSMS 95th"        = "../data/processed/lsms_trimmed_95th_africa.rds"),
  outputs = list("PNG" = "../output/other_illustr/graphs/Suppl.Fig02.png"),
  elapsed_sec = elapsed)
message("S02_cropland_uncertainty.R done in ", round(elapsed, 1), "s")
