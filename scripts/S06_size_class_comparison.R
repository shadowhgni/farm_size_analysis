# ==============================================================================
# Script: S06_size_class_comparison.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Supplementary Figure 6 — farm size class comparison vs Lowder census
#          Panel A: nb farms per country × size class (predicted vs reported)
#          Panel B: cropland ha per country × size class (predicted vs reported)
#          Divergence scores annotated on bars
#
# PRODUCTION inputs:
#   ../data/processed/summarized_farm_area_ha_per_class_vs_sarah.rds
#     $comp_fsize_classes_nb  — NAME_0, GID_0, farm_class, nb_farms, pred_nb_farms
#     $comp_fsize_classes_ha  — NAME_0, GID_0, farm_class, cropland_ha, pred_cropland_ha
#   Suppl.Fig06_divergence_table.rds  — NAME_0, GID_0, divergence_nb, divergence_ha
#
# CI: all inputs are stubs from 00_synthetic_data.R
# Output: ../output/other_illustr/graphs/Suppl.Fig06.png
# ==============================================================================

source("00_report_utils.R")
t0 <- proc.time()[["elapsed"]]
require(tidyverse)
require(patchwork)
rm(list = setdiff(ls(), c("t0","write_report","capture_output","ci_trees","ci_folds")))
setwd(paste0(here::here(), "/scripts"))
dir.create("../output/other_illustr/graphs", recursive = TRUE, showWarnings = FALSE)

# ── Load data ─────────────────────────────────────────────────────────────────
# NOTE: geodata::world() was removed — ssa was only used to attach GID_0 via
# NAME_0, but both comp tables already carry GID_0 from 00_synthetic_data.R /
# production pipeline. The join is done directly on GID_0 below.
xx                  <- readRDS("../data/processed/summarized_farm_area_ha_per_class_vs_sarah.rds")
comp_fsize_classes_ha <- xx$comp_fsize_classes_ha
comp_fsize_classes_nb <- xx$comp_fsize_classes_nb
rm(xx)
div_table <- readRDS("Suppl.Fig06_divergence_table.rds")

# The stub already carries GID_0 (three-letter ISO code).
# In production the join was used to add GID_0 from the ssa boundary via NAME_0,
# but name mismatches (e.g. "Burkina" vs "Burkina Faso") make that fragile.
# Both tables already have GID_0 so we skip the join entirely.

# ── Panel A: nb farms ────────────────────────────────────────────────────────
P00 <- ggplot(
  comp_fsize_classes_nb |>
    pivot_longer(cols = c(nb_farms, pred_nb_farms),
                 names_to = "category", values_to = "val") |>
    mutate(category = if_else(grepl("pred", category), "Predicted", "Reported")),
  aes(GID_0, val / 1e6)) +
  geom_col(aes(colour = category, fill = category, group = farm_class,
               width = if_else(category == "Reported", 0.8, 0.2)),
           position = position_dodge(width = 0.8), linewidth = 0.8) +
  geom_text(x = 1, y = 8.5, label = "A)", size = 9, inherit.aes = FALSE) +
  geom_text(
    data = div_table |>
      inner_join(comp_fsize_classes_nb |> select(GID_0) |> distinct(), by = "GID_0"),
    aes(x = GID_0, label = round(divergence_nb, 2)),
    y = 6, size = 5, angle = 60, inherit.aes = FALSE) +
  labs(x = "Country", y = "Million farms per farm size class",
       fill = NULL, colour = NULL) +
  scale_y_continuous(expand = c(0,0), limits = c(0,9)) +
  scale_colour_manual(values = c("red3","blue4")) +
  scale_fill_manual(values   = c("red3","lightskyblue1")) +
  theme_test() +
  theme(axis.title     = element_text(size = 17),
        axis.text      = element_text(size = 12),
        axis.ticks.x   = element_blank(),
        legend.text    = element_text(size = 14),
        legend.position = c(0.9, 0.9),
        plot.margin    = margin(3, 0, 15, 0))

# ── Panel B: cropland ha ──────────────────────────────────────────────────────
P01 <- ggplot(
  comp_fsize_classes_ha |>
    pivot_longer(cols = c(cropland_ha, pred_cropland_ha),
                 names_to = "category", values_to = "val") |>
    mutate(category = if_else(grepl("pred", category), "Predicted", "Reported")),
  aes(GID_0, val / 1e6)) +
  geom_col(aes(colour = category, fill = category, group = farm_class,
               width = if_else(category == "Reported", 0.8, 0.2)),
           position = position_dodge(width = 0.8), linewidth = 0.8) +
  geom_text(x = 1, y = 11, label = "B)", size = 9, inherit.aes = FALSE) +
  geom_text(
    data = div_table |>
      inner_join(comp_fsize_classes_ha |> select(GID_0) |> distinct(), by = "GID_0"),
    aes(x = GID_0, label = round(divergence_ha, 2)),
    y = 8.5, size = 5, angle = 60, inherit.aes = FALSE) +
  labs(x = "Country", y = "Million ha cultivated per farm size class",
       fill = NULL, colour = NULL) +
  scale_y_continuous(expand = c(0,0), limits = c(0,12)) +
  scale_colour_manual(values = c("red3","blue4")) +
  scale_fill_manual(values   = c("red3","lightskyblue1")) +
  theme_test() +
  theme(axis.title      = element_text(size = 17),
        axis.text       = element_text(size = 12),
        axis.ticks.x    = element_blank(),
        legend.text     = element_text(size = 14),
        legend.position = "none",
        plot.margin     = margin(3, 0, 15, 0))

P02 <- P00 / P01 + patchwork::plot_layout(ncol = 1)
ggsave("../output/other_illustr/graphs/Suppl.Fig06.png",
       P02, width = 9, height = 9, units = "in", dpi = 200)

elapsed <- proc.time()[["elapsed"]] - t0
write_report("S06_size_class_comparison.R",
  "Supp Fig 6: predicted vs census farm count and cropland ha by country and size class",
  inputs = list(
    "Sarah comparison RDS" = "../data/processed/summarized_farm_area_ha_per_class_vs_sarah.rds",
    "Divergence table"     = "Suppl.Fig06_divergence_table.rds"
  ),
  outputs = list("Suppl.Fig06.png" = "../output/other_illustr/graphs/Suppl.Fig06.png"),
  sections = list(
    "Data dimensions" = c(
      paste("comp_fsize_classes_nb:", nrow(comp_fsize_classes_nb), "rows"),
      paste("comp_fsize_classes_ha:", nrow(comp_fsize_classes_ha), "rows"),
      paste("div_table countries:", nrow(div_table))
    )
  ),
  elapsed_sec = elapsed)
message("S06 done in ", round(elapsed, 1), "s")
