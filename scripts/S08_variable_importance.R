# ==============================================================================
# Script: S08_variable_importance.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Supplementary Figure 8 — RF variable importance
#          Panel A: lollipop chart — overall relative importance (ExtraTrees)
#          Panel B: heatmap — per-country importance rank (LOO RF)
#
# PRODUCTION inputs:
#   ../data/processed/cross_validation_graphs.rds  ($var_importance_table)
#   ../output/other_illustr/tables/etr_variable_importance.csv
#
# CI: stubs from 04.5 and 06.1.py
# Output: ../output/other_illustr/graphs/Suppl.Fig08.png
# ==============================================================================

source("00_report_utils.R")
t0 <- proc.time()[["elapsed"]]
require(tidyverse)
require(patchwork)
setwd(paste0(here::here(), "/scripts"))
dir.create("../output/other_illustr/graphs", recursive = TRUE, showWarnings = FALSE)

sixteen_countries    <- c("Benin","Burkina","Cote_d_Ivoire","Ethiopia","Ghana","Guinea_Bissau",
                           "Malawi","Mali","Niger","Nigeria","Rwanda","Senegal",
                           "Tanzania","Togo","Uganda","Zambia")
sixteen_country_codes <- c("BEN","BFA","CIV","ETH","GHA","GNB","MWI","MLI",
                            "NER","NGA","RWA","SEN","TZA","TGO","UGA","ZMB")

var_labels <- c(cropland            = "Cropland",
                cattle              = "Cattle density",
                pop                 = "Population density",
                cropland_per_capita = "Cropland per capita",
                sand                = "Sand content",
                slope               = "Terrain slope",
                temperature         = "Temperature",
                rainfall            = "Precipitation",
                maizeyield          = "Water-limited maize yield",
                market              = "Distance to nearest town")

# ── Load data ─────────────────────────────────────────────────────────────────
xx <- readRDS("../data/processed/cross_validation_graphs.rds")
var_importance_table <- xx$var_importance_table |>
  mutate(var = dplyr::recode(var, !!!var_labels)) |>
  inner_join(tibble(country = sixteen_countries, GID_0 = sixteen_country_codes),
             by = "country")
rm(xx)

var_imp <- read.csv("../output/other_illustr/tables/etr_variable_importance.csv") |>
  mutate(Variable = dplyr::recode(Variable, !!!var_labels)) |>
  arrange(-Importance)

# ── Panel A: lollipop — overall importance ────────────────────────────────────
P00 <- ggplot(var_imp, aes(reorder(Variable, Importance), 100 * Importance)) +
  geom_segment(y = 0, aes(yend = 100 * Importance)) +
  geom_point() +
  labs(x = "Feature", y = "Relative importance (%)") +
  coord_flip() +
  geom_text(x = 1.5, y = 19, label = "A)", size = 8) +
  theme_test() +
  theme(axis.title   = element_text(size = 17),
        axis.text    = element_text(size = 13, colour = "grey25"),
        axis.ticks.y = element_blank(),
        title        = element_text(size = 14),
        plot.margin  = margin(3, 5, 20, 3))

# ── Panel B: heatmap — per-country rank ───────────────────────────────────────
P01 <- ggplot(var_importance_table, aes(GID_0, var, fill = rank)) +
  geom_raster() +
  geom_text(aes(label = rank), size = 6) +
  geom_hline(yintercept = seq(0.5, 9.5, by = 1)) +
  geom_vline(xintercept = seq(0.5, 15.5, by = 1)) +
  geom_text(x = 1, y = 11.5, label = "B)", size = 8) +
  labs(x = "Country", y = "Feature", fill = "Rank  ") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_continuous(low = "steelblue1", high = "grey95", breaks = c(1, 5, 10)) +
  coord_cartesian(clip = "off") +
  theme_test() +
  theme(axis.title        = element_text(size = 17),
        axis.text         = element_text(size = 13, colour = "grey25"),
        axis.text.x       = element_text(angle = -90, hjust = 1),
        axis.ticks        = element_blank(),
        legend.text       = element_text(size = 14),
        legend.title      = element_text(size = 15),
        legend.key.width  = unit(0.5, "in"),
        legend.position   = "top",
        legend.justification = "right",
        legend.direction  = "horizontal",
        title             = element_text(size = 14),
        plot.margin       = margin(20, 5, 3, 3))

P02 <- P00 / P01 + patchwork::plot_layout(ncol = 1, heights = c(1, 2))
ggsave("../output/other_illustr/graphs/Suppl.Fig08.png",
       P02, width = 9, height = 9, units = "in", dpi = 200)

# ── Report ────────────────────────────────────────────────────────────────────
elapsed <- proc.time()[["elapsed"]] - t0
write_report(
  "S08_variable_importance.R",
  "Supp Fig 8: RF variable importance — lollipop (ExtraTrees) + per-country rank heatmap",
  inputs  = list(
    "cross_validation_graphs" = "../data/processed/cross_validation_graphs.rds",
    "etr_variable_importance" = "../output/other_illustr/tables/etr_variable_importance.csv"),
  outputs = list("PNG" = "../output/other_illustr/graphs/Suppl.Fig08.png"),
  elapsed_sec = elapsed)
message("S08_variable_importance.R done in ", round(elapsed, 1), "s")
