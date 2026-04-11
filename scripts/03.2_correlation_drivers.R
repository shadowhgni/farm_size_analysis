# ==============================================================================
# Script: 03.2_correlation_drivers.R
# Purpose: Analyze correlations between predictor variables
# Reads:  data/processed/lsms_trimmed_95th_africa.rds
# Writes: output/other_illustr/tables/correlation_matrix.csv
#         output/other_illustr/graphs/correlation_matrix.png
# ==============================================================================

source("00_report_utils.R"); t0 <- proc.time()[["elapsed"]]
require(tidyverse); rm(list = setdiff(ls(), c("t0","write_report","capture_output","ci_trees","ci_folds")))
setwd(paste0(here::here(), "/scripts"))
dir.create("../output/other_illustr/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("../output/other_illustr/graphs", recursive = TRUE, showWarnings = FALSE)

lsms <- readRDS("../data/processed/lsms_trimmed_95th_africa.rds")
pred_cols <- c("cropland","cattle","pop","cropland_per_capita",
               "sand","slope","temperature","rainfall","maizeyield","market")
pred_cols <- intersect(pred_cols, names(lsms))
num_data  <- lsms[, c("farm_area_ha", pred_cols)]
num_data  <- num_data[complete.cases(num_data), ]

# Correlation matrix
cor_mat <- round(cor(num_data, use = "pairwise.complete.obs"), 3)
write.csv(as.data.frame(cor_mat),
          "../output/other_illustr/tables/correlation_matrix.csv")

# Plot — ggplot2 tile (corrplot not required)
cor_long <- as.data.frame(as.table(cor_mat)) |>
  setNames(c("Var1","Var2","r")) |>
  dplyr::filter(as.integer(Var1) >= as.integer(Var2))
P <- ggplot(cor_long, aes(Var1, Var2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(r, 2)), size = 2.5) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       midpoint = 0, limits = c(-1,1)) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Predictor correlation matrix", x = NULL, y = NULL, fill = "r")
ggsave("../output/other_illustr/graphs/correlation_matrix.png",
       P, width = 8, height = 7, units = "in", dpi = 150)

elapsed <- proc.time()[["elapsed"]] - t0
write_report("03.2_correlation_drivers.R", "Predictor correlation analysis",
  inputs  = list("95th trim RDS" = "../data/processed/lsms_trimmed_95th_africa.rds"),
  outputs = list("Correlation CSV" = "../output/other_illustr/tables/correlation_matrix.csv",
                 "Correlation PNG" = "../output/other_illustr/graphs/correlation_matrix.png"),
  sections = list("Correlation with farm_area_ha" =
    capture_output(print(sort(cor_mat["farm_area_ha",], decreasing = TRUE)))),
  elapsed_sec = elapsed)
message("03.2 done in ", round(elapsed,1), "s")
