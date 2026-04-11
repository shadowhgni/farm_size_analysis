# ==============================================================================
# Script: 03.3_descriptive_stats.R
# Purpose: Generate descriptive statistics of LSMS farm size data
# Reads:  data/processed/lsms_trimmed_95th_africa.rds
# Writes: output/other_illustr/graphs/farm_size_distribution.png
#         output/other_illustr/tables/descriptive_stats.csv
# ==============================================================================

source("00_report_utils.R"); t0 <- proc.time()[["elapsed"]]
require(tidyverse); rm(list = setdiff(ls(), c("t0","write_report","capture_output","ci_trees","ci_folds")))
setwd(paste0(here::here(), "/scripts"))
dir.create("../output/other_illustr/graphs", recursive = TRUE, showWarnings = FALSE)

lsms <- readRDS("../data/processed/lsms_trimmed_95th_africa.rds")

stats <- lsms |>
  group_by(country) |>
  summarise(n       = n(),
            mean_ha = round(mean(farm_area_ha, na.rm=TRUE), 3),
            median_ha = round(median(farm_area_ha, na.rm=TRUE), 3),
            sd_ha   = round(sd(farm_area_ha, na.rm=TRUE), 3),
            p10     = round(quantile(farm_area_ha, .10, na.rm=TRUE), 3),
            p25     = round(quantile(farm_area_ha, .25, na.rm=TRUE), 3),
            p75     = round(quantile(farm_area_ha, .75, na.rm=TRUE), 3),
            p90     = round(quantile(farm_area_ha, .90, na.rm=TRUE), 3),
            pct_below_0.5 = round(100*mean(farm_area_ha < 0.5, na.rm=TRUE), 1),
            pct_below_1   = round(100*mean(farm_area_ha < 1.0, na.rm=TRUE), 1),
            .groups = "drop")

write.csv(stats, "../output/other_illustr/tables/descriptive_stats.csv", row.names = FALSE)

P <- ggplot(lsms, aes(farm_area_ha, fill = country)) +
  stat_ecdf(aes(colour = country), geom = "line", linewidth = 0.6, show.legend = FALSE) +
  scale_x_continuous(limits = c(0, 10), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  labs(x = "Farm size (ha)", y = "ECDF", title = "Farm size distribution by country") +
  theme_minimal(base_size = 11)
ggsave("../output/other_illustr/graphs/farm_size_distribution.png", P,
       width = 9, height = 5, dpi = 150)

elapsed <- proc.time()[["elapsed"]] - t0
write_report("03.3_descriptive_stats.R", "Descriptive statistics of LSMS farm size data",
  inputs  = list("95th trim RDS" = "../data/processed/lsms_trimmed_95th_africa.rds"),
  outputs = list("Stats CSV" = "../output/other_illustr/tables/descriptive_stats.csv",
                 "ECDF PNG"  = "../output/other_illustr/graphs/farm_size_distribution.png"),
  sections = list("Summary" = capture_output(print(stats, n=20))),
  elapsed_sec = elapsed)
message("03.3 done in ", round(elapsed,1), "s")
