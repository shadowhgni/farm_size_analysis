# ==============================================================================
# Script: 04.3_RF_between_countries.R
# Purpose: Cross-country RF transferability (train on N-1, predict on 1)
# Reads:  data/processed/lsms_trimmed_95th_africa.rds
# Writes: output/other_illustr/tables/country_auto_evaluation_rsquares.csv
#         output/other_illustr/tables/country_pairwise_point_based_cross_validation.csv
# ==============================================================================

source("00_report_utils.R"); t0 <- proc.time()[["elapsed"]]
require(tidyverse); require(ranger)
rm(list = setdiff(ls(), c("t0","write_report","capture_output","ci_trees","ci_folds")))
setwd(paste0(here::here(), "/scripts"))

lsms <- readRDS("../data/processed/lsms_trimmed_95th_africa.rds")
pred_cols <- intersect(c("cropland","cattle","pop","cropland_per_capita",
               "sand","slope","temperature","rainfall","maizeyield","market"), names(lsms))
countries <- unique(lsms$country)

run_loo <- function(test_cty) {
  train <- lsms[lsms$country != test_cty, c("farm_area_ha",pred_cols)] |> na.omit()
  test  <- lsms[lsms$country == test_cty, c("farm_area_ha",pred_cols)] |> na.omit()
  if (nrow(train) < 50 || nrow(test) < 5) return(NA_real_)
  rf <- tryCatch(ranger::ranger(farm_area_ha ~ ., data=train,
    num.trees=ci_trees(train), importance="none"), error=function(e) NULL)
  if (is.null(rf)) return(NA_real_)
  preds <- predict(rf, test)$predictions
  cor(preds, test$farm_area_ha, use="complete.obs")^2
}

message("Leave-one-country-out RF...")
loo_rsq <- sapply(countries, function(c) {
  r <- tryCatch(run_loo(c), error=function(e) NA_real_)
  message("  ", c, ": R2 = ", round(r,3))
  r
})

cty_auto <- data.frame(country=countries, rsq=round(loo_rsq,3), stringsAsFactors=FALSE)
write.csv(cty_auto, "../output/other_illustr/tables/country_auto_evaluation_rsquares.csv", row.names=FALSE)

# Pairwise (sub-sampled for CI speed)
message("Pairwise RF evaluation (sampled)...")
pairs <- expand.grid(train_country=countries, test_country=countries, stringsAsFactors=FALSE)
pairs$rf1_test_rsq <- NA_real_; pairs$rf2_test_rsq <- NA_real_

for (i in seq_len(nrow(pairs))) {
  tr <- pairs$train_country[i]; te <- pairs$test_country[i]
  if (tr == te) { pairs$rf1_test_rsq[i] <- loo_rsq[tr]; next }
  train <- lsms[lsms$country==tr, c("farm_area_ha",pred_cols)] |> na.omit()
  test  <- lsms[lsms$country==te, c("farm_area_ha",pred_cols)] |> na.omit()
  if (nrow(train)<30 || nrow(test)<3) next
  rf <- tryCatch(ranger::ranger(farm_area_ha~., data=train,
    num.trees=ci_trees(train)), error=function(e) NULL)
  if (is.null(rf)) next
  p <- predict(rf, test)$predictions
  pairs$rf1_test_rsq[i] <- round(cor(p, test$farm_area_ha, use="complete.obs")^2, 3)
}
pairs$rf2_test_rsq <- pairs$rf1_test_rsq  # second model same for now
pairs$rsq <- pairs$rf1_test_rsq
write.csv(pairs, "../output/other_illustr/tables/country_pairwise_point_based_cross_validation.csv", row.names=FALSE)

elapsed <- proc.time()[["elapsed"]] - t0
write_report("04.3_RF_between_countries.R",
  "Cross-country RF transferability",
  inputs  = list("95th trim RDS"="../data/processed/lsms_trimmed_95th_africa.rds"),
  outputs = list(
    "Auto eval"  = "../output/other_illustr/tables/country_auto_evaluation_rsquares.csv",
    "Pairwise"   = "../output/other_illustr/tables/country_pairwise_point_based_cross_validation.csv"),
  sections = list("LOO R2 by country" = capture_output(print(cty_auto))),
  elapsed_sec = elapsed)
message("04.3 done in ", round(elapsed,1), "s")
