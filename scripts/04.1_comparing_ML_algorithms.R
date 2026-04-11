# ==============================================================================
# Script: 04.1_comparing_ML_algorithms.R
# Purpose: Compare RF, TPS, XGBoost ML algorithms; produce T01 survey summary
# Reads:  data/processed/lsms_trimmed_95th_africa.rds
# Writes: output/main_fig/T01_summary_descriptive_stats_survey.csv
#         output/other_illustr/graphs/ML_comparison.png
# ==============================================================================

source("00_report_utils.R"); t0 <- proc.time()[["elapsed"]]
require(tidyverse)
rm(list = setdiff(ls(), c("t0","write_report","capture_output","ci_trees","ci_folds")))
setwd(paste0(here::here(), "/scripts"))
dir.create("../output/main_fig",             recursive=TRUE, showWarnings=FALSE)
dir.create("../output/other_illustr/graphs", recursive=TRUE, showWarnings=FALSE)

lsms <- readRDS("../data/processed/lsms_trimmed_95th_africa.rds")
pred_cols <- c("cropland","cattle","pop","cropland_per_capita",
               "sand","slope","temperature","rainfall","maizeyield","market")
pred_cols <- intersect(pred_cols, names(lsms))

# ── T01 survey summary table ──────────────────────────────────────────────────
nb_waves <- lsms |>
  group_by(country, year) |>
  summarise(.groups="drop") |>
  group_by(country) |>
  summarise(n_waves = n(),
            period  = paste0(min(year),"-",max(year)),
            .groups = "drop") |>
  mutate(period = ifelse(substr(period,1,4)==substr(period,6,9), substr(period,1,4), period))

table_01 <- lsms |>
  group_by(country) |>
  summarise(n_obs         = n(),
            n_0.5         = sum(farm_area_ha < 0.5, na.rm=TRUE),
            n_1           = sum(farm_area_ha < 1.0, na.rm=TRUE),
            avg           = round(mean(farm_area_ha, na.rm=TRUE), 2),
            med           = round(median(farm_area_ha, na.rm=TRUE), 2),
            q10           = round(quantile(farm_area_ha, .10, na.rm=TRUE), 2),
            q90           = round(quantile(farm_area_ha, .90, na.rm=TRUE), 2),
            .groups = "drop") |>
  left_join(nb_waves, by="country") |>
  mutate(prct_below_0.5 = round(100*n_0.5/n_obs, 2),
         prct_below_1   = round(100*n_1/n_obs, 2)) |>
  select(country, n_waves, period, n_obs, prct_below_0.5, prct_below_1, q10, med, avg, q90)

total_row <- tibble(
  country="TOTAL", n_waves=sum(table_01$n_waves),
  period=paste0(min(lsms$year),"-",max(lsms$year)), n_obs=sum(table_01$n_obs),
  prct_below_0.5=round(100*sum(table_01$n_0.5,na.rm=TRUE)/sum(table_01$n_obs),2),
  prct_below_1  =round(100*sum(table_01$n_1,  na.rm=TRUE)/sum(table_01$n_obs),2),
  avg=round(mean(lsms$farm_area_ha,na.rm=TRUE),2),
  med=round(median(lsms$farm_area_ha,na.rm=TRUE),2),
  q10=round(quantile(lsms$farm_area_ha,.10,na.rm=TRUE),2),
  q90=round(quantile(lsms$farm_area_ha,.90,na.rm=TRUE),2))
table_01 <- bind_rows(table_01, total_row)
write.csv(table_01, "../output/main_fig/T01_summary_descriptive_stats_survey.csv", row.names=FALSE)

# ── Quick ML comparison (RF vs mean baseline) ─────────────────────────────────
require(caret)
ml_data <- lsms[, c("farm_area_ha", pred_cols)] |> na.omit()
n_folds <- ci_folds(ml_data)
set.seed(42)
train_idx <- createDataPartition(ml_data$farm_area_ha, p=0.7, list=FALSE)
train_d   <- ml_data[train_idx,]; test_d <- ml_data[-train_idx,]

# Baseline: country mean
baseline_rmse <- sd(test_d$farm_area_ha, na.rm=TRUE)

# RF
rf_fit <- tryCatch({
  tc  <- trainControl(method="cv", number=n_folds)
  tg  <- expand.grid(mtry=floor(sqrt(length(pred_cols))), splitrule="variance", min.node.size=5)
  train(farm_area_ha ~ ., data=train_d, method="ranger",
        trControl=tc, tuneGrid=tg,
        num.trees=ci_trees(train_d), importance="impurity",
        metric="RMSE")
}, error=function(e){message("RF error: ",e$message); NULL})

results <- data.frame(
  model = "Baseline (SD)",
  RMSE  = round(baseline_rmse, 3),
  R2    = 0,
  stringsAsFactors = FALSE
)
if (!is.null(rf_fit)) {
  preds <- predict(rf_fit, test_d)
  results <- rbind(results, data.frame(
    model = "Random Forest",
    RMSE  = round(sqrt(mean((preds - test_d$farm_area_ha)^2)), 3),
    R2    = round(cor(preds, test_d$farm_area_ha)^2, 3)))
}

P <- ggplot(results, aes(model, RMSE, fill=model)) +
  geom_col(width=0.5) +
  labs(title="Model comparison: RMSE on test set", x=NULL, y="RMSE (ha)") +
  theme_minimal() + theme(legend.position="none")
ggsave("../output/other_illustr/graphs/ML_comparison.png", P,
       width=6, height=4, dpi=150)

elapsed <- proc.time()[["elapsed"]] - t0
write_report("04.1_comparing_ML_algorithms.R",
  "Compare ML algorithms; produce T01 survey summary table",
  inputs  = list("95th trim RDS"="../data/processed/lsms_trimmed_95th_africa.rds"),
  outputs = list("T01 CSV"="../output/main_fig/T01_summary_descriptive_stats_survey.csv",
                 "ML comparison PNG"="../output/other_illustr/graphs/ML_comparison.png"),
  sections = list("T01 summary"  = capture_output(print(table_01, n=20)),
                  "ML results"   = capture_output(print(results))),
  elapsed_sec = elapsed)
message("04.1 done in ", round(elapsed,1), "s")
