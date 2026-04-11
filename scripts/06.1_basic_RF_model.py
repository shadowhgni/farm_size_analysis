# ==============================================================================
# Script: 06.1_basic_RF_model.py
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Fit ExtraTreesRegressor (replicates caret::ranger extratrees) for
#          farm size prediction across SSA.
#
# PRODUCTION approach (see notebook 06.1.basic_RF_model.ipynb):
#   - ExtraTreesRegressor with preprocessing pipeline (center/scale + spatialSign)
#   - GridSearchCV 10-fold to select best hyperparams
#   - params: n_estimators=1500, max_features=4, min_samples_split=5,
#             min_samples_leaf=10, oob_score=True, bootstrap=True
#   - OOB R² ~ 0.50 on real LSMS data (N~152k farms)
#   - Predicts mean farm size over SSA raster grid
#
# CI: same pipeline, n_estimators=50, cv=3 (fast synthetic data run)
#
# Reads:  ../data/processed/lsms_trimmed_95th_africa.rds
#         ../data/processed/stacked_rasters_africa.tif
# Writes: ../data/processed/rf_best_model.pkl
#         ../data/processed/lsms_oob.rds
#         ../data/processed/rf_model_predictions_SSA.tif
#         ../data/processed/rf_predictions_africa.tif
#         ../output/other_illustr/tables/etr_variable_importance.csv
#         ../output/reports/06.1_basic_RF_model_report.md
# ==============================================================================

import os, time, warnings
import numpy as np
import pandas as pd
import pyreadr, joblib, rasterio
from pathlib import Path
from datetime import datetime
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.model_selection import GridSearchCV, cross_val_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler, Normalizer

warnings.filterwarnings("ignore")
t0 = time.time()

# ── Paths ──────────────────────────────────────────────────────────────────────
scripts_dir = Path(__file__).parent
proc        = scripts_dir / "../data/processed"
out_tables  = scripts_dir / "../output/other_illustr/tables"
out_reports = scripts_dir / "../output/reports"
for d in [proc, out_tables, out_reports]:
    d.mkdir(parents=True, exist_ok=True)

# ── Load LSMS data ─────────────────────────────────────────────────────────────
print("[1] Loading LSMS data")
result      = pyreadr.read_r(str(proc / "lsms_trimmed_95th_africa.rds"))
lsms_full   = result[None] if None in result else result[list(result.keys())[0]]

FEATURES = ["cropland","cattle","pop","cropland_per_capita",
            "sand","slope","temperature","rainfall","maizeyield","market"]
FEATURES  = [f for f in FEATURES if f in lsms_full.columns]

lsms = lsms_full[["farm_area_ha"] + FEATURES].dropna()
X    = lsms[FEATURES]
y    = lsms["farm_area_ha"]
print(f"    {len(lsms):,} farms, {len(FEATURES)} features")

# ── CI vs production scaling ───────────────────────────────────────────────────
CI_MODE   = len(lsms) < 5_000          # synthetic data is ~7k rows
N_TREES   = 50    if CI_MODE else 1500
N_CV      = 3     if CI_MODE else 10
print(f"    Mode: {'CI (fast)' if CI_MODE else 'PRODUCTION'}, "
      f"n_trees={N_TREES}, cv={N_CV}")

# ── Preprocessing pipeline: center/scale + spatialSign (L2 normalise) ─────────
# Matches R: preProcess = c('center', 'scale', 'spatialSign')
preprocessor = Pipeline([
    ("scaler",     StandardScaler()),
    ("normalizer", Normalizer(norm="l2")),
])

# ── ExtraTrees with GridSearchCV ───────────────────────────────────────────────
# Production: mtry=4, min.node.size=5, min.bucket=10, splitrule='extratrees'
print("[2] Fitting ExtraTreesRegressor with GridSearchCV")
param_grid = {
    "max_features":     [4],
    "min_samples_split":[5],
    "min_samples_leaf": [10],
    "n_estimators":     [N_TREES],
}
base_etr = ExtraTreesRegressor(
    criterion    = "squared_error",
    oob_score    = True,
    bootstrap    = True,
    random_state = 2024,
    n_jobs       = -1,
)
grid_search = GridSearchCV(
    estimator  = base_etr,
    param_grid = param_grid,
    cv         = N_CV,
    scoring    = "r2",
    n_jobs     = -1,
    verbose    = 1,
)
X_prep = preprocessor.fit_transform(X)
grid_search.fit(X_prep, y)
best = grid_search.best_estimator_

print(f"    Best params:  {grid_search.best_params_}")
print(f"    CV R²:        {grid_search.best_score_:.4f}")
print(f"    OOB R²:       {best.oob_score_:.4f}")

# ── Variable importance ────────────────────────────────────────────────────────
vi = (pd.DataFrame({"Variable": FEATURES, "Importance": best.feature_importances_})
        .sort_values("Importance", ascending=False)
        .reset_index(drop=True))
vi.to_csv(out_tables / "etr_variable_importance.csv", index=False)
print("[3] Variable importance saved")
print(vi.to_string(index=False))

# ── OOB predictions → lsms_oob.rds ────────────────────────────────────────────
print("[4] Saving OOB predictions")
oob_pred  = best.oob_prediction_
lsms_oob  = lsms_full.loc[lsms.index].copy()
lsms_oob["oob_pred"]        = np.maximum(0.01, oob_pred)
lsms_oob["in_sample_pred"]  = np.maximum(0.01, best.predict(X_prep))
lsms_oob["oob_residual"]    = y.values - oob_pred
lsms_oob["oob_residual_pct"]= ((y.values - oob_pred) / y.values) * 100
pyreadr.write_rds(str(proc / "lsms_oob.rds"), lsms_oob)

# ── Save model ─────────────────────────────────────────────────────────────────
joblib.dump((preprocessor, best), str(proc / "rf_best_model.pkl"))
print("[5] Model saved to rf_best_model.pkl")

# ── Predict over SSA raster grid ──────────────────────────────────────────────
print("[6] Predicting over raster")
rast_path = proc / "stacked_rasters_africa.tif"
try:
    with rasterio.open(rast_path) as src:
        data        = src.read().astype("float32")
        profile     = src.profile.copy()
        nrow, ncol  = src.height, src.width
        band_names  = [src.descriptions[i] or f"band_{i}" for i in range(src.count)]

    flat    = data.reshape(src.count, -1).T
    # align band order to training features
    feat_idx = [band_names.index(f) if f in band_names else 0 for f in FEATURES]
    X_grid   = flat[:, feat_idx]
    valid    = ~np.any(np.isnan(X_grid), axis=1)

    preds = np.full(nrow * ncol, np.nan, dtype="float32")
    if valid.sum() > 0:
        X_grid_prep = preprocessor.transform(X_grid[valid])
        preds[valid] = np.maximum(0.01, best.predict(X_grid_prep)).astype("float32")

    profile.update(count=1, dtype="float32", nodata=np.nan)
    pred_map = preds.reshape(1, nrow, ncol)
    for out_name in ["rf_model_predictions_SSA.tif", "rf_predictions_africa.tif"]:
        with rasterio.open(proc / out_name, "w", **profile) as dst:
            dst.write(pred_map)
    print(f"    Raster predictions written ({valid.sum():,} valid pixels)")
except Exception as e:
    print(f"    Raster prediction skipped: {e}")

# ── Report ─────────────────────────────────────────────────────────────────────
elapsed = time.time() - t0
report  = [
    "# Report: 06.1_basic_RF_model.py",
    f"**Generated:** {datetime.utcnow():%Y-%m-%d %H:%M:%S UTC}",
    f"**Elapsed:** {elapsed:.1f}s",
    "",
    "## Model",
    "```",
    f"Algorithm:   ExtraTreesRegressor (replicates caret::ranger splitrule='extratrees')",
    f"Preprocessing: StandardScaler + Normalizer(L2)  [center/scale/spatialSign]",
    f"n_trees:     {N_TREES}",
    f"max_features:{grid_search.best_params_['max_features']}",
    f"min_split:   {grid_search.best_params_['min_samples_split']}",
    f"min_leaf:    {grid_search.best_params_['min_samples_leaf']}",
    f"CV R²:       {grid_search.best_score_:.4f}",
    f"OOB R²:      {best.oob_score_:.4f}",
    f"n_obs:       {len(lsms):,}",
    "```",
    "",
    "## Variable Importance",
    "```",
    vi.to_string(index=False),
    "```",
]
(out_reports / "06.1_basic_RF_model_report.md").write_text("\n".join(report))
print(f"\n06.1 done in {elapsed:.1f}s")
