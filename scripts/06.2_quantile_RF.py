# ==============================================================================
# Script: 06.2_quantile_RF.py
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Fit ExtraTreesQuantileRegressor and predict 100 quantiles over SSA.
#
# PRODUCTION approach (see notebook 06.2.basic_quantile_RF.ipynb):
#   - ExtraTreesQuantileRegressor
#   - params: n_estimators=1500, min_samples_split=49, min_samples_leaf=5,
#             max_features=8, oob_score=True, bootstrap=True, random_state=2024
#   - OOB R² ~ 0.33 on real LSMS data (N~155k farms)
#   - 100 quantiles: np.arange(0.01, 1.01, 0.01)
#   - Output: 100-band raster, band i = quantile (i/100)
#
# CI: same algorithm, n_estimators=50 (fast synthetic data run)
#
# Reads:  ../data/processed/lsms_spatial_with_country_names.csv
#         ../data/processed/stacked_rasters_africa.tif
# Writes: ../data/processed/qrf_100quantiles_predictions_africa.tif
#         ../output/reports/06.2_quantile_RF_report.md
# ==============================================================================

import time, warnings
import numpy as np
import pandas as pd
import rasterio
from pathlib import Path
from datetime import datetime
from quantile_forest import ExtraTreesQuantileRegressor

warnings.filterwarnings("ignore")
t0 = time.time()

# ── Paths ──────────────────────────────────────────────────────────────────────
scripts_dir = Path(__file__).parent
proc        = scripts_dir / "../data/processed"
out_reports = scripts_dir / "../output/reports"
for d in [proc, out_reports]:
    d.mkdir(parents=True, exist_ok=True)

# ── Load data ──────────────────────────────────────────────────────────────────
print("[1] Loading LSMS data")
# Production uses CSV; RDS also accepted
csv_path = proc / "lsms_spatial_with_country_names.csv"
rds_path = proc / "lsms_trimmed_95th_africa.rds"

if csv_path.exists():
    lsms = pd.read_csv(csv_path)
    print(f"    Loaded CSV: {len(lsms):,} rows")
else:
    import pyreadr
    result = pyreadr.read_r(str(rds_path))
    lsms   = result[None] if None in result else result[list(result.keys())[0]]
    print(f"    Loaded RDS: {len(lsms):,} rows")

FEATURES = ["cropland","cattle","pop","cropland_per_capita",
            "sand","slope","temperature","rainfall","maizeyield","market"]
FEATURES = [f for f in FEATURES if f in lsms.columns]

lsms = lsms[["farm_area_ha"] + FEATURES].dropna()
X    = lsms[FEATURES].values
y    = lsms["farm_area_ha"].values
print(f"    {len(lsms):,} farms, {len(FEATURES)} features")

# ── CI vs production scaling ───────────────────────────────────────────────────
CI_MODE = len(lsms) < 5_000
N_TREES = 50 if CI_MODE else 1500
print(f"    Mode: {'CI (fast)' if CI_MODE else 'PRODUCTION'}, n_trees={N_TREES}")

# ── Fit ExtraTreesQuantileRegressor ───────────────────────────────────────────
# Production: min_samples_split=49, min_samples_leaf=5, max_features=8
print("[2] Fitting ExtraTreesQuantileRegressor")
qrf = ExtraTreesQuantileRegressor(
    n_estimators     = N_TREES,
    min_samples_split= 49,
    min_samples_leaf = 5,
    max_features     = min(8, len(FEATURES)),
    oob_score        = True,
    bootstrap        = True,
    random_state     = 2024,
    n_jobs           = -1,
)
qrf.fit(X, y)
print(f"    OOB R²: {qrf.oob_score_:.4f}")

# ── Predict 100 quantiles over SSA raster ────────────────────────────────────
print("[3] Predicting 100 quantiles over raster")
QUANTILES = np.arange(0.01, 1.01, 0.01).tolist()   # 100 levels: 0.01 … 1.00
out_path  = proc / "qrf_100quantiles_predictions_africa.tif"

rast_path = proc / "stacked_rasters_africa.tif"
with rasterio.open(rast_path) as src:
    data       = src.read().astype("float32")
    profile    = src.profile.copy()
    nrow, ncol = src.height, src.width
    band_names = [src.descriptions[i] or f"band_{i}" for i in range(src.count)]

flat     = data.reshape(src.count, -1).T
feat_idx = [band_names.index(f) if f in band_names else 0 for f in FEATURES]
X_grid   = flat[:, feat_idx]
valid    = ~np.any(np.isnan(X_grid), axis=1)
print(f"    Valid pixels: {valid.sum():,} / {len(valid):,}")

# Predict in chunks to manage memory
n_q    = len(QUANTILES)
qpreds = np.full((nrow * ncol, n_q), np.nan, dtype="float32")

CHUNK = 1000
valid_idx = np.where(valid)[0]
for start in range(0, len(valid_idx), CHUNK):
    chunk  = valid_idx[start:start + CHUNK]
    preds  = qrf.predict(X_grid[chunk], quantiles=QUANTILES)   # (n_pts, n_q)
    qpreds[chunk] = np.maximum(0.01, preds).astype("float32")

# Write 100-band raster; band i corresponds to quantile QUANTILES[i-1]
profile.update(count=n_q, dtype="float32", nodata=np.nan)
q_map = qpreds.reshape(nrow, ncol, n_q).transpose(2, 0, 1)   # (n_q, nrow, ncol)

with rasterio.open(out_path, "w", **profile) as dst:
    dst.write(q_map)
    for i, q in enumerate(QUANTILES):
        dst.update_tags(i + 1, name=f"qrf_q{i+1:03d}",
                        quantile=f"{q:.2f}")
print(f"    Written: {out_path}")

# ── Report ─────────────────────────────────────────────────────────────────────
elapsed = time.time() - t0
report  = [
    "# Report: 06.2_quantile_RF.py",
    f"**Generated:** {datetime.utcnow():%Y-%m-%d %H:%M:%S UTC}",
    f"**Elapsed:** {elapsed:.1f}s",
    "",
    "## Model",
    "```",
    f"Algorithm:        ExtraTreesQuantileRegressor",
    f"n_trees:          {N_TREES}",
    f"min_samples_split:{qrf.min_samples_split}",
    f"min_samples_leaf: {qrf.min_samples_leaf}",
    f"max_features:     {qrf.max_features}",
    f"OOB R²:           {qrf.oob_score_:.4f}",
    f"n_obs:            {len(lsms):,}",
    f"n_quantiles:      {n_q}",
    f"output:           {out_path}",
    "```",
]
(out_reports / "06.2_quantile_RF_report.md").write_text("\n".join(report))
print(f"\n06.2 done in {elapsed:.1f}s")
