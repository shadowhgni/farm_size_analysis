# CI Run Log
Run: 24280385759  Commit: 3cb1880e9f428e2a0089c18fcfb410b425574448  Time: Sat Apr 11 10:34:26 UTC 2026

## Raw Output
```

Cleaning output/ and data/processed/ from previous run...
  Done. Directories recreated.

======================================================================
FARM SIZE PREDICTION - FULL SEQUENTIAL PIPELINE TEST
======================================================================
Started: 2026-04-11 10:26:48.257066

Scripts dir: /home/runner/work/farm_size_analysis/farm_size_analysis/scripts

----------------------------------------------------------------------
PHASE 0: Synthetic Data Generation
----------------------------------------------------------------------
[00.3_synthetic_data.R] === FULL Synthetic Data Generation for CI ===
[00.3_synthetic_data.R] 
[00.3_synthetic_data.R] terra 1.9.11
[00.3_synthetic_data.R] terra: OK  dplyr: OK
[00.3_synthetic_data.R] 1. Creating synthetic rasters...
[00.3_synthetic_data.R]    Rasters done.
[00.3_synthetic_data.R]    Yearly rainfall stubs done.
[00.3_synthetic_data.R]    AEZ stub written.
[00.3_synthetic_data.R]    SPAM stubs written.
[00.3_synthetic_data.R] 2. Creating synthetic LSMS survey data...
[00.3_synthetic_data.R]    LSMS CSV + RDS done  (7028 farms).
[00.3_synthetic_data.R] 3. Extracting predictors at farm locations...
[00.3_synthetic_data.R]    Predictor extraction done  (4200 farms in 95th trim).
[00.3_synthetic_data.R] 4. Creating synthetic GADM boundaries...
[00.3_synthetic_data.R]    GADM boundaries done.
[00.3_synthetic_data.R] 5. Creating output table stubs (skipped scripts only)...
[00.3_synthetic_data.R]    Output stubs done.
[00.3_synthetic_data.R] 6. Creating Sarah Lowder xlsx stubs...
[00.3_synthetic_data.R] There were 15 warnings (use warnings() to see them)
[00.3_synthetic_data.R]    Sarah Lowder xlsx stubs done.
[00.3_synthetic_data.R] 7. Creating figure stubs...
[00.3_synthetic_data.R]    Figure stubs done.
[00.3_synthetic_data.R] 8. Creating leave-one stubs...
[00.3_synthetic_data.R]    Leave-one stubs done.
[00.3_synthetic_data.R] 9. Creating country-year raw files...
[00.3_synthetic_data.R]    fsize_distribution_resample_long.rds stub written.
[00.3_synthetic_data.R]    back_transf raster stubs done.
[00.3_synthetic_data.R]    RF model stub done.
[00.3_synthetic_data.R] 
[00.3_synthetic_data.R] ======================================================================
[00.3_synthetic_data.R] SYNTHETIC DATA GENERATION COMPLETE
[00.3_synthetic_data.R] ======================================================================
[00.3_synthetic_data.R]   Farms generated:   7028
[00.3_synthetic_data.R]   After 95th trim:   4200
[00.3_synthetic_data.R]   Countries:         16
[00.3_synthetic_data.R]   Raster layers:     11
[00.3_synthetic_data.R]   Training res:      0.5deg  Prediction res: 5deg
[00.3_synthetic_data.R] 
[00.3_synthetic_data.R]   NOT STUBBED (pipeline scripts will produce):
[00.3_synthetic_data.R]     03.1 -> lsms_trimmed rds + lsms_spatial csv
[00.3_synthetic_data.R]     04.2 -> comparison_ML_models, country_variable_importance
[00.3_synthetic_data.R]     04.3 -> country_auto/pairwise evaluation
[00.3_synthetic_data.R]     04.5 -> leave_one_RF/TPS/cor.rds, cross_validation_graphs.rds
[00.3_synthetic_data.R]     06.1.py -> etr_variable_importance.csv
[00.3_synthetic_data.R]     08.2 -> nb_farms_per_grid_cell.tif
[00.3_synthetic_data.R]     08.3 -> fsize_distribution_resample_long.rds, farm_size_distribution_parms.tif
[00.3_synthetic_data.R]     10.1 -> cropland_stats_per_aez.rds
[00.3_synthetic_data.R]     S06  -> Suppl.Fig06_divergence_table.rds
  ✓ PASS  00_synthetic_data                              (  9.7s)  
----------------------------------------------------------------------
PHASE 1: Install/Download Scripts (skipped in CI)
----------------------------------------------------------------------
  ✓ PASS  00.1_install_packages.R                        (  0.0s)  SKIPPED (download/SLURM/timeout script)
  ✓ PASS  00.2_download_spatial_data.R                   (  0.0s)  SKIPPED (download/SLURM/timeout script)
  ✓ PASS  01.2_chirps_summarize.R                        (  0.0s)  SKIPPED (download/SLURM/timeout script)
  ✓ PASS  02.1_compile_LSMS.R                            (  0.0s)  SKIPPED (download/SLURM/timeout script)
  ✓ PASS  05.2_RF_optimization_summary.R                 (  0.0s)  SKIPPED (download/SLURM/timeout script)
  ✓ PASS  08.1_predictions_by_country.R                  (  0.0s)  SKIPPED (download/SLURM/timeout script)
  ✓ PASS  04.4_RF_model_evaluation.R                     (  0.0s)  SKIPPED (download/SLURM/timeout script)
----------------------------------------------------------------------
PHASE 2: Raw Data Compilation (01.x – 02.x)
----------------------------------------------------------------------
[01.1_chirps_download.R] 
[01.1_chirps_download.R] 
[01.1_chirps_download.R] ======================================================================
[01.1_chirps_download.R] SYNTHETIC DATA GENERATION (Base R)
[01.1_chirps_download.R] ======================================================================
[01.1_chirps_download.R] Started: 2026-04-11 10:26:58.357203
[01.1_chirps_download.R] 
[01.1_chirps_download.R] [1/6] Creating directory structure...
[01.1_chirps_download.R]   Created 20 directories
[01.1_chirps_download.R] 
[01.1_chirps_download.R] [2/6] Setting configuration...
[01.1_chirps_download.R]   Countries: 16
[01.1_chirps_download.R]   Target farms: 5000
[01.1_chirps_download.R] 
[01.1_chirps_download.R] [3/6] Generating synthetic spatial predictor grid...
[01.1_chirps_download.R]   Grid points: 2000
[01.1_chirps_download.R]   Predictors: 13
[01.1_chirps_download.R]   Saved: cattle-glw2010/ (ML predictor)
[01.1_chirps_download.R]   Saved: cattle-du2025/ (Figure 3)
[01.1_chirps_download.R] 
[01.1_chirps_download.R] [4/6] Generating synthetic LSMS farm data...
[01.1_chirps_download.R]   Generated 4291 synthetic farms
[01.1_chirps_download.R]   Countries: 16
[01.1_chirps_download.R] 
[01.1_chirps_download.R] [5/6] Creating analysis-ready datasets...
[01.1_chirps_download.R]   Extracting predictor values at farm locations...
[01.1_chirps_download.R]   Trimmed datasets: 95th (4070), 99th (4236)
[01.1_chirps_download.R] 
[01.1_chirps_download.R] [6/6] Generating descriptive statistics...
[01.1_chirps_download.R] 
[01.1_chirps_download.R] 
[01.1_chirps_download.R] ======================================================================
[01.1_chirps_download.R] SYNTHETIC DATA GENERATION COMPLETE
[01.1_chirps_download.R] ======================================================================
[01.1_chirps_download.R] 
[01.1_chirps_download.R] Generated files:
[01.1_chirps_download.R]   Processed data: 111
[01.1_chirps_download.R]   Output tables:  0
[01.1_chirps_download.R] 
[01.1_chirps_download.R] Data summary:
[01.1_chirps_download.R]   Total farms:    4291
[01.1_chirps_download.R]   Countries:      16
[01.1_chirps_download.R]   Farm size range:0.1-21.18ha
[01.1_chirps_download.R]   Median farm:    1.33ha
[01.1_chirps_download.R] 
[01.1_chirps_download.R] Finished: 2026-04-11 10:27:00.036276
[01.1_chirps_download.R] ======================================================================
  ✓ PASS  01.1_chirps_download.R                         (  1.9s)  
[01.3_chirps_trends.R] Loading required package: terra
[01.3_chirps_trends.R] terra 1.9.11
[01.3_chirps_trends.R] Loading required package: geodata
[01.3_chirps_trends.R] === Loading SSA boundaries ===
[01.3_chirps_trends.R] trying URL 'https://geodata.ucdavis.edu/gadm/gadm3.6/gadm36_adm0_r5_pk.rds'
[01.3_chirps_trends.R] Content type 'unknown' length 711937 bytes (695 KB)
[01.3_chirps_trends.R] ==================================================
[01.3_chirps_trends.R] downloaded 695 KB
[01.3_chirps_trends.R] 
[01.3_chirps_trends.R] SSA countries loaded: 44
[01.3_chirps_trends.R] Created: ../data/raw/spatial/rainfall/rainfall_monthly
[01.3_chirps_trends.R] 
[01.3_chirps_trends.R] === Processing CHIRPS data ===
[01.3_chirps_trends.R] CI: No CHIRPS tifs found — skipping 01.3
  ✓ PASS  01.3_chirps_trends.R                           (  4.1s)  
[01.4_prepare_spatial_layers.R] Loading required package: terra
[01.4_prepare_spatial_layers.R] terra 1.9.11
[01.4_prepare_spatial_layers.R] === Loading yearly rainfall data ===
[01.4_prepare_spatial_layers.R] Found 5 yearly rasters
[01.4_prepare_spatial_layers.R] Loaded raster stack with 5 layers
[01.4_prepare_spatial_layers.R] Years: NA to NA
[01.4_prepare_spatial_layers.R] 
[01.4_prepare_spatial_layers.R] === Calculating long-term statistics ===
[01.4_prepare_spatial_layers.R] Calculating mean...
[01.4_prepare_spatial_layers.R] Calculating standard deviation...
[01.4_prepare_spatial_layers.R] Calculating coefficient of variation...
[01.4_prepare_spatial_layers.R] 
[01.4_prepare_spatial_layers.R] === Generating preview plots ===
[01.4_prepare_spatial_layers.R] 
[01.4_prepare_spatial_layers.R] === Saving outputs ===
[01.4_prepare_spatial_layers.R] Saved: ../data/raw/spatial/rainfall/rainfall_yearly/#_long_term_rainfall_avg.tif
[01.4_prepare_spatial_layers.R] Saved: ../data/raw/spatial/rainfall/rainfall_yearly/#_long_term_rainfall_cv.tif
[01.4_prepare_spatial_layers.R] 
[01.4_prepare_spatial_layers.R] === Summary Statistics ===
[01.4_prepare_spatial_layers.R] Mean Annual Rainfall (mm):
[01.4_prepare_spatial_layers.R]   Min:    396.1
[01.4_prepare_spatial_layers.R]   Median: 1001.7
[01.4_prepare_spatial_layers.R]   Max:    1606.7
[01.4_prepare_spatial_layers.R] 
[01.4_prepare_spatial_layers.R] Rainfall CV:
[01.4_prepare_spatial_layers.R]   Min:    0.024
[01.4_prepare_spatial_layers.R]   Median: 0.277
[01.4_prepare_spatial_layers.R]   Max:    1.035
[01.4_prepare_spatial_layers.R] 
[01.4_prepare_spatial_layers.R]   % area with high variability (CV > 0.3): 42.3%
  ✓ PASS  01.4_prepare_spatial_layers.R                  (  3.1s)  
[02.2_harmonize_farm_area.R] Loading required package: tidyverse
[02.2_harmonize_farm_area.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[02.2_harmonize_farm_area.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[02.2_harmonize_farm_area.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[02.2_harmonize_farm_area.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[02.2_harmonize_farm_area.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[02.2_harmonize_farm_area.R] ✔ purrr     1.2.2     
[02.2_harmonize_farm_area.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[02.2_harmonize_farm_area.R] ✖ dplyr::filter() masks stats::filter()
[02.2_harmonize_farm_area.R] ✖ dplyr::lag()    masks stats::lag()
[02.2_harmonize_farm_area.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[02.2_harmonize_farm_area.R] 
[02.2_harmonize_farm_area.R] ======================================================================
[02.2_harmonize_farm_area.R] PROCESSING: ETHIOPIA
[02.2_harmonize_farm_area.R] ======================================================================
[02.2_harmonize_farm_area.R] 
[02.2_harmonize_farm_area.R] --- Ethiopia 2018 ---
[02.2_harmonize_farm_area.R]   WARNING: Ethiopia 2018 data not found
[02.2_harmonize_farm_area.R] 
[02.2_harmonize_farm_area.R] ======================================================================
[02.2_harmonize_farm_area.R] LSMS COMPILATION COMPLETE
[02.2_harmonize_farm_area.R] ======================================================================
  ✓ PASS  02.2_harmonize_farm_area.R                     (  3.7s)  
[02.3_measured_vs_reported.R]   Mali_2010_raw.csv: 12 farms
[02.3_measured_vs_reported.R]   Mali_2012_raw.csv: 6 farms
[02.3_measured_vs_reported.R]   Mali_2014_raw.csv: 3 farms
[02.3_measured_vs_reported.R]   Mali_2016_raw.csv: 8 farms
[02.3_measured_vs_reported.R]   Mali_2018_raw.csv: 11 farms
[02.3_measured_vs_reported.R]   Mali_2020_raw.csv: 6 farms
[02.3_measured_vs_reported.R]   Niger_2010_raw.csv: 12 farms
[02.3_measured_vs_reported.R]   Niger_2012_raw.csv: 6 farms
[02.3_measured_vs_reported.R]   Niger_2014_raw.csv: 1 farms
[02.3_measured_vs_reported.R]   Niger_2016_raw.csv: 4 farms
[02.3_measured_vs_reported.R]   Niger_2018_raw.csv: 9 farms
[02.3_measured_vs_reported.R]   Niger_2020_raw.csv: 6 farms
[02.3_measured_vs_reported.R]   Nigeria_2010_raw.csv: 55 farms
[02.3_measured_vs_reported.R]   Nigeria_2012_raw.csv: 50 farms
[02.3_measured_vs_reported.R]   Nigeria_2014_raw.csv: 49 farms
[02.3_measured_vs_reported.R]   Nigeria_2016_raw.csv: 56 farms
[02.3_measured_vs_reported.R]   Nigeria_2018_raw.csv: 51 farms
[02.3_measured_vs_reported.R]   Nigeria_2020_raw.csv: 52 farms
[02.3_measured_vs_reported.R]   Rwanda_2010_raw.csv: 50 farms
[02.3_measured_vs_reported.R]   Rwanda_2012_raw.csv: 34 farms
[02.3_measured_vs_reported.R]   Rwanda_2014_raw.csv: 66 farms
[02.3_measured_vs_reported.R]   Rwanda_2016_raw.csv: 61 farms
[02.3_measured_vs_reported.R]   Rwanda_2018_raw.csv: 50 farms
[02.3_measured_vs_reported.R]   Rwanda_2020_raw.csv: 52 farms
[02.3_measured_vs_reported.R]   Senegal_2010_raw.csv: 37 farms
[02.3_measured_vs_reported.R]   Senegal_2012_raw.csv: 34 farms
[02.3_measured_vs_reported.R]   Senegal_2014_raw.csv: 33 farms
[02.3_measured_vs_reported.R]   Senegal_2016_raw.csv: 43 farms
[02.3_measured_vs_reported.R]   Senegal_2018_raw.csv: 33 farms
[02.3_measured_vs_reported.R]   Senegal_2020_raw.csv: 25 farms
[02.3_measured_vs_reported.R]   Tanzania_2010_raw.csv: 61 farms
[02.3_measured_vs_reported.R]   Tanzania_2012_raw.csv: 39 farms
[02.3_measured_vs_reported.R]   Tanzania_2014_raw.csv: 56 farms
[02.3_measured_vs_reported.R]   Tanzania_2016_raw.csv: 61 farms
[02.3_measured_vs_reported.R]   Tanzania_2018_raw.csv: 59 farms
[02.3_measured_vs_reported.R]   Tanzania_2020_raw.csv: 37 farms
[02.3_measured_vs_reported.R]   Togo_2010_raw.csv: 49 farms
[02.3_measured_vs_reported.R]   Togo_2012_raw.csv: 50 farms
[02.3_measured_vs_reported.R]   Togo_2014_raw.csv: 60 farms
[02.3_measured_vs_reported.R]   Togo_2016_raw.csv: 53 farms
[02.3_measured_vs_reported.R]   Togo_2018_raw.csv: 50 farms
[02.3_measured_vs_reported.R]   Togo_2020_raw.csv: 50 farms
[02.3_measured_vs_reported.R]   Uganda_2010_raw.csv: 64 farms
[02.3_measured_vs_reported.R]   Uganda_2012_raw.csv: 47 farms
[02.3_measured_vs_reported.R]   Uganda_2014_raw.csv: 56 farms
[02.3_measured_vs_reported.R]   Uganda_2016_raw.csv: 43 farms
[02.3_measured_vs_reported.R]   Uganda_2018_raw.csv: 47 farms
[02.3_measured_vs_reported.R]   Uganda_2020_raw.csv: 56 farms
[02.3_measured_vs_reported.R]   Zambia_2010_raw.csv: 61 farms
[02.3_measured_vs_reported.R]   Zambia_2012_raw.csv: 63 farms
[02.3_measured_vs_reported.R]   Zambia_2014_raw.csv: 50 farms
[02.3_measured_vs_reported.R]   Zambia_2016_raw.csv: 50 farms
[02.3_measured_vs_reported.R]   Zambia_2018_raw.csv: 42 farms
[02.3_measured_vs_reported.R]   Zambia_2020_raw.csv: 47 farms
[02.3_measured_vs_reported.R] 
[02.3_measured_vs_reported.R] Total plots: 4291
[02.3_measured_vs_reported.R] Total unique farms: 4291
[02.3_measured_vs_reported.R] 
[02.3_measured_vs_reported.R] === Correcting measurement errors ===
[02.3_measured_vs_reported.R] Plots with GPS measurement: 3024 (70.5%)
[02.3_measured_vs_reported.R] 
[02.3_measured_vs_reported.R] === Calculating harmonized plot area ===
[02.3_measured_vs_reported.R] 
[02.3_measured_vs_reported.R] === Aggregating to farm level ===
[02.3_measured_vs_reported.R] Farms excluded (missing plot data): 0
[02.3_measured_vs_reported.R] Farms with complete data: 4291
[02.3_measured_vs_reported.R] Farms with ALL plots measured: 3024 (70.5% of total)
[02.3_measured_vs_reported.R] 
[02.3_measured_vs_reported.R] === Integrating Zambia RALS data ===
[02.3_measured_vs_reported.R] WARNING: Zambia RALS file not found
[02.3_measured_vs_reported.R] Total farms (LSMS + Zambia): 4291
[02.3_measured_vs_reported.R] 
[02.3_measured_vs_reported.R] === Summary Statistics ===
[02.3_measured_vs_reported.R] # A tibble: 16 × 5
[02.3_measured_vs_reported.R]    country       n_farms median_ha mean_ha sd_ha
[02.3_measured_vs_reported.R]    <chr>           <int>     <dbl>   <dbl> <dbl>
[02.3_measured_vs_reported.R]  1 Benin             313      1.22    1.69  1.5 
[02.3_measured_vs_reported.R]  2 Burkina           288      1.38    1.85  1.83
[02.3_measured_vs_reported.R]  3 Cote_d_Ivoire     313      1.39    1.76  1.29
[02.3_measured_vs_reported.R]  4 Ethiopia          313      1.4     1.88  1.82
[02.3_measured_vs_reported.R]  5 Ghana             313      1.33    1.74  1.55
[02.3_measured_vs_reported.R]  6 Guinea_Bissau     272      1.34    1.86  1.87
[02.3_measured_vs_reported.R]  7 Malawi            313      1.39    1.91  1.87
[02.3_measured_vs_reported.R]  8 Mali               46      1.35    2.03  1.87
[02.3_measured_vs_reported.R]  9 Niger              38      1.14    1.42  0.91
[02.3_measured_vs_reported.R] 10 Nigeria           313      1.41    2.12  2.32
[02.3_measured_vs_reported.R] 11 Rwanda            313      1.47    1.97  1.73
[02.3_measured_vs_reported.R] 12 Senegal           205      1.44    1.86  1.55
[02.3_measured_vs_reported.R] 13 Tanzania          313      1.24    1.7   1.61
[02.3_measured_vs_reported.R] 14 Togo              312      1.35    1.72  1.34
[02.3_measured_vs_reported.R] 15 Uganda            313      1.17    1.78  1.62
[02.3_measured_vs_reported.R] 16 Zambia            313      1.25    1.75  1.67
[02.3_measured_vs_reported.R] 
[02.3_measured_vs_reported.R] === Saving outputs ===
[02.3_measured_vs_reported.R] Saved: lsms_number_of_farms_all_inclusive.csv
[02.3_measured_vs_reported.R] Saved: lsms_raw_data.csv
[02.3_measured_vs_reported.R] Saved: lsms_and_zambia.csv
[02.3_measured_vs_reported.R] Saved: lsms_and_zambia.rds
[02.3_measured_vs_reported.R] 
[02.3_measured_vs_reported.R] === Processing Complete ===
  ✓ PASS  02.3_measured_vs_reported.R                    (  1.9s)  
----------------------------------------------------------------------
PHASE 3: Analysis Preparation (03.x)
----------------------------------------------------------------------
[03.1_pooled_data.R] Loading required package: tidyverse
[03.1_pooled_data.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[03.1_pooled_data.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[03.1_pooled_data.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[03.1_pooled_data.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[03.1_pooled_data.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[03.1_pooled_data.R] ✔ purrr     1.2.2     
[03.1_pooled_data.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[03.1_pooled_data.R] ✖ dplyr::filter() masks stats::filter()
[03.1_pooled_data.R] ✖ dplyr::lag()    masks stats::lag()
[03.1_pooled_data.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[03.1_pooled_data.R] Loading required package: terra
[03.1_pooled_data.R] terra 1.9.11
[03.1_pooled_data.R] 
[03.1_pooled_data.R] Attaching package: ‘terra’
[03.1_pooled_data.R] 
[03.1_pooled_data.R] The following object is masked from ‘package:tidyr’:
[03.1_pooled_data.R] 
[03.1_pooled_data.R]     extract
[03.1_pooled_data.R] 
[03.1_pooled_data.R] Raw LSMS loaded: 4291 farms, 16 countries
[03.1_pooled_data.R] After predictor extraction: 3547 farms
[03.1_pooled_data.R] # A tibble: 16 × 6
[03.1_pooled_data.R]    country           n mean_ha med_ha   p10   p90
[03.1_pooled_data.R]    <chr>         <int>   <dbl>  <dbl> <dbl> <dbl>
[03.1_pooled_data.R]  1 Benin           245    1.47   1.21  0.49  2.89
[03.1_pooled_data.R]  2 Burkina         242    1.54   1.3   0.49  2.95
[03.1_pooled_data.R]  3 Cote_d_Ivoire   241    1.64   1.32  0.49  3.31
[03.1_pooled_data.R]  4 Ethiopia        282    1.6    1.34  0.42  3.31
[03.1_pooled_data.R]  5 Ghana           271    1.48   1.25  0.46  2.85
[03.1_pooled_data.R]  6 Guinea_Bissau   192    1.55   1.27  0.5   3.29
[03.1_pooled_data.R]  7 Malawi          236    1.62   1.3   0.5   3.26
[03.1_pooled_data.R]  8 Mali             32    1.64   1.34  0.5   2.87
[03.1_pooled_data.R]  9 Niger            32    1.32   1.14  0.53  2.77
[03.1_pooled_data.R] 10 Nigeria         266    1.62   1.26  0.54  3.21
[03.1_pooled_data.R] 11 Rwanda          220    1.66   1.32  0.45  3.67
[03.1_pooled_data.R] 12 Senegal         145    1.57   1.34  0.53  3.09
[03.1_pooled_data.R] 13 Tanzania        233    1.41   1.14  0.42  2.75
[03.1_pooled_data.R] 14 Togo            261    1.56   1.28  0.5   2.98
[03.1_pooled_data.R] 15 Uganda          248    1.53   1.13  0.45  3.34
[03.1_pooled_data.R] 16 Zambia          217    1.5    1.2   0.42  3.01
[03.1_pooled_data.R] 03.1 done in 3.9s
  ✓ PASS  03.1_pooled_data.R                             (  4.1s)  
[03.2_correlation_drivers.R] Loading required package: tidyverse
[03.2_correlation_drivers.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[03.2_correlation_drivers.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[03.2_correlation_drivers.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[03.2_correlation_drivers.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[03.2_correlation_drivers.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[03.2_correlation_drivers.R] ✔ purrr     1.2.2     
[03.2_correlation_drivers.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[03.2_correlation_drivers.R] ✖ dplyr::filter() masks stats::filter()
[03.2_correlation_drivers.R] ✖ dplyr::lag()    masks stats::lag()
[03.2_correlation_drivers.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[03.2_correlation_drivers.R] 03.2 done in 1.6s
  ✓ PASS  03.2_correlation_drivers.R                     (  1.8s)  
[03.3_descriptive_stats.R] Loading required package: tidyverse
[03.3_descriptive_stats.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[03.3_descriptive_stats.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[03.3_descriptive_stats.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[03.3_descriptive_stats.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[03.3_descriptive_stats.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[03.3_descriptive_stats.R] ✔ purrr     1.2.2     
[03.3_descriptive_stats.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[03.3_descriptive_stats.R] ✖ dplyr::filter() masks stats::filter()
[03.3_descriptive_stats.R] ✖ dplyr::lag()    masks stats::lag()
[03.3_descriptive_stats.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[03.3_descriptive_stats.R] 03.3 done in 1.4s
  ✓ PASS  03.3_descriptive_stats.R                       (  1.7s)  
----------------------------------------------------------------------
PHASE 4: ML Model Training (04.x)
----------------------------------------------------------------------
[04.1_comparing_ML_algorithms.R] Loading required package: tidyverse
[04.1_comparing_ML_algorithms.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[04.1_comparing_ML_algorithms.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[04.1_comparing_ML_algorithms.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[04.1_comparing_ML_algorithms.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[04.1_comparing_ML_algorithms.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[04.1_comparing_ML_algorithms.R] ✔ purrr     1.2.2     
[04.1_comparing_ML_algorithms.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[04.1_comparing_ML_algorithms.R] ✖ dplyr::filter() masks stats::filter()
[04.1_comparing_ML_algorithms.R] ✖ dplyr::lag()    masks stats::lag()
[04.1_comparing_ML_algorithms.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[04.1_comparing_ML_algorithms.R] Warning messages:
[04.1_comparing_ML_algorithms.R] 1: Unknown or uninitialised column: `n_0.5`. 
[04.1_comparing_ML_algorithms.R] 2: Unknown or uninitialised column: `n_1`. 
[04.1_comparing_ML_algorithms.R] Loading required package: caret
[04.1_comparing_ML_algorithms.R] Loading required package: lattice
[04.1_comparing_ML_algorithms.R] 
[04.1_comparing_ML_algorithms.R] Attaching package: ‘caret’
[04.1_comparing_ML_algorithms.R] 
[04.1_comparing_ML_algorithms.R] The following object is masked from ‘package:purrr’:
[04.1_comparing_ML_algorithms.R] 
[04.1_comparing_ML_algorithms.R]     lift
[04.1_comparing_ML_algorithms.R] 
[04.1_comparing_ML_algorithms.R] 04.1 done in 16.9s
  ✓ PASS  04.1_comparing_ML_algorithms.R                 ( 17.2s)  
[04.2_RF_within_country.R] Loading required package: tidyverse
[04.2_RF_within_country.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[04.2_RF_within_country.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[04.2_RF_within_country.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[04.2_RF_within_country.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[04.2_RF_within_country.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[04.2_RF_within_country.R] ✔ purrr     1.2.2     
[04.2_RF_within_country.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[04.2_RF_within_country.R] ✖ dplyr::filter() masks stats::filter()
[04.2_RF_within_country.R] ✖ dplyr::lag()    masks stats::lag()
[04.2_RF_within_country.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[04.2_RF_within_country.R] Loading required package: ranger
[04.2_RF_within_country.R] Running per-country RF CV...
[04.2_RF_within_country.R]   Benin: R2 = 0.011
[04.2_RF_within_country.R]   Burkina: R2 = 0.026
[04.2_RF_within_country.R]   Cote_d_Ivoire: R2 = 0.017
[04.2_RF_within_country.R]   Ethiopia: R2 = 0.017
[04.2_RF_within_country.R]   Ghana: R2 = 0.014
[04.2_RF_within_country.R]   Guinea_Bissau: R2 = 0.047
[04.2_RF_within_country.R]   Malawi: R2 = 0.016
[04.2_RF_within_country.R]   Mali: R2 = 0.168
[04.2_RF_within_country.R]   Niger: R2 = 0.379
[04.2_RF_within_country.R]   Nigeria: R2 = 0.032
[04.2_RF_within_country.R]   Rwanda: R2 = 0.04
[04.2_RF_within_country.R]   Senegal: R2 = 0.053
[04.2_RF_within_country.R]   Tanzania: R2 = 0.027
[04.2_RF_within_country.R]   Togo: R2 = 0.02
[04.2_RF_within_country.R]   Uganda: R2 = 0.037
[04.2_RF_within_country.R]   Zambia: R2 = 0.034
[04.2_RF_within_country.R] 04.2 done in 2.6s
  ✓ PASS  04.2_RF_within_country.R                       (  2.8s)  
[04.3_RF_between_countries.R] Loading required package: tidyverse
[04.3_RF_between_countries.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[04.3_RF_between_countries.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[04.3_RF_between_countries.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[04.3_RF_between_countries.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[04.3_RF_between_countries.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[04.3_RF_between_countries.R] ✔ purrr     1.2.2     
[04.3_RF_between_countries.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[04.3_RF_between_countries.R] ✖ dplyr::filter() masks stats::filter()
[04.3_RF_between_countries.R] ✖ dplyr::lag()    masks stats::lag()
[04.3_RF_between_countries.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[04.3_RF_between_countries.R] Loading required package: ranger
[04.3_RF_between_countries.R] Leave-one-country-out RF...
[04.3_RF_between_countries.R]   Benin: R2 = 0.013
[04.3_RF_between_countries.R]   Burkina: R2 = 0.001
[04.3_RF_between_countries.R]   Cote_d_Ivoire: R2 = 0.004
[04.3_RF_between_countries.R]   Ethiopia: R2 = 0.002
[04.3_RF_between_countries.R]   Ghana: R2 = 0
[04.3_RF_between_countries.R]   Guinea_Bissau: R2 = 0.012
[04.3_RF_between_countries.R]   Malawi: R2 = 0.015
[04.3_RF_between_countries.R]   Mali: R2 = 0.03
[04.3_RF_between_countries.R]   Niger: R2 = 0.066
[04.3_RF_between_countries.R]   Nigeria: R2 = 0.006
[04.3_RF_between_countries.R]   Rwanda: R2 = 0.004
[04.3_RF_between_countries.R]   Senegal: R2 = 0.006
[04.3_RF_between_countries.R]   Tanzania: R2 = 0
[04.3_RF_between_countries.R]   Togo: R2 = 0.005
[04.3_RF_between_countries.R]   Uganda: R2 = 0.01
[04.3_RF_between_countries.R]   Zambia: R2 = 0.003
[04.3_RF_between_countries.R] Pairwise RF evaluation (sampled)...
[04.3_RF_between_countries.R] 04.3 done in 32.7s
  ✓ PASS  04.3_RF_between_countries.R                    ( 32.9s)  
[04.5_cross_country_graphs.R] Loading required package: tidyverse
[04.5_cross_country_graphs.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[04.5_cross_country_graphs.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[04.5_cross_country_graphs.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[04.5_cross_country_graphs.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[04.5_cross_country_graphs.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[04.5_cross_country_graphs.R] ✔ purrr     1.2.2     
[04.5_cross_country_graphs.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[04.5_cross_country_graphs.R] ✖ dplyr::filter() masks stats::filter()
[04.5_cross_country_graphs.R] ✖ dplyr::lag()    masks stats::lag()
[04.5_cross_country_graphs.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[04.5_cross_country_graphs.R] Loading required package: ranger
[04.5_cross_country_graphs.R]   Benin done
[04.5_cross_country_graphs.R]   Burkina done
[04.5_cross_country_graphs.R]   Cote_d_Ivoire done
[04.5_cross_country_graphs.R]   Ethiopia done
[04.5_cross_country_graphs.R]   Ghana done
[04.5_cross_country_graphs.R]   Guinea_Bissau done
[04.5_cross_country_graphs.R]   Malawi done
[04.5_cross_country_graphs.R]   Mali done
[04.5_cross_country_graphs.R]   Niger done
[04.5_cross_country_graphs.R]   Nigeria done
[04.5_cross_country_graphs.R]   Rwanda done
[04.5_cross_country_graphs.R]   Senegal done
[04.5_cross_country_graphs.R]   Tanzania done
[04.5_cross_country_graphs.R]   Togo done
[04.5_cross_country_graphs.R]   Uganda done
[04.5_cross_country_graphs.R]   Zambia done
[04.5_cross_country_graphs.R] Warning message:
[04.5_cross_country_graphs.R] In gzfile(file, "rb") :
[04.5_cross_country_graphs.R]   cannot open compressed file '../output/other_illustr/tables/country_pairwise_point_based_cross_validation.rds', probable reason 'No such file or directory'
[04.5_cross_country_graphs.R] 04.5 done in 91.7s
  ✓ PASS  04.5_cross_country_graphs.R                    ( 91.9s)  
----------------------------------------------------------------------
PHASE 5: RF Optimisation (05.x)
----------------------------------------------------------------------
[05.1_RF_optimization.R] [1] "--------- j = 4 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.447826435154792"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.365606002277904"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.658936466809556"
[05.1_RF_optimization.R] [1] "new_diff is  0.293330464531652"
[05.1_RF_optimization.R] [1] "--------- j = 5 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.447826435154792"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.32485672600799"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.890260102515022"
[05.1_RF_optimization.R] [1] "new_diff is  0.565403376507033"
[05.1_RF_optimization.R] [1] "--------- j = 6 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.447826435154792"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.200162188495399"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.705441541283838"
[05.1_RF_optimization.R] [1] "new_diff is  0.505279352788439"
[05.1_RF_optimization.R] [1] "--------- j = 7 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.447826435154792"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.61627008402608"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.577663763405608"
[05.1_RF_optimization.R] [1] "new_diff is  -0.0386063206204721"
[05.1_RF_optimization.R] [1] "--------- j = 8 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.447826435154792"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.3238751391249"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.639068893245316"
[05.1_RF_optimization.R] [1] "new_diff is  0.315193754120416"
[05.1_RF_optimization.R] [1] "--------- j = 9 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.447826435154792"
[05.1_RF_optimization.R] [1] "cor (a, c) =-0.00160145345013401"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.714279373875542"
[05.1_RF_optimization.R] [1] "new_diff is  0.715880827325676"
[05.1_RF_optimization.R] [1] "--------- j = 10 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.447826435154792"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.353389993285407"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.662461111337991"
[05.1_RF_optimization.R] [1] "new_diff is  0.309071118052584"
[05.1_RF_optimization.R] [1] "========== i = 30 ========"
[05.1_RF_optimization.R] [1] "--------- j = 1 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.426837145431879"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.408557994777174"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.883955971117413"
[05.1_RF_optimization.R] [1] "new_diff is  0.475397976340239"
[05.1_RF_optimization.R] [1] "--------- j = 2 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.426837145431879"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.311728299881828"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.865015498434775"
[05.1_RF_optimization.R] [1] "new_diff is  0.553287198552947"
[05.1_RF_optimization.R] [1] "--------- j = 3 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.426837145431879"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.379495091961926"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.784441804670163"
[05.1_RF_optimization.R] [1] "new_diff is  0.404946712708236"
[05.1_RF_optimization.R] [1] "--------- j = 4 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.426837145431879"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.356347519341537"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.682524457172496"
[05.1_RF_optimization.R] [1] "new_diff is  0.326176937830959"
[05.1_RF_optimization.R] [1] "--------- j = 5 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.426837145431879"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.317494659950196"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.893511933944088"
[05.1_RF_optimization.R] [1] "new_diff is  0.576017273993892"
[05.1_RF_optimization.R] [1] "--------- j = 6 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.426837145431879"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.202669725694547"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.725772266288621"
[05.1_RF_optimization.R] [1] "new_diff is  0.523102540594074"
[05.1_RF_optimization.R] [1] "--------- j = 7 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.426837145431879"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.602492113333214"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.620003666494294"
[05.1_RF_optimization.R] [1] "new_diff is  0.0175115531610801"
[05.1_RF_optimization.R] [1] "--------- j = 8 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.426837145431879"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.322817250072163"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.665482138875679"
[05.1_RF_optimization.R] [1] "new_diff is  0.342664888803516"
[05.1_RF_optimization.R] [1] "--------- j = 9 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.426837145431879"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.0109939096582909"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.739329491901337"
[05.1_RF_optimization.R] [1] "new_diff is  0.728335582243046"
[05.1_RF_optimization.R] [1] "--------- j = 10 --------"
[05.1_RF_optimization.R] [1] "cor (a, b) =0.426837145431879"
[05.1_RF_optimization.R] [1] "cor (a, c) =0.349905772229744"
[05.1_RF_optimization.R] [1] "cor (b, c) =0.682250205545544"
[05.1_RF_optimization.R] [1] "new_diff is  0.3323444333158"
[05.1_RF_optimization.R]          i          j       diff 
[05.1_RF_optimization.R] 18.0000000  9.0000000  0.4939707 
[05.1_RF_optimization.R] [1] "---------- case1 -------------"
[05.1_RF_optimization.R] `geom_smooth()` using formula = 'y ~ x'
[05.1_RF_optimization.R] `geom_smooth()` using formula = 'y ~ x'
[05.1_RF_optimization.R] `geom_smooth()` using formula = 'y ~ x'
[05.1_RF_optimization.R] [1] "---------- case2 -------------"
[05.1_RF_optimization.R] `geom_smooth()` using formula = 'y ~ x'
[05.1_RF_optimization.R] `geom_smooth()` using formula = 'y ~ x'
[05.1_RF_optimization.R] `geom_smooth()` using formula = 'y ~ x'
[05.1_RF_optimization.R] [1] "---------- case3 -------------"
[05.1_RF_optimization.R] `geom_smooth()` using formula = 'y ~ x'
[05.1_RF_optimization.R] `geom_smooth()` using formula = 'y ~ x'
[05.1_RF_optimization.R] `geom_smooth()` using formula = 'y ~ x'
  ✓ PASS  05.1_RF_optimization.R                         (  3.7s)  
[05.3_RF_robustness.R] No RFoptim files - skipping (needs 05.1 outputs)
  ✓ PASS  05.3_RF_robustness.R                           (  0.2s)  
----------------------------------------------------------------------
PHASE 5b: Python ML Scripts
----------------------------------------------------------------------
[06.1_basic_RF_model.py] [1] Loading LSMS data
[06.1_basic_RF_model.py]     3,363 farms, 10 features
[06.1_basic_RF_model.py]     Mode: CI (fast), n_trees=50, cv=3
[06.1_basic_RF_model.py] [2] Fitting ExtraTreesRegressor with GridSearchCV
[06.1_basic_RF_model.py] Fitting 3 folds for each of 1 candidates, totalling 3 fits
[06.1_basic_RF_model.py]     Best params:  {'max_features': 4, 'min_samples_leaf': 10, 'min_samples_split': 5, 'n_estimators': 50}
[06.1_basic_RF_model.py]     CV R²:        -0.0158
[06.1_basic_RF_model.py]     OOB R²:       -0.0189
[06.1_basic_RF_model.py] [3] Variable importance saved
[06.1_basic_RF_model.py]            Variable  Importance
[06.1_basic_RF_model.py]          maizeyield    0.118051
[06.1_basic_RF_model.py]              cattle    0.111592
[06.1_basic_RF_model.py]                 pop    0.111216
[06.1_basic_RF_model.py]         temperature    0.110829
[06.1_basic_RF_model.py]               slope    0.105916
[06.1_basic_RF_model.py]                sand    0.102487
[06.1_basic_RF_model.py]            cropland    0.100325
[06.1_basic_RF_model.py]            rainfall    0.099001
[06.1_basic_RF_model.py]              market    0.097961
[06.1_basic_RF_model.py] cropland_per_capita    0.042622
[06.1_basic_RF_model.py] [4] Saving OOB predictions
[06.1_basic_RF_model.py] [5] Model saved to rf_best_model.pkl
[06.1_basic_RF_model.py] [6] Predicting over raster
[06.1_basic_RF_model.py]     Raster predictions written (10,278 valid pixels)
[06.1_basic_RF_model.py] 
[06.1_basic_RF_model.py] 06.1 done in 1.8s
  ✓ PASS  06.1_basic_RF_model.py                         (  3.2s)  
[06.2_quantile_RF.py] [1] Loading LSMS data
[06.2_quantile_RF.py]     Loaded CSV: 3,363 rows
[06.2_quantile_RF.py]     3,363 farms, 10 features
[06.2_quantile_RF.py]     Mode: CI (fast), n_trees=50
[06.2_quantile_RF.py] [2] Fitting ExtraTreesQuantileRegressor
[06.2_quantile_RF.py]     OOB R²: -0.0246
[06.2_quantile_RF.py] [3] Predicting 100 quantiles over raster
[06.2_quantile_RF.py]     Valid pixels: 10,278 / 14,000
[06.2_quantile_RF.py]     Written: /home/runner/work/farm_size_analysis/farm_size_analysis/scripts/../data/processed/qrf_100quantiles_predictions_africa.tif
[06.2_quantile_RF.py] 
[06.2_quantile_RF.py] 06.2 done in 0.5s
  ✓ PASS  06.2_quantile_RF.py                            (  1.5s)  
----------------------------------------------------------------------
PHASE 6: Quantile RF & Prediction Maps (06.x)
----------------------------------------------------------------------
[06.1_quantile_RF.R] Loading required package: tidyverse
[06.1_quantile_RF.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[06.1_quantile_RF.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[06.1_quantile_RF.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[06.1_quantile_RF.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[06.1_quantile_RF.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[06.1_quantile_RF.R] ✔ purrr     1.2.2     
[06.1_quantile_RF.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[06.1_quantile_RF.R] ✖ dplyr::filter() masks stats::filter()
[06.1_quantile_RF.R] ✖ dplyr::lag()    masks stats::lag()
[06.1_quantile_RF.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[06.1_quantile_RF.R] `summarise()` has regrouped the output.
[06.1_quantile_RF.R] ℹ Summaries were computed grouped by hyper_parameter, val, and splitrule.
[06.1_quantile_RF.R] ℹ Output is grouped by hyper_parameter and val.
[06.1_quantile_RF.R] ℹ Use `summarise(.groups = "drop_last")` to silence this message.
[06.1_quantile_RF.R] ℹ Use `summarise(.by = c(hyper_parameter, val, splitrule))` for per-operation
[06.1_quantile_RF.R]   grouping (`?dplyr::dplyr_by`) instead.
[06.1_quantile_RF.R] `geom_line()`: Each group consists of only one observation.
[06.1_quantile_RF.R] ℹ Do you need to adjust the group aesthetic?
[06.1_quantile_RF.R] `geom_line()`: Each group consists of only one observation.
[06.1_quantile_RF.R] ℹ Do you need to adjust the group aesthetic?
[06.1_quantile_RF.R] Saving 7.5 x 5 in image
[06.1_quantile_RF.R] `geom_line()`: Each group consists of only one observation.
[06.1_quantile_RF.R] ℹ Do you need to adjust the group aesthetic?
[06.1_quantile_RF.R] pdf 
[06.1_quantile_RF.R]   2 
  ✓ PASS  06.1_quantile_RF.R                             (  3.2s)  
[06.3_prediction_maps.R] Loading required package: tidyverse
[06.3_prediction_maps.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[06.3_prediction_maps.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[06.3_prediction_maps.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[06.3_prediction_maps.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[06.3_prediction_maps.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[06.3_prediction_maps.R] ✔ purrr     1.2.2     
[06.3_prediction_maps.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[06.3_prediction_maps.R] ✖ dplyr::filter() masks stats::filter()
[06.3_prediction_maps.R] ✖ dplyr::lag()    masks stats::lag()
[06.3_prediction_maps.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[06.3_prediction_maps.R] OOB R2 (linear): 0
[06.3_prediction_maps.R] Loaded 100-quantile raster (100 bands)
[06.3_prediction_maps.R] Saved: africa_pred_obs.png
[06.3_prediction_maps.R] Saved: africa_log_pred_obs.png
[06.3_prediction_maps.R] Saved: africa_sq_pred_obs.png
[06.3_prediction_maps.R] Warning messages:
[06.3_prediction_maps.R] 1: Removed 904 rows containing non-finite outside the scale range
[06.3_prediction_maps.R] (`stat_density2d_filled()`). 
[06.3_prediction_maps.R] 2: Removed 904 rows containing non-finite outside the scale range
[06.3_prediction_maps.R] (`stat_density2d_filled()`). 
[06.3_prediction_maps.R] 3: Removed 904 rows containing non-finite outside the scale range
[06.3_prediction_maps.R] (`stat_density2d_filled()`). 
[06.3_prediction_maps.R] 06.3_prediction_maps.R complete.
  ✓ PASS  06.3_prediction_maps.R                         (  5.3s)  
[06.4_cropland_sensitivity.R] Loading required package: tidyverse
[06.4_cropland_sensitivity.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[06.4_cropland_sensitivity.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[06.4_cropland_sensitivity.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[06.4_cropland_sensitivity.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[06.4_cropland_sensitivity.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[06.4_cropland_sensitivity.R] ✔ purrr     1.2.2     
[06.4_cropland_sensitivity.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[06.4_cropland_sensitivity.R] ✖ dplyr::filter() masks stats::filter()
[06.4_cropland_sensitivity.R] ✖ dplyr::lag()    masks stats::lag()
[06.4_cropland_sensitivity.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[06.4_cropland_sensitivity.R] 
[06.4_cropland_sensitivity.R] 	Moran I test under randomisation
[06.4_cropland_sensitivity.R] 
[06.4_cropland_sensitivity.R] data:  values  
[06.4_cropland_sensitivity.R] weights: lw    
[06.4_cropland_sensitivity.R] 
[06.4_cropland_sensitivity.R] Moran I statistic standard deviate = 0.23388, p-value = 0.4075
[06.4_cropland_sensitivity.R] alternative hypothesis: greater
[06.4_cropland_sensitivity.R] sample estimates:
[06.4_cropland_sensitivity.R] Moran I statistic       Expectation          Variance 
[06.4_cropland_sensitivity.R]      1.483373e-03     -9.730466e-05      4.567648e-05 
[06.4_cropland_sensitivity.R] 
[06.4_cropland_sensitivity.R] class       : SpatRaster 
[06.4_cropland_sensitivity.R] size        : 100, 140, 1  (nrow, ncol, nlyr)
[06.4_cropland_sensitivity.R] resolution  : 0.5, 0.5  (x, y)
[06.4_cropland_sensitivity.R] extent      : -18, 52, -35, 15  (xmin, xmax, ymin, ymax)
[06.4_cropland_sensitivity.R] coord. ref. : lon/lat WGS 84 (EPSG:4326) 
[06.4_cropland_sensitivity.R] source      : spat_580015c1471d_22528_rG2neQku8ByG59H.tif 
[06.4_cropland_sensitivity.R] varname     : rf_predictions_africa 
[06.4_cropland_sensitivity.R] name        : rf_predictions_africa 
[06.4_cropland_sensitivity.R] min value   :              1.234877 
[06.4_cropland_sensitivity.R] max value   :              2.027411 
[06.4_cropland_sensitivity.R]  rf_predictions_africa
[06.4_cropland_sensitivity.R]  Min.   :1.235        
[06.4_cropland_sensitivity.R]  1st Qu.:1.503        
[06.4_cropland_sensitivity.R]  Median :1.561        
[06.4_cropland_sensitivity.R]  Mean   :1.560        
[06.4_cropland_sensitivity.R]  3rd Qu.:1.617        
[06.4_cropland_sensitivity.R]  Max.   :2.027        
[06.4_cropland_sensitivity.R] null device 
[06.4_cropland_sensitivity.R]           1 
[06.4_cropland_sensitivity.R] null device 
[06.4_cropland_sensitivity.R]           1 
[06.4_cropland_sensitivity.R] Warning message:
[06.4_cropland_sensitivity.R] Removed 1436 rows containing non-finite outside the scale range
[06.4_cropland_sensitivity.R] (`stat_density2d_filled()`). 
[06.4_cropland_sensitivity.R] Warning message:
[06.4_cropland_sensitivity.R] Removed 1436 rows containing non-finite outside the scale range
[06.4_cropland_sensitivity.R] (`stat_density2d_filled()`). 
[06.4_cropland_sensitivity.R] Saving 7.5 x 5 in image
[06.4_cropland_sensitivity.R] Warning message:
[06.4_cropland_sensitivity.R] Removed 1436 rows containing non-finite outside the scale range
[06.4_cropland_sensitivity.R] (`stat_density2d_filled()`). 
[06.4_cropland_sensitivity.R] pdf 
[06.4_cropland_sensitivity.R]   2 
[06.4_cropland_sensitivity.R] Saving 7.5 x 5 in image
[06.4_cropland_sensitivity.R] pdf 
[06.4_cropland_sensitivity.R]   2 
[06.4_cropland_sensitivity.R] Warning message:
[06.4_cropland_sensitivity.R] In e1@pntr$arith_rast(e2@pntr, oper, FALSE, opt) :
[06.4_cropland_sensitivity.R]   GDAL Message 1: /tmp/RtmpIiVyd3/spat_58004437b731_22528_Py3ptIw2UFUXKtt.tif: Metadata exceeding 32000 bytes cannot be written into GeoTIFF. Transferred to PAM instead.
[06.4_cropland_sensitivity.R] Warning message:
[06.4_cropland_sensitivity.R] In e1@pntr$arith_rast(e2@pntr, oper, FALSE, opt) :
[06.4_cropland_sensitivity.R]   GDAL Message 1: /tmp/RtmpIiVyd3/spat_58003a9f043b_22528_8UwxxeZk3QEYL3o.tif: Metadata exceeding 32000 bytes cannot be written into GeoTIFF. Transferred to PAM instead.
[06.4_cropland_sensitivity.R] pdf 
[06.4_cropland_sensitivity.R]   2 
[06.4_cropland_sensitivity.R] pdf 
[06.4_cropland_sensitivity.R]   2 
[06.4_cropland_sensitivity.R] pdf 
[06.4_cropland_sensitivity.R]   2 
  ✓ PASS  06.4_cropland_sensitivity.R                    ( 14.9s)  
----------------------------------------------------------------------
PHASE 7: Predictions & Validation (07.x – 10.x)
----------------------------------------------------------------------
[07.1_QRF_distribution_eval.R] Loading required package: tidyverse
[07.1_QRF_distribution_eval.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[07.1_QRF_distribution_eval.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[07.1_QRF_distribution_eval.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[07.1_QRF_distribution_eval.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[07.1_QRF_distribution_eval.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[07.1_QRF_distribution_eval.R] ✔ purrr     1.2.2     
[07.1_QRF_distribution_eval.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[07.1_QRF_distribution_eval.R] ✖ dplyr::filter() masks stats::filter()
[07.1_QRF_distribution_eval.R] ✖ dplyr::lag()    masks stats::lag()
[07.1_QRF_distribution_eval.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[07.1_QRF_distribution_eval.R] Joining with `by = join_by(source, NAME_0)`
[07.1_QRF_distribution_eval.R] # A tibble: 20 × 4
[07.1_QRF_distribution_eval.R]    source          rank NAME_0                   cropland
[07.1_QRF_distribution_eval.R]    <chr>          <dbl> <fct>                       <dbl>
[07.1_QRF_distribution_eval.R]  1 ESA 2020           1 Central African Republic   123.  
[07.1_QRF_distribution_eval.R]  2 ESA 2020           2 Mozambique                 126.  
[07.1_QRF_distribution_eval.R]  3 ESA 2020           3 Nigeria                    185.  
[07.1_QRF_distribution_eval.R]  4 ESA 2020           4 Senegal                     24.6 
[07.1_QRF_distribution_eval.R]  5 ESA 2020           5 Madagascar                  97.0 
[07.1_QRF_distribution_eval.R]  6 ESA 2020           6 Eritrea                     11.0 
[07.1_QRF_distribution_eval.R]  7 ESA 2020           7 Sierra Leone                14.5 
[07.1_QRF_distribution_eval.R]  8 ESA 2020           8 Togo                         9.88
[07.1_QRF_distribution_eval.R]  9 ESA 2020           9 Lesotho                      6.61
[07.1_QRF_distribution_eval.R] 10 ESA 2020          10 Burundi                      6.21
[07.1_QRF_distribution_eval.R] 11 GEOSURVEY 2015     1 Central African Republic   122.  
[07.1_QRF_distribution_eval.R] 12 GEOSURVEY 2015     2 Nigeria                    180.  
[07.1_QRF_distribution_eval.R] 13 GEOSURVEY 2015     3 Mozambique                 136.  
[07.1_QRF_distribution_eval.R] 14 GEOSURVEY 2015     4 Senegal                     30.0 
[07.1_QRF_distribution_eval.R] 15 GEOSURVEY 2015     5 Sierra Leone                12.9 
[07.1_QRF_distribution_eval.R] 16 GEOSURVEY 2015     6 Eritrea                     10.1 
[07.1_QRF_distribution_eval.R] 17 GEOSURVEY 2015     7 Lesotho                      6.37
[07.1_QRF_distribution_eval.R] 18 GEOSURVEY 2015     8 Togo                        10.2 
[07.1_QRF_distribution_eval.R] 19 GEOSURVEY 2015     9 Madagascar                 101.  
[07.1_QRF_distribution_eval.R] 20 GEOSURVEY 2015    10 Rwanda                       5.44
[07.1_QRF_distribution_eval.R] # A tibble: 20 × 4
[07.1_QRF_distribution_eval.R]    source     rank NAME_0                   cropland
[07.1_QRF_distribution_eval.R]    <chr>     <dbl> <fct>                       <dbl>
[07.1_QRF_distribution_eval.R]  1 SPAM 2017     1 Central African Republic   123.  
[07.1_QRF_distribution_eval.R]  2 SPAM 2017     2 Nigeria                    182.  
[07.1_QRF_distribution_eval.R]  3 SPAM 2017     3 Mozambique                 134.  
[07.1_QRF_distribution_eval.R]  4 SPAM 2017     4 Sierra Leone                16.4 
[07.1_QRF_distribution_eval.R]  5 SPAM 2017     5 Lesotho                      5.13
[07.1_QRF_distribution_eval.R]  6 SPAM 2017     6 Eritrea                     12.7 
[07.1_QRF_distribution_eval.R]  7 SPAM 2017     7 Senegal                     30.1 
[07.1_QRF_distribution_eval.R]  8 SPAM 2017     8 Togo                         9.29
[07.1_QRF_distribution_eval.R]  9 SPAM 2017     9 Madagascar                  94.7 
[07.1_QRF_distribution_eval.R] 10 SPAM 2017    10 Burundi                      5.96
[07.1_QRF_distribution_eval.R] 11 SPAM 2020     1 Central African Republic   122.  
[07.1_QRF_distribution_eval.R] 12 SPAM 2020     2 Mozambique                 127.  
[07.1_QRF_distribution_eval.R] 13 SPAM 2020     3 Nigeria                    180.  
[07.1_QRF_distribution_eval.R] 14 SPAM 2020     4 Eritrea                     10.7 
[07.1_QRF_distribution_eval.R] 15 SPAM 2020     5 Sierra Leone                14.2 
[07.1_QRF_distribution_eval.R] 16 SPAM 2020     6 Senegal                     26.1 
[07.1_QRF_distribution_eval.R] 17 SPAM 2020     7 Madagascar                 101.  
[07.1_QRF_distribution_eval.R] 18 SPAM 2020     8 Lesotho                      5.50
[07.1_QRF_distribution_eval.R] 19 SPAM 2020     9 Togo                        11.7 
[07.1_QRF_distribution_eval.R] 20 SPAM 2020    10 Rwanda                       3.94
[07.1_QRF_distribution_eval.R] Saving 7.87 x 5.91 in image
[07.1_QRF_distribution_eval.R] pdf 
[07.1_QRF_distribution_eval.R]   2 
[07.1_QRF_distribution_eval.R] Saving 7.87 x 5.91 in image
[07.1_QRF_distribution_eval.R] pdf 
[07.1_QRF_distribution_eval.R]   2 
[07.1_QRF_distribution_eval.R] Saving 7.87 x 5.91 in image
[07.1_QRF_distribution_eval.R] pdf 
[07.1_QRF_distribution_eval.R]   2 
  ✓ PASS  07.1_QRF_distribution_eval.R                   ( 17.9s)  
[08.2_generate_virtual_farms.R] `summarise()` has regrouped the output.
[08.2_generate_virtual_farms.R] ℹ Summaries were computed grouped by country and region.
[08.2_generate_virtual_farms.R] ℹ Output is grouped by country.
[08.2_generate_virtual_farms.R] ℹ Use `summarise(.groups = "drop_last")` to silence this message.
[08.2_generate_virtual_farms.R] ℹ Use `summarise(.by = c(country, region))` for per-operation grouping
[08.2_generate_virtual_farms.R]   (`?dplyr::dplyr_by`) instead.
[08.2_generate_virtual_farms.R] Joining with `by = join_by(country, gadm_1)`
[08.2_generate_virtual_farms.R] `summarise()` has regrouped the output.
[08.2_generate_virtual_farms.R] ℹ Summaries were computed grouped by country and gadm_1.
[08.2_generate_virtual_farms.R] ℹ Output is grouped by country.
[08.2_generate_virtual_farms.R] ℹ Use `summarise(.groups = "drop_last")` to silence this message.
[08.2_generate_virtual_farms.R] ℹ Use `summarise(.by = c(country, gadm_1))` for per-operation grouping
[08.2_generate_virtual_farms.R]   (`?dplyr::dplyr_by`) instead.
[08.2_generate_virtual_farms.R] Saving 5 x 5 in image
[08.2_generate_virtual_farms.R] pdf 
[08.2_generate_virtual_farms.R]   2 
[08.2_generate_virtual_farms.R] Saving 5 x 5 in image
[08.2_generate_virtual_farms.R] pdf 
[08.2_generate_virtual_farms.R]   2 
[08.2_generate_virtual_farms.R] Warning message:
[08.2_generate_virtual_farms.R] There was 1 warning in `mutate()`.
[08.2_generate_virtual_farms.R] ℹ In argument: `census_year = as.numeric(substr(census_year, nchar(census_year)
[08.2_generate_virtual_farms.R]   - 3, nchar(census_year)))`.
[08.2_generate_virtual_farms.R] Caused by warning:
[08.2_generate_virtual_farms.R] ! NAs introduced by coercion 
[08.2_generate_virtual_farms.R] Joining with `by = join_by(country)`
[08.2_generate_virtual_farms.R] # A tibble: 1 × 2
[08.2_generate_virtual_farms.R]   estim_nb_farms nb_farms
[08.2_generate_virtual_farms.R]            <dbl>    <dbl>
[08.2_generate_virtual_farms.R] 1           737. 78900000
[08.2_generate_virtual_farms.R] # A tibble: 1 × 2
[08.2_generate_virtual_farms.R]   estim_nb_farms nb_farms
[08.2_generate_virtual_farms.R]            <dbl>    <dbl>
[08.2_generate_virtual_farms.R] 1           737. 78900000
[08.2_generate_virtual_farms.R] [1] 0.72
[08.2_generate_virtual_farms.R] [1] 0.07
[08.2_generate_virtual_farms.R] Warning message:
[08.2_generate_virtual_farms.R] Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
[08.2_generate_virtual_farms.R] ℹ Please use `linewidth` instead. 
[08.2_generate_virtual_farms.R] Warning messages:
[08.2_generate_virtual_farms.R] 1: Removed 4 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_point()`). 
[08.2_generate_virtual_farms.R] 2: Removed 4 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_text()`). 
[08.2_generate_virtual_farms.R] Saving 5 x 5 in image
[08.2_generate_virtual_farms.R] Warning messages:
[08.2_generate_virtual_farms.R] 1: Removed 4 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_point()`). 
[08.2_generate_virtual_farms.R] 2: Removed 4 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_text()`). 
[08.2_generate_virtual_farms.R] pdf 
[08.2_generate_virtual_farms.R]   2 
[08.2_generate_virtual_farms.R] Warning messages:
[08.2_generate_virtual_farms.R] 1: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_point()`). 
[08.2_generate_virtual_farms.R] 2: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_text()`). 
[08.2_generate_virtual_farms.R] Warning messages:
[08.2_generate_virtual_farms.R] 1: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_point()`). 
[08.2_generate_virtual_farms.R] 2: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_text()`). 
[08.2_generate_virtual_farms.R] Saving 5 x 5 in image
[08.2_generate_virtual_farms.R] Warning messages:
[08.2_generate_virtual_farms.R] 1: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_point()`). 
[08.2_generate_virtual_farms.R] 2: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_text()`). 
[08.2_generate_virtual_farms.R] pdf 
[08.2_generate_virtual_farms.R]   2 
[08.2_generate_virtual_farms.R] Joining with `by = join_by(NAME_0)`
[08.2_generate_virtual_farms.R] Warning message:
[08.2_generate_virtual_farms.R] In matrix(as.numeric(xyz), ncol = ncol(xyz), nrow = nrow(xyz)) :
[08.2_generate_virtual_farms.R]   NAs introduced by coercion
[08.2_generate_virtual_farms.R] Saving 5 x 5 in image
[08.2_generate_virtual_farms.R] Warning messages:
[08.2_generate_virtual_farms.R] 1: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_point()`). 
[08.2_generate_virtual_farms.R] 2: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_text()`). 
[08.2_generate_virtual_farms.R] pdf 
[08.2_generate_virtual_farms.R]   2 
[08.2_generate_virtual_farms.R] Saving 5 x 5 in image
[08.2_generate_virtual_farms.R] Warning messages:
[08.2_generate_virtual_farms.R] 1: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_point()`). 
[08.2_generate_virtual_farms.R] 2: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_text()`). 
[08.2_generate_virtual_farms.R] pdf 
[08.2_generate_virtual_farms.R]   2 
[08.2_generate_virtual_farms.R] Saving 5 x 5 in image
[08.2_generate_virtual_farms.R] Warning messages:
[08.2_generate_virtual_farms.R] 1: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_point()`). 
[08.2_generate_virtual_farms.R] 2: Removed 22 rows containing missing values or values outside the scale range
[08.2_generate_virtual_farms.R] (`geom_text()`). 
[08.2_generate_virtual_farms.R] pdf 
[08.2_generate_virtual_farms.R]   2 
[08.2_generate_virtual_farms.R] Joining with `by = join_by(country)`
[08.2_generate_virtual_farms.R] [1] 0.9973789
  ✓ PASS  08.2_generate_virtual_farms.R                  ( 37.3s)  
[08.3_farm_size_classes.R] 1: Removed 147 rows containing non-finite outside the scale range (`stat_ecdf()`). 
[08.3_farm_size_classes.R] 2: Removed 5 rows containing non-finite outside the scale range (`stat_ecdf()`). 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] Warning messages:
[08.3_farm_size_classes.R] 1: Removed 147 rows containing non-finite outside the scale range (`stat_ecdf()`). 
[08.3_farm_size_classes.R] 2: Removed 5 rows containing non-finite outside the scale range (`stat_ecdf()`). 
[08.3_farm_size_classes.R] `summarise()` has regrouped the output.
[08.3_farm_size_classes.R] ℹ Summaries were computed grouped by farm_class and x.
[08.3_farm_size_classes.R] ℹ Output is grouped by farm_class.
[08.3_farm_size_classes.R] ℹ Use `summarise(.groups = "drop_last")` to silence this message.
[08.3_farm_size_classes.R] ℹ Use `summarise(.by = c(farm_class, x))` for per-operation grouping
[08.3_farm_size_classes.R]   (`?dplyr::dplyr_by`) instead.
[08.3_farm_size_classes.R] `summarise()` has regrouped the output.
[08.3_farm_size_classes.R] ℹ Summaries were computed grouped by farm_class and x.
[08.3_farm_size_classes.R] ℹ Output is grouped by farm_class.
[08.3_farm_size_classes.R] ℹ Use `summarise(.groups = "drop_last")` to silence this message.
[08.3_farm_size_classes.R] ℹ Use `summarise(.by = c(farm_class, x))` for per-operation grouping
[08.3_farm_size_classes.R]   (`?dplyr::dplyr_by`) instead.
[08.3_farm_size_classes.R] `summarise()` has regrouped the output.
[08.3_farm_size_classes.R] ℹ Summaries were computed grouped by farm_class and x.
[08.3_farm_size_classes.R] ℹ Output is grouped by farm_class.
[08.3_farm_size_classes.R] ℹ Use `summarise(.groups = "drop_last")` to silence this message.
[08.3_farm_size_classes.R] ℹ Use `summarise(.by = c(farm_class, x))` for per-operation grouping
[08.3_farm_size_classes.R]   (`?dplyr::dplyr_by`) instead.
[08.3_farm_size_classes.R] Saving 7.5 x 5 in image
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] Saving 7.5 x 5 in image
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] Saving 7.5 x 5 in image
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] Warning message:
[08.3_farm_size_classes.R] Computation failed in `stat_density2d_filled()`.
[08.3_farm_size_classes.R] Caused by error in `zero_range()`:
[08.3_farm_size_classes.R] ! `x` must be length 1 or 2 
[08.3_farm_size_classes.R] Saving 7.5 x 5 in image
[08.3_farm_size_classes.R] Warning message:
[08.3_farm_size_classes.R] Computation failed in `stat_density2d_filled()`.
[08.3_farm_size_classes.R] Caused by error in `zero_range()`:
[08.3_farm_size_classes.R] ! `x` must be length 1 or 2 
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] Ignoring unknown labels:
[08.3_farm_size_classes.R] • fill : "Density of datapoints"
[08.3_farm_size_classes.R] Saving 7.5 x 5 in image
[08.3_farm_size_classes.R] Ignoring unknown labels:
[08.3_farm_size_classes.R] • fill : "Density of datapoints"
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] Saving 7.5 x 5 in image
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] Saving 7.5 x 5 in image
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] Saving 7.5 x 5 in image
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] Saving 7.5 x 5 in image
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] (Intercept) 
[08.3_farm_size_classes.R]   0.4587514 
[08.3_farm_size_classes.R]         avg 
[08.3_farm_size_classes.R] -0.02581739 
[08.3_farm_size_classes.R] Saving 7.5 x 5 in image
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
[08.3_farm_size_classes.R] Saving 7.5 x 5 in image
[08.3_farm_size_classes.R] pdf 
[08.3_farm_size_classes.R]   2 
  ✓ PASS  08.3_farm_size_classes.R                       ( 59.4s)  
[09.1_AEZ_characterization.R] Loading required package: tidyverse
[09.1_AEZ_characterization.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[09.1_AEZ_characterization.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[09.1_AEZ_characterization.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[09.1_AEZ_characterization.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[09.1_AEZ_characterization.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[09.1_AEZ_characterization.R] ✔ purrr     1.2.2     
[09.1_AEZ_characterization.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[09.1_AEZ_characterization.R] ✖ dplyr::filter() masks stats::filter()
[09.1_AEZ_characterization.R] ✖ dplyr::lag()    masks stats::lag()
[09.1_AEZ_characterization.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[09.1_AEZ_characterization.R] Joining with `by = join_by(NAME_0, farm_class)`
[09.1_AEZ_characterization.R] Joining with `by = join_by(NAME_0, farm_class)`
[09.1_AEZ_characterization.R] Joining with `by = join_by(NAME_0, farm_class)`
[09.1_AEZ_characterization.R] Joining with `by = join_by(NAME_0, farm_class)`
[09.1_AEZ_characterization.R] Saving 7.5 x 5 in image
[09.1_AEZ_characterization.R] pdf 
[09.1_AEZ_characterization.R]   2 
[09.1_AEZ_characterization.R] Saving 7.5 x 5 in image
[09.1_AEZ_characterization.R] pdf 
[09.1_AEZ_characterization.R]   2 
[09.1_AEZ_characterization.R] Saving 7.5 x 5 in image
[09.1_AEZ_characterization.R] pdf 
[09.1_AEZ_characterization.R]   2 
[09.1_AEZ_characterization.R] Saving 7.5 x 5 in image
[09.1_AEZ_characterization.R] pdf 
[09.1_AEZ_characterization.R]   2 
  ✓ PASS  09.1_AEZ_characterization.R                    (  9.2s)  
[10.1_prepare_validation_data.R]   2 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 8302 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 8302 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Saving 7.5 x 5 in image
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 8302 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] pdf 
[10.1_prepare_validation_data.R]   2 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 5240 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 5240 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Saving 7.5 x 5 in image
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 5240 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] pdf 
[10.1_prepare_validation_data.R]   2 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 2620 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 2620 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Saving 7.5 x 5 in image
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 2620 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] pdf 
[10.1_prepare_validation_data.R]   2 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 4149 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 4149 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Saving 7.5 x 5 in image
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 4149 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] pdf 
[10.1_prepare_validation_data.R]   2 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 12447 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 12447 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 12447 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Saving 7.5 x 5 in image
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 12447 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] pdf 
[10.1_prepare_validation_data.R]   2 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 7860 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 7860 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Saving 7.5 x 5 in image
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 7860 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] pdf 
[10.1_prepare_validation_data.R]   2 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 12447 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 12447 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Saving 7.5 x 5 in image
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 12447 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] pdf 
[10.1_prepare_validation_data.R]   2 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 7860 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 7860 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] Saving 7.5 x 5 in image
[10.1_prepare_validation_data.R] Warning message:
[10.1_prepare_validation_data.R] Removed 7860 rows containing missing values or values outside the scale range
[10.1_prepare_validation_data.R] (`geom_line()`). 
[10.1_prepare_validation_data.R] pdf 
[10.1_prepare_validation_data.R]   2 
  ✓ PASS  10.1_prepare_validation_data.R                 ( 18.0s)  
[10.2_external_validation.R] $`Democratic Republic of the Congo`
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Congo
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Eritrea
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Ethiopia
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $Gabon
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Ghana
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $Guinea
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Gambia
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $`Guinea-Bissau`
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Kenya
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Liberia
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Madagascar
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Mali
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $Mozambique
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Mauritania
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Malawi
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $Namibia
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Niger
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $Nigeria
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $Rwanda
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $Sudan
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Senegal
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $`Sierra Leone`
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Somalia
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $`South Sudan`
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Eswatini
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Chad
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Togo
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $Tanzania
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $Uganda
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $`South Africa`
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] $Zambia
[10.2_external_validation.R] [1] 0
[10.2_external_validation.R] 
[10.2_external_validation.R] $Zimbabwe
[10.2_external_validation.R] NULL
[10.2_external_validation.R] 
[10.2_external_validation.R] There were 24 warnings (use warnings() to see them)
  ✓ PASS  10.2_external_validation.R                     (  9.7s)  
----------------------------------------------------------------------
PHASE 8: Figures & Supplementary (F/S/T)
----------------------------------------------------------------------
[F01_main_figure1.R] Loading required package: tidyverse
[F01_main_figure1.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[F01_main_figure1.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[F01_main_figure1.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[F01_main_figure1.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[F01_main_figure1.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[F01_main_figure1.R] ✔ purrr     1.2.2     
[F01_main_figure1.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[F01_main_figure1.R] ✖ dplyr::filter() masks stats::filter()
[F01_main_figure1.R] ✖ dplyr::lag()    masks stats::lag()
[F01_main_figure1.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[F01_main_figure1.R] Loading required package: MASS
[F01_main_figure1.R] 
[F01_main_figure1.R] Attaching package: ‘MASS’
[F01_main_figure1.R] 
[F01_main_figure1.R] The following object is masked from ‘package:dplyr’:
[F01_main_figure1.R] 
[F01_main_figure1.R]     select
[F01_main_figure1.R] 
[F01_main_figure1.R] New names:
[F01_main_figure1.R] • `` -> `...1`
[F01_main_figure1.R] • `` -> `...2`
[F01_main_figure1.R] null device 
[F01_main_figure1.R]           1 
[F01_main_figure1.R] F01 done in 4.5s
  ✓ PASS  F01_main_figure1.R                             (  4.7s)  
[F02_main_figure2.R] Loading required package: tidyverse
[F02_main_figure2.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[F02_main_figure2.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[F02_main_figure2.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[F02_main_figure2.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[F02_main_figure2.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[F02_main_figure2.R] ✔ purrr     1.2.2     
[F02_main_figure2.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[F02_main_figure2.R] ✖ dplyr::filter() masks stats::filter()
[F02_main_figure2.R] ✖ dplyr::lag()    masks stats::lag()
[F02_main_figure2.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[F02_main_figure2.R] null device 
[F02_main_figure2.R]           1 
[F02_main_figure2.R] F02 done in 4.3s
  ✓ PASS  F02_main_figure2.R                             (  4.5s)  
[F03_main_figure3.R] Loading required package: tidyverse
[F03_main_figure3.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[F03_main_figure3.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[F03_main_figure3.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[F03_main_figure3.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[F03_main_figure3.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[F03_main_figure3.R] ✔ purrr     1.2.2     
[F03_main_figure3.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[F03_main_figure3.R] ✖ dplyr::filter() masks stats::filter()
[F03_main_figure3.R] ✖ dplyr::lag()    masks stats::lag()
[F03_main_figure3.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[F03_main_figure3.R] null device 
[F03_main_figure3.R]           1 
[F03_main_figure3.R] F03 done in 1.3s
  ✓ PASS  F03_main_figure3.R                             (  1.5s)  
[S01_drivers.R] Loading required package: tidyverse
[S01_drivers.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[S01_drivers.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[S01_drivers.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[S01_drivers.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[S01_drivers.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[S01_drivers.R] ✔ purrr     1.2.2     
[S01_drivers.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[S01_drivers.R] ✖ dplyr::filter() masks stats::filter()
[S01_drivers.R] ✖ dplyr::lag()    masks stats::lag()
[S01_drivers.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[S01_drivers.R] ℹ tmap modes "plot" - "view"
[S01_drivers.R] ℹ toggle with `tmap::ttm()`
[S01_drivers.R] This message is displayed once per session.
[S01_drivers.R] Map saved to ../output/other_illustr/graphs/Suppl.Fig01.png
[S01_drivers.R] Resolution: 1500 by 1050 pixels
[S01_drivers.R] Size: 10 by 7 inches (150 dpi)
[S01_drivers.R] S01_drivers.R done in 10.7s
  ✓ PASS  S01_drivers.R                                  ( 11.0s)  
[S02_cropland_uncertainty.R] Loading required package: tidyverse
[S02_cropland_uncertainty.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[S02_cropland_uncertainty.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[S02_cropland_uncertainty.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[S02_cropland_uncertainty.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[S02_cropland_uncertainty.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[S02_cropland_uncertainty.R] ✔ purrr     1.2.2     
[S02_cropland_uncertainty.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[S02_cropland_uncertainty.R] ✖ dplyr::filter() masks stats::filter()
[S02_cropland_uncertainty.R] ✖ dplyr::lag()    masks stats::lag()
[S02_cropland_uncertainty.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[S02_cropland_uncertainty.R] Loading required package: patchwork
[S02_cropland_uncertainty.R] S02_cropland_uncertainty.R done in 16.1s
  ✓ PASS  S02_cropland_uncertainty.R                     ( 16.3s)  
[S03_aggregate_vs_disaggregate.R] Loading required package: tidyverse
[S03_aggregate_vs_disaggregate.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[S03_aggregate_vs_disaggregate.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[S03_aggregate_vs_disaggregate.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[S03_aggregate_vs_disaggregate.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[S03_aggregate_vs_disaggregate.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[S03_aggregate_vs_disaggregate.R] ✔ purrr     1.2.2     
[S03_aggregate_vs_disaggregate.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[S03_aggregate_vs_disaggregate.R] ✖ dplyr::filter() masks stats::filter()
[S03_aggregate_vs_disaggregate.R] ✖ dplyr::lag()    masks stats::lag()
[S03_aggregate_vs_disaggregate.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[S03_aggregate_vs_disaggregate.R] Loading required package: patchwork
[S03_aggregate_vs_disaggregate.R]           used (Mb) gc trigger  (Mb) max used  (Mb)
[S03_aggregate_vs_disaggregate.R] Ncells 1805945 96.5    2854015 152.5  2854015 152.5
[S03_aggregate_vs_disaggregate.R] Vcells 2546799 19.5    8388608  64.0  4403151  33.6
[S03_aggregate_vs_disaggregate.R] Joining with `by = join_by(source)`
[S03_aggregate_vs_disaggregate.R] S03_aggregate_vs_disaggregate.R done in 29.1s
  ✓ PASS  S03_aggregate_vs_disaggregate.R                ( 30.3s)  
[S04_RF_hyperparameters.R] Loading required package: tidyverse
[S04_RF_hyperparameters.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[S04_RF_hyperparameters.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[S04_RF_hyperparameters.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[S04_RF_hyperparameters.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[S04_RF_hyperparameters.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[S04_RF_hyperparameters.R] ✔ purrr     1.2.2     
[S04_RF_hyperparameters.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[S04_RF_hyperparameters.R] ✖ dplyr::filter() masks stats::filter()
[S04_RF_hyperparameters.R] ✖ dplyr::lag()    masks stats::lag()
[S04_RF_hyperparameters.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[S04_RF_hyperparameters.R] Loading required package: patchwork
[S04_RF_hyperparameters.R]           used (Mb) gc trigger  (Mb) max used  (Mb)
[S04_RF_hyperparameters.R] Ncells 1805945 96.5    2854127 152.5  2854127 152.5
[S04_RF_hyperparameters.R] Vcells 2546799 19.5    8388608  64.0  4403020  33.6
[S04_RF_hyperparameters.R] Joining with `by = join_by(country, gadm_0)`
[S04_RF_hyperparameters.R] Warning message:
[S04_RF_hyperparameters.R] In text.default(6, 7.5, "Model performance by \naggregation level",  :
[S04_RF_hyperparameters.R]   "line" is not a graphical parameter
[S04_RF_hyperparameters.R] null device 
[S04_RF_hyperparameters.R]           1 
[S04_RF_hyperparameters.R] S04_RF_hyperparameters.R done in 7.9s
  ✓ PASS  S04_RF_hyperparameters.R                       (  8.1s)  
[S05_RF_unseen_performance.R] Loading required package: tidyverse
[S05_RF_unseen_performance.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[S05_RF_unseen_performance.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[S05_RF_unseen_performance.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[S05_RF_unseen_performance.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[S05_RF_unseen_performance.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[S05_RF_unseen_performance.R] ✔ purrr     1.2.2     
[S05_RF_unseen_performance.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[S05_RF_unseen_performance.R] ✖ dplyr::filter() masks stats::filter()
[S05_RF_unseen_performance.R] ✖ dplyr::lag()    masks stats::lag()
[S05_RF_unseen_performance.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[S05_RF_unseen_performance.R] Loading required package: patchwork
[S05_RF_unseen_performance.R] Warning message:
[S05_RF_unseen_performance.R] There was 1 warning in `mutate()`.
[S05_RF_unseen_performance.R] ℹ In argument: `mbucket = as.integer(gsub("*.*mbucket\\-", "", gsub("\\.Rds",
[S05_RF_unseen_performance.R]   "", filename)))`.
[S05_RF_unseen_performance.R] Caused by warning:
[S05_RF_unseen_performance.R] ! NAs introduced by coercion 
[S05_RF_unseen_performance.R] Warning messages:
[S05_RF_unseen_performance.R] 1: Removed 10 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_ribbon()`). 
[S05_RF_unseen_performance.R] 2: Removed 2 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_line()`). 
[S05_RF_unseen_performance.R] 3: Removed 2 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_point()`). 
[S05_RF_unseen_performance.R] 4: Removed 10 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_ribbon()`). 
[S05_RF_unseen_performance.R] 5: Removed 2 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_line()`). 
[S05_RF_unseen_performance.R] 6: Removed 2 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_point()`). 
[S05_RF_unseen_performance.R] Warning messages:
[S05_RF_unseen_performance.R] 1: Removed 10 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_ribbon()`). 
[S05_RF_unseen_performance.R] 2: Removed 2 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_line()`). 
[S05_RF_unseen_performance.R] 3: Removed 2 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_point()`). 
[S05_RF_unseen_performance.R] 4: Removed 10 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_ribbon()`). 
[S05_RF_unseen_performance.R] 5: Removed 2 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_line()`). 
[S05_RF_unseen_performance.R] 6: Removed 2 rows containing missing values or values outside the scale range
[S05_RF_unseen_performance.R] (`geom_point()`). 
[S05_RF_unseen_performance.R] S05_RF_unseen_performance.R done in 4s
  ✓ PASS  S05_RF_unseen_performance.R                    (  5.2s)  
[S06_size_class_comparison.R] Loading required package: tidyverse
[S06_size_class_comparison.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[S06_size_class_comparison.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[S06_size_class_comparison.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[S06_size_class_comparison.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[S06_size_class_comparison.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[S06_size_class_comparison.R] ✔ purrr     1.2.2     
[S06_size_class_comparison.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[S06_size_class_comparison.R] ✖ dplyr::filter() masks stats::filter()
[S06_size_class_comparison.R] ✖ dplyr::lag()    masks stats::lag()
[S06_size_class_comparison.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[S06_size_class_comparison.R] Loading required package: patchwork
[S06_size_class_comparison.R] Warning messages:
[S06_size_class_comparison.R] 1: `position_dodge()` requires non-overlapping x intervals. 
[S06_size_class_comparison.R] 2: `position_dodge()` requires non-overlapping x intervals. 
[S06_size_class_comparison.R] S06 done in 2.1s
  ✓ PASS  S06_size_class_comparison.R                    (  2.3s)  
[S07_distribution_parameters.R] Loading required package: tidyverse
[S07_distribution_parameters.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[S07_distribution_parameters.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[S07_distribution_parameters.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[S07_distribution_parameters.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[S07_distribution_parameters.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[S07_distribution_parameters.R] ✔ purrr     1.2.2     
[S07_distribution_parameters.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[S07_distribution_parameters.R] ✖ dplyr::filter() masks stats::filter()
[S07_distribution_parameters.R] ✖ dplyr::lag()    masks stats::lag()
[S07_distribution_parameters.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[S07_distribution_parameters.R] Warning message:
[S07_distribution_parameters.R] [rast] CRS do not match 
[S07_distribution_parameters.R] ℹ tmap modes "plot" - "view"
[S07_distribution_parameters.R] ℹ toggle with `tmap::ttm()`
[S07_distribution_parameters.R] This message is displayed once per session.
[S07_distribution_parameters.R] Map saved to ../output/other_illustr/graphs/Suppl.Fig07.png
[S07_distribution_parameters.R] Resolution: 1050 by 1500 pixels
[S07_distribution_parameters.R] Size: 7 by 10 inches (150 dpi)
[S07_distribution_parameters.R] S07_distribution_parameters.R done in 8.4s
  ✓ PASS  S07_distribution_parameters.R                  (  8.7s)  
[S08_variable_importance.R] Loading required package: tidyverse
[S08_variable_importance.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[S08_variable_importance.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[S08_variable_importance.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[S08_variable_importance.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[S08_variable_importance.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[S08_variable_importance.R] ✔ purrr     1.2.2     
[S08_variable_importance.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[S08_variable_importance.R] ✖ dplyr::filter() masks stats::filter()
[S08_variable_importance.R] ✖ dplyr::lag()    masks stats::lag()
[S08_variable_importance.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[S08_variable_importance.R] Loading required package: patchwork
[S08_variable_importance.R] S08_variable_importance.R done in 1.8s
  ✓ PASS  S08_variable_importance.R                      (  2.0s)  
[T01_area_production_tables.R] Loading required package: tidyverse
[T01_area_production_tables.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[T01_area_production_tables.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[T01_area_production_tables.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[T01_area_production_tables.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[T01_area_production_tables.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[T01_area_production_tables.R] ✔ purrr     1.2.2     
[T01_area_production_tables.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[T01_area_production_tables.R] ✖ dplyr::filter() masks stats::filter()
[T01_area_production_tables.R] ✖ dplyr::lag()    masks stats::lag()
[T01_area_production_tables.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[T01_area_production_tables.R] Total observations: 4291
[T01_area_production_tables.R] Countries: 16
[T01_area_production_tables.R] 
[T01_area_production_tables.R] === T01 Survey Summary Statistics ===
[T01_area_production_tables.R] # A tibble: 17 × 10
[T01_area_production_tables.R]    country    n_waves period n_obs prct_below_0.5 prct_below_1   q10   med   avg
[T01_area_production_tables.R]    <chr>        <int> <chr>  <int>          <dbl>        <dbl> <dbl> <dbl> <dbl>
[T01_area_production_tables.R]  1 Benin            6 2010-…   313           9.58         40.3  0.51  1.22  1.69
[T01_area_production_tables.R]  2 Burkina          6 2010-…   288          11.5          35.4  0.5   1.38  1.85
[T01_area_production_tables.R]  3 Cote_d_Iv…       6 2010-…   313           9.9          31.6  0.51  1.39  1.76
[T01_area_production_tables.R]  4 Ethiopia         6 2010-…   313          11.5          37.4  0.44  1.4   1.88
[T01_area_production_tables.R]  5 Ghana            6 2010-…   313          10.5          36.7  0.49  1.33  1.74
[T01_area_production_tables.R]  6 Guinea_Bi…       6 2010-…   272          10.7          35.7  0.5   1.34  1.86
[T01_area_production_tables.R]  7 Malawi           6 2010-…   313           9.58         34.5  0.52  1.39  1.91
[T01_area_production_tables.R]  8 Mali             6 2010-…    46          10.9          26.1  0.51  1.35  2.03
[T01_area_production_tables.R]  9 Niger            6 2010-…    38          10.5          34.2  0.5   1.14  1.42
[T01_area_production_tables.R] 10 Nigeria          6 2010-…   313           8.63         35.5  0.53  1.41  2.12
[T01_area_production_tables.R] 11 Rwanda           6 2010-…   313          12.1          34.5  0.45  1.47  1.97
[T01_area_production_tables.R] 12 Senegal          6 2010-…   205           8.78         32.7  0.54  1.44  1.86
[T01_area_production_tables.R] 13 Tanzania         6 2010-…   313          12.8          39.3  0.44  1.24  1.7 
[T01_area_production_tables.R] 14 Togo             6 2010-…   312          10.3          33.3  0.49  1.35  1.72
[T01_area_production_tables.R] 15 Uganda           6 2010-…   313          12.1          39.6  0.46  1.17  1.78
[T01_area_production_tables.R] 16 Zambia           6 2010-…   313          10.9          36.4  0.43  1.25  1.75
[T01_area_production_tables.R] 17 TOTAL           96 2010-…  4291          10.7          35.9  0.49  1.32  1.83
[T01_area_production_tables.R] # ℹ 1 more variable: q90 <dbl>
[T01_area_production_tables.R] Saved: ../output/main_fig/T01_summary_descriptive_stats_survey.csv
  ✓ PASS  T01_area_production_tables.R                   (  1.2s)  
[T02_heterogeneity_drivers.R] Loading required package: tidyverse
[T02_heterogeneity_drivers.R] ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
[T02_heterogeneity_drivers.R] ✔ dplyr     1.2.1     ✔ readr     2.2.0
[T02_heterogeneity_drivers.R] ✔ forcats   1.0.1     ✔ stringr   1.6.0
[T02_heterogeneity_drivers.R] ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
[T02_heterogeneity_drivers.R] ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
[T02_heterogeneity_drivers.R] ✔ purrr     1.2.2     
[T02_heterogeneity_drivers.R] ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
[T02_heterogeneity_drivers.R] ✖ dplyr::filter() masks stats::filter()
[T02_heterogeneity_drivers.R] ✖ dplyr::lag()    masks stats::lag()
[T02_heterogeneity_drivers.R] ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
[T02_heterogeneity_drivers.R] 
[T02_heterogeneity_drivers.R] === T02 by AEZ ===
[T02_heterogeneity_drivers.R] # A tibble: 6 × 11
[T02_heterogeneity_drivers.R]   aez       prop_prop_area_< 0.5…¹ prop_prop_area_0.5 -…² prop_prop_area_1 - 2…³
[T02_heterogeneity_drivers.R]   <chr>                      <dbl>                  <dbl>                  <dbl>
[T02_heterogeneity_drivers.R] 1 humid                        3.7                   14.6                   28.7
[T02_heterogeneity_drivers.R] 2 sub-humid                    2.1                   11.3                   34  
[T02_heterogeneity_drivers.R] 3 semi-arid                    2.4                   10.7                   26.5
[T02_heterogeneity_drivers.R] 4 tropical…                    6.4                   21.1                   37.7
[T02_heterogeneity_drivers.R] 5 sub-trop…                    7.2                   25.3                   36.3
[T02_heterogeneity_drivers.R] 6 <NA>                         1.6                    7.2                   23.2
[T02_heterogeneity_drivers.R] # ℹ abbreviated names: ¹​`prop_prop_area_< 0.5 ha`,
[T02_heterogeneity_drivers.R] #   ²​`prop_prop_area_0.5 - 1 ha`, ³​`prop_prop_area_1 - 2 ha`
[T02_heterogeneity_drivers.R] # ℹ 7 more variables: `prop_prop_area_2 - 5 ha` <dbl>,
[T02_heterogeneity_drivers.R] #   `prop_prop_area_> 5 ha` <dbl>, `prop_prop_farms_< 0.5 ha` <dbl>,
[T02_heterogeneity_drivers.R] #   `prop_prop_farms_0.5 - 1 ha` <dbl>, `prop_prop_farms_1 - 2 ha` <dbl>,
[T02_heterogeneity_drivers.R] #   `prop_prop_farms_2 - 5 ha` <dbl>, `prop_prop_farms_> 5 ha` <dbl>
[T02_heterogeneity_drivers.R] 
[T02_heterogeneity_drivers.R] === T02 by Region ===
[T02_heterogeneity_drivers.R] # A tibble: 3 × 11
[T02_heterogeneity_drivers.R]   region  prop_prop_area_< 0.5 h…¹ prop_prop_area_0.5 -…² prop_prop_area_1 - 2…³
[T02_heterogeneity_drivers.R]   <chr>                      <dbl>                  <dbl>                  <dbl>
[T02_heterogeneity_drivers.R] 1 Central                      2.7                   17.4                   35.2
[T02_heterogeneity_drivers.R] 2 Eastern                      4.8                   17.5                   32.9
[T02_heterogeneity_drivers.R] 3 Western                      1.9                    8.6                   24.9
[T02_heterogeneity_drivers.R] # ℹ abbreviated names: ¹​`prop_prop_area_< 0.5 ha`,
[T02_heterogeneity_drivers.R] #   ²​`prop_prop_area_0.5 - 1 ha`, ³​`prop_prop_area_1 - 2 ha`
[T02_heterogeneity_drivers.R] # ℹ 7 more variables: `prop_prop_area_2 - 5 ha` <dbl>,
[T02_heterogeneity_drivers.R] #   `prop_prop_area_> 5 ha` <dbl>, `prop_prop_farms_< 0.5 ha` <dbl>,
[T02_heterogeneity_drivers.R] #   `prop_prop_farms_0.5 - 1 ha` <dbl>, `prop_prop_farms_1 - 2 ha` <dbl>,
[T02_heterogeneity_drivers.R] #   `prop_prop_farms_2 - 5 ha` <dbl>, `prop_prop_farms_> 5 ha` <dbl>
[T02_heterogeneity_drivers.R] Saved T02_heterogeneity_by_aez.csv and T02_heterogeneity_by_region.csv
  ✓ PASS  T02_heterogeneity_drivers.R                    (  1.2s)  

======================================================================
TEST SUMMARY
======================================================================

  Stat   Script                                          Time(s)  Note
  ------------------------------------------------------------------------
  ✓ PASS  00_synthetic_data                                 9.7    
  ✓ PASS  00.1_install_packages.R                           0.0    SKIPPED (download/SLURM/timeout script)
  ✓ PASS  00.2_download_spatial_data.R                      0.0    SKIPPED (download/SLURM/timeout script)
  ✓ PASS  01.2_chirps_summarize.R                           0.0    SKIPPED (download/SLURM/timeout script)
  ✓ PASS  02.1_compile_LSMS.R                               0.0    SKIPPED (download/SLURM/timeout script)
  ✓ PASS  05.2_RF_optimization_summary.R                    0.0    SKIPPED (download/SLURM/timeout script)
  ✓ PASS  08.1_predictions_by_country.R                     0.0    SKIPPED (download/SLURM/timeout script)
  ✓ PASS  04.4_RF_model_evaluation.R                        0.0    SKIPPED (download/SLURM/timeout script)
  ✓ PASS  01.1_chirps_download.R                            1.9    
  ✓ PASS  01.3_chirps_trends.R                              4.1    
  ✓ PASS  01.4_prepare_spatial_layers.R                     3.1    
  ✓ PASS  02.2_harmonize_farm_area.R                        3.7    
  ✓ PASS  02.3_measured_vs_reported.R                       1.9    
  ✓ PASS  03.1_pooled_data.R                                4.1    
  ✓ PASS  03.2_correlation_drivers.R                        1.8    
  ✓ PASS  03.3_descriptive_stats.R                          1.7    
  ✓ PASS  04.1_comparing_ML_algorithms.R                   17.2    
  ✓ PASS  04.2_RF_within_country.R                          2.8    
  ✓ PASS  04.3_RF_between_countries.R                      32.9    
  ✓ PASS  04.5_cross_country_graphs.R                      91.9    
  ✓ PASS  05.1_RF_optimization.R                            3.7    
  ✓ PASS  05.3_RF_robustness.R                              0.2    
  ✓ PASS  06.1_basic_RF_model.py                            3.2    
  ✓ PASS  06.2_quantile_RF.py                               1.5    
  ✓ PASS  06.1_quantile_RF.R                                3.2    
  ✓ PASS  06.3_prediction_maps.R                            5.3    
  ✓ PASS  06.4_cropland_sensitivity.R                      14.9    
  ✓ PASS  07.1_QRF_distribution_eval.R                     17.9    
  ✓ PASS  08.2_generate_virtual_farms.R                    37.3    
  ✓ PASS  08.3_farm_size_classes.R                         59.4    
  ✓ PASS  09.1_AEZ_characterization.R                       9.2    
  ✓ PASS  10.1_prepare_validation_data.R                   18.0    
  ✓ PASS  10.2_external_validation.R                        9.7    
  ✓ PASS  F01_main_figure1.R                                4.7    
  ✓ PASS  F02_main_figure2.R                                4.5    
  ✓ PASS  F03_main_figure3.R                                1.5    
  ✓ PASS  S01_drivers.R                                    11.0    
  ✓ PASS  S02_cropland_uncertainty.R                       16.3    
  ✓ PASS  S03_aggregate_vs_disaggregate.R                  30.3    
  ✓ PASS  S04_RF_hyperparameters.R                          8.1    
  ✓ PASS  S05_RF_unseen_performance.R                       5.2    
  ✓ PASS  S06_size_class_comparison.R                       2.3    
  ✓ PASS  S07_distribution_parameters.R                     8.7    
  ✓ PASS  S08_variable_importance.R                         2.0    
  ✓ PASS  T01_area_production_tables.R                      1.2    
  ✓ PASS  T02_heterogeneity_drivers.R                       1.2    

======================================================================
Total: 46   Passed: 46   Failed: 0   Time: 458s

Report: ../output/reports/full_pipeline_test_report.md

✅ CORE PIPELINE OK (23/23 core scripts passed = 100%)
```

## Pipeline Report
# Farm Size Prediction — Full Pipeline CI Report

**Generated:** 2026-04-11 10:34:26 UTC
**R Version:** R version 4.3.3 (2024-02-29)

## Summary

| Metric | Value |
|--------|-------|
| Total Scripts  | 46 |
| Passed         | 46 |
| Failed         | 0 |
| Total Time     | 457.9s |

## Per-Script Results

| Phase | Script | Status | Time | Note |
|-------|--------|--------|------|------|
| 00 | `00_synthetic_data` | ✅ PASS | 9.7s |  |
| 00.1 | `00.1_install_packages.R` | ✅ PASS | 0s | SKIPPED (download/SLURM/timeout script) |
| 00.2 | `00.2_download_spatial_data.R` | ✅ PASS | 0s | SKIPPED (download/SLURM/timeout script) |
| 01.2 | `01.2_chirps_summarize.R` | ✅ PASS | 0s | SKIPPED (download/SLURM/timeout script) |
| 02.1 | `02.1_compile_LSMS.R` | ✅ PASS | 0s | SKIPPED (download/SLURM/timeout script) |
| 05.2 | `05.2_RF_optimization_summary.R` | ✅ PASS | 0s | SKIPPED (download/SLURM/timeout script) |
| 08.1 | `08.1_predictions_by_country.R` | ✅ PASS | 0s | SKIPPED (download/SLURM/timeout script) |
| 04.4 | `04.4_RF_model_evaluation.R` | ✅ PASS | 0s | SKIPPED (download/SLURM/timeout script) |
| 01.1 | `01.1_chirps_download.R` | ✅ PASS | 1.9s |  |
| 01.3 | `01.3_chirps_trends.R` | ✅ PASS | 4.1s |  |
| 01.4 | `01.4_prepare_spatial_layers.R` | ✅ PASS | 3.1s |  |
| 02.2 | `02.2_harmonize_farm_area.R` | ✅ PASS | 3.7s |  |
| 02.3 | `02.3_measured_vs_reported.R` | ✅ PASS | 1.9s |  |
| 03.1 | `03.1_pooled_data.R` | ✅ PASS | 4.1s |  |
| 03.2 | `03.2_correlation_drivers.R` | ✅ PASS | 1.8s |  |
| 03.3 | `03.3_descriptive_stats.R` | ✅ PASS | 1.7s |  |
| 04.1 | `04.1_comparing_ML_algorithms.R` | ✅ PASS | 17.2s |  |
| 04.2 | `04.2_RF_within_country.R` | ✅ PASS | 2.8s |  |
| 04.3 | `04.3_RF_between_countries.R` | ✅ PASS | 32.9s |  |
| 04.5 | `04.5_cross_country_graphs.R` | ✅ PASS | 91.9s |  |
| 05.1 | `05.1_RF_optimization.R` | ✅ PASS | 3.7s |  |
| 05.3 | `05.3_RF_robustness.R` | ✅ PASS | 0.2s |  |
| 06.1 | `06.1_basic_RF_model.py` | ✅ PASS | 3.2s |  |
| 06.2 | `06.2_quantile_RF.py` | ✅ PASS | 1.5s |  |
| 06.1 | `06.1_quantile_RF.R` | ✅ PASS | 3.2s |  |
| 06.3 | `06.3_prediction_maps.R` | ✅ PASS | 5.3s |  |
| 06.4 | `06.4_cropland_sensitivity.R` | ✅ PASS | 14.9s |  |
| 07.1 | `07.1_QRF_distribution_eval.R` | ✅ PASS | 17.9s |  |
| 08.2 | `08.2_generate_virtual_farms.R` | ✅ PASS | 37.3s |  |
| 08.3 | `08.3_farm_size_classes.R` | ✅ PASS | 59.4s |  |
| 09.1 | `09.1_AEZ_characterization.R` | ✅ PASS | 9.2s |  |
| 10.1 | `10.1_prepare_validation_data.R` | ✅ PASS | 18s |  |
| 10.2 | `10.2_external_validation.R` | ✅ PASS | 9.7s |  |
| F01 | `F01_main_figure1.R` | ✅ PASS | 4.7s |  |
| F02 | `F02_main_figure2.R` | ✅ PASS | 4.5s |  |
| F03 | `F03_main_figure3.R` | ✅ PASS | 1.5s |  |
| S01 | `S01_drivers.R` | ✅ PASS | 11s |  |
| S02 | `S02_cropland_uncertainty.R` | ✅ PASS | 16.3s |  |
| S03 | `S03_aggregate_vs_disaggregate.R` | ✅ PASS | 30.3s |  |
| S04 | `S04_RF_hyperparameters.R` | ✅ PASS | 8.1s |  |
| S05 | `S05_RF_unseen_performance.R` | ✅ PASS | 5.2s |  |
| S06 | `S06_size_class_comparison.R` | ✅ PASS | 2.3s |  |
| S07 | `S07_distribution_parameters.R` | ✅ PASS | 8.7s |  |
| S08 | `S08_variable_importance.R` | ✅ PASS | 2s |  |
| T01 | `T01_area_production_tables.R` | ✅ PASS | 1.2s |  |
| T02 | `T02_heterogeneity_drivers.R` | ✅ PASS | 1.2s |  |
