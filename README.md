# 🌍 Farm Size Prediction Across Sub-Saharan Africa

<div align="center">

[![Test Scripts](https://github.com/shadowhgni/farm_size_analysis/actions/workflows/test_scripts.yml/badge.svg)](https://github.com/shadowhgni/farm_size_analysis/actions/workflows/test_scripts.yml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15652768.svg)](https://doi.org/10.5281/zenodo.15652768)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![R](https://img.shields.io/badge/R-%3E%3D4.3-276DC3?logo=r)](https://www.r-project.org/)
[![Python](https://img.shields.io/badge/Python-%3E%3D3.9-3776AB?logo=python)](https://www.python.org/)

**Random Forest and Quantile Regression Forest models to predict farm size distributions and number of farms across Sub-Saharan Africa**

*Trained on ~180,000 LSMS farm surveys from 16 countries · Applied continent-wide at ~10 km resolution*

</div>

---

## 📋 Overview

This project develops machine learning models to characterize the spatial distribution of smallholder farm sizes across Sub-Saharan Africa. Starting from harmonized LSMS household survey data, the pipeline builds Random Forest (RF) and Quantile Regression Forest (QRF) models that predict the full empirical distribution of farm sizes — not just the mean — in every 10×10 km grid cell across the continent.

| | |
|---|---|
| 📐 **Spatial resolution** | 10 × 10 km grid cells |
| 🗺️ **Spatial extent** | Sub-Saharan Africa (continent-wide) |
| 🌾 **Training data** | ~180,000 farms · 16 countries · LSMS surveys |
| 🤖 **Models** | Random Forest · Quantile RF · ExtraTrees |
| 📦 **Languages** | R (statistical modelling, figures) · Python (ML pipeline) |

---

## 🚀 Quick Start

### Option 1 — Run Locally with Synthetic Data

The test suite uses synthetic stubs: randomly generated data that mimic the structure of real inputs. This verifies that the entire pipeline runs correctly without requiring real survey or spatial data.

> ⚠️ Synthetic results are **not scientifically meaningful**. Use real data (Option 3) to reproduce the paper's findings.

```r
# Clone and enter the scripts directory
git clone https://github.com/shadowhgni/farm_size_analysis.git
cd farm_size_analysis/scripts

# Generate synthetic stubs and run the full pipeline (~7 min)
Rscript 00.4_run_all_tests.R
```

### Option 2 — Run with GitHub Actions (No Local Setup)

1. **Fork** this repository
2. Navigate to **Actions** → **Test R Scripts** → **Run workflow**
3. The pipeline runs automatically on every push to `main`

The workflow uses synthetic stub data — no downloads needed.

### Option 3 — Full Pipeline with Real Data

Requires downloading large spatial datasets (see [Data Requirements](#-data-requirements)) and is best run on an **HPC cluster**.

```r
# 1. Install dependencies
source("scripts/00.1_install_packages.R")

# 2. Download spatial layers (~several GB)
Rscript scripts/00.2_download_spatial_data.R

# 3. Run the pipeline sequentially
# (see individual script headers for HPC/SLURM instructions)
```

---

## 📁 Project Structure

```
farm_size_analysis/
├── scripts/
│   ├── 00_report_utils.R              # Shared logging utility
│   ├── 00.1_install_packages.R        # Package installation
│   ├── 00.2_download_spatial_data.R   # Spatial data downloads
│   ├── 00.3_synthetic_data.R          # Synthetic stub generator (CI/testing)
│   ├── 00.4_run_all_tests.R           # Full pipeline test runner
│   │
│   ├── 01.1–01.4_*.R                  # CHIRPS rainfall & spatial layer prep
│   ├── 02.1–02.3_*.R                  # LSMS compilation & harmonization
│   ├── 03.1–03.3_*.R                  # Data pooling & descriptive stats
│   ├── 04.1–04.5_*.R                  # ML algorithm comparison & evaluation
│   ├── 05.1–05.3_*.R                  # RF optimization & robustness
│   ├── 06.1–06.4_*.R                  # QRF models & prediction maps
│   ├── 07.1_*.R                       # Distribution evaluation
│   ├── 08.1–08.3_*.R                  # Country predictions & farm size classes
│   ├── 09.1_*.R                       # AEZ characterization
│   ├── 10.1–10.2_*.R                  # External validation
│   │
│   ├── F01–F03_*.R                    # Main manuscript figures
│   └── S01–S08_*.R  T01–T02_*.R      # Supplementary figures & tables
│
├── data/
│   ├── raw/
│   │   ├── spatial/                   # Downloaded spatial predictor layers
│   │   └── web_scrapped/              # LSMS surveys, FAOSTAT, census data
│   └── processed/                     # Analysis-ready datasets
│
├── output/
│   ├── main_fig/                      # Main figures (Fig.01–03)
│   ├── other_illustr/
│   │   ├── graphs/                    # Supplementary figures (Suppl.Fig01–08)
│   │   ├── maps/
│   │   └── tables/
│   └── reports/                       # Per-script execution reports
│
├── .github/workflows/                 # CI/CD pipeline
└── README.md
```

---

## 📊 Data Requirements

### Auto-Downloaded (via `geodata` R package)

| Data | Source | Resolution |
|------|--------|-----------|
| GADM boundaries | [GADM](https://gadm.org/) | Country / Admin 1–2 |
| SPAM 2010/2017 cropland | IFPRI / [SPAM](https://www.mapspam.info/) | 10 km |
| Population density | [GPW v4](https://sedac.ciesin.columbia.edu/data/collection/gpw-v4) | ~1 km |
| Soil properties | [SoilGrids](https://soilgrids.org/) / [iSDA](https://www.isda-africa.com/) | 250 m |
| Elevation | [WorldClim](https://www.worldclim.org/) | ~1 km |
| Climate (temp, precip) | [WorldClim](https://www.worldclim.org/) | ~1 km |
| Travel time to cities | [Nelson et al. 2019](https://doi.org/10.1038/s41597-019-0265-5) via `geodata` | ~1 km |
| CHIRPS rainfall | [UCSB](https://www.chc.ucsb.edu/data/chirps) | 5 km |

### Manual Downloads Required

| Data | Source | Destination path |
|------|--------|-----------------|
| SPAM 2020 cropland | [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SWPENT) | `data/raw/spatial/spam/spam2020/` |
| Cattle density (GLW) | [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GIVQ75) | `data/raw/spatial/cattle-density/` |
| Du et al. 2025 livestock | [Zenodo 17128483](https://zenodo.org/records/17128483) | `data/raw/spatial/livestock-du2025/` |
| LSMS surveys | [World Bank LSMS](https://www.worldbank.org/en/programs/lsms) | `data/raw/web_scrapped/survey_data/` |
| Lowder et al. 2021 census | [Lowder et al. 2021](https://doi.org/10.1016/j.worlddev.2021.105455) | `data/raw/web_scrapped/` |

---

## 🔧 Installation

<details>
<summary><b>Using renv (recommended — reproducible package versions)</b></summary>

```r
install.packages("renv")
renv::restore()
```
</details>

<details>
<summary><b>Manual installation</b></summary>

```r
source("scripts/00.1_install_packages.R")
```

Key packages: `terra`, `sf`, `geodata`, `ranger`, `quantregForest`, `xgboost`, `tidyverse`, `tmap`, `patchwork`, `GGally`

</details>

---

## 🧪 Testing

```bash
# Full sequential pipeline test (~7 min on synthetic data)
Rscript scripts/00.4_run_all_tests.R
```

```r
# Run a single script after generating stubs
setwd("scripts")
source("00.3_synthetic_data.R")    # generate stubs first
source("06.3_prediction_maps.R")   # then any downstream script
```

The test runner produces a summary report at `output/reports/full_pipeline_test_report.md`.

---

## 📈 Key Outputs

| File | Description |
|------|-------------|
| `stacked_rasters_africa.tif` | 10-layer spatial predictor stack (input to models) |
| `lsms_trimmed_95th_africa.rds` | Analysis-ready farm-level dataset (95th-pct trimmed) |
| `rf_model_predictions_SSA.tif` | RF median farm size map across SSA |
| `qrf_100quantiles_predictions_africa.tif` | QRF predictions at 100 quantile levels |
| `nb_farms_per_grid_cell.tif` | Estimated farm count per 10×10 km cell |

---

## 🌍 Country Coverage

The models are trained on LSMS surveys from **16 countries**:

Benin, Burkina Faso, Côte d'Ivoire, Ethiopia, Ghana, Guinea-Bissau, Malawi, Mali, Niger, Nigeria, Rwanda, Senegal, Tanzania, Togo, Uganda, Zambia
> The survey wave counts and year ranges shown in the CI pipeline are synthetic stubs, **not actual values**. Refer to the original paper for the true survey inventory.

---

## 📚 Citation

If you use this code or data, please cite the original authors:

> Hougni D.G.J.M., Chamberlin J., Hijmans R., Baudron F., Giller K. & Silva J.V. (2025). *Dataset: A third of sub-Saharan Africa's farms cultivate less than half a hectare of land*. Zenodo. https://doi.org/10.5281/zenodo.15652768

The documented and reorganized repository (script headers, CI pipeline, synthetic data framework, README) was produced by D. Hougni (CGIAR) with assistance from Claude (Anthropic, 2025–2026).

<details>
<summary>BibTeX</summary>

```bibtex
@dataset{hougni_etal_2025_farmsize,
  author    = {Hougni, Deo-Gratias Judrita Mawugnon and Chamberlin, Jordan and
               Hijmans, Robert and Baudron, Fr{\'e}d{\'e}ric and
               Giller, Ken and Silva, Jo{\~a}o Vasco},
  title     = {{Dataset: A third of sub-Saharan Africa's farms cultivate
                less than half an hectare of land}},
  year      = {2025},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.15652768},
  url       = {https://doi.org/10.5281/zenodo.15652768}
}
```
</details>

---

## 📄 License

[![CC BY 4.0](https://licensebuttons.net/l/by/4.0/88x31.png)](https://creativecommons.org/licenses/by/4.0/)

Released under **CC BY 4.0** — consistent with the original Zenodo record. Free to share and adapt with attribution.

---

## 📞 Contact

* **D. Hougni (CGIAR):** &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;d.hougni@cgiar.org
* **D. Hougni (Personal):** &nbsp;&nbsp;&nbsp;shadowhgni@yahoo.fr
* **Issues:** &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[github.com/shadowhgni/farm_size_analysis/issues](https://github.com/shadowhgni/farm_size_analysis/issues) |
---

<div align="center"><i>Last updated: April 2026</i></div>
