# Farm Size Prediction Across Sub-Saharan Africa

[![Test R Scripts](https://github.com/shadowhgni/quick_test_farm_size_claude/actions/workflows/test-scripts.yml/badge.svg)](https://github.com/shadowhgni/quick_test_farm_size_claude/actions/workflows/test-scripts.yml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15652768.svg)](https://doi.org/10.5281/zenodo.15652768)

Machine learning models for predicting farm sizes across Sub-Saharan Africa using LSMS survey data and spatial predictors.

## 📋 Overview

This project develops Random Forest and Quantile Regression Forest models to predict farm size distributions and number of farms across Sub-Saharan Africa, trained on household survey data from 16 countries and applied continent-wide.

- **Survey Data:** Living Standards Measurement Study (LSMS) household surveys (~180,000 farms)
- **Spatial Predictors:** Cropland, population density, climate, soil, market access, and more

## 🚀 Quick Start

### Option 1: Run Locally with Synthetic Data

The test suite uses synthetic stubs (randomly generated data that mimic the structure of real inputs). This is the fastest way to verify that the code runs correctly on your machine. It does **not** reproduce the actual scientific results.

```r
# Clone the repository
git clone https://github.com/shadowhgni/quick_test_farm_size_claude.git
cd quick_test_farm_size_claude/farm_size_project_complete/scripts

# Generate synthetic stubs and run the full pipeline
Rscript 00.4_run_all_tests.R
```

### Option 2: Run with GitHub Actions (No Local Setup)

1. Fork this repository
2. Push any change to trigger the workflow (or use the **Run workflow** button in the Actions tab)
3. The pipeline runs on **synthetic stub data** — small, fast stand-ins that exercise every script end-to-end without requiring real survey or spatial data

### Option 3: Full Pipeline with Real Data

Running with actual LSMS surveys and spatial layers requires downloading several large datasets (see [Data Requirements](#-data-requirements)) and is best done on an HPC cluster. See [Installation](#-installation) and the individual script headers for details.

## 📁 Project Structure

```
farm_size_project_complete/
├── scripts/                       # R and Python scripts
│   ├── 00_report_utils.R          # Shared logging utility (sourced by other scripts)
│   ├── 00.1_install_packages.R    # Package installation
│   ├── 00.2_download_spatial_data.R  # Spatial data downloads
│   ├── 00.3_synthetic_data.R      # Synthetic stub generator (CI/local testing)
│   ├── 00.4_run_all_tests.R       # Test runner
│   ├── 01.1–01.4_*.R              # CHIRPS rainfall & spatial layer prep
│   ├── 02.1–02.3_*.R              # LSMS data compilation & harmonization
│   ├── 03.1–03.3_*.R              # Data pooling & descriptive stats
│   ├── 04.1–04.5_*.R              # ML algorithm comparison & evaluation
│   ├── 05.1–05.3_*.R              # Random Forest optimization & robustness
│   ├── 06.1–06.4_*.R              # Quantile RF models & prediction maps
│   ├── 07.1_*.R                   # Distribution evaluation
│   ├── 08.1–08.3_*.R              # Country-level predictions & farm size classes
│   ├── 09.1_*.R                   # AEZ characterization
│   ├── 10.1–10.2_*.R              # External validation
│   ├── F01–F03_*.R                # Main manuscript figures
│   └── S01–S08_*.R / T01–T02_*.R # Supplementary figures & tables
├── data/
│   ├── raw/
│   │   ├── spatial/               # Spatial predictor layers
│   │   └── web_scrapped/          # Survey data, FAOSTAT
│   └── processed/                 # Analysis-ready datasets
├── output/
│   ├── main_fig/                  # Main manuscript figures
│   ├── other_illustr/graphs/      # Supplementary figures
│   ├── other_illustr/maps/        # Maps
│   ├── other_illustr/tables/      # Tables
│   └── reports/                   # Per-script run reports
├── .github/workflows/             # CI pipeline
└── README.md
```

## 📊 Data Requirements

### Auto-Downloaded (via `geodata` package)

| Data | Source | Script |
|------|--------|--------|
| GADM boundaries | GADM | `00.2_download_spatial_data.R` |
| SPAM 2010/2017 cropland | IFPRI | `00.2_download_spatial_data.R` |
| Population density | GPW v4 | `00.2_download_spatial_data.R` |
| Soil (SoilGrids / iSDA) | ISRIC / iSDA | `00.2_download_spatial_data.R` |
| Elevation | WorldClim | `00.2_download_spatial_data.R` |
| Climate (temp, precip) | WorldClim | `00.2_download_spatial_data.R` |
| Travel time to cities/ports | Nelson et al. (2019) via `geodata` | `00.2_download_spatial_data.R` |
| CHIRPS rainfall | UCSB | `01.1_chirps_download.R` |

Travel time data are from: Nelson A., Weiss D.J., van Etten J., Cattaneo A., McMenomy T.S. & Koo J. (2019). A suite of global accessibility indicators. *Scientific Data* 6: 266. https://doi.org/10.1038/s41597-019-0265-5

### Manual Downloads Required

| Data | Source | Path |
|------|--------|------|
| SPAM 2020 | [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SWPENT) | `data/raw/spatial/spam/spam2020/` |
| Cattle density | [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GIVQ75) | `data/raw/spatial/cattle-density/` |
| Du et al. 2025 Livestock | [Zenodo](https://zenodo.org/records/17128483) | `data/raw/spatial/livestock-du2025/` |
| LSMS surveys | [World Bank](https://www.worldbank.org/en/programs/lsms) | `data/raw/web_scrapped/survey_data/` |
| Lowder et al. 2021 census data | [FAO / Lowder et al. 2021](https://doi.org/10.1016/j.worlddev.2021.105455) | `data/raw/web_scrapped/` |

## 🔧 Installation

### Using renv (Recommended)

```r
install.packages("renv")
renv::restore()
```

### Manual Installation

```r
source("scripts/00.1_install_packages.R")
```

## 🧪 Testing

### Run Full Test Suite

```bash
Rscript scripts/00.4_run_all_tests.R
```

### Run Individual Scripts

```r
setwd("farm_size_project_complete/scripts")
source("00.3_synthetic_data.R")   # generate stubs first
source("03.3_descriptive_stats.R") # then any downstream script
```

### GitHub Actions

Tests run automatically on:
- Push to `main` or `develop`
- Pull requests to `main`
- Manual trigger (Actions tab → **Run workflow**)

The workflow always uses **synthetic stub data**, not real survey data. It verifies that all scripts execute without error and produce the expected output files.

## 📈 Key Outputs

| Output | Description |
|--------|-------------|
| `stacked_rasters_africa.tif` | 10-layer spatial predictor stack |
| `lsms_trimmed_95th_africa.rds` | Analysis-ready farm-level data (95th-percentile trimmed) |
| `rf_model_predictions_SSA.tif` | RF median farm size predictions across SSA |
| `qrf_100quantiles_predictions_africa.tif` | QRF predictions at 100 quantiles across SSA |
| `nb_farms_per_grid_cell.tif` | Estimated number of farms per 10×10 km grid cell |

## 🌍 Country Coverage

The models are trained on LSMS surveys from **16 countries**:

Benin, Burkina Faso, Côte d'Ivoire, Ethiopia, Ghana, Guinea-Bissau, Malawi, Mali, Niger, Nigeria, Rwanda, Senegal, Tanzania, Togo, Uganda, Zambia

> The survey wave counts and year ranges shown in the CI pipeline are synthetic stubs, not actual values. Refer to the original paper for the true survey inventory.

## 📚 Citation

The original analysis scripts and data were developed by the following authors and are the primary work to cite:

> Hougni D.G.J.M., Chamberlin J., Hijmans R., Baudron F., Giller K. & Silva J.V. (2025). *Dataset: A third of sub-Saharan Africa's farms cultivate less than half an hectare of land*. Zenodo. https://doi.org/10.5281/zenodo.15652768

The documented and reorganized version of this repository (script headers, CI pipeline, synthetic data framework, README) was produced by D. Hougni (CGIAR) with assistance from Claude (Anthropic, 2025–2026).

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

## 📄 License

This repository is released under the **Creative Commons Attribution 4.0 International (CC BY 4.0)** license, consistent with the original Zenodo record.

You are free to share and adapt this material for any purpose, provided appropriate credit is given to the original authors (see [Citation](#-citation) above).

[![CC BY 4.0](https://licensebuttons.net/l/by/4.0/88x31.png)](https://creativecommons.org/licenses/by/4.0/)

Full license text: https://creativecommons.org/licenses/by/4.0/legalcode

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-fix`)
3. Commit your changes (`git commit -m 'Describe the fix'`)
4. Push to your fork and open a Pull Request

Bug reports and questions are welcome via [GitHub Issues](https://github.com/shadowhgni/quick_test_farm_size_claude/issues) — the issue tracker is active and monitored.

## 📞 Contact

- **D. Hougni (CGIAR):** d.hougni@cgiar.org
- **Personal:** shadowhgni@yahoo.fr
- **Issues:** [github.com/shadowhgni/quick_test_farm_size_claude/issues](https://github.com/shadowhgni/quick_test_farm_size_claude/issues)

---

*Last updated: April 2026*
