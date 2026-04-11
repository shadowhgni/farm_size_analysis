# ==============================================================================
# Script: 00.1_install_packages.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Install all R packages required by the project
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - March 2026
#
# Usage:
#   Rscript 00_install_packages.R
#   # Or in R: source("00_install_packages.R")
#
# Notes:
#   - terra requires system libraries: libgdal-dev, libgeos-dev, libproj-dev
#     Install on Ubuntu/Debian: sudo apt-get install -y libgdal-dev libgeos-dev libproj-dev
#   - tabulapdf requires Java (rJava): sudo apt-get install -y default-jdk
#   - magick requires ImageMagick: sudo apt-get install -y libmagick++-dev
#   - sf requires: sudo apt-get install -y libudunits2-dev libgdal-dev
#
# Package sources:
#   CRAN  : all standard packages below
#   GitHub: afrilearndata (afrimapr/afrilearndata)
#           SpatialML    (rCarto/SpatialML)
# ==============================================================================

message("=== Farm Size Project - Package Installer ===\n")

# ------------------------------------------------------------------------------
# 1. CRAN PACKAGES
# ------------------------------------------------------------------------------

cran_packages <- c(
  # ── Core data wrangling ──────────────────────────────────────────────────
  "tidyverse",       # dplyr, ggplot2, purrr, tidyr, readr, stringr, forcats
  "dplyr",           # data manipulation (explicit, for :: usage)
  "purrr",           # functional programming
  "reshape2",        # melt/cast data frames
  "haven",           # read SPSS/Stata/SAS files (LSMS survey data)
  "labelled",        # labelled data from survey files
  "readxl",          # read Excel files
  "fuzzyjoin",       # fuzzy matching joins
  "stringdist",      # string distance for fuzzy matching
  "here",            # project-relative paths

  # ── Spatial / GIS ────────────────────────────────────────────────────────
  "terra",           # raster & vector spatial data (core dependency)
  "sf",              # simple features vector data
  "spdep",           # spatial dependence / Moran's I
  "tmap",            # thematic mapping
  "geodata",         # download spatial data (GADM, SPAM, WorldClim, etc.)

  # ── Machine learning ─────────────────────────────────────────────────────
  "randomForest",    # Random Forest (baseline model)
  "ranger",          # fast Random Forest implementation
  "quantregForest",  # Quantile Regression Forest
  "xgboost",         # Gradient Boosted Trees
  "caret",           # ML training framework
  "e1071",           # SVM + misc ML utilities
  "gbm",             # Gradient Boosting Machines
  "mlr",             # Machine learning framework
  "tuneRanger",      # Hyperparameter tuning for ranger
  "doParallel",      # Parallel backend for foreach/caret

  # ── Statistical modeling ─────────────────────────────────────────────────
  "quantreg",        # Quantile regression (Koenker)
  "car",             # Companion to applied regression
  "MASS",            # Modern Applied Statistics with S
  "moments",         # Skewness, kurtosis
  "ineq",            # Gini coefficient, inequality measures
  "fields",          # Spatial statistics / thin-plate splines
  "EnvStats",        # Environmental statistics

  # ── Visualisation ────────────────────────────────────────────────────────
  "ggplot2",         # Elegant graphics (included in tidyverse)
  "GGally",          # ggplot2 extensions (pair plots)
  "patchwork",       # Combining ggplot2 panels
  "ggpubr",          # Publication-ready ggplot2 plots
  "cowplot",         # ggplot2 plot composition
  "viridis",         # Colour-blind-friendly palettes
  "RColorBrewer",    # Colour palettes
  "plotrix",         # Misc plot utilities

  # ── Web / data download ──────────────────────────────────────────────────
  "httr",            # HTTP requests
  "curl",            # HTTP client
  "rvest",           # Web scraping
  "R.utils",         # Misc utilities incl. gunzip

  # ── File I/O / utilities ─────────────────────────────────────────────────
  "zip",             # Read/write ZIP archives
  "magick",          # Image processing (requires ImageMagick system lib)
  "tabulapdf",       # Extract tables from PDFs (requires Java/rJava)
  "profvis",         # Profiling

  # ── Large data ───────────────────────────────────────────────────────────
  "disk.frame",      # Larger-than-RAM data frames

  # ── Package management ───────────────────────────────────────────────────
  "remotes",         # Install packages from GitHub/URL
  "XNomial"          # Exact multinomial test
)

# Install missing CRAN packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing: ", pkg)
    install.packages(pkg,
                     repos = "https://cloud.r-project.org",
                     dependencies = TRUE,
                     quiet = FALSE)
  } else {
    message("Already installed: ", pkg)
  }
}

message("--- Installing CRAN packages ---\n")
results <- lapply(cran_packages, function(pkg) {
  tryCatch({
    install_if_missing(pkg)
    list(pkg = pkg, ok = TRUE, msg = "")
  }, error = function(e) {
    message("ERROR installing ", pkg, ": ", e$message)
    list(pkg = pkg, ok = FALSE, msg = e$message)
  })
})

# ------------------------------------------------------------------------------
# 2. GITHUB PACKAGES
# ------------------------------------------------------------------------------
message("\n--- Installing GitHub packages ---\n")

if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes", repos = "https://cloud.r-project.org")

github_packages <- list(
  list(
    repo    = "afrimapr/afrilearndata",
    pkg     = "afrilearndata",
    desc    = "African spatial datasets for learning/examples"
  ),
  list(
    repo    = "rCarto/SpatialML",
    pkg     = "SpatialML",
    desc    = "Geographically Weighted Random Forest"
  )
)

for (gp in github_packages) {
  if (!requireNamespace(gp$pkg, quietly = TRUE)) {
    message("Installing from GitHub: ", gp$repo, " (", gp$desc, ")")
    tryCatch(
      remotes::install_github(gp$repo, dependencies = TRUE),
      error = function(e) message("ERROR: ", e$message)
    )
  } else {
    message("Already installed: ", gp$pkg)
  }
}

# ------------------------------------------------------------------------------
# 3. SUMMARY
# ------------------------------------------------------------------------------
message("\n", paste(rep("=", 70), collapse = ""))
message("INSTALLATION SUMMARY")
message(paste(rep("=", 70), collapse = ""))

failed  <- Filter(function(r) !r$ok, results)
success <- Filter(function(r)  r$ok, results)

message("\nCRAN packages: ", length(success), "/", length(cran_packages), " installed OK")
message("GitHub packages: see messages above\n")

if (length(failed) > 0) {
  message("Failed packages (may need system libraries):")
  for (r in failed) message("  - ", r$pkg, ": ", r$msg)
  message("")
  message("Tip for Ubuntu/Debian - install system dependencies first:")
  message("  sudo apt-get install -y \\")
  message("    libgdal-dev libgeos-dev libproj-dev \\")
  message("    libudunits2-dev libmagick++-dev \\")
  message("    default-jdk  # for tabulapdf/rJava")
}

message("\nAll done!")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
