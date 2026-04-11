# ==============================================================================
# Script: 08.1_predictions_by_country.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Evaluate predicted distributions from Quantile RF
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - February 2026
# ==============================================================================


require(tidyverse)

# Clean environment
rm(list=ls())

# Set working directory
setwd(paste0(here::here(), '/scripts'))

# ------------------------------------------------------------------------------
# Preparation for functions and mapping
input_path <- '../data/raw/spatial'
country <- geodata::world(path=input_path, resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='Réunion' & NAME!='Saint Helena' & NAME!='São Tomé and Príncipe' & NAME!='Seychelles') # keep the mainland + Madagascar only, remove islands
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
pal <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))
pal2 <- colorRampPalette(c('#c6dbef','#6baed6','#3182bd', '#08519c', '#08306b'))
# force terra to use disk-based processing and 20% of RAM (Use this if R crashes because of limited memory)
gc()
# terra::terraOptions(memfrac = 0.2, todisk = T)

# ------------------------------------------------------------------------------
#define the countries for which LSMS data are available
sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# Prepare data rasters: lsms and predictions + virtual list of farm sizes
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')
rf_model_predictions <- terra::rast('../data/processed/rf_model_predictions_SSA.tif')
names(rf_model_predictions) <- 'pred_farm_area_ha'
qrf_model_predictions <- terra::rast('../data/processed/qrf_100quantiles_predictions_africa.tif')
names(qrf_model_predictions) <- paste0('qrf_q', sprintf('%03g', 1:100))
# Prepare lsms data
lsms_spatial_with_country_names <-  readRDS('../data/processed/lsms_trimmed_95th_africa.rds') |>
  select(x, y, country, farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market)

# ------------------------------------------------------------------------
# (Robert) defined a customized function to test whether observations fall in the quantile bins
# But I now believe we can just use a Kolmogorov test for 2 eempirical distributions

f1_quantiles <- function(farmsizes, quantiles, MC = FALSE) { 
  # farmsizes : observed farm sizes in a given location
  # quantiles: predicted boundaries of quantiles (based on predictions)
  # MC (should Monte Carlo simulations be run)? default: No
  # exapand quantiles to capture observations outside quantiles
  
  n_fold <- table(quantiles)       # for probability of each quantile
  q2 <- as.numeric(names(n_fold))  # quantiles without duplicated boundaries
  exp_quantiles <- c(-1, q2, 1.1 * max(farmsizes, q2))
  print('expected boundaries of quantiles')
  print(exp_quantiles)
  # assign to groups
  groups <- cut(farmsizes, exp_quantiles)
  counts <- table(groups) |> as.vector()
  n <- length(q2)-1
  expected <- c(n_fold, 0.01) # 0.01 is the prob to fall outside the boundaries of expected quantiles
  if (MC) {
    e <- XNomial::xmonte(counts, expected, detail=0)
  } else {
    e <- XNomial::xmulti(counts, expected, detail=0)
  }
  return(e$pLLR)
}



emp_dist_10 <- lsms_spatial_with_country_names |>
  group_by(x, y, country) |>
  select(farm_area_ha) |>
  summarize(n_obs = n(),
            farm_area_ha = list(farm_area_ha)) |>
  filter(n_obs > 9) |>
  ungroup()

pred_dist <- terra::extract(qrf_model_predictions,
                             emp_dist_10 |>
                               select(x, y) ) |>
  cbind(emp_dist_10 |>
          select(x, y, country)) |>
  terra::as.data.frame(xy = T) |>
  pivot_longer(cols = starts_with('qrf_q'),
               names_to = 'quantile',
               values_to = 'pred_farm_area_ha') |>
  group_by(x, y, country) |>
  summarize(pred_farm_area_ha = list(pred_farm_area_ha)) |>
  ungroup()

compare_dist <- inner_join(emp_dist_10, pred_dist)
compare_empirical_predicted_distr <- function(df){
  df2 = data.frame()
  for(i in unique(c(df$x, df$y))){
    tryCatch({
      emp = unlist(df$farm_area_ha[c(df$x, df$y) == i])
      pred = unlist(df$pred_farm_area_ha[c(df$x, df$y) == i])
      ks_results = ks.test(emp, pred)
      ks_D = ks_results$statistic
      ks_pval = ks_results$p.value
      xnomial_pval = f1_quantiles(emp, pred, MC = T)
      one_row = cbind(x = df$x[c(df$x, df$y) == i],
                      y = df$y[c(df$x, df$y) == i],
                      country = df$country,
                      ks_D = ks_D, ks_pval = ks_pval,
                      xnomial_pval = xnomial_pval)
      df2 = rbind(df2, one_row)
      print(paste0('---------- nrow = ', nrow(df2), '------------'))
    }
    ,
    error = function(e) print(e),
    finally = print('')
    )
  }
  return(df2)
}

deb <- Sys.time()
compare_dist_table <- compare_empirical_predicted_distr(compare_dist) |>
  mutate(across(c(x, y, ks_D, ks_pval, xnomial_pval), ~ as.numeric(.)))
fin <- Sys.time() - deb; print(fin)

eval_distr_fit_vect <- terra::vect(compare_dist_table, geom = c('x', 'y'))
terra::writeVector(eval_distr_fit_vect, '../data/f_size_distribution_fit.shp', overwrite = T)
eval_distr_fit <- terra::rasterize(eval_distr_fit_vect, stacked$temperature, 
                                   field = c('ks_D', 'ks_pval', 'xnomial_pval'))
terra::writeRaster(eval_distr_fit, '../data/f_size_distribution_fit.tif', overwrite = T)
terra::plot(ssa, main = 'P values for Kolmogorov test')
terra::plot(eval_distr_fit$ks_pval, add = T)

terra::plot(ssa, main = 'P values for multinomial test')
terra::plot(eval_distr_fit$xnomial_pval, add = T)
# percentage of EA where predicted distribution is different from observed
compare_dist_table |>
  filter(country %in% sixteen_country_codes) |>
  mutate(ks_p0.05 = ifelse(ks_pval < 0.05, 1, 0),
         xnomial_p0.05 = ifelse(xnomial_pval < 0.05, 1, 0)) |>
  group_by(country) |>
  summarize(ks_p0.05 = 100 * sum(ks_p0.05) / n(),
            xnomial_p0.05 = 100 * sum(xnomial_p0.05) / n())
