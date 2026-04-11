# ==============================================================================
# Script: 10.1_prepare_validation_data.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Prepare external validation datasets
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
pal3 <- colorRampPalette(c('skyblue1', 'blue4'))
# force terra to use disk-based processing and 50% of RAM (Use this if R crashes because of limited memory)
# terra::terraOptions(memfrac = 0.5, todisk = T)

# ------------------------------------------------------------------------------
#define the countries for which LSMS data are available
sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# # Prepare data rasters
# stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')
# rf_model_predictions <- terra::rast('../data/processed/rf_model_predictions_SSA.tif')
# names(rf_model_predictions) <- 'pred_farm_area_ha'
# mask_forest_ssa <- terra::rast('../data/processed/mask_forest_ssa.tif')
# mask_drylands_ssa <- terra::rast('../data/processed/mask_drylands_ssa.tif')

# ------------------------------------------------------------------------------
# extract predicted avg farm areas 
pred_farm_area_ha <- terra::rast('../data/processed/rf_model_predictions_SSA.tif')
names(pred_farm_area_ha) <- 'pred_farm_area_ha'
# add the GID_0 and country names to the created raster, and make it a data frame
val_farm_area <- bind_cols(
  terra::as.data.frame(pred_farm_area_ha, xy = T),
  terra::extract(ssa, 
                 terra::as.data.frame(pred_farm_area_ha, xy = T) |> 
                   select(x, y))
)

# function to extract predicted avg farm sizes per GADM1 and GADM2 for country(ies) given as input
# first, set locale to accept special characters
Sys.setlocale("LC_ALL", "en_US.UTF-8")
country_farm_area_validation <- function(cty = 'Angola'){
  print(paste0('------------ ', cty, '---------------'))
  cty_farm_pred_df <- subset(val_farm_area, NAME_0 == cty)
  gadm_dir <- paste0(input_path, '/gadm/', cty, '/gadm/')
  cty_gadm2_files <- if (dir.exists(gadm_dir)) {
    dir(gadm_dir, full.names = TRUE)[grep('_2_pk.rds$', dir(gadm_dir), ignore.case = TRUE)]
  } else character(0)
  if (!length(cty_gadm2_files)) {
    message('CI-SKIP 10.2: no GADM file for ', cty)
    return(invisible(NULL))
  }
  cty_vect <- terra::vect(cty_gadm2_files[1])
  cty_farm_pred_df <- bind_cols(
    cty_farm_pred_df |>
      select(x, y, pred_farm_area_ha),
    terra::extract(cty_vect,
                   cty_farm_pred_df |>
                     select(x, y)) |>
      select(GID_0, GID_1, NAME_1, GID_2, NAME_2)
  )

  cty_avg_gadm1 <- cty_farm_pred_df |>
    na.omit() |>
    group_by(GID_1, NAME_1) |>
    summarize(avg_pred_farm_area_ha = mean(pred_farm_area_ha, na.rm = T),
              sd_pred_farm_area_ha = sd(pred_farm_area_ha, na.rm = T))

  gadm1_data <- inner_join(
    cty_farm_pred_df |> select(x, y, GID_1) |> distinct(),
    cty_avg_gadm1) |>
    select(x, y, avg_pred_farm_area_ha, sd_pred_farm_area_ha)
  cty_gadm1 <- tryCatch(terra::rast(gadm1_data),
                        error = function(e) { message('CI: gadm1 rast skipped: ', e$message); NULL })

  cty_avg_gadm2 <- cty_farm_pred_df |>
    na.omit() |>
    group_by(GID_2, NAME_2) |>
    summarize(avg_pred_farm_area_ha = mean(pred_farm_area_ha, na.rm = T),
              sd_pred_farm_area_ha = sd(pred_farm_area_ha, na.rm = T))

  gadm2_data <- inner_join(
    cty_farm_pred_df |> select(x, y, GID_2, NAME_2) |> distinct(),
    cty_avg_gadm2) |>
    select(x, y, avg_pred_farm_area_ha, sd_pred_farm_area_ha)
  cty_gadm2 <- tryCatch(terra::rast(gadm2_data),
                        error = function(e) { message('CI: gadm2 rast skipped: ', e$message); NULL })

  cty_continuous <- tryCatch(
    terra::rast(cty_farm_pred_df |> select(x, y, pred_farm_area_ha)),
    error = function(e) { message('CI: continuous rast skipped: ', e$message); NULL }
  )
  if (is.null(cty_continuous)) return(invisible(NULL))

  # Only plot if the raster has finite values (all-NA rasters crash plot.window)
  has_data <- function(r) !is.null(r) && terra::nlyr(r) > 0 && any(is.finite(terra::values(r)), na.rm = TRUE)

  if (has_data(cty_continuous)) {
    png(paste0('../validation/', cty, '_continuous.png'), units = 'in', width = 5.5, height = 5.5, res = 150)
    tryCatch({
      terra::plot(cty_continuous, main = paste0('Predicted average farm sizes in ', cty))
      terra::plot(cty_vect, add = T)
    }, error = function(e) message('CI: continuous plot skipped: ', e$message))
    dev.off()

    pal     <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))
    new_ext <- floor(terra::ext(cty_vect))
    new_ext[1] <- new_ext[1] - 3.5
    leg_x   <- unname(terra::ext(cty_vect)[1] - 3)
    leg_y   <- unname(terra::ext(cty_vect)[3] + 5.5)
    png(paste0('../validation/', cty, '_catego.png'), units = 'in', width = 5.5, height = 5.5, res = 150)
    tryCatch({
      terra::plot(new_ext, main = paste0('Predicted average farm sizes in ', cty), axes = F)
      terra::plot(cty_continuous, breaks = c(0, 0.5, 1, 1.5, 2, 5, Inf), col = pal(6),
                  legend = F, cex = 1, axes = F, add = T)
      legend(leg_x, leg_y, bty = 'y', cex = 0.7, ncol = 1, box.col = 'white',
             title = 'Farm size', legend = c('< 0.5 ha','0.5 - 1 ha','1 - 1.5 ha','1.5 - 2 ha','2 - 5 ha','> 5 ha'),
             fill = pal(6), horiz = F)
      terra::plot(cty_vect, axes = F, add = T)
    }, error = function(e) message('CI: catego plot skipped: ', e$message))
    dev.off()
  } else {
    message('CI: ', cty, ' continuous raster has no finite values — skipping plots')
  }

  if (has_data(cty_gadm1)) {
    png(paste0('../validation/', cty, '_gadm1_avg.png'), units = 'in', width = 5.5, height = 5.5, res = 150)
    tryCatch({
      terra::plot(cty_gadm1$avg_pred_farm_area_ha,
                  main = paste0('Pred. avg. farm sizes - admin level 1 in ', cty))
      terra::plot(cty_vect, add = T)
    }, error = function(e) message('CI: gadm1 plot skipped: ', e$message))
    dev.off()
  }

  if (has_data(cty_gadm2)) {
    png(paste0('../validation/', cty, '_gadm2_avg.png'), units = 'in', width = 5.5, height = 5.5, res = 150)
    tryCatch({
      terra::plot(cty_gadm2$avg_pred_farm_area_ha,
                  main = paste0('Pred. avg. farm sizes - admin level 2 in ', cty))
      terra::plot(cty_vect, add = T)
    }, error = function(e) message('CI: gadm2 plot skipped: ', e$message))
    dev.off()
  }

  write_csv(cty_avg_gadm1, file = paste0('../validation/', cty, '_gadm1.csv'))
  write_csv(cty_avg_gadm2, file = paste0('../validation/', cty, '_gadm2.csv'))
  if(file.exists(paste0('../validation/', cty, '.zip'))) unlink(paste0('../validation/', cty, '.zip'))
  file_names <- dir('../validation', full.names = T, pattern = cty)
  zip::zipr(paste0('../validation/', cty, '.zip'), file_names)
  # rm(cty_farm_pred_df, cty_avg_gadm1, cty_avg_gadm2, cty_farm_pred_df, 
  #    cty_continuous, cty_gadm1, cty_gadm2,
  #    M_cont, M_catego, M_gadm1, M_gadm2)
  unlink(c(paste0('../validation/', cty, '_gadm1.csv'),
           paste0('../validation/', cty, '_gadm2.csv'),
           paste0('../validation/', cty, '_gadm1_avg.png'),
           paste0('../validation/', cty, '_gadm2_avg.png'),
           paste0('../validation/', cty, '_catego.png'),
           paste0('../validation/', cty, '_continuous.png')) )
}

# Initializing the validation folder
if (!dir.exists('../validation')) dir.create('../validation')
cat('Here is the list of valid country names \n')
cat(paste0(unique(ssa$NAME_0), collapse = ';')) 

routine_1 <- {
  # input_cty <- readline('Please, enter a valid country name. \nIf interested in more than one country, please, use semi-colon (;) as separator:  \n')
  input_cty <- unique(ssa$NAME_0)
  input_cty <- unlist(strsplit(input_cty, split = ';'))
}
print('Important notice: In case, you would like to include Burkina, Cote_d_Ivoire and/or Guinea_Bissau in the validation countries,
      make sure you manually duplicate the related folders in input_path and rename the new folders using the actual GADM names.
      Besides some countries are shown to lack enough prediction data: Equatorial Guinea, Lesotho. Please skip them' )
routine_2 <- {
  bad_input <- c()
  for(i in input_cty){
    if(!(i %in% unique(ssa$NAME_0)) | i %in% c('Djibouti', 'Equatorial Guinea', 'Lesotho')) { # countries excluded because no farms were predicted there!!!
      bad_input <- c(bad_input, i)
    }
  }
  good_input <- input_cty[!input_cty  %in% bad_input]
  print('------------------------------------------------')
  print('The following countries are not eligible')
  print(bad_input)
  print('------------------------------------------------')
}

sapply(good_input, country_farm_area_validation)
