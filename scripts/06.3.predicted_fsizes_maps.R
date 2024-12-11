# DRaw maps of average farm sizes and quantiles of farm sizes per grid cell

# load packages
require(tidyverse)

# Clean environment
rm(list=ls())

# Set working directory
setwd(paste0(here::here(), '/scripts'))

# ------------------------------------------------------------------------------
# Preparation for functions and mapping
input_path <- 'C:/Users/DHOUGNI/OneDrive - CIMMYT/Documents/Harare 2023/Spatial_data_repository'
country <- geodata::world(path=input_path, resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='Réunion' & NAME!='Saint Helena' & NAME!='São Tomé and Príncipe' & NAME!='Seychelles') # keep the mainland + Madagascar only, remove islands
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
pal <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))
# force terra to use disk-based processing and 50% of RAM (Use this if R crashes because of limited memory)
terra::terraOptions(memfrac = 0.8, todisk = T, verbose = F)

# ------------------------------------------------------------------------------
fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# Prepare data rasters: lsms and predictions
lsms_spatial_with_country_names <- read.csv('../data/processed/lsms_spatial_with_country_names.csv')
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')
rf_model_predictions <- terra::rast('../data/processed/rf_predictions_africa.tif')
qrf_model_predictions <- terra::rast('../data/processed/qrf_100quantiles_predictions_africa.tif')
names(qrf_model_predictions) <- paste0('qrf_q', sprintf('%03g', 1:100))

# ------------------------------------------------------------------------------
# calculate Moran's I
values <- as.vector(na.omit(terra::values(rf_model_predictions)))
coords <- terra::crds(na.omit(rf_model_predictions))
# nb <- spdep::dnearneigh(coords, 0, 2) # Adjust the distance threshold as needed [moran.test failed with 0.1-1.2, regions with no links in lw]
knn <- spdep::knearneigh(coords, k = 4)  # supposing 1 nearest neighbor north, 1 south, 1 east, 1 west
nb <- spdep::knn2nb(knn)
lw <- spdep::nb2listw(nb, style='W', zero.policy = T)
moran <- spdep::moran.test(values, lw, zero.policy = T)
moran

# ------------------------------------------------------------------------------
# Create mask for forest areas (from Afrilearn, available on GiTHUB), and for desertic areas
land_cover <- terra::rast(system.file('extdata', 'afrilandcover.grd', package = 'afrilearndata', mustWork = TRUE))
forests <- land_cover == 2 | land_cover == 4 | land_cover == 5 | land_cover == 8
forests <- terra::ifel(forests, NA, 1)
mask_forest_ssa <- terra::crop(forests, ssa)
rf_model_predictions <- rf_model_predictions * terra::resample(mask_forest_ssa, rf_model_predictions)

drylands <- terra::ifel(stacked$rainfall < 200, NA, 1)
rf_model_predictions <- rf_model_predictions * terra::resample(drylands, rf_model_predictions)

# ------------------------------------------------------------------------------
# Map predicted average farm sizes
png('../output/maps/predicted_RF_farm_size_africa.png', units = 'in', width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Predicted average farm sizes',
            panel.first = grid(col = 'gray', lty = 'solid'), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(rf_model_predictions, col  =  rev(terrain.colors(100)), axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

png('../output/maps/predicted_RF_farm_size_class_africa.png', units = 'in', width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Predicted average farm sizes',
            panel.first = grid(col = 'gray', lty = 'solid'), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(rf_model_predictions, breaks = c(0, 0.5, 1, 1.5, 2, 5, Inf), col = pal(6), legend = F, cex = 1, axes = F, add = T)
legend(-15, -10, bty = 'y', cex = 0.7, ncol = 1, box.col = 'white',
       title = 'Farm size', legend = c('< 0.5 ha', '0.5 - 1 ha', '1 - 1.5 ha', '1.5 - 2 ha', '2 - 5 ha', '> 5 ha'),
       fill = pal(6), horiz = FALSE)
terra::plot(ssa, axes = F, add = T)
dev.off()
terra::writeRaster(rf_model_predictions, '../data/processed/rf_model_predictions_SSA.tif', overwrite  =  T)
# ------------------------------------------------------------------------------
# predict farm size quantiles (0.1 and 0.9) all over SSA, based on the random forest model for the continent
qrf_model_predictions <- qrf_model_predictions * terra::resample(mask_forest_ssa, qrf_model_predictions)
qrf_model_predictions <- qrf_model_predictions * terra::resample(drylands, qrf_model_predictions)

my_q10 <- qrf_model_predictions$qrf_q010
my_q10 <- my_q10 * terra::resample(mask_forest_ssa, my_q10)
my_q10 <- my_q10 * terra::resample(drylands, my_q10)

my_q90 <- qrf_model_predictions$qrf_q090
my_q90 <- my_q90 * terra::resample(mask_forest_ssa, my_q90)
my_q90 <- my_q90 * terra::resample(drylands, my_q90)

terra::writeRaster(mask_forest_ssa, file = '../data/processed/mask_forest_ssa.tif', overwrite = T)
terra::writeRaster(drylands, file = '../data/processed/mask_drylands_ssa.tif', overwrite = T)
terra::writeRaster(my_q10, '../data/processed/QRF_q10_africa.tif', overwrite = T)
terra::writeRaster(my_q90, '../data/processed/QRF_q90_africa.tif', overwrite = T)

png('../output/maps/predicted_q10_QF_class_africa.png', units = 'in', width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Max. size of the 10% smallest smallholder farms',
            panel.first = grid(col = 'gray', lty = 'solid'), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(my_q10,  breaks = c(0, 0.1, 0.2, 0.5, 1, 2, Inf), col = pal(6), legend = F, cex = 1, axes  =  F, add = T)
legend(-18, -10, bty = 'y', cex = 0.7, ncol = 1, box.col = 'white',
       title = 'Farm size',
       legend = c('< 0.1 ha', '0.1 - 0.2 ha', '0.2 - 0.5 ha', '0.5 - 1 ha', '1 - 2 ha', '> 2 ha'),
       fill = pal(6), horiz = F)
terra::plot(ssa, axes = F, add = T)
dev.off()

png('../output/maps/predicted_q90_QF_class_africa.png', units = 'in', width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Min. size of the 10% largest smallholder farms',
            panel.first = grid(col = 'gray', lty = 'solid'), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(my_q90,  breaks = c(0, 1, 2, 5, 10, 15, Inf), col = pal(6), legend = F, cex = 1, axes  =  F, add = T)
legend(-18, -10, bty = 'y', cex = 0.7, ncol = 1, box.col = 'white',
       title = 'Farm size',
       legend = c('< 1 ha', '1 - 2 ha', '2 - 5 ha', '5 - 10 ha', '10 - 15 ha', '> 15 ha'),
       fill = pal(6), horiz = F)
terra::plot(ssa, axes = F, add = T)
dev.off()

png('../output/maps/predicted_mangnitude_q10_q90_QF_class_africa.png', units = 'in', width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Magnitude of farm size difference (10-90th quantiles)',
            panel.first = grid(col = 'gray', lty = 'solid'), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(my_q90,  breaks = c(0, 1, 2, 5, 10, 15, Inf), col = pal(6), legend = F, cex = 1, axes  =  F, add = T)
legend(-18, -10, bty = 'y', cex = 0.7, ncol = 1, box.col = 'white',
       title = 'Farm size',
       legend = c('< 1 ha', '1 - 2 ha', '2 - 5 ha', '5 - 10 ha', '10 - 15 ha', '> 15 ha'),
       fill = pal(6), horiz = F)
terra::plot(ssa, axes = F, add = T)
dev.off()