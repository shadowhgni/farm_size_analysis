# Understanding and predicting the variability of farm size across SSA
# making spatial data ready for analysis
# Note that all spatial data (shapefiles and rasters) are stored in a separate folder, 
# which is convenient for multiple projects.

# Open the file from the folder and set working directory, using the here package
setwd(paste0(here::here(), '/scripts'))

# Clean environment
rm(list=ls())

# load packages
require(tidyverse)

#############################################################################################################
# Here are stored all the maps that will serve as predictors in the machine-learning (ML) models. Adjust accordingly
input_path <- 'C:/Users/DHOUGNI/OneDrive - CIMMYT/Documents/Harare 2023/Spatial_data_repository'

#############################################################################################################
#define the region of interest: subSaharan Africa, excluding small islands 
country <- geodata::world(path=input_path, resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='RC)union' & NAME!='Saint Helena' & NAME!='SC#o TomC) and PrC-ncipe' & NAME!='Seychelles') # keep the mainland + Madagascar only, remove islands
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)

#############################################################################################################
#define the countries for which LSMS data are available
sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')

# clean previous maps from the output folder (in case there is an update)
outdated_maps <- c('../output/maps/africa-cattle.png', '../output/maps/africa-cropland.png',
                   '../output/maps/africa-cropland_2.png', '../output/maps/africa-elevation.png',
                   '../output/maps/africa-lsms.png', '../output/maps/africa-market.png',
                   '../output/maps/africa-population.png','../output/maps/africa-soil.png')

map_names <- c('cattle', 'cropland', 'gdp', 'lsms', 'maizeyield', 'market',
               'pop', 'poverty', 'rainfall', 'sand0_30', 'slope' )
for(i in sixteen_countries){
  for(j in map_names){
    country_map=paste0('../output/maps/', i, '-', j, '.png')
    outdated_maps=c(outdated_maps,country_map)
  }
}
for(i in outdated_maps){
  if(file.exists(i)) file.remove(i)
}

# ------------------------------------------------------------------------------
# retrieve the available GADM levels of all SSA countries

gadm_me <- function(cty){
  cty_dir <- paste0(input_path, '/gadm/', cty)
  if(!dir.exists(cty_dir)) dir.create(cty_dir)
  for(lvl in 1:5){
    tryCatch({
      print(paste0('------- ', cty, ': GADM level', lvl, '------'))
      gadm_lvl <- geodata::gadm(country = cty, level = lvl, version = 'latest', path = cty_dir )
    },
    error = function(e) {paste('level ', lvl, 'not found in ', cty, '\n', message(e))},
    finally = print('---------') 
    )
  }
}
sapply(unique(ssa$NAME_0), gadm_me)
# Make sure to manually change the folder name of Cote d'Ivoire and Guinea Bissau to the spelling in sixteen_countries

# force terra to use disk-based processing and 20% of RAM (Use this if R crashes because of limited memory)
terra::terraOptions(memfrac = 0.2, todisk = T, verbose = F)

# ------------------------------------------------------------------------------ 
# Using cropland from SPAM; note that the raster is named cropland_ha
# spam
all_spam_crops <- data.frame(geodata::spamCrops())[['code']]
# for(sv in c('harv_area', 'phys_area', 'prod', 'yield', 'val_prod')){  
#   print(sv)
#   variable <- lapply(
#     all_spam_crops, 
#     function(crop){
#       if(!dir.exists(paste0(input_path, '/spam/spam2017/', sv)))
#         dir.create(paste0(input_path, '/spam/spam2017/', sv))
#       geodata::crop_spam(crop, sv, path = paste0(input_path, '/spam/spam2017/', sv), africa = T) # africa = T means 2017 layer (without Sudan)
#       
#       if(!dir.exists(paste0(input_path, '/spam/spam2010/', sv)))
#         dir.create(paste0(input_path, '/spam/spam2010/', sv))
#       geodata::crop_spam(crop, sv, path = paste0(input_path, '/spam/spam2010/', sv), africa = F) # africa = F means 2010 layer 
#     } 
#   )   
#   variable <- terra::rast(variable)
#   names(variable) <- all_spam_crops
#   variable <- terra::crop(variable, ssa, mask = T)
#   terra::writeRaster(variable, paste0(input_path, '/spam/', sv, '/all_spam_crops.tif'), overwrite = T)
# }

# Manually download SPAM2020 from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SWPENT 
# place the .tif in the paste0(input_path, '/spam/spam2020/') folder
each_2020_crop <- terra::rast(paste0(input_path, '/spam/spam2020/', dir(paste0(input_path,'/spam/spam2020'))[grep('_H_[A-Z]+_A.tif$', dir(paste0(input_path,'/spam/spam2020')))]) ) # _A is total area (rainfed  + irrigated)
each_2020_crop <- terra::crop(each_2020_crop, ssa, mask = T)
names(each_2020_crop) <- substr(names(each_2020_crop), 24, 27)

each_2017_crop <- terra::rast(paste0(input_path, '/spam/spam2017/', dir(paste0(input_path,'/spam/spam2017'))[grep('_H_[A-Z]+_A.tif$', dir(paste0(input_path,'/spam/spam2017')))]) ) # _A is total area (rainfed  + irrigated)
each_2017_crop <- terra::crop(each_2017_crop, ssa, mask = T)
names(each_2017_crop) <- substr(names(each_2017_crop), 20, 23)

# Pick Sudan data from SPAM 2010, and merge with SPAM 2017
each_2010_crop <- terra::rast(paste0(input_path, '/spam/spam2010/', dir(paste0(input_path,'/spam/spam2010'))[grep('_H_[A-Z]+_A.tif$', dir(paste0(input_path,'/spam/spam2010')))]) )
names(each_2010_crop) <- substr(names(each_2010_crop), 23, 26)
sudan_mask <- terra::crop(each_2010_crop, subset(ssa, ssa$NAME_0 == 'Sudan'), mask = T)
each_2017_crop <- terra::merge(each_2017_crop, sudan_mask)

all_2020_crops <- sum(each_2020_crop, na.rm = T)
all_2020_crops <- terra::crop(all_2020_crops, ssa, mask = T)
all_2017_crops <- sum(each_2017_crop, na.rm = T)
all_2017_crops <- terra::crop(all_2017_crops, ssa, mask = T)
all_2010_crops <- sum(each_2010_crop, na.rm = T)
all_2010_crops <- terra::crop(all_2010_crops, ssa, mask = T)

terra::writeRaster(all_2020_crops, file = paste0(input_path, '/spam/spam2020_cropland_ssa.tif'), overwrite = T)
terra::writeRaster(all_2017_crops, file = paste0(input_path, '/spam/spam2017_cropland_ssa.tif'), overwrite = T)
terra::writeRaster(all_2010_crops, file = paste0(input_path, '/spam/spam2010_cropland_ssa.tif'), overwrite = T)

ssa_grid <- terra::rast(ssa, nrow = 2000, ncol = 2000)
ssa_rast <- terra::rasterize(ssa, ssa_grid, field = 'NAME_0')
ssa_rast <- terra::resample(ssa_rast, all_2010_crops)

esa_cropland <- geodata::cropland(source = 'WorldCover', path = input_path)
# esa_cropland <- terra::rast(paste0(input_path, '/landuse/landuse/WorldCover_cropland_30s.tif'))

# esa_cropland <- terra::crop(esa_cropland, ssa, mask = T) # failed in knitr::stitch_rhtml
esa_cropland <- terra::crop(esa_cropland, ssa_rast, mask = T)
esa_cropland <- esa_cropland * terra::cellSize(esa_cropland, unit = 'ha')

geosurvey_cropland <- geodata::cropland(source = 'QED', path = input_path)
# geosurvey_cropland <- terra::rast(paste0(input_path, '/landuse/landuse/geosurvey_cropland.tif'))

# geosurvey_cropland <- terra::crop(geosurvey_cropland, ssa, mask = T) # failed in knitr::stitch_rhtml
geosurvey_cropland <- terra::crop(geosurvey_cropland, ssa_rast, mask = T)
geosurvey_cropland <- geosurvey_cropland * terra::cellSize(geosurvey_cropland, unit = 'ha')

potapov_cropland <- geodata::cropland(source = 'GLAD', year = 2019, path = input_path)
# potapov_cropland <- terra::rast(paste0(input_path, '/landuse/landuse/glad_cropland_2019.tif'))

# potapov_cropland <- terra::crop(potapov_cropland, ssa, mask = T) # failed in knitr::stitch_rhtml
potapov_cropland <- terra::crop(potapov_cropland, ssa_rast, mask = T)
potapov_cropland <- potapov_cropland * terra::cellSize(potapov_cropland, unit = 'ha')

spam2010 <- c(ssa_rast, all_2010_crops)
spam2017 <- c(ssa_rast, all_2017_crops)
spam2020 <- c(ssa_rast, all_2020_crops)

# compare cropland of top 10 countries, for different sources
esa2 <- c(terra::resample(ssa_rast, esa_cropland), esa_cropland)
esa2_df <- terra::as.data.frame(esa2)
esa2_df |> group_by(NAME_0) |> summarize(cropland = sum(cropland, na.rm = T)) |> arrange(-cropland)

potapov2 <- c(terra::resample(ssa_rast, potapov_cropland), potapov_cropland)
potapov2_df <- terra::as.data.frame(potapov2)
potapov2_df |> group_by(NAME_0) |> summarize(cropland = sum(crop_2019, na.rm = T)) |> arrange(-cropland)

geosurvey2 <- c(terra::resample(ssa_rast, geosurvey_cropland), geosurvey_cropland)
geosurvey2_df <- terra::as.data.frame(geosurvey2)
geosurvey2_df |> group_by(NAME_0) |> summarize(cropland = sum(cropland, na.rm = T)) |> arrange(-cropland)

terra::as.data.frame(spam2010) |> group_by(NAME_0) |> summarize(cropland = sum(sum, na.rm = T)) |> arrange(-cropland)
terra::as.data.frame(spam2017) |> group_by(NAME_0) |> summarize(cropland = sum(sum, na.rm = T)) |> arrange(-cropland)
terra::as.data.frame(spam2020) |> group_by(NAME_0) |> summarize(cropland = sum(sum, na.rm = T)) |> arrange(-cropland)
# compare the total cropland in SSA
sum(esa2_df$cropland, na.rm = T)
sum(potapov2_df$crop_2019, na.rm = T)
sum(geosurvey2_df$cropland, na.rm = T)
sum(terra::as.data.frame(spam2010)$sum, na.rm = T)
sum(terra::as.data.frame(spam2017)$sum, na.rm = T)
sum(terra::as.data.frame(spam2020)$sum, na.rm = T)
# esa_croland is preferred because of highest spatial resolution of Sentinel2
# potapov seems to underestimate total cropland, spam2017 seems to overestimate total cropland
# cropland_ha <- terra::mean(
#   terra::aggregate(esa_cropland, 10, fun = sum, na.rm = T),
#   terra::aggregate(spam2017, 10, fun = sum, na.rm = T),
#   terra::aggregate(geosurvey_cropland, 10, fun = sum, na.rm = T),
#   terra::aggregate(potapov_cropland, 10, fun = sum, na.rm = T)
# )
geosurvey_cropland <- terra::aggregate(geosurvey_cropland, 10, fun = 'sum', na.rm = T)
esa_cropland <- terra::aggregate(esa_cropland, 10, fun = 'sum', na.rm = T)
potapov_cropland <- terra::aggregate(potapov_cropland, 10, fun = 'sum', na.rm = T)
terra::ext(geosurvey_cropland) <- terra::ext(esa_cropland) <- terra::ext(potapov_cropland) <- terra::ext(all_2017_crops) <- floor(terra::ext(esa_cropland))
# cropland_ha <- c(all_2017_crops, 
#                  terra::resample(esa_cropland, all_2017_crops), 
#                  terra::resample(potapov_cropland, all_2017_crops))
# 
# cropland_ha <- terra::mean(cropland_ha, na.rm = T)
# cropland_ha[cropland_ha$mean == 0] <- NA
all_cropland_mask <- c(terra::resample(all_2010_crops, all_2017_crops),
                       all_2017_crops,
                       terra::resample(all_2020_crops, all_2017_crops),
                       terra::resample(esa_cropland, all_2017_crops),
                       terra::resample(potapov_cropland, all_2017_crops),
                       terra::resample(geosurvey_cropland, all_2017_crops))
names(all_cropland_mask) <- c('SPAM 2010', 'SPAM 2017', 'SPAM 2020',
                              'ESA 2020', 'GLAD 2019', 'GEOSURVEY 2015')
png("../output/maps/africa_all_croplands.png", units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(all_cropland_mask, range = c(0, 5000), fill_range = T, cex = 1.2, axes = F)
dev.off()

# SPAM 2017 is chosen as default cropland because it is a middle year, and SPAM 2017 was dedicated to Africa only
cropland_ha <- spam2017$sum
names(cropland_ha) <- 'cropland'
terra::writeRaster(all_cropland_mask, file = paste0(input_path, '/landuse/landuse/all_cropland_mask.tif'), overwrite = T)
terra::writeRaster(cropland_ha, file = paste0(input_path, '/spam/cropland_ssa.tif'), overwrite = T)

png("../output/maps/africa-cropland.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Cropland (ha)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(cropland_ha, range = c(0, 5000), fill_range = T, cex = 1.2, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

# ------------------------------------------------------------------------------
# Cattle density
# Manually download from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GIVQ75 
# create a sub-folder for cattle density to store the .tif file 
if(!dir.exists(paste0(input_path, '/cattle-density')))
  dir.create(paste0(input_path, '/cattle-density'))
cattle <- terra::rast(paste0(input_path,'/cattle-density/5_Ct_2010_DA.tif') )
cattle <- terra::crop(cattle, ssa, mask = T)
cattle <- terra::resample(cattle, cropland_ha)
names(cattle) <- 'cattle'
terra::writeRaster(cattle, paste0(input_path, '/cattle-density/2010_cattle_density_ssa.tif'), overwrite = T)

png("../output/maps/africa-cattle-density.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Cattle density (# /km2)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(cattle, range = c(0, 2000), fill_range = T, cex = 1.2, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()
# ------------------------------------------------------------------------------
# Population density
# create a sub-folder for population density to store the .tif file 
if(!dir.exists(paste0(input_path, '/population')))
  dir.create(paste0(input_path, '/population'))
pop <- geodata::population(2020, 0.5, path = input_path)
pop <- terra::rast(paste0(input_path,'/population/pop/gpw_v4_population_density_rev11_2020_30s.tif') )

# pop <- terra::crop(pop, ssa, mask = T) # failed in knitr::stitch_rhtml
pop <- terra::crop(pop, ssa_rast, mask = T)
pop <- terra::resample(pop, cropland_ha)
names(pop) <- 'pop'
terra::writeRaster(pop, paste0(input_path, '/population/2020_population_density_ssa.tif'), overwrite = T)

png("../output/maps/africa-population-density.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Population density (inhabitants/km2)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(pop, range = c(0, 500), fill_range = T, cex = 1.2, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()
# ------------------------------------------------------------------------------
# Create a raster for cropland per capita
cropland_per_capita <- cropland_ha / pop
cropland_per_capita[is.infinite(cropland_per_capita)] <- NA
names(cropland_per_capita) <- 'cropland_per_capita'
terra::writeRaster(cropland_per_capita, paste0(input_path, '/spam/cropland_per_capita_ssa.tif'), overwrite = T)

png("../output/maps/africa-croplaand-per-capita.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Cropland per capita (ha/person)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(cropland_per_capita, range = c(0, 50), fill_range = T, cex = 1.2, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()
# ------------------------------------------------------------------------------
# Sand content
# create a sub-folder for cattle density to store the .tif file 
if(!dir.exists(paste0(input_path, '/soil_world')))
  dir.create(paste0(input_path, '/soil_world'))
sand05 <- geodata::soil_world('sand', 5, path = input_path)
sand15 <- geodata::soil_world('sand', 15, path = input_path)
sand05 <- geodata::soil_world('sand', 30, path = input_path)
sand05 <- terra::rast(paste0(input_path,'/soil_world/sand_0-5cm_mean_30s.tif') )
sand15 <- terra::rast(paste0(input_path,'/soil_world/sand_5-15cm_mean_30s.tif') )
sand30 <- terra::rast(paste0(input_path,'/soil_world/sand_15-30cm_mean_30s.tif') )
sand0_30 <- (5*sand05 + 10*sand15 + 15*sand30)/30  # finish with 5*sand30 if 0-20cm is preferred over 0-30cm

# sand0_30 <- terra::crop(sand0_30, ssa, mask = T) # failed in knitr::stitch_rhtml
sand0_30 <- terra::crop(sand0_30, ssa_rast, mask = T)
sand0_30 <- terra::resample(sand0_30, cropland_ha)
names(sand0_30) <- 'sand'
terra::writeRaster(sand0_30, paste0(input_path, '/soil_world/sand_content_0_30cm_ssa.tif'), overwrite = T)

png("../output/maps/africa-soil.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Texture (% sand)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(sand0_30, range = c(0, 80), fill_range = T, cex = 1.2, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

# ------------------------------------------------------------------------------
# Elevation slope
# create a sub-folder for elevation to store the .tif file 
if(!dir.exists(paste0(input_path, '/wc2.1_30s')))
  dir.create(paste0(input_path, '/wc2.1_30s'))
elevation <- geodata::elevation_global(0.5, path = input_path)
elevation <- terra::rast(paste0(input_path,'/wc2.1_30s/wc2.1_30s_elev.tif')) # elevation map at 30 sec resolution

# elevation <- terra::crop(elevation, ssa, mask = T) # failed in knitr::stitch_rhtml
elevation <- terra::crop(elevation, ssa_rast, mask = T)
elevation <- terra::resample(elevation, cropland_ha)
names(elevation) <- 'elevation'
terra::writeRaster(elevation, paste0(input_path, '/wc2.1_30s/elevation_ssa.tif'), overwrite = T)

png("../output/maps/africa-elevation.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Elevation map (m.a.s.l)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(elevation, range = c(0, 2500), fill_range = T, cex = 1.2, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

slope <- terra::terrain(elevation, 'slope', unit="radians", neighbors=8)
slope <- terra::crop(slope, ssa, mask = T)
slope <- terra::resample(slope, cropland_ha)
names(slope) <- 'slope'
terra::writeRaster(slope, paste0(input_path, '/wc2.1_30s/terrain_slope_ssa.tif'), overwrite = T)

png("../output/maps/africa_terrain-slope.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Terrain slope (%)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(100 * slope, range = c(0, 5), fill_range = T, cex = 1.2, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

# ------------------------------------------------------------------------------
# Annual average temperatures from WorldClim
# create a sub-folder for temperatures to store the .tif file
if(!dir.exists(paste0(input_path, '/temperature')))
  dir.create(paste0(input_path, '/temperature'))
# temperature <- geodata::worldclim_global('tavg', 0.5, path = input_path) #if it fails, download manually
temperature <- c(sapply(isocodes_ssa$ISO3, function(x) geodata::worldclim_country(x, 'tavg', path = input_path)))

temp0 <- terra::resample(terra::rast(), cropland_ha)
temperature <- for(i in isocodes_ssa$ISO3){
  temp1 <- terra::resample(terra::mean(temperature[[i]], na.rm = T), cropland_ha)
  temp0 <- terra::merge(temp0, temp1)
}
temperature <- terra::crop(temp0, ssa, mask = T)
terra::ext(temperature) <- terra::ext(cropland_ha)
names(temperature) <- 'temperature'
terra::writeRaster(temperature, paste0(input_path, '/temperature/avg_temperature_ssa.tif'), overwrite = T)

png("../output/maps/africa-temperature.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Average annual temperatures (m.a.s.l)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(temperature, range = c(0, 30), fill_range = T, cex = 1.2, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

# ------------------------------------------------------------------------------
# Travel time to the nearest city of 50,000 inhabitants
# create a sub-folder for travel times to store the .tif file
if(!dir.exists(paste0(input_path, '/travel')))
  dir.create(paste0(input_path, '/travel'))
market <- geodata::travel_time(to = 'city', size = 6, up = T, path = input_path) #if it fails, download manually

market <- terra::rast(paste0(input_path,'/travel/travel_time_to_cities_6.tif')) #travel time to the nearest city 6 is 50.000
market <- terra::crop(market, ssa, mask = T)
market <- terra::resample(market, cropland_ha)
names(market) <- 'market'
terra::writeRaster(market, paste0(input_path, '/travel/travel_time_to_cities_6.tif'), overwrite = T)

png("../output/maps/africa-market.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Travel time to the nearest city (min)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(market, range = c(0, 300), fill_range = T, cex = 1.2, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

# ------------------------------------------------------------------------------
# Run scripts 01.1 to 01.1.1 to 01.1.3 before continuing
# It summarizes annual average rainfall from 1981 to 2023

rainfall <- terra::rast(paste0(input_path,'/rainfall/rainfall_yearly/#_long_term_rainfall_avg.tif')) 
rainfall <- terra::crop(rainfall, ssa, mask = T)
rainfall <- terra::resample(rainfall, cropland_ha)
names(rainfall) <- 'rainfall'
terra::writeRaster(rainfall, paste0(input_path, '/rainfall/rainfall_ssa.tif'), overwrite = T)

png("../output/maps/africa-rainfall.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Annual rainfall (mm)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(rainfall, range = c(0, 2000), fill_range = T, cex = 1.2, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

# ------------------------------------------------------------------------------
# Climatic potential for agricultural production (maize) from  Bonilla-Cedrez et al, 2021
# create a sub-folder for water-limited yield potential to store the .tif file
if(!dir.exists(paste0(input_path, '/maize_water_lim_yield_SSA')))
  dir.create(paste0(input_path, '/FAO-maize_water_lim_yield_SSA'))

# Manually download the file and store in paste0(input_path, '/maize_water_lim_yield_SSA')
maizeyield <- terra::rast(paste0(input_path,'/maize_water_lim_yield_SSA/watlimsummary.tif')) # this has 3 layers
maizeyield <- maizeyield[[2]] # extract only the median
maizeyield <- terra::crop(maizeyield, ssa, mask = T)
maizeyield <- terra::resample(maizeyield, cropland_ha)
names(maizeyield) <- 'maizeyield'
terra::writeRaster(maizeyield, paste0(input_path, '/maize_water_lim_yield_SSA/maize_yield_ssa.tif'), overwrite = T)

png("../output/maps/africa-maizeyield.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Water-limited maize yield (kg/ha)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(maizeyield, range = c(0, 15000), fill_range = T, cex = 1.2, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

# ------------------------------------------------------------------------------
# Gross domestic product from FAOSTAT
# create a sub-folder for GDP to store the .tif file
if(!dir.exists(paste0(input_path, '/FAO-GDP')))
  dir.create(paste0(input_path, '/FAO-GDP'))

# Manually download countries GDP and store in "../data/raw/web_scrapped/date.file_name.csv" and adjust the following line
gdp_csv <- read_csv('../data/raw/web_scrapped/faostat/2024-04-18.FAOSTAT_data_en_GDP_per_capita.csv')       # GDP per capita in US $, 2015 retrieved from FAOSTAT
gdp_ssa <- gdp_csv |>
  select(Area, Year, Item, Value) |>
  pivot_wider(id_cols = c(Area, Year), names_from = Item, values_from = Value) |>
  rename(NAME_0 = Area, year = Year, gdp = 'Gross Domestic Product') |> 
  filter(NAME_0 %in% isocodes_ssa$NAME) |>
  group_by(NAME_0) |>
  summarize(gdp = mean(gdp, na.rm = T)) |> 
  inner_join(terra::as.data.frame(ssa))

gdp_vect <- ssa # Using  the country boundaries as a template and turning the vector into a raster
gdp_vect$gdp <- gdp_ssa$gdp
gdp_vect$NAME_0 <- gdp_ssa$NAME_0
gdp_vect$GID_0 <- gdp_ssa$GID_0
gdp_grid <- terra::rast(gdp_vect, nrow = 1000, ncol = 1000)
gdp <- terra::rasterize(gdp_vect, gdp_grid, field = 'gdp')
gdp <- terra::resample(gdp, cropland_ha)
names(gdp) <- 'gdp'
terra::writeRaster(gdp, paste0(input_path, '/FAO-GDP/gdp_ssa.tif'), overwrite = T)

png("../output/maps/africa-gdp.png", units = "in", width = 5.5, height = 5.5, res = 1000)
par(oma = c(0, 0, 0, 4), mar = c(5, 4, 4, 8) + 0.1)  # adjust margins
terra::plot(ssa, col = 'azure', main = 'Gross Domestic Product per country, $', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(gdp, range = c(0, 5000), fill_range = T, cex = 1.2, axes = F, add = T)
dev.off()
# ------------------------------------------------------------------------------
# Poverty index from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5OGWYM
# create a sub-folder for GDP to store the .tif file
if(!dir.exists(paste0(input_path, '/poverty')))
  dir.create(paste0(input_path, '/poverty'))

# create a temporary directory
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)

# Manually download country wealth indices and store in paste0(input_path, '/poverty') and adjust the following line
poverty_file_list <- unzip(dir(paste0(input_path, '/poverty'), full.names = T)[grep('poverty.zip$', dir(paste0(input_path, '/poverty')))], exdir = temporary_dir)
wealth_index_zip_list <- poverty_file_list[grep('estimated_wealth_index\\.shp\\.zip$', poverty_file_list)]
wealth_index_list <- lapply(wealth_index_zip_list, function(x){
  unzip(x, exdir = temporary_dir)
})
wealth_index_shp <- unlist(wealth_index_list)[grep('_estimated_wealth_index\\.shp$', unlist(wealth_index_list))]
wealth_index_rast <- sapply(wealth_index_shp, function(x) {
  empty_rast <- terra::rast(terra::vect(x), res = 0.01)
  prob_poor <- terra::resample(terra::rasterize(terra::vect(x), empty_rast, field = 'img_prob_p'), cropland_ha)
  names(prob_poor) <- 'prob_poor'
  wealth_index <- terra::resample(terra::rasterize(terra::vect(x), empty_rast, field = 'img_prob_p'), cropland_ha)
  names(wealth_index) <- 'wealth_index'
  cty_name <- unique(terra::vect(x)$country_na)
  terra::writeRaster(c(prob_poor, wealth_index), paste0(temporary_dir, '/final_rast_',cty_name, '.tif'), overwrite = T)
})
wealth_index_ssa <- Sys.glob(paste0(temporary_dir, '/final_rast*.tif'))
wealth <- terra::rast()
for(i in sort(wealth_index_ssa)) {
  print(i)
  wealth <- terra::merge(terra::resample(wealth, cropland_ha), terra::rast(i))
}
names(wealth) <- c('prob_poor', 'wealth_index')
unlink(temporary_dir, recursive = T)
terra::writeRaster(wealth, paste0(input_path, '/poverty/wealth_ssa.tif'), overwrite = T)
# ------------------------------------------------------------------------------
# the inital stack of layers
stacked_00 <- c(cropland_ha, cattle, pop, cropland_per_capita, 
                sand0_30, elevation, slope, temperature, rainfall,
                market, maizeyield, gdp, wealth)
terra::writeRaster(stacked_00, '../data/processed/all_predictors.tif', overwrite = T)
# ------------------------------------------------------------------------------
# The following layers will serve for post-processing, namely masking out areas where farm size will not be predicted
# the first is to mask out forest areas, the second for drylands (receiving less than 200 mm/year)
# Mind that there are not require for modelling, but only for visualization

# Mask out forest areas (from Afrilearn on GITHUB)
require(afrilearndata)
land_cover <- terra::rast(system.file('extdata', 'afrilandcover.grd', package = 'afrilearndata', mustWork = TRUE))
forests <- land_cover == 2 | land_cover == 4 | land_cover == 5 | land_cover == 8
forests <- terra::ifel(forests, NA, 1)
mask_forest_ssa <- terra::crop(forests, ssa)

# Mask out drylands
drylands <- terra::ifel(stacked_00$rainfall < 200, NA, 1)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------