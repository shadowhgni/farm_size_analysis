# Understanding and predicting the variability of farm size across SSA
# making spatial data ready for analysis
# Note that all spatial data (shapefiles and rasters) are stored in a separate folder, 
# which is convenient for multiple projects.

# Open the file from the folder and set working directory, using the here package
setwd(here::here())

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
fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')

# clean previous maps from the output folder (in case there is an update)
outdated_maps=c('../output/maps/africa-cattle.png', '../output/maps/africa-cropland.png',
                '../output/maps/africa-cropland_2.png', '../output/maps/africa-elevation.png',
                '../output/maps/africa-lsms.png', '../output/maps/africa-market.png',
                '../output/maps/africa-population.png','../output/maps/africa-soil.png')

map_names=c('cattle', 'cropland', 'gdp', 'lsms', 'maizeyield', 'market',
            'pop', 'poverty', 'rainfall', 'sand0_30', 'slope' )
for(i in fourteen_countries){
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
# Make sure to manually change the folder name of Cote d'Ivoire and Guinea Bissau to the spelling in fourteen_countries

# ------------------------------------------------------------------------------ 
# Using cropland from SPAM; note that the raster is named geosurvey_ha
# spam
all_spam_crops <- data.frame(geodata::spamCrops())[['code']]
for(sv in c('harv_area', 'phys_area', 'prod', 'yield', 'val_prod')){  
  print(sv)
  variable <- lapply(
    all_spam_crops, 
    function(crop){
      if(!dir.exists(paste0(input_path, '/spam/spam2017/', sv)))
        create(paste0(input_path, '/spam/spam2017/', sv))
      geodata::crop_spam(crop, sv, path = paste0(input_path, '/spam/spam2017/', sv), africa = T) # africa = T means 2017 layer (without Sudan)
      if(!dir.exists(paste0(input_path, '/spam/spam2010/', sv)))
        create(paste0(input_path, '/spam/spam2010/', sv))
      geodata::crop_spam(crop, sv, path = paste0(input_path, '/spam/spam2010/', sv), africa = F) # africa = F means 2010 layer 
    } 
  )   
  variable <- terra::rast(variable)
  names(variable) <- all_spam_crops
  variable <- terra::crop(variable, ssa, mask = T)
  terra::writeRaster(variable, paste0(input_path, '/spam/', sv, '/all_spam_crops.tif'), overwrite = T)
}

each_2017_crop <- terra::rast(paste0(input_path, '/spam/spam2017/', dir(paste0(input_path,'/spam/spam2017'))[grep('_H_[A-Z]+_A.tif$', dir(paste0(input_path,'/spam/spam2017')))]) ) # _A is total area (rainfed  + irrigated)
each_2017_crop <- terra::crop(each_2017_crop, ssa, mask = T)
names(each_2017_crop) <- substr(names(each_2017_crop), 20, 23)
# Pick Sudan data from SPAM 2010, and merge with SPAM 2017
each_2010_crop <- terra::rast(paste0(input_path, '/spam/spam2010/', dir(paste0(input_path,'/spam/spam2010'))[grep('_H_[A-Z]+_A.tif$', dir(paste0(input_path,'/spam/spam2010')))]) )
names(each_2010_crop) <- substr(names(each_2010_crop), 23, 26)
sudan_mask <- terra::crop(each_2010_crop, subset(ssa, ssa$NAME_0 == 'Sudan'), mask = T)
each_crop <- terra::merge(each_2017_crop, sudan_mask); rm(each_2010_crop, each_2017_crop)

all_2017_crops <- terra::rast(paste0(input_path, '/spam/spam2017/', dir(paste0(input_path,'/spam/spam2017'))[grep('_H_[A-Z]+_A.tif$', dir(paste0(input_path,'/spam/spam2017')))]) ) # _A is total area (rainfed  + irrigated)
all_2017_crops <- sum(all_2017_crops, na.rm = T)
crop_mask <- all_2017_crops
crop_mask <- terra::crop(crop_mask, ssa, mask =T)

all_2010_crops <- terra::rast(paste0(input_path, '/spam/spam2010/', dir(paste0(input_path,'/spam/spam2010'))[grep('_H_[A-Z]+_A.tif$', dir(paste0(input_path,'/spam/spam2010')))]) )
all_2010_crops <- sum(all_2010_crops, na.rm = T)
sudan_mask <- terra::crop(all_2010_crops, subset(ssa, ssa$NAME_0 == 'Sudan'), mask = T)

geosurvey_ha <- terra::merge(crop_mask, sudan_mask)
names(geosurvey_ha) <- 'cropland'
terra::writeRaster(geosurvey_ha, file = paste0(input_path, '/spam/spam_cropland_ssa.tif'), overwrite = T)

png("../output/maps/africa-cropland.png", units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Cropland (ha)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(geosurvey_ha, cex = 1.2, axes = F, add = T, plg = list(loc  =  "bottom"))
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
cattle <- terra::resample(cattle, geosurvey_ha)
names(cattle) <- 'cattle'
terra::writeRaster(cattle, paste0(input_path, '/cattle-density/2010_cattle_density_ssa.tif'), overwrite = T)


# ------------------------------------------------------------------------------
# Population density
# create a sub-folder for population density to store the .tif file 
if(!dir.exists(paste0(input_path, '/population')))
  dir.create(paste0(input_path, '/population'))
pop <- geodata::population(2020, 0.5, path = input_path)
pop <- terra::rast(paste0(input_path,'/population/pop/gpw_v4_population_density_rev11_2020_30s.tif') )
pop <- terra::crop(pop, ssa, mask = T)
pop <- terra::resample(pop, geosurvey_ha)
names(pop) <- 'pop'
terra::writeRaster(pop, paste0(input_path, '/population/2020_population_density_ssa.tif'), overwrite = T)


# ------------------------------------------------------------------------------
# Create a raster for cropland per capita
cropland_per_capita <- geosurvey_ha / pop
cropland_per_capita[is.infinite(cropland_per_capita)] <- NA
names(cropland_per_capita) <- 'cropland_per_capita'
terra::writeRaster(cropland_per_capita, paste0(input_path, '/spam/cropland_per_capita_ssa.tif'), overwrite = T)

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
sand0_30 <- terra::crop(sand0_30, ssa, mask = T)
sand0_30 <- terra::resample(sand0_30, geosurvey_ha)
names(sand0_30) <- 'sand'
terra::writeRaster(sand0_30, paste0(input_path, '/soil_world/sand_content_0_30cm_ssa.tif'), overwrite = T)

png("../output/maps/africa-soil.png", units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Texture (% sand)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(sand0_30, cex = 1.2, axes = F, add = T, plg = list(loc  =  "bottom"))
terra::plot(ssa, axes = F, add = T)
dev.off()
# ------------------------------------------------------------------------------
# Elevation slope
# create a sub-folder for elevation to store the .tif file 
if(!dir.exists(paste0(input_path, '/wc2.1_30s')))
  dir.create(paste0(input_path, '/wc2.1_30s'))
elevation <- geodata::elevation_global(0.5, path = input_path)
elevation <- terra::rast(paste0(input_path,'/wc2.1_30s/wc2.1_30s_elev.tif')) # elevation map at 30 sec resolution
elevation <- terra::crop(elevation, ssa, mask = T)
elevation <- terra::resample(elevation, geosurvey_ha)
names(elevation) <- 'elevation'
terra::writeRaster(elevation, paste0(input_path, '/wc2.1_30s/elevation_ssa.tif'), overwrite = T)

png("../output/maps/africa-elevation.png", units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Elevation map (m.a.s.l)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(elevation, cex = 1.2, axes = F, add = T, plg = list(loc  =  "bottom"))
terra::plot(ssa, axes = F, add = T)
dev.off()

slope <- terra::terrain(elevation, 'slope', unit="radians", neighbors=8)
slope <- terra::crop(slope, ssa, mask = T)
slope <- terra::resample(slope, geosurvey_ha)
names(slope) <- 'slope'
terra::writeRaster(slope, paste0(input_path, '/wc2.1_30s/terrain_slope_ssa.tif'), overwrite = T)

png("../output/maps/africa_terrain-slope.png", units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Terrain slope map (m.a.s.l)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(slope, cex = 1.2, axes = F, add = T, plg = list(loc  =  "bottom"))
terra::plot(ssa, axes = F, add = T)
dev.off()
# ------------------------------------------------------------------------------
# Annual average temperatures from WorldClim
# create a sub-folder for temperatures to store the .tif file
if(!dir.exists(paste0(input_path, '/temperature')))
  dir.create(paste0(input_path, '/temperature'))
# temperature <- geodata::worldclim_global('tavg', 0.5, path = input_path) #if it fails, download manually
temperature <- c(sapply(isocodes_ssa$ISO3, function(x) geodata::worldclim_country(x, 'tavg', path = input_path)))

temp0 <- terra::resample(terra::rast(), geosurvey_ha)
temperature <- for(i in isocodes_ssa$ISO3){
  temp1 <- terra::resample(terra::mean(temperature[[i]], na.rm = T), geosurvey_ha)
  temp0 <- terra::merge(temp0, temp1)
}
temperature <- terra::crop(temp0, ssa, mask = T)
names(temperature) <- 'temperature'
terra::writeRaster(temperature, paste0(input_path, '/temperature/avg_temperature_ssa.tif'), overwrite = T)

png("../output/maps/africa-temperature.png", units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Average annual temperatures (m.a.s.l)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(temperature, cex = 1.2, axes = F, add = T, plg = list(loc  =  "bottom"))
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
market <- terra::resample(market, geosurvey_ha)
names(market) <- 'market'
terra::writeRaster(market, paste0(input_path, '/travel/travel_time_to_cities_6.tif'), overwrite = T)

png("../output/maps/africa-market.png", units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Travel time to the nearest city (min)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(market, cex = 1.2, axes = F, add = T, plg = list(loc = "bottom"))
terra::plot(cty, axes = F, add = T)
dev.off()

# ------------------------------------------------------------------------------
# Run scripts 01.1 to 01.1.1 to 01.1.3 before continuing
# It summarizes annual average rainfall from 1981 to 2023

rainfall <- terra::rast(paste0(input_path,'/rainfall/rainfall_yearly/#_long_term_rainfall_avg.tif')) 
rainfall <- terra::crop(rainfall, ssa, mask = T)
rainfall <- terra::resample(rainfall, geosurvey_ha)
names(rainfall) <- 'rainfall'
terra::writeRaster(rainfall, paste0(input_path, '/rainfall/rainfall_ssa.tif'), overwrite = T)

png("../output/maps/africa-rainfall.png", units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Annual rainfall (mm)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(rainfall, cex = 1.2, axes = F, add = T, plg = list(loc = "bottom"))
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
maizeyield <- terra::resample(maizeyield, geosurvey_ha)
names(maizeyield) <- 'maizeyield'
terra::writeRaster(maizeyield, paste0(input_path, '/maize_water_lim_yield_SSA/maize_yield_ssa.tif'), overwrite = T)

png("../output/maps/africa-maizeyield.png", units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Water-limited maize yield (kg/ha)', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(maizeyield, cex = 1.2, axes = F, add = T, plg = list(loc = "bottom"))
terra::plot(cty, axes = F, add = T)
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
gdp <- terra::resample(gdp, geosurvey_ha)
names(gdp) <- 'gdp'
terra::writeRaster(gdp, paste0(input_path, '/FAO-GDP/gdp_ssa.tif'), overwrite = T)

png("../output/maps/africa-gdp.png", units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Gross Domestic Product per country, $', panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4))
terra::plot(maizeyield, cex = 1.2, axes = F, add = T, plg = list(loc = "bottom"))
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
  prob_poor <- terra::resample(terra::rasterize(terra::vect(x), empty_rast, field = 'img_prob_p'), geosurvey_ha)
  names(prob_poor) <- 'prob_poor'
  wealth_index <- terra::resample(terra::rasterize(terra::vect(x), empty_rast, field = 'img_prob_p'), geosurvey_ha)
  names(wealth_index) <- 'wealth_index'
  cty_name <- unique(terra::vect(x)$country_na)
  terra::writeRaster(c(prob_poor, wealth_index), paste0(temporary_dir, '/final_rast_',cty_name, '.tif'), overwrite = T)
})
wealth_index_ssa <- Sys.glob(paste0(temporary_dir, '/final_rast*.tif'))
wealth <- terra::rast()
for(i in sort(wealth_index_ssa)) {
  print(i)
  wealth <- terra::merge(terra::resample(wealth_index_rast, geosurvey_ha), terra::rast(i))
}
names(wealth) <- c('prob_poor', 'wealth_index')
unlink(temporary_dir, recursive = T)
terra::writeRaster(wealth, paste0(input_path, '/poverty/wealth_ssa.tif'), overwrite = T)
# ------------------------------------------------------------------------------
# the inital stack of layers
stacked_00 <- c(geosurvey_ha, cattle, pop, cropland_per_capita, 
                sand0_30, elevation, slope, temperature, rainfall,
                market, maizeyield, gdp, wealth)
terra::writeRaster(stacked_00, '../data/processed/all_predictorss.tif', overwrite = T)
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
drylands <- terra::ifel(stacked$rainfall < 200, NA, 1)
# # force terra to use disk-based processing and 50% of RAM (Use this if R crashes because of limited memory)
# terra::terraOptions(memfrac = 0.5)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------