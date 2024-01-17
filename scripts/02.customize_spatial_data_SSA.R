# Understanding and predicting the variability of farm size across SSA
# making spatial data ready for the random-forest

# load packages
require(tidyverse)
# Set working directory
setwd('C:/Users/Gebruiker/Documents/Harare 2023/Farm sizes across Africa/scripts')
# Clean environment
rm(list=ls())

# the RF initially starts with 3 variables (cattle density, rural population [not density], and cropland [in use, not available]), but we will discuss adding some more variables
input_path <- 'C:/Users/Gebruiker/Documents/Harare 2023/Spatial_data_repository'


# clean previous maps from the output foutdateder (in case there is an update)
outdated_maps=c('../output/maps/africa-cattle.png', '../output/maps/africa-cropland.png',
                '../output/maps/africa-cropland_2.png', '../output/maps/africa-elevation.png',
                '../output/maps/africa-lsms.png', '../output/maps/africa-market.png',
                '../output/maps/africa-population.png','../output/maps/africa-soil.png')
six_countries=c('Ethiopia', 'Malawi', 'Niger', 'Nigeria', 'Tanzania', 'Uganda')
map_names=c('cattle', 'cereals', 'cropland', 'elevation', 'lsms', 'market',
            'pop', 'rainfall', 'roots', 'sand0_30')
for(i in six_countries){
  for(j in map_names){
    country_map=paste0('../output/maps/', i, '-', j, '.png')
    outdated_maps=c(outdated_maps,country_map)
  }
}
for(i in outdated_maps){
  if(file.exists(i)) file.remove(i)
}

# ------------------------------------------------------------------------------
# shapefile
country <- geodata::world(path=input_path, resolution=5, level=0)
# sub_districts <- geodata::world(path=input_path, resolution=5, level=1) # modify the level to the desired adminstrative division. NOT YET AVAILABLE!
# unique(country$NAME_0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='Réunion' & NAME!='Saint Helena' & NAME!='São Tomé and Príncipe' & NAME!='Seychelles') # keep the mainland + Madagascar only, remove islands
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
cty <- subset(ssa, ssa$NAME_0=='Nigeria')  #Change Nigeria to the desired country

# ------------------------------------------------------------------------------ 
# geosurvey
geosurvey <- terra::rast(paste0(input_path,'/geosurvey_cropland.tif'))
geosurvey <- terra::crop(x=geosurvey, y=ssa, mask=T)
cs <- terra::cellSize(geosurvey, unit = "ha") # find the size of a cell. important for cell of cell * prob. to find a crop ==> estimated cultivated area for that crop. here, it is just for cultivated area
geosurvey_ha <- cs * geosurvey 
geosurvey_ha <- terra::aggregate(geosurvey_ha, 10, FUN=mean, na.rm=T) ##check this in other codes!!
names(geosurvey_ha) <- 'cropland'

# Surface area under cereal crops
maiz_ha <- geodata::crop_spam(crop='maize', path=input_path, africa=T) 
sorg_ha <- geodata::crop_spam(crop='sorghum', path=input_path, africa=T) 
pmil_ha <- geodata::crop_spam(crop='pearl millet', path=input_path, africa=T) 
smil_ha <- geodata::crop_spam(crop='small millet', path=input_path, africa=T) 
rice_ha <- geodata::crop_spam(crop='rice', path=input_path, africa=T) 
whea_ha <- geodata::crop_spam(crop='wheat', path=input_path, africa=T) 
barl_ha <- geodata::crop_spam(crop='barley', path=input_path, africa=T) 
ocer_ha <- geodata::crop_spam(crop='other cereals', path=input_path, africa=T) 

cere_ha <- maiz_ha + sorg_ha + pmil_ha + smil_ha + rice_ha + whea_ha + barl_ha + ocer_ha
cere_ha <- terra::resample(cere_ha, geosurvey_ha)
cere_prct <- 100*cere_ha/geosurvey_ha  #share of the cropland under cereals
names(cere_ha)[1] <- 'cereal_ha'

# Surface area under root and tuber crops
cass_ha <- geodata::crop_spam(crop='cassava', path=input_path, africa=T) 
pota_ha <- geodata::crop_spam(crop='potato', path=input_path, africa=T) 
swpo_ha <- geodata::crop_spam(crop='sweet potato', path=input_path, africa=T) 
yams_ha <- geodata::crop_spam(crop='yams', path=input_path, africa=T) 
orts_ha <- geodata::crop_spam(crop='other roots', path=input_path, africa=T) 

root_ha <- cass_ha + pota_ha + swpo_ha + yams_ha + orts_ha
root_ha <- terra::resample(root_ha, geosurvey_ha)
root_prct <- root_ha/geosurvey_ha  #share of the cropland under root crops
names(root_ha)[1] <- 'root_ha'

png("../output/maps/africa-cropland.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(ssa, col='azure', main='Cropland (ha)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(geosurvey_ha, cex=1.2, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()

png("../output/maps/africa-cropland_2.png", units="in", width=5.5, height=5.5, res=1000)
par(mfrow=c(1,3),mpg=c(2,0.5,1.5))
terra::plot(ssa, col='azure', main='Cropland (ha)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(geosurvey_ha, cex=1.2, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)

terra::plot(ssa, col='azure', main='Cereals (ha)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(cere_ha$cereal_ha, cex=1.2, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)

terra::plot(ssa, col='azure', main='Roots & tubers (ha)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(root_ha$root_ha, cex=1.8, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()


# ------------------------------------------------------------------------------
cattle <- terra::rast(paste0(input_path,'/cattle-density/5_Ct_2010_DA.tif') )
cattle <- terra::crop(cattle, ssa, mask=T)
cattle <- terra::resample(cattle, geosurvey_ha) 
names(cattle) <- 'cattle'

png("../output/maps/africa-cattle.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(ssa, col='azure', main='Cattle density (TLU)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(cattle, cex=1.2, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()
# ------------------------------------------------------------------------------
pop <- terra::sprc(Sys.glob(paste0(input_path,'/rural-population/*_rural_pop_1km.tif')))
pop <- terra::merge(pop)
pop <- terra::crop(pop, ssa, mask=T)
pop <- terra::resample(pop, geosurvey_ha) 
names(pop) <- 'population'

png("../output/maps/africa-population.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(ssa, col='azure', main='Population density (inhabitants)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(pop, cex=1.2, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()

# ------------------------------------------------------------------------------
sand05 <- terra::rast(paste0(input_path,'/soil_world/sand_0-5cm_mean_30s.tif') )
sand15 <- terra::rast(paste0(input_path,'/soil_world/sand_5-15cm_mean_30s.tif') )
sand30 <- terra::rast(paste0(input_path,'/soil_world/sand_15-30cm_mean_30s.tif') )
sand0_30 <- (5*sand05 + 10*sand15 + 15*sand30)/30  # finish with 5*sand30 if 0-20cm is preferred over 0-30cm
sand0_30 <- terra::crop(sand0_30, ssa, mask=T)
sand0_30 <- terra::resample(sand0_30, geosurvey_ha)
names(sand0_30) <- 'sand'

png("../output/maps/africa-soil.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(ssa, col='azure', main='Texture (% sand)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(sand0_30, cex=1.2, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()
# ------------------------------------------------------------------------------
elevation <- terra::rast(paste0(input_path,'/wc2.1_30s/wc2.1_30s_elev.tif')) # elevation map at 30 sec resolution
elevation <- terra::crop(elevation, ssa, mask=T)
elevation <- terra::resample(elevation, geosurvey_ha)
names(elevation) <- 'elevation'

png("../output/maps/africa-elevation.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(ssa, col='azure', main='Elevation map (m.a.s.l)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(elevation, cex=1.2, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()

# ------------------------------------------------------------------------------
market <- terra::rast(paste0(input_path,'/travel/travel_time_to_cities_1.tif')) #travel distance to the nearest city
# market <- terra::project(market, terra::crs(geosurvey_ha))
market <- terra::crop(market, ssa, mask=T)
market <- terra::resample(market, geosurvey_ha)
names(market) <- 'market'

png("../output/maps/africa-market.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(ssa, col='azure', main='Travel time to the nearest city (min)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(market, cex=1.2, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()


# ------------------------------------------------------------------------------
rainfall <- terra::rast(paste0(input_path,'/CHIRPS/longterm-mean-yearly-rainfall.tif')) #travel distance to the nearest city
rainfall <- terra::crop(rainfall, ssa, mask=T)
rainfall <- terra::resample(rainfall, geosurvey_ha)
names(rainfall) <- 'rainfall'

png("../output/maps/africa-rainfall.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(ssa, col='azure', main='Annual rainfall (mm)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(rainfall, cex=1.2, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()


# ------------------------------------------------------------------------------
# lsms data
load('../data/raw/received/lsms_and_geodata.rda') # this dataset has served for previous publication, sent by Thomas Delaune
lsms <- subset(lsms_and_geodata, !is.na(lsms_and_geodata$farm_area_ha)) # get rid of records that contain no values for the farm size
lsms <- subset(lsms, farm_area_ha > 0 & farm_area_ha < 50)              # get rod of records of farms with 0 ha or less, as well as those with more than 50 ha (arbitrary threshold for smallholding, I believe)
lsms$farm_area_ha <- round(lsms$farm_area_ha, 2) 
lsms <- terra::vect(sf::st_transform(lsms, crs = 4326))

length(unique(lsms$geometry)) # 2444 unique GPS points. These points are meant not for individual farms but for a cluster of farms in a sampling zone (approx 10 farms share the same geopoint)
length(unique(lsms$geometry[lsms$country_name=='Nigeria'])) # 2444 unique GPS points
length(lsms$farm_area_ha[lsms$country_name=='Nigeria'])     # here, it appears that some 500 additional farms make use of existing records... so not 10 farms for a single record in Nigeria. 1.2 farms for 1 geo-point 

png("../output/maps/africa-lsms.png", units="in", width=11, height=5.5, res=1000)
par(mfrow=c(1,2), mgp=c(2,0.5,0))
terra::plot(ssa, col='azure', main='LSMS all countries', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(lsms, col='red', cex=0.4, axes=F, add=T)
terra::plot(ssa, axes=F, add=T)
boxplot(lsms$farm_area_ha~lsms$country_name, ylim=c(0, 15), xlab='', ylab='Farm size (ha)', cex.axis=0.85, cex.lab=1.3)
dev.off()

# raster stack
stacked <- c(geosurvey_ha, cere_ha, root_ha, cattle, pop, sand0_30,
             elevation, market, rainfall, lsms)  # sand; TO DO rainfall, elevation, market

# prepare LSMS dataset for RF analysis
lsms_final <- lsms[c('farm_area_ha')] 
lsms_final <- cbind(data.frame(lsms_final), lsms_final |> terra::geom() |> as.data.frame())
lsms_final <- lsms_final[c(1,4,5)]
lsms_spatial <- lsms_final %>%
  sf::st_as_sf(coords = c("x", "y")) %>%
  sf::st_set_crs(4326)

# merge data sets
lsms_spatial <- data.frame(cbind(lsms_spatial, terra::extract(stacked, terra::vect(lsms_spatial))))
lsms_spatial <- lsms_spatial[c('farm_area_ha',
                               'cropland', 'cereal_ha', 'root_ha','cattle', 
                               'population', 'sand', 'elevation', 'market', 'rainfall')]
lsms_spatial <- na.omit(lsms_spatial) # ~600 observations in/close to urban areas; see how to solve
lsms_spatial <- subset(lsms_spatial, farm_area_ha < 20) # noise?


save(stacked,lsms_spatial,file='../data/processed/stacked_africa.Rdata')


# ------------------------------------------------------------------------------
# per country
per_country_data=function(my_country){
  cty <- subset(ssa, ssa$NAME_0==my_country)
  lsms_cty <- terra::crop(lsms, cty)
  png(paste0('../output/maps/',my_country,'-lsms.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0('LSMS - ',my_country), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(lsms_cty, col='red', cex=1, axes=F, add=T)
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  
  cropland_cty <- terra::crop(geosurvey_ha, cty, mask=T)
  png(paste0('../output/maps/',my_country,'-cropland.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Cropland (ha)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(cropland_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  
  cere_ha_cty <- terra::crop(cere_ha, cty, mask=T)
  png(paste0('../output/maps/',my_country,'-cereals.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Area under cereal cultivation (ha)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(cere_ha_cty$cereal_ha , cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  
  root_ha_cty <- terra::crop(root_ha, cty, mask=T)
  png(paste0('../output/maps/',my_country,'-roots.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Area under root and tuber cultivation (ha)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(root_ha_cty$root_ha ,cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  
  cattle_cty <- terra::crop(cattle, cty, mask=T)
  png(paste0('../output/maps/',my_country,'-cattle.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Cattle density (#/km2)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(cattle_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  
  pop_cty <- terra::crop(pop, cty, mask=T)
  png(paste0('../output/maps/',my_country,'-pop.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Population density (#/km2)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(pop_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  
  sand0_30_cty <- terra::crop(sand0_30, cty, mask=T)
  png(paste0('../output/maps/',my_country,'-sand0_30.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Soil texture (% sand)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(sand0_30_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  
  pop_cty <- terra::crop(pop, cty, mask=T)
  png(paste0('../output/maps/',my_country,'-pop.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Population density (#/km2)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(pop_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  
  elevation_cty <- terra::crop(elevation, cty, mask=T)
  png(paste0('../output/maps/',my_country,'-elevation.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Elevation map (m.a.s.l)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
  terra::plot(elevation_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
  terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  
  market_cty <- terra::crop(market, cty, mask=T)
  png(paste0('../output/maps/',my_country,'-market.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Travel time to nearest city (min)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
  terra::plot(market_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
  terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  
  rainfall_cty <- terra::crop(rainfall, cty, mask=T)
  png(paste0('../output/maps/',my_country,'-rainfall.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Rainfall (mm)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(rainfall_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
    dev.off()
  })
  
  stacked_cty <- c(cropland_cty, cere_ha_cty, root_ha_cty,
                   cattle_cty, pop_cty, sand0_30_cty,
                   elevation_cty, market_cty, rainfall_cty, lsms_cty)  
  
  # prepare lsms_cty dataset for RF analysis
  lsms_cty_final <- lsms_cty[c('farm_area_ha','household_size_ind')] 
  lsms_cty_final <- cbind(data.frame(lsms_cty_final), lsms_cty_final |> terra::geom() |> as.data.frame())
  lsms_cty_final <- lsms_cty_final[c(1,2,5,6)]
  lsms_cty_spatial <- lsms_cty_final %>%
    sf::st_as_sf(coords = c("x", "y")) %>%
    sf::st_set_crs(4326)
  
  # merge data sets
  lsms_cty_spatial <- data.frame(cbind(lsms_cty_spatial, terra::extract(stacked_cty, terra::vect(lsms_cty_spatial))))
  lsms_cty_spatial <- lsms_cty_spatial[c('farm_area_ha', 'household_size_ind',
                                 'cropland', 'cereal_ha', 'root_ha',
                                 'cattle', 'population', 'sand', 'elevation', 'market')]
  lsms_cty_spatial <- na.omit(lsms_cty_spatial) # ~600 observations in/close to urban areas; see how to solve
  lsms_cty_spatial <- subset(lsms_cty_spatial, farm_area_ha < 20) # noise?
  
  
  assign(paste0('stacked_',my_country),stacked_cty,envir = .GlobalEnv)
  save(stacked_cty,lsms_cty,file=paste0('../data/processed/stacked_',my_country,'.Rdata'))
}

sapply(six_countries,per_country_data)