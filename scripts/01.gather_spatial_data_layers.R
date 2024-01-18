# making spatial data ready for the random-forest
# the RF initially starts with 3 variables (cattle density, rural population [not density], and cropland [in use, not available])

# Create a path where all the spatial data will be downloaded
input_path <- 'D:/User/Spatial_data_repository'  #this is just an example

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
geosurvey <- geodata::cropland(source='QED', path=paste0(input_path)) # takes a while
geosurvey <- terra::crop(x=geosurvey, y=ssa, mask=T)
# terra::plot(geosurvey); terra::plot(ssa, add=T)
# terra::boxplot(geosurvey)
# as.data.frame(geosurvey)
cs <- terra::cellSize(geosurvey, unit = "ha") # find the size of a cell. important for cell of cell * prob. to find a crop ==> estimated cultivated area for that crop. here, it is just for cultivated area
geosurvey_ha <- cs * geosurvey 
geosurvey_ha <- terra::aggregate(geosurvey_ha, 10, FUN=mean, na.rm=T) ##check this in other codes!!
names(geosurvey_ha) <- 'cropland'
# mz_ha <- geodata::crop_spam(crop='MAIZ', path=input_path, africa=T) # check spamCrops for crop codes. 
# terra::plot(mz_ha$MAIZ_area_all)


# maiz_ha <- geodata::crop_spam(crop='maize', path=input_path, africa=T) 
# sorg_ha <- geodata::crop_spam(crop='sorghum', path=input_path, africa=T) 
# pmil_ha <- geodata::crop_spam(crop='pearl millet', path=input_path, africa=T) 
# smil_ha <- geodata::crop_spam(crop='small millet', path=input_path, africa=T) 
# rice_ha <- geodata::crop_spam(crop='rice', path=input_path, africa=T) 
# whea_ha <- geodata::crop_spam(crop='wheat', path=input_path, africa=T) 
# barl_ha <- geodata::crop_spam(crop='barley', path=input_path, africa=T) 
# ocer_ha <- geodata::crop_spam(crop='other cereals', path=input_path, africa=T) 

# cassava_ha <- geodata::crop_spam(crop='cassava', path=input_path, africa=T)
# terra::plot(cassava_ha$cassava_area_all)

# ------------------------------------------------------------------------------
# lsms data
load('../data/raw/lsms_and_geodata.rda') # this dataset has served for previous publication, sent by Thomas Delaune
lsms <- subset(lsms_and_geodata, !is.na(lsms_and_geodata$farm_area_ha)) # get rid of records that contain no values for the farm size
lsms <- subset(lsms, farm_area_ha > 0 & farm_area_ha < 50)              # get rod of records of farms with 0 ha or less, as well as those with more than 50 ha (arbitrary threshold for smallholding, I believe)
lsms$farm_area_ha <- round(lsms$farm_area_ha, 2) 
length(unique(lsms$geometry)) # 2444 unique GPS points. These points are meant not for individual farms but for a cluster of farms in a sampling zone (approx 10 farms share the same geopoint)
length(unique(lsms$geometry[lsms$country_name=='Nigeria'])) # 2444 unique GPS points
length(lsms$farm_area_ha[lsms$country_name=='Nigeria'])     # here, it appears that some 500 additional farms make use of existing records... so not 10 farms for a single record in Nigeria. 1.2 farms for 1 geo-point 
png("../output/maps/africa-lsms.png", units="in", width=11, height=5.5, res=1000)
par(mfrow=c(1,2), mgp=c(2,0.5,0))
lsms <- terra::vect(sf::st_transform(lsms, crs = 4326))
terra::plot(ssa, col='azure', main='LSMS all countries', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(lsms, col='red', cex=0.4, axes=F, add=T)
terra::plot(ssa, axes=F, add=T)
boxplot(lsms$farm_area_ha~lsms$country_name, ylim=c(0, 15), xlab='', ylab='Farm size (ha)', cex.axis=0.85, cex.lab=1.3)
dev.off()

# ------------------------------------------------------------------------------
# per country
# (1) lsms data
lsms_cty <- terra::crop(lsms, cty)
png("../output/maps//nigeria-lsms.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(cty, col='azure', main='LSMS Nigeria', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(lsms_cty, col='red', cex=1, axes=F, add=T)
terra::plot(cty, axes=F, add=T)
dev.off()
# (2) cropland
cropland_cty <- terra::crop(geosurvey_ha, cty, mask=T)
png("../output/maps/nigeria-cropland.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(cty, col='azure', main='Cropland (ha)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(cropland_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()
# (3) cattle
cattle <- terra::rast(paste0(input_path,'/cattle-density/5_Ct_2010_DA.tif') )
cattle <- terra::crop(cattle, ssa, mask=T)
cattle <- terra::resample(cattle, geosurvey_ha) 
names(cattle) <- 'cattle'
cattle_cty <- terra::crop(cattle, cty, mask=T)
png("../output/maps/nigeria-cattle.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(cty, col='azure', main='Cattle density (#/km2)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(cattle_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()
# (4) rural population
pop <- terra::sprc(Sys.glob(paste0(input_path,'/rural-population/*_rural_pop_1km.tif')))
pop <- terra::merge(pop)
pop <- terra::crop(pop, ssa, mask=T)
pop <- terra::resample(pop, geosurvey_ha) 
names(pop) <- 'population'


# (5) sand percent at 0-30 cm
sand05 <- geodata::soil_world('sand',5,'mean', path = input_path)
sand15 <- geodata::soil_world('sand',15,'mean', path = input_path)
sand30 <- geodata::soil_world('sand',30,'mean', path = input_path)



# (6) rainfall (length of growing season)


# (7) elevation
elevation <- geodata::elevation_global(0.5, path = input_path)
# elevation <- terra::project(elevation, terra::crs(geosurvey_ha))  # Iskip this for it takes too long
elevation <- terra::crop(elevation, ssa, mask=T)
elevation <- terra::resample(elevation, geosurvey_ha)
names(elevation) <- 'elevation'

# (8) market access
# to do
# geodata::travel_time()
market <- geodata::travel_time(ssa,path = input_path)
# market <- terra::project(market, terra::crs(geosurvey_ha))
market <- terra::crop(market, ssa, mask=T)
market <- terra::resample(market, geosurvey_ha)
