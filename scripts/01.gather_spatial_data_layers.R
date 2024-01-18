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
load('../data/raw/received/lsms_and_geodata.rda') # this dataset has served for previous publication, sent by Thomas Delaune
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
pop_cty <- terra::crop(pop, cty, mask=T)
png("../output/maps/nigeria-population.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(cty, col='azure', main='Rural population (persons)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(pop_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()

# (5) sand percent at 0-30 cm
sand05 <- geodata::soil_world('sand',5,'mean', path = input_path)
sand15 <- geodata::soil_world('sand',15,'mean', path = input_path)
sand30 <- geodata::soil_world('sand',30,'mean', path = input_path)
sand0_30 <- (5*sand05 + 10*sand15 + 15*sand30)/30  # finish with 5*sand30 if 0-20cm is preferred over 0-30cm
sand0_30 <- terra::project(sand0_30, terra::crs(geosurvey_ha))
sand0_30 <- terra::crop(sand0_30, ssa, mask=T)
sand0_30 <- terra::resample(sand0_30, geosurvey_ha)
names(sand0_30) <- 'sand'
sand0_30_cty <- terra::crop(sand0_30, cty, mask=T)
png("../output/maps/nigeria-sand.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(cty, col='azure', main='Soil texture at 0-30cm (% sand)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(sand0_30_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()


# (6) rainfall (length of growing season)


# (7) elevation
elevation <- geodata::elevation_global(0.5, path = input_path)
# elevation <- terra::project(elevation, terra::crs(geosurvey_ha))  # Iskip this for it takes too long
elevation <- terra::crop(elevation, ssa, mask=T)
elevation <- terra::resample(elevation, geosurvey_ha)
names(elevation) <- 'elevation'
elevation_cty <- terra::crop(elevation, cty, mask=T)
png("../output/maps/nigeria-elevation.png", units="in", widht=5.5, height=5.5, res=1000)
terra::plot(cty, col='azure', main='Elevation map', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(elevation_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()

# (8) market access
# to do
# geodata::travel_time()
market <- geodata::travel_time(ssa,path = input_path)
# market <- terra::project(market, terra::crs(geosurvey_ha))
market <- terra::crop(market, ssa, mask=T)
market <- terra::resample(market, geosurvey_ha)
names(market) <- 'sand'
market_cty <- terra::crop(market, cty, mask=T)
png("../output/maps/nigeria-sand.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(cty, col='azure', main='Soil texture at 0-30cm (% sand)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(market_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()

# (9) share non-food/cash crops


# (10) share of root/tubers vs cereals


# ------------------------------------------------------------------------------
# finalize data 

# raster stack
stacked <- c(geosurvey_ha, cattle, pop)  # sand; TO DO rainfall, elevation, market
stacked_cty <- c(cropland_cty, cattle_cty, pop_cty)  # sand; TO DO rainfall, elevation, market

# df to spatial
library(tidyverse)
lsms_final <- lsms_cty[c('farm_area_ha')] 
lsms_final <- cbind(data.frame(lsms_final), lsms_final |> terra::geom() |> as.data.frame())
lsms_final <- lsms_final[c(1,4,5)]
lsms_spatial <- lsms_final %>%
  sf::st_as_sf(coords = c("x", "y")) %>%
  sf::st_set_crs(4326)

# merge data sets
lsms_spatial <- data.frame(cbind(lsms_spatial, terra::extract(stacked_cty, terra::vect(lsms_spatial))))
lsms_spatial <- lsms_spatial[c('farm_area_ha', 'cropland', 'cattle', 'population')]
lsms_spatial <- na.omit(lsms_spatial) # ~600 observations in/close to urban areas; see how to solve
lsms_spatial <- subset(lsms_spatial, farm_area_ha < 20) # noise?

# ------------------------------------------------------------------------------
# random forest

rf_model <- randomForest::randomForest(farm_area_ha ~ ., data=lsms_spatial, ntree=1500)
# model performance
mean(rf_model$rsq)
mean(rf_model$mse)
lsms_spatial$pred_oob <- rf_model$predicted
png("./rf-nigeria.png", units="in", width=5.5, height=5.5, res=1000)
par(mar=c(5,5,1,1), cex.axis=1.3, cex.lab=1.4)
plot(lsms_spatial$farm_area_ha, lsms_spatial$pred_oob, xlim=c(0, 15), ylim=c(0, 15),
     ylab='Predicted farm size (ha)', xlab='Reported farm size (ha)') 
abline(a=0, b=1, col=2, lwd=2)
abline(a=0, b=0.5, col=2, lwd=3, lty=2)
abline(a=0, b=2, col=2, lwd=3, lty=2)
dev.off()
r2 <- round(cor(lsms_spatial$farm_area_ha, lsms_spatial$pred_oob)^2, 2)
rmse <- round(100 * sqrt(mean((lsms_spatial$farm_area_ha-lsms_spatial$pred_oob)^2, na.rm=T)) / mean(lsms_spatial$farm_area_ha, na.rm=T), 1) 
# variable importance
vi <- data.frame(rf_model$importance, 'variable'='farm_area_ha')
png("./pdp-nigeria.png", units="in", width=5.5, height=5.5, res=1000)
randomForest::partialPlot(rf_model, lsms_spatial, population, "yes")
dev.off()
# spatial prediction
rf_model_pred <- terra::predict(stacked, rf_model, type='response', na.rm=T)
names(rf_model_pred) <- c('farm_area_ha_pred')
rf_model_pred_cty <- terra::predict(stacked_cty, rf_model, type='response', na.rm=T)
names(rf_model_pred) <- c('farm_area_ha_pred')

# plot nigeria
png("./farm-size-nigeria.png", units="in", width=5.5, height=5.5, res=1000)
pal <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))
terra::plot(cty, col='azure', main='', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(rf_model_pred_cty, breaks=c(0, 0.5, 1, 1.5, 2, 5, Inf), col=pal(6), legend=F, cex=1, axes=F, add=T)
legend(9.5, 6.4, bty='y', cex=0.8, ncol=2, box.col="white", 
       title="Farm size", legend=c('< 0.5 ha', '0.5 - 1 ha', '1 - 1.5 ha', '1.5 - 2 ha', '2 - 5 ha', '> 5 ha'), 
       fill=pal(6), horiz=FALSE)
terra::plot(cty, axes=F, add=T)
dev.off()

# plot africa
png("./farm-size-africa.png", units="in", width=5.5, height=5.5, res=1000)
pal <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))
terra::plot(ssa, col='azure', main='', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
geosurvey <- terra::aggregate(geosurvey, 10, FUN=mean, na.rm=T) 
crop_10 <- terra::ifel(geosurvey$cropland > 0.1, 1, NA)
rf_model_pred <- rf_model_pred * crop_10
terra::plot(rf_model_pred, breaks=c(0, 0.5, 1, 1.5, 2, 5, Inf), col=pal(6), legend=F, cex=1, axes=F, add=T)
legend(-15, -10, bty='y', cex=1, ncol=1, box.col="white", 
       title="Farm size", legend=c('< 0.5 ha', '0.5 - 1 ha', '1 - 1.5 ha', '1.5 - 2 ha', '2 - 5 ha', '> 5 ha'), 
       fill=pal(6), horiz=FALSE)
terra::plot(ssa, axes=F, add=T)
dev.off()
