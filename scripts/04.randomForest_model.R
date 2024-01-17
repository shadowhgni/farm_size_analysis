
input_path <- 'C:/Users/JSILVA/OneDrive - CIMMYT/Desktop/Farm size'

# ------------------------------------------------------------------------------
# shapefile
country <- geodata::world(path=input_path, resolution=5, level=0)
# unique(country$NAME_0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='Réunion' & NAME!='Saint Helena' & NAME!='São Tomé and Príncipe' & NAME!='Seychelles')
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
cty <- subset(ssa, ssa$NAME_0=='Nigeria')

# ------------------------------------------------------------------------------ 
# geosurvey
geosurvey <- geodata::cropland(source='QED', path=paste0(input_path))
geosurvey <- terra::crop(x=geosurvey, y=ssa, mask=T)
# terra::plot(geosurvey); terra::plot(ssa, add=T)
# terra::boxplot(geosurvey)
# as.data.frame(geosurvey)
cs <- terra::cellSize(geosurvey, unit = "ha")
geosurvey_ha <- cs * geosurvey 
geosurvey_ha <- terra::aggregate(geosurvey_ha, 10, FUN=mean, na.rm=T) ##check this in other codes!!
names(geosurvey_ha) <- 'cropland'
# mz_ha <- geodata::crop_spam(crop='MAIZ', path=input_path, africa=T)
# terra::plot(mz_ha$MAIZ_area_all)

# ------------------------------------------------------------------------------
# lsms data
load("./lsms_and_geodata.rda")
lsms <- subset(lsms_and_geodata, !is.na(lsms_and_geodata$farm_area_ha))
lsms <- subset(lsms, farm_area_ha > 0 & farm_area_ha < 50)
lsms$farm_area_ha <- round(lsms$farm_area_ha, 2) 
length(unique(lsms$geometry)) # 2444 unique GPS points
length(unique(lsms$geometry[lsms$country_name=='Nigeria'])) # 2444 unique GPS points
length(lsms$farm_area_ha[lsms$country_name=='Nigeria']) 
png("./africa-lsms.png", units="in", width=11, height=5.5, res=1000)
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
png("./nigeria-lsms.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(cty, col='azure', main='LSMS Nigeria', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(lsms_cty, col='red', cex=1, axes=F, add=T)
terra::plot(cty, axes=F, add=T)
dev.off()
# (2) cropland
cropland_cty <- terra::crop(geosurvey_ha, cty, mask=T)
png("./nigeria-cropland.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(cty, col='azure', main='Cropland (ha)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(cropland_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()
# (3) cattle
cattle <- terra::rast('./cattle-density/5_Ct_2010_DA.tif')
cattle <- terra::crop(cattle, ssa, mask=T)
cattle <- terra::resample(cattle, geosurvey_ha) 
names(cattle) <- 'cattle'
cattle_cty <- terra::crop(cattle, cty, mask=T)
png("./nigeria-cattle.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(cty, col='azure', main='Cattle density (#/km2)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(cattle_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()
# (4) rural population
pop <- terra::sprc(Sys.glob('./rural-population/*_rural_pop_1km.tif'))
pop <- terra::merge(pop)
pop <- terra::crop(pop, ssa, mask=T)
pop <- terra::resample(pop, geosurvey_ha) 
names(pop) <- 'population'
pop_cty <- terra::crop(pop, cty, mask=T)
png("./nigeria-population.png", units="in", width=5.5, height=5.5, res=1000)
terra::plot(cty, col='azure', main='Rural population (persons)', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
terra::plot(pop_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
terra::plot(cty, axes=F, add=T)
dev.off()

# (5) sand
# sand5 <- terra::rast('./soilgrids/af_SNDPPT_T__M_sd1_250m.tif')
# not sure why not working
# sand15 <- terra::rast('./soilgrids/af_SNDPPT_T__M_sd2_250m.tif')
# sand30 <- terra::rast('./soilgrids/af_SNDPPT_T__M_sd3_250m.tif')
# sand <- (5*sand5 + 10*sand15 + 15*sand30)/30
# sand <- terra::project(sand5, terra::crs(geosurvey_ha))
# sand <- terra::crop(sand, cty, mask=T)
# sand <- terra::resample(sand, cropland) 
# names(sand) <- 'sand'
# terra::plot(cty, col='azure', main='', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
# terra::plot(sand, cex=1, axes=F, add=T)
# terra::plot(cty, axes=F, add=T)

# (6) rainfall (length of growing season)

# (7) elevation
# elevation <- geodata::elevation_30s("Nigeria", path='.')
# elevation <- terra::crop(elevation, ssa, mask=T)
# pal <- colorRampPalette(c('wheat', 'darkgreen'))
# terra::plot(cty, col='azure', main='', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
# terra::plot(elevation, breaks=c(0, 500, 1000, 1500, 2000, Inf), col=pal(5), cex=1, axes=F, add=T)
# legend(24.2, -20, cex=1.4, title='m a.s.l.', legend=c('0 - 500', '500 - 1000', '1000 - 1500', '1500 - 2000', '> 2000'), fill=pal(5), horiz=FALSE)
# terra::plot(cty, axes=F, add=T)

# (8) market access
# to do
# geodata::travel_time()

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
