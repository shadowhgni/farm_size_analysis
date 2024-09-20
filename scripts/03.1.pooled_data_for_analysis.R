# Understanding and predicting the variability of farm size across SSA

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
# retrieve all required spatial layers from input_path
geosurvey_ha = terra::rast(paste0(input_path, '/spam/spam_cropland_ssa.tif'))
cattle = terra::rast(paste0(input_path, '/cattle-density/2010_cattle_density_ssa.tif'))
pop <- terra::rast(paste0(input_path, '/population/2020_population_density_ssa.tif'))
cropland_per_capita <- terra:: rast(paste0(input_path, '/spam/cropland_per_capita_ssa.tif'))
sand0_30 <- terra::rast(paste0(input_path, '/soil_world/sand_content_0_30cm_ssa.tif'))
elevation <- terra::rast(paste0(input_path, '/wc2.1_30s/elevation_slope_ssa.tif'))
# temperature <- terra::rast(paste0(input_path, '/temperature/temperature_ssa.tif'))
market <- terra::rast(paste0(input_path, '/travel/travel_time_to_cities_6.tif'))
rainfall <- terra::rast(paste0(input_path, '/rainfall/rainfall_ssa.tif'))
maizeyield <- terra::rast(paste0(input_path, '/maize_water_lim_yield_SSA/maize_yield_ssa.tif'))
gdp <- terra::rast(paste0(input_path, '/FAO-GDP/gdp_ssa.tif'))
wealth <- terra::rast(paste0(input_path, '/poverty/wealth_ssa.tif'))

# ------------------------------------------------------------------------------
# lsms data
load('../data/processed/lsms_and_zambia.rdata') # this is the updated dataset with 14 countries surveyed
lsms <- lsms_and_zambia |>
  filter(!is.na(farm_area_ha), !is.na(x), !is.na(y), !(x == 0 & y == 0) )  # get rid of farms whose size or GPS coord. are not available
  # mutate(farm_area_ha = ceiling(100 * farm_area_ha) / 100) # round UP to 2 digits which assigns 0.01 to the smallest farms (instead of 0)
lsms_00 <- lsms # backup the whole initial dataset (LSMS + Zambia) 

# # Restrict LSMS to 2018-2019 waves + 2015 for Tanzania and 2014 for Uganda (less than 1000 farms in 2018-2019)
# lsms <- bind_rows(
#   lsms |>
#     filter(!country %in% c('Tanzania', 'Uganda'), year > 2017, year < 2020),
#   lsms |>
#     filter(paste0(country, '_', year) %in% c('Tanzania_2014', 'Uganda_2015')),
# )

# Restrict data to 2008-2021 years  (Malawi_2004 and Uganda_2005 are excluded)
lsms <- lsms |> filter(year > 2007)

# Remove waves with less than 700 datapoints per country
summary_lsms <- lsms |> 
  group_by(country, year) |> 
  summarize(n_farms = n())

small_waves_lsms <- summary_lsms |>
  filter(n_farms < 700 )

lsms <- lsms |>
  anti_join(small_waves_lsms |>
              select(country, year))

# Number of unique GPS points. These points are meant not for individual farms but for a cluster of farms in an Enumeration Area (EA, approx 10 farms share the same geo-point, depending on sampling strategy per country)
length(unique(paste0(lsms$x, '_', lsms$y))) 

# Display the number of unique GPS points (EAs) in a particular country
nb_pts <- function(p)length(unique(paste0(lsms$x[lsms$country == p], '_', lsms$y[lsms$country == p]))); sapply(fourteen_countries, nb_pts) 

# Display the number of unique farms in a particular country
nb_farms <- function(p)length(unique(lsms$farm_id[lsms$country == p])); sapply(fourteen_countries, nb_farms) 

# Display the number of unique observations (farm x year) in a particular country
nb_obs <- function(p)length(unique(paste0(lsms$country[lsms$country == p], '_', lsms$year[lsms$country == p], '_', lsms$farm_id[lsms$country == p]))); sapply(fourteen_countries, nb_obs) 

# Assign unique farm IDs to avoid confusion across countries and  across years
# lsms$farm_id <- paste0('id_', sprintf('%05.0f', as.numeric(rownames(terra::as.data.frame(lsms)))) )  # modify farm_id to have uniform ID structure across countries
lsms$farm_id <- with(lsms, paste0(country, '_', year, '_', farm_id))

# create empty columns for GADM levels to be filled in later on
lsms$gadm_4 <- lsms$gadm_3 <- lsms$gadm_2 <- lsms$gadm_1 <- lsms$gadm_0 <- NA

# Transform the LSMS dataframe into a spat vector with point geometries
lsms <- terra::vect(lsms, geom = c('x', 'y'), crs = 4326)  

# Assign admin div names and unique farm ID to all observations in the dataset
# lsms$country <- terra::extract(fourteen_count_distr[,3], lsms)$COUNTRY  # assign the GADM country names using the level 0 of GADM division
lsms$gadm_0 <- terra::extract(fourteen_count_distr[, 'GID_0'], lsms)$GID_0                                   # create region names using the country name of GADM division
lsms$gadm_1 <- terra::extract(fourteen_count_distr[, 'NAME_1'], lsms)$NAME_1                                 # create region names using the level 1 of GADM division
lsms$gadm_2 <- terra::extract(fourteen_count_distr[, 'NAME_2'], lsms)$NAME_2                                 # create region names using the level 2 of GADM division
lsms$gadm_3 <- terra::extract(fourteen_count_distr[, 'NAME_3'], lsms)$NAME_3                                 # create region names using the level 3 of GADM division
lsms$gadm_4 <- terra::extract(fourteen_count_distr[, 'NAME_4'], lsms)$NAME_4                                 # create region names using the level 4 of GADM division
lsms_01 <- lsms # backup the whole LSMS + Zambia spat vector

# Trim to exclude extremely large farms and landless farms (at GADM_1 level)
lsms_per_region <- terra::as.data.frame(lsms) |>
  group_by(country, gadm_0, gadm_1) |>
  summarize(n_farms_years = n(),
            min  = min(farm_area_ha, na.rm = T), max = max(farm_area_ha, na.rm = T),
            q_01 = quantile(farm_area_ha, 0.01), q_99 = quantile(farm_area_ha, 0.99),
            q_05 = quantile(farm_area_ha, 0.05), q_95 = quantile(farm_area_ha, 0.95),
            q_10 = quantile(farm_area_ha, 0.10), q_90 = quantile(farm_area_ha, 0.90),
            low_fence = quantile(farm_area_ha, 0.25) - 1.5 * IQR(farm_area_ha, na.rm = T),
            high_fence = quantile(farm_area_ha, 0.75) + 1.5 * IQR(farm_area_ha, na.rm = T) ) |>
  ungroup() |>
  arrange(desc(max))

trim_1 <- inner_join(
  lsms_per_region |>
    select(country, gadm_0, gadm_1, q_95),
  cbind(
    terra::as.data.frame(lsms),
    terra::crds(lsms)
  )
)

trim_1 <- terra::vect(trim_1, geom = c('x', 'y'), crs = 'EPSG:4326')
trim_1 <- subset(trim_1, trim_1$farm_area_ha <= trim_1$q_95 & trim_1$farm_area_ha > 0)
trim_1[['q_95']] <- NULL
# trim_1 <- na.omit(trim_1)
lsms <- trim_1; rm(trim_1)
lsms_02 <- lsms

# plot the LSMS + Zambia data points
lsms_colour <- cbind(terra::as.data.frame(lsms), terra::crds(lsms)) |>
  group_by(x, y) |>
  summarize(nb_farms = n()) |>
  ungroup()
pal <- colorRampPalette(c('turquoise1', 'blue4'))(max(lsms_colour$nb_farms))
ea_colours <- pal[lsms_colour$nb_farms]
png('../output/maps/africa-lsms.png', units = 'in', width = 11, height = 5.5, res = 1000)
par(mfrow = c(1, 2), mgp = c(2, 0.5, 0))
terra::plot(ssa, col = 'azure', main = 'LSMS all countries', panel.first = grid(col = "gray", lty = 'solid'), pax = list(cex.axis = 1.4))
terra::plot(lsms, col = ea_colours, cex = 0.4, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
boxplot(lsms$farm_area_ha ~ lsms$country, ylim = c(0, 15), xlab = '', ylab = 'Farm size (ha)', cex.axis = 0.85, cex.lab = 1.3)
dev.off()

# stack all the raster layers and save it as a single .tiff file
stacked <- c(geosurvey_ha, cattle, pop, sand0_30,
             elevation, market, rainfall, maizeyield, gdp)  
terra::writeRaster(stacked, '../data/processed/stacked_rasters_africa.tif', overwrite = T)
terra::writeVector(lsms, '../data/processed/lsms_africa.shp', overwrite = T)
terra::writeVector(lsms_01, '../data/processed/backup_untrimmed_lsms_01_africa.shp', overwrite = T)

# prepare LSMS dataset for RF analysis
my_lsms <- cbind(data.frame(lsms), 
                 lsms |> 
                   terra::geom() |> 
                   as.data.frame())
my_lsms <- my_lsms[c('x', 'y', 'country', 'gadm_0', 'gadm_1', 'gadm_2', 'gadm_3', 'gadm_4', 'year', 
                     'farm_id', 'farm_area_ha', 'hh_size')]
my_lsms <- my_lsms |>
  sf::st_as_sf(coords = c('x', 'y')) |>
  sf::st_set_crs(4326)
lsms_03 <- my_lsms # backup the dataset as SF object
my_lsms <- data.frame(cbind(my_lsms, terra::extract(stacked, terra::vect(my_lsms))))

# merge data sets
lsms_spatial <- my_lsms[c('farm_area_ha',
                          'cropland', 'cattle', 
                          'population', 'sand', 'elevation', 'market',
                          'rainfall', 'maizeyield', 'gdp')]
lsms_spatial <- na.omit(lsms_spatial) 

save(stacked, file='../data/processed/stacked_africa.Rdata')
save(lsms_spatial, file='../data/processed/lsms_spatial_africa.Rdata')
save(lsms_00, lsms_03, lsms_spatial, my_lsms, file='../data/processed/my_lsms_africa.Rdata') 

# ------------------------------------------------------------------------------
# per country
per_country_data=function(my_country){
  print(paste0('==========================', my_country, '======================='))
  cty <- subset(ssa, ssa$GID_0==fourteen_country_codes[which(fourteen_countries == my_country)])
  lsms_cty <- terra::crop(lsms, cty)
  png(paste0('../output/maps/',my_country,'-lsms.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0('LSMS - ',my_country), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(lsms_cty, col='red', cex=1, axes=F, add=T)
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  cropland_cty <- terra::crop(geosurvey_ha, lsms_cty, mask=T)
  png(paste0('../output/maps/',my_country,'-cropland.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Cropland (ha)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(cropland_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  cattle_cty <- terra::crop(cattle, lsms_cty, mask=T)
  png(paste0('../output/maps/',my_country,'-cattle.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Cattle density (#/km2)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(cattle_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  pop_cty <- terra::crop(pop, lsms_cty, mask=T)
  png(paste0('../output/maps/',my_country,'-pop.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Population density (#/km2)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(pop_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  sand0_30_cty <- terra::crop(sand0_30, lsms_cty, mask=T)
  png(paste0('../output/maps/',my_country,'-sand0_30.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Soil texture (% sand)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(sand0_30_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  elevation_cty <- terra::crop(elevation, lsms_cty, mask=T)
  png(paste0('../output/maps/',my_country,'-elevation.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Elevation map (m.a.s.l)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
  terra::plot(elevation_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
  terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  market_cty <- terra::crop(market, lsms_cty, mask=T)
  png(paste0('../output/maps/',my_country,'-market.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Travel time to nearest city (min)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
  terra::plot(market_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
  terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  rainfall_cty <- terra::crop(rainfall, lsms_cty, mask=T)
  png(paste0('../output/maps/',my_country,'-rainfall.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Rainfall (mm)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(rainfall_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
  dev.off()
  })
  
  maizeyield_cty <- terra::crop(maizeyield, lsms_cty, mask=T)
  png(paste0('../output/maps/',my_country,'-maizeyield.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Water-limited potential maize yield (kg/ha)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(maizeyield_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
    dev.off()
  })
  
  gdp_cty <- terra::crop(gdp, lsms_cty, mask=T)
  png(paste0('../output/maps/',my_country,'-gdp.png'), units="in", width=5.5, height=5.5, res=1000)
  print({
    terra::plot(cty, col='azure', main=paste0(my_country,'- Country GDP (US$, price 2015)'), panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.4))
    terra::plot(gdp_cty, cex=1, axes=F, add=T, plg=list(loc = "bottom"))
    terra::plot(cty, axes=F, add=T)
    dev.off()
  })
  
  stacked_cty <- terra::crop(stacked, cty)
  terra::writeRaster(stacked_cty, paste0('../data/processed/stacked_cty_rasters_', my_country, '.tif'), overwrite = T)
  
  # prepare lsms_cty dataset for RF analysis
  lsms_cty_final <- lsms_cty[c('farm_area_ha')] 
  lsms_cty_final <- cbind(data.frame(lsms_cty_final), lsms_cty_final |> terra::geom() |> as.data.frame())
  lsms_cty_final <- lsms_cty_final[c(1,4,5)]
  lsms_cty_spatial <- lsms_cty_final %>%
    sf::st_as_sf(coords = c('x', 'y')) %>%
    sf::st_set_crs(4326)  
  
  # merge data sets
  lsms_cty_spatial <- data.frame(cbind(lsms_cty_spatial, terra::extract(stacked_cty, terra::vect(lsms_cty_spatial))))
  lsms_cty_spatial <- lsms_cty_spatial[c('farm_area_ha', 'cropland', 'cattle', 
                                         'population', 'sand', 'elevation', 'market',
                                         'rainfall', 'maizeyield', 'gdp')]
  
  save(stacked_cty, file=paste0('../data/processed/stacked_',my_country,'.Rdata'))
  save(lsms_cty_spatial, file=paste0('../data/processed/lsms_cty_spatial_',my_country,'.Rdata'))
}
sapply(fourteen_countries, per_country_data)