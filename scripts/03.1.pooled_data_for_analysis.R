# Understanding and predicting the variability of farm size across SSA

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

#define the countries for which LSMS data are available
fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')

# shapefile of administrative units
if(!dir.exists(paste0(input_path,'/gadm/Benin'))) dir.create(paste0(input_path,'/gadm/Benin'))
if(!dir.exists(paste0(input_path,'/gadm/Burkina'))) dir.create(paste0(input_path,'/gadm/Burkina'))
if(!dir.exists(paste0(input_path,'/gadm/Cote_d_Ivoire'))) dir.create(paste0(input_path,'/gadm/Cote_d_Ivoire'))
if(!dir.exists(paste0(input_path,'/gadm/Ethiopia'))) dir.create(paste0(input_path,'/gadm/Ethiopia'))
if(!dir.exists(paste0(input_path,'/gadm/Guinea_Bissau'))) dir.create(paste0(input_path,'/gadm/Guinea_Bissau'))

if(!dir.exists(paste0(input_path,'/gadm/Malawi'))) dir.create(paste0(input_path,'/gadm/Malawi'))
if(!dir.exists(paste0(input_path,'/gadm/Mali'))) dir.create(paste0(input_path,'/gadm/Mali'))

if(!dir.exists(paste0(input_path,'/gadm/Niger'))) dir.create(paste0(input_path,'/gadm/Niger'))
if(!dir.exists(paste0(input_path,'/gadm/Nigeria'))) dir.create(paste0(input_path,'/gadm/Nigeria'))
if(!dir.exists(paste0(input_path,'/gadm/Senegal'))) dir.create(paste0(input_path,'/gadm/Senegal'))
if(!dir.exists(paste0(input_path,'/gadm/Tanzania'))) dir.create(paste0(input_path,'/gadm/Tanzania'))
if(!dir.exists(paste0(input_path,'/gadm/Togo'))) dir.create(paste0(input_path,'/gadm/Togo'))

if(!dir.exists(paste0(input_path,'/gadm/Uganda'))) dir.create(paste0(input_path,'/gadm/Uganda'))
if(!dir.exists(paste0(input_path,'/gadm/Zambia'))) dir.create(paste0(input_path,'/gadm/Zambia'))

ben_distr <- geodata::gadm('Benin', level=3, path=paste0(input_path,'/gadm/Benin'))
bfa_distr <- geodata::gadm('Burkina Faso', level=3, path=paste0(input_path,'/gadm/Burkina'))
civ_distr <- geodata::gadm('CIV', level=4, path=paste0(input_path,'/gadm/Cote_d_Ivoire'))
eth_distr <- geodata::gadm('Ethiopia', level=3, path=paste0(input_path,'/gadm/Ethiopia'))
gnb_distr <- geodata::gadm('GNB', level=2, path=paste0(input_path,'/gadm/Guinea_Bissau'))

mwi_distr <- geodata::gadm('Malawi', level=3, path=paste0(input_path,'/gadm/Malawi'))
mli_distr <- geodata::gadm('Mali', level=4, path=paste0(input_path,'/gadm/Mali'))
ner_distr <- geodata::gadm('Niger', level=3, path=paste0(input_path,'/gadm/Niger'))
nga_distr <- geodata::gadm('Nigeria', level=2, path=paste0(input_path,'/gadm/Nigeria'))
sen_distr <- geodata::gadm('Senegal', level=4, path=paste0(input_path,'/gadm/Senegal'))
tza_distr <- geodata::gadm('Tanzania', level=3, path=paste0(input_path,'/gadm/Tanzania'))
tgo_distr <- geodata::gadm('Togo', level=3, path=paste0(input_path,'/gadm/Togo'))
uga_distr <- geodata::gadm('Uganda', level=4, path=paste0(input_path,'/gadm/Uganda'))
zmb_distr <- geodata::gadm('Zambia', level=2, path=paste0(input_path,'/gadm/Zambia'))

fourteen_count_distr <- rbind(ben_distr, bfa_distr, civ_distr, eth_distr, gnb_distr, mwi_distr, mli_distr, ner_distr, nga_distr, sen_distr,  tza_distr, tgo_distr, uga_distr, zmb_distr)

#############################################################################################################
# retrieve all required spatial layers from input_path
stacked_00 <- terra::rast('../data/processed/all_predictors.tif')


# ------------------------------------------------------------------------------
# lsms data
load('../data/processed/lsms_and_zambia.rdata') # this is the updated dataset with 14 countries surveyed
lsms <- lsms_and_zambia |>
  filter(!is.na(farm_area_ha), !is.na(x), !is.na(y), !(x == 0 & y == 0) )  # get rid of farms whose size or GPS coord. are not available
lsms_00 <- lsms # backup the whole initial dataset (LSMS + Zambia) 

# Restrict data to 2008-2021 years  (Malawi_2004 and Uganda_2005 are excluded)
lsms <- lsms |> filter(year > 2007)


# Remove Tanzania 2019 (based on difficulties to retrieve EA and assumption that the 154 farms are no longer representative)
# Generally, remove all surveys yielding less than 500 farms (as issues of representativeness arise)
summary_lsms <- lsms |> 
  group_by(country, year) |> 
  summarize(n_farms = n())

small_waves_lsms <- summary_lsms |>
  filter(n_farms < 500 )

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
lsms$gadm_0 <- terra::extract(fourteen_count_distr[, 'GID_0'], lsms)$GID_0                                   # create region names using the country name of GADM division
lsms$gadm_1 <- terra::extract(fourteen_count_distr[, 'NAME_1'], lsms)$NAME_1                                 # create region names using the level 1 of GADM division
lsms$gadm_2 <- terra::extract(fourteen_count_distr[, 'NAME_2'], lsms)$NAME_2                                 # create region names using the level 2 of GADM division
lsms$gadm_3 <- terra::extract(fourteen_count_distr[, 'NAME_3'], lsms)$NAME_3                                 # create region names using the level 3 of GADM division
lsms$gadm_4 <- terra::extract(fourteen_count_distr[, 'NAME_4'], lsms)$NAME_4                                 # create region names using the level 4 of GADM division
lsms_01 <- lsms # backup the whole LSMS + Zambia spat vector
terra::writeVector(lsms_01, '../data/processed/backup_untrimmed_lsms_01_africa.shp', overwrite = T)

# Restrict Nigerian data to exclude Bauchi, Borno and Yobe from 2011 to 2015 (Boko Haram)
lsms <- lsms [!lsms$gadm_1 %in% c('Bauchi', 'Borno', 'Yobe')]
lsms_02 <- lsms
# Trim to exclude extremely large farms (> 95th quantile) and landless farms (at GADM_1 level)
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
    select(country, gadm_0, gadm_1, q_95, q_99),
  cbind(
    terra::as.data.frame(lsms),
    terra::crds(lsms)
  )
)

trim_1 <- terra::vect(trim_1, geom = c('x', 'y'), crs = 'EPSG:4326')
trim_1 <- subset(trim_1, trim_1$farm_area_ha > 0)
trim_2 <- subset(trim_1, trim_1$farm_area_ha <= trim_1$q_99)
trim_2[['q_99']] <- NULL
trim_3 <- subset(trim_1, trim_1$farm_area_ha <= trim_1$q_95)
trim_3[['q_95']] <- NULL

# plot the LSMS + Zambia data points

lsms_colour <- cbind(terra::as.data.frame(lsms), terra::crds(lsms)) |>
  group_by(x, y) |>
  summarize(nb_farms = n()) |>
  ungroup()
# pal <- colorRampPalette(c('turquoise1', 'blue4'))(max(lsms_colour$nb_farms))
pal <- colorRampPalette(c('skyblue1', 'blue4'))(max(lsms_colour$nb_farms))
ea_colours <- pal[lsms_colour$nb_farms]
png('../output/maps/africa-lsms.png', units = 'in', width = 11, height = 5.5, res = 1000)
par(mfrow = c(1, 2), mgp = c(2, 0.5, 0))
terra::plot(ssa, col = 'azure', main = 'LSMS all countries', panel.first = grid(col = "gray", lty = 'solid'), pax = list(cex.axis = 1.4))
terra::plot(lsms, col = ea_colours, cex = 0.4, axes = F, add = T)
terra::plot(ssa, axes = F, add = T)
boxplot(lsms$farm_area_ha ~ lsms$country, ylim = c(0, 15), xlab = '', ylab = 'Farm size (ha)', cex.axis = 0.85, cex.lab = 1.3)
dev.off()

# stack all the raster layers needed for analysis
stacked <- c(stacked_00$cropland, stacked_00$cattle, stacked_00$pop, 
             stacked_00$cropland_per_capita,
             stacked_00$sand, stacked_00$slope,
             stacked_00$temperature, stacked_00$rainfall, stacked_00$maizeyield, 
             stacked_00$market )  

terra::writeRaster(stacked, '../data/processed/stacked_rasters_africa.tif', overwrite = T)
# terra::writeVector(lsms, '../data/processed/lsms_africa.shp', overwrite = T)

# prepare LSMS dataset for RF analysis
select_variables <- function(x){
  lsms <- x
  my_lsms <- cbind(
    data.frame(lsms), 
    lsms |> 
      terra::geom() |> 
      as.data.frame()
  )
  my_lsms <- my_lsms[c('x', 'y', 'country', 'gadm_0', 'gadm_1', 'gadm_2', 'gadm_3', 'gadm_4', 'year', 
                       'farm_id', 'farm_area_ha', 'hh_size')]
  my_lsms <- data.frame(cbind(my_lsms, terra::extract(stacked, my_lsms |> select(x, y), na.rm = T) )) 
  lsms_03 <- my_lsms # backup the dataset as SF object
  lsms_spatial <- my_lsms
  return(lsms_spatial)
}
# my_lsms <- my_lsms |>
#   sf::st_as_sf(coords = c('x', 'y')) |>
#   sf::st_set_crs(4326)
# my_lsms <- data.frame(cbind(my_lsms, terra::extract(stacked, terra::vect(my_lsms), na.rm = T))) 

lsms_spatial <- select_variables(trim_1); print(nrow(lsms_spatial))
      save(lsms_spatial, file='../data/processed/lsms_untrimmed_africa.rdata')
lsms_spatial <- select_variables(trim_2); print(nrow(lsms_spatial))
        save(lsms_spatial, file='../data/processed/lsms_trimmed_99th_africa.rdata')
lsms_spatial <- select_variables(trim_3); print(nrow(lsms_spatial))
        save(lsms_spatial, file='../data/processed/lsms_trimmed_95th_africa.rdata')

# terra::writeVector(terra::vect(lsms_03), '../data/processed/lsms_trimmed_africa.shp', overwrite = T)

lsms_spatial <- lsms_spatial |>
  select(x, y, farm_area_ha, cropland, cattle, pop,cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>
  na.omit() 
write.csv(lsms_spatial |> select(!c(x, y)), '../data/processed/lsms_spatial.csv', row.names = F)
save(stacked, file='../data/processed/stacked_africa.Rdata')
save(lsms_spatial, file='../data/processed/lsms_spatial_africa.Rdata')
save(lsms_00, lsms_01, lsms_02, # lsms_03, my_lsms,
     lsms_spatial,  file='../data/processed/my_lsms_africa.Rdata') 

# ------------------------------------------------------------------------------
# # per country
# per_country_data=function(my_country){
#   print(paste0('==========================', my_country, '======================='))
#   cty <- subset(ssa, ssa$GID_0==fourteen_country_codes[which(fourteen_countries == my_country)])
#   lsms03 <- terra::vect(lsms_spatial, geom = c('x', 'y'))
#   lsms_cty <- terra::crop(lsms_03, cty, mask = T)
#   
#   stacked_cty <- terra::crop(stacked, cty)
#   terra::writeRaster(stacked_cty, paste0('../data/processed/stacked_cty_rasters_', my_country, '.tif'), overwrite = T)
#   
#   # prepare lsms_cty dataset for RF analysis
#   lsms_cty_final <- lsms_cty[c('farm_area_ha')] 
#   lsms_cty_final <- cbind(data.frame(lsms_cty_final), lsms_cty_final |> terra::geom() |> as.data.frame())
#   lsms_cty_final <- lsms_cty_final[c(1,4,5)]
#   lsms_cty_spatial <- lsms_cty_final %>%
#     sf::st_as_sf(coords = c('x', 'y')) %>%
#     sf::st_set_crs(4326)  
#   
#   # merge data sets
#   lsms_cty_spatial <- data.frame(cbind(lsms_cty_spatial, terra::extract(stacked_cty, terra::vect(lsms_cty_spatial))))
#   lsms_cty_spatial <- lsms_cty_spatial[c('farm_area_ha', 'cropland', 'cattle', 
#                                          'pop', 'cropland_per_capita', 
#                                          'sand', 'slope', 'temperature', 'rainfall', 
#                                          'market', 'maizeyield')] # gdp and wealth_index were removed
#   
#   save(stacked_cty, file=paste0('../data/processed/stacked_',my_country,'.Rdata'))
#   save(lsms_cty_spatial, file=paste0('../data/processed/lsms_cty_spatial_',my_country,'.Rdata'))
# }
# sapply(fourteen_countries, per_country_data)
