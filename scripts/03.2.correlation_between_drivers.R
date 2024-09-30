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
load('../data/processed/lsms_and_zambia.rdata') 
lsms_spatial_raw <- cbind(lsms_and_zambia, 
                      terra::extract(stacked_00, lsms_and_zambia |>
                                       select(x, y), ID = F)) |>
  filter(!(x == 0 & y == 0))
lsms_spatial_for_all_analyses <- lsms_spatial_raw |>
  select(!c(country, year, farm_id, hh_size, gdp, wealth_index)) |>
  na.omit()
save(lsms_spatial_raw, lsms_spatial_for_all_analyses, file = '../data/processed/lsms_spatial_raw.rdata')


# Check correlation matrix to select relevant drivers
P00 <- lsms_spatial_for_all_analyses |>
  select(!c(x, y)) |>
  GGally::ggpairs(upper = list(continuous = GGally::wrap("cor", size = 4)), 
                  diag = list(continuous = GGally::wrap("densityDiag")))
P00  
