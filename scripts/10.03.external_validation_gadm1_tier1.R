# Validate predictions in any country of interest

# load packages
require(tidyverse)

# Clean environment
rm(list=ls())

# Set the appropriate JAVA environment
Sys.setenv(JAVA_HOME='C:/Program Files/Eclipse Adoptium/jdk-21.0.3.9-hotspot')

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
pal2 <- colorRampPalette(c('#c6dbef','#6baed6','#3182bd', '#08519c', '#08306b'))
pal3 <- colorRampPalette(c('skyblue1', 'blue4'))
# force terra to use disk-based processing and 20% of RAM (Use this if R crashes because of limited memory)
terra::terraOptions(memfrac = 0.2, todisk = T)

# ------------------------------------------------------------------------------
#define the countries for which LSMS data are available
sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# Prepare data rasters
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')
rf_model_predictions <- terra::rast('../data/processed/rf_model_predictions_SSA.tif')
names(rf_model_predictions) <- 'pred_farm_area_ha'
mask_forest_ssa <- terra::rast('../data/processed/mask_forest_ssa.tif')
mask_drylands_ssa <- terra::rast('../data/processed/mask_drylands_ssa.tif')


# ------------------------------------------------------------------------------
# Retrieve external data aggregated per GADM1 level for Kenya, Botswana, and Mozambique
bostwana_census_aggregated <- '../data/raw/web_scrapped/survey_data/other_surveys/Botswana_census_2015/Botswana Agriculture Census Report 2015.pdf'
bwa_messy_data <- tabulapdf::extract_tables(file = bostwana_census_aggregated, method = 'lattice', guess = F,
                                            area = list(c(70, 35, 380, 565)), pages = 93, output = 'tibble')
bwa_gadm1 <- bwa_messy_data[[1]] |>
  as_tibble() |>
  rename(NAME_1 = ...1,
         nb_male_farms = ...2,
         nb_female_farms = ...3,
         nb_all_farms = Total...4,
         planted_male_farms = ...5,
         planted_female_farms = ...6,
         planted_all_farms = `Total Area Planted Per`) |>
  select(NAME_1, contains('_')) |>
  slice(-c(1, 2)) |>
  mutate(planted_all_farms = substr(planted_all_farms, 1, nchar(planted_all_farms) - 2),
         across(matches('_\\w{2,}'), ~ as.numeric(gsub(',', '', .))),
         avg_farm_area_ha = planted_all_farms / nb_all_farms)

# kenya_aggregated <- '../data/raw/web_scrapped/survey_data/other_surveys/Kenya_aggregate_2019/2019_KENYA_AGRIC_CENSUS_FROM_ESTHER.xlsx'
kenya_aggregated <- rvest::read_html('https://statistics.kilimo.go.ke/en/1_7/#') |>
  rvest::html_element("table") |>
  rvest::html_table() 
names(kenya_aggregated) <- c('NAME_1', 'nb_farms', 
                        paste0('acres_', c(sprintf('%04g', c(1, 2, 5, 10, 20, 50, 100, 500, 1000)), 'plus', 'unknown')))
ken_gadm1 <- kenya_aggregated |>
  slice(-1) |>
  mutate(across(matches('_\\w{2,}'), ~ as.numeric(gsub('\\-', 0, gsub(',', '', .)))), # I interpret NA as 0 (farms of this size are unavailable)
         avg_farm_area_ha = 0.4047 * (0.5 * acres_0001 + 1.5 * acres_0002 + 3.5 * acres_0005 + 7.5 * acres_0010 +
                                        15 * acres_0020 + 35 * acres_0050 + 75 * acres_0100 + 300 * acres_0500 + 
                                        750 * acres_1000) / (acres_0001 + acres_0002 + acres_0005 + acres_0010 +
                                                               acres_0020 + acres_0050 + acres_0100 + acres_0500 + 
                                                               acres_1000)) # I ignored farms of more than 1000 acres as I could not find any central value for this class.


mozambique_aggregated <- '../data/raw/received/Mozambique_agregate_2023/Mozambique_Cultivated area per district.xlsx'
moz_gadm1 <- readxl::read_excel(mozambique_aggregated) |>
  slice(-c(1, 2)) |>
  rename(NAME_1 = `Pequenas e medias exploracoes agro-pecuarias`,
         holdings = ...2,
         avg_season1 = ...3,
         tot_season1 = ...4,
         tot_season2 = ...5,
         tot_all = ...6) |>
  mutate(across(.cols = -NAME_1, as.numeric),
         avg_farm_area_ha = avg_season1 * tot_all/tot_season1)

zimbabwe_aggregated <- '../data/raw/received/Zimbabwe/FIRST_ROUND_FARMER___LEVEL_2022_ HM.xlsx'
zwe_gadm1 <- readxl::read_excel(zimbabwe_aggregated, sheet = 'FIRST_ROUND_FARMER __LEVEL_2022') |>
  rename(NAME_1 = Province,
         farm_area_ha = `Total area planted all crops (ha)`) |>
  group_by(NAME_1) |>
  summarize(avg_farm_area_ha = mean(farm_area_ha, na.rm = T))


gadm1_validation_countries <- bwa_gadm1 |>
  select(NAME_1, avg_farm_area_ha) |>
  mutate(NAME_0 = 'Botswana') |>
  bind_rows(
    moz_gadm1 |>
      select(NAME_1, avg_farm_area_ha) |>
      mutate(NAME_0 = 'Mozambique')  
  ) |>
  bind_rows(
    ken_gadm1 |>
      select(NAME_1, avg_farm_area_ha) |>
      mutate(NAME_0 = 'Kenya') 
  ) |>
  bind_rows(
    zwe_gadm1 |>
      select(NAME_1, avg_farm_area_ha) |>
      mutate(NAME_0 = 'Zimbabwe') 
  ) |>
  na.omit()

# ------------------------------------------------------------------------------
# Getting predicted averages per gadm1 level from the validation folder 
# collate country data from respective zip
temporary_dir <- '../data/processed/temporary_dir'
if(!dir.exists(temporary_dir)) dir.create(temporary_dir)
get_gadm1_avg <- function(cty){
  print(paste0('------------- ', cty, '-----------------'))
  
  if(cty == 'Mozambique') xx <- unzip(paste0('../validation/', cty, '.zip'), files = paste0(cty, '_gadm2.csv'), exdir = temporary_dir) else xx <- unzip(paste0('../validation/', cty, '.zip'), files = paste0(cty, '_gadm1.csv'), exdir = temporary_dir)
  cty_data <- read.csv(xx) |>
    mutate(NAME_0 = cty)
  if(cty == 'Mozambique') names(cty_data) <- gsub('_2', '_1', names(cty_data))
  return(cty_data)
}
predicted_avg_per_gadm1 <- do.call(bind_rows, lapply(c('Botswana', 'Kenya', 'Mozambique', 'Zimbabwe'), get_gadm1_avg))

match_fun_2 <- function(a, b) {
  stringdist::stringdist(a, b, method = 'jw') <= 0.2  # may need to adjust the method and threshold (increase to loose)
}
compare_pred_measured_gadm1 <- fuzzyjoin::fuzzy_inner_join(
  predicted_avg_per_gadm1 |>
    mutate(across(contains('NAME_1'), ~ tolower(.))), 
  gadm1_validation_countries |>
    mutate(across(contains('NAME_1'), ~ tolower(.))), 
  by = c('NAME_0', 'NAME_1'),
  match_fun = match_fun_2
) |> 
  filter(!(grepl('north', NAME_1.x, ignore.case = T) & !grepl('north', NAME_1.y, ignore.case = T)),
         !(grepl('south', NAME_1.x, ignore.case = T) & !grepl('south', NAME_1.y, ignore.case = T)),
         !(grepl('east', NAME_1.x, ignore.case = T) & !grepl('east', NAME_1.y, ignore.case = T)),
         !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         !(grepl('centr', NAME_1.x, ignore.case = T) & !grepl('centr', NAME_1.y, ignore.case = T)),
         !(grepl('kilifi', NAME_1.x, ignore.case = T) & !grepl('kilifi', NAME_1.y, ignore.case = T)),
         !(grepl('kisii', NAME_1.x, ignore.case = T) & !grepl('kisii', NAME_1.y, ignore.case = T)),
         !(grepl('barue', NAME_1.x, ignore.case = T) & !grepl('barue', NAME_1.y, ignore.case = T)),
         !(grepl('bilene', NAME_1.x, ignore.case = T) & !grepl('bilene', NAME_1.y, ignore.case = T)),
         !(grepl('boane', NAME_1.x, ignore.case = T) & !grepl('boane', NAME_1.y, ignore.case = T)),
         !(grepl('changara', NAME_1.x, ignore.case = T) & !grepl('changara', NAME_1.y, ignore.case = T)),
         !(grepl('chemba', NAME_1.x, ignore.case = T) & !grepl('chemba', NAME_1.y, ignore.case = T)),
         !(grepl('chibuto', NAME_1.x, ignore.case = T) & !grepl('chibuto', NAME_1.y, ignore.case = T)),
         !(grepl('chifunde', NAME_1.x, ignore.case = T) & !grepl('chifunde', NAME_1.y, ignore.case = T)),
         !(grepl('chigubo', NAME_1.x, ignore.case = T) & !grepl('chigubo', NAME_1.y, ignore.case = T)),
         !(grepl('chinde', NAME_1.x, ignore.case = T) & !grepl('chinde', NAME_1.y, ignore.case = T)),
         !(grepl('chiuta', NAME_1.x, ignore.case = T) & !grepl('chiuta', NAME_1.y, ignore.case = T)),
         !(grepl('erati', NAME_1.x, ignore.case = T) & !grepl('erati', NAME_1.y, ignore.case = T)),
         !(grepl('gile', NAME_1.x, ignore.case = T) & !grepl('gile', NAME_1.y, ignore.case = T)),
         !(grepl('^ile$', NAME_1.x, ignore.case = T) & !grepl('^ile$', NAME_1.y, ignore.case = T)),
         !(grepl('mabote', NAME_1.x, ignore.case = T) & !grepl('mabote', NAME_1.y, ignore.case = T)),
         !(grepl('macanga', NAME_1.x, ignore.case = T) & !grepl('macanga', NAME_1.y, ignore.case = T)),
         !(grepl('machanga', NAME_1.x, ignore.case = T) & !grepl('machanga', NAME_1.y, ignore.case = T)),
         !(grepl('magoe', NAME_1.x, ignore.case = T) & !grepl('magoe', NAME_1.y, ignore.case = T)),
         !(grepl('magude', NAME_1.x, ignore.case = T) & !grepl('magude', NAME_1.y, ignore.case = T)),
         !(grepl('malema', NAME_1.x, ignore.case = T) & !grepl('malema', NAME_1.y, ignore.case = T)),
         !(grepl('mandimba', NAME_1.x, ignore.case = T) & !grepl('mandimba', NAME_1.y, ignore.case = T)),
         !(grepl('manhiça', NAME_1.x, ignore.case = T) & !grepl('manhiça', NAME_1.y, ignore.case = T)),
         !(grepl('manica', NAME_1.x, ignore.case = T) & !grepl('manica', NAME_1.y, ignore.case = T)),
         !(grepl('maravia', NAME_1.x, ignore.case = T) & !grepl('maravia', NAME_1.y, ignore.case = T)),
         !(grepl('maringue', NAME_1.x, ignore.case = T) & !grepl('maringue', NAME_1.y, ignore.case = T)),
         !(grepl('marrupa', NAME_1.x, ignore.case = T) & !grepl('marrupa', NAME_1.y, ignore.case = T)),
         !(grepl('massangena', NAME_1.x, ignore.case = T) & !grepl('massangena', NAME_1.y, ignore.case = T)),
         !(grepl('massinga', NAME_1.x, ignore.case = T) & !grepl('massinga', NAME_1.y, ignore.case = T)),
         !(grepl('massingir', NAME_1.x, ignore.case = T) & !grepl('massingir', NAME_1.y, ignore.case = T)),
         !(grepl('mavago', NAME_1.x, ignore.case = T) & !grepl('mavago', NAME_1.y, ignore.case = T)),
         !(grepl('mecuburi', NAME_1.x, ignore.case = T) & !grepl('mecuburi', NAME_1.y, ignore.case = T)),
         !(grepl('mecufi', NAME_1.x, ignore.case = T) & !grepl('mecufi', NAME_1.y, ignore.case = T)),
         !(grepl('mecula', NAME_1.x, ignore.case = T) & !grepl('mecula', NAME_1.y, ignore.case = T)),
         !(grepl('meluco', NAME_1.x, ignore.case = T) & !grepl('meluco', NAME_1.y, ignore.case = T)),
         !(grepl('memba', NAME_1.x, ignore.case = T) & !grepl('memba', NAME_1.y, ignore.case = T)),
         !(grepl('metarica', NAME_1.x, ignore.case = T) & !grepl('metarica', NAME_1.y, ignore.case = T)),
         !(grepl('milange', NAME_1.x, ignore.case = T) & !grepl('milange', NAME_1.y, ignore.case = T)),
         !(grepl('moamba', NAME_1.x, ignore.case = T) & !grepl('moamba', NAME_1.y, ignore.case = T)),
         !(grepl('mocuba', NAME_1.x, ignore.case = T) & !grepl('mocuba', NAME_1.y, ignore.case = T)),
         !(grepl('moma', NAME_1.x, ignore.case = T) & !grepl('moma', NAME_1.y, ignore.case = T)),
         !(grepl('mossuril', NAME_1.x, ignore.case = T) & !grepl('mossuril', NAME_1.y, ignore.case = T)),
         !(grepl('mossurize', NAME_1.x, ignore.case = T) & !grepl('mossurize', NAME_1.y, ignore.case = T)),
         !(grepl('muanza', NAME_1.x, ignore.case = T) & !grepl('muanza', NAME_1.y, ignore.case = T)),
         !(grepl('muembe', NAME_1.x, ignore.case = T) & !grepl('muembe', NAME_1.y, ignore.case = T)),
         
         !(grepl('muecate', NAME_1.x, ignore.case = T) & !grepl('muecate', NAME_1.y, ignore.case = T)),
         !(grepl('mutarara', NAME_1.x, ignore.case = T) & !grepl('mutarara', NAME_1.y, ignore.case = T)),
         
         !(grepl('namaacha', NAME_1.x, ignore.case = T) & !grepl('namaacha', NAME_1.y, ignore.case = T)),
         !(grepl('namacurra', NAME_1.x, ignore.case = T) & !grepl('namacurra', NAME_1.y, ignore.case = T)),
         !(grepl('namapa', NAME_1.x, ignore.case = T) & !grepl('namapa', NAME_1.y, ignore.case = T)),
         !(grepl('namarroi', NAME_1.x, ignore.case = T) & !grepl('namarroi', NAME_1.y, ignore.case = T)),
         !(grepl('pebane', NAME_1.x, ignore.case = T) & !grepl('pebane', NAME_1.y, ignore.case = T)),
         !(grepl('pemba', NAME_1.x, ignore.case = T) & !grepl('pemba', NAME_1.y, ignore.case = T)),
         !(grepl('ribaue', NAME_1.x, ignore.case = T) & !grepl('ribaue', NAME_1.y, ignore.case = T)),
         !(grepl('^sanga$', NAME_1.x, ignore.case = T) & !grepl('^sanga$', NAME_1.y, ignore.case = T)),
         !(grepl('tsangano', NAME_1.x, ignore.case = T) & !grepl('tsangano', NAME_1.y, ignore.case = T)),
         !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         # !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         # !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         # !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         # !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         # !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         # !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         # !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         # !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         # !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         # !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T)),
         !(grepl('west', NAME_1.x, ignore.case = T) & !grepl('west', NAME_1.y, ignore.case = T))) |>
  rename(NAME_0 = NAME_0.x, NAME_1 = NAME_1.x) |>
  group_by(NAME_0, NAME_1) |>
  summarize(across(starts_with('avg_'), ~ mean(., na.rm =  T)))

r2_gadm1_validation <- with(compare_pred_measured_gadm1, round(cor(avg_farm_area_ha, avg_pred_farm_area_ha)^2, 2))
P00 <- ggplot(compare_pred_measured_gadm1, aes(avg_farm_area_ha, avg_pred_farm_area_ha, colour = NAME_0)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linewidth = 0.8) +
  geom_abline(intercept = 0, slope = 0.5, linewidth = 0.8, linetype = 'dashed') +
  geom_abline(intercept = 0, slope = 2, linewidth = 0.8, linetype = 'dashed') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 7.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7.5)) +
  labs(x = 'Average farm size from national sources, ha', y = 'Predicted average farm size, ha', colour = 'Country') +
  facet_grid(~ NAME_0) +
  theme_test() 
P00

P01 <- ggplot(compare_pred_measured_gadm1, aes(avg_farm_area_ha, avg_pred_farm_area_ha, colour = NAME_0)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linewidth = 0.8) +
  geom_abline(intercept = 0, slope = 0.5, linewidth = 0.8, linetype = 'dashed') +
  geom_abline(intercept = 0, slope = 2, linewidth = 0.8, linetype = 'dashed') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 7.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7.5)) +
  labs(x = 'Average farm size from national sources, ha', y = 'Predicted average farm size, ha', colour = 'Country') +
  annotate('text', x = 6, y = 6.5, label = bquote(R^2 == .(r2_gadm1_validation))) +
  scale_colour_brewer(palette = 'Set1') +
  theme_test() 
P01
png(paste0('../output/graphs/external_validation_GADM1.2_for_4countries.png'), height = 5, width = 7.5, units = 'in', res = 600)
P01
ggsave(paste0('../output/graphs/external_validation_GADM1.2_for_4countries.png'))
dev.off()