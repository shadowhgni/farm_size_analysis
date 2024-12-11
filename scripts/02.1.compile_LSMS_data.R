# Retrieving LSMS data
# Note that a manual screening of the fuzzy match of location names was done in Uganda. 
# The related file is named "Uganda_proposed_matching_names-jvs.csv" and located in "../data/raw/received" 

# load packages
require(tidyverse)
# Set working directory
setwd(paste0(here::here(), '/scripts'))

# Set the appropriate JAVA environment
Sys.setenv(JAVA_HOME='C:/Program Files/Eclipse Adoptium/jdk-21.0.3.9-hotspot')
# devtools::install_github("ropensci/tabulizer") # Update package, skip if not connected. 

# Clean environment
rm(list=ls())

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
fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')


#######################################################################
# Get LSMS data from Ethiopia 2018
# in Ethiopia 2018, GPS coordinates of the Enumeration Areas are lat_mod and long_mod in the ETH_HouseholdGeovariables_Y4 csv file
# in Ethiopia 2018, household size can be derived from s1q01 (relation to the head of household), s1q02 (sex), and s1q04 (is XXX a new member) and s1q05 (is XXX still a member of the household) in sect1_hh_w4 file
# in Ethiopia 2018, field number ==> s2q02 + parcel_id in sect2ppw4, 
# in Ethiopia 2018, plot number can be derived from  parcel_id, field_id in sect3_pp_w4
# in Ethiopia 2018, farmer-reported plot size ==> s3q02a in sect3_pp_w4
# in Ethiopia 2018, farmer-reported plot size ==> s3q02b in sect3_pp_w4
# in Ethiopia 2018, was plot measured with GPS ==>  s3q07 in sect3_pp_w4
# in Ethiopia 2018, GPS measured plot size (ha) ==> s3q08/10000 in sect3_pp_w4
# in Ethiopia 2018, crop code was not available, but the status of the plot(cultivated or not) ==> s3q03 in sect3_pp_w4
# in Ethiopia 2018, household ID and holder ID refer to saq08 and saq09, but also has __id variables in  sect3_pp_w4

eth_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Ethiopia_2018', full.names = T)
eth_zip <- eth_fold[grep('Stata.zip$', eth_fold, ignore.case = T)]
eth_file_list <- unzip(eth_zip, list = T)$Name
eth_sel_files <- eth_file_list[grep('ETH_HouseholdGeovariables_Y4|sect1_hh_w4|sect2ppw4|sect3_pp_w4|ET_local_area_unit_conversion', eth_file_list)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(eth_zip, files = gsub('^./', '', eth_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir)[grep('sect1_hh_w4', dir(temporary_dir))]
plot_roster <- dir(temporary_dir)[grep('sect3_pp_w4', dir(temporary_dir))]
ea_characteristics <- dir(temporary_dir)[grep('ETH_HouseholdGeovariables_Y4', dir(temporary_dir))]
unit_conv <- dir(temporary_dir)[grep('ET_local_area_unit_conversion', dir(temporary_dir))]

hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))
eth_unit_conversion <- haven::read_dta(paste0(temporary_dir, '/', unit_conv))

eth_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(ea_id, household_id, s1q01, s1q02, saq01, saq02, saq03) |>
    filter(!is.na(s1q02)) |> # Sex must be filled in
    mutate(ea_id = as.character(ea_id), 
           woreda_id = as.character(paste0(substr(saq01, 1, 1), '_', saq02, '_', saq03)), 
           farm_id = gsub('^s|^0', '', household_id)) |>
    group_by(ea_id, woreda_id, farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    as_tibble() |>
    select(ea_id, household_id, holder_id, parcel_id, field_id, s3q02a, s3q02b, s3q07, s3q08, s3q03) |>
    rename(farm_id = household_id, field_id = parcel_id, plot_id = field_id,
           reported_area = s3q02a, report_unit = s3q02b, measured_plot = s3q07, 
           plot_land_use = s3q03, measured_plot_area = s3q08) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = gsub('^s|^0', '', substr(holder_id, 1, 18)),
           field_id = paste0(farm_id, '_', sprintf('%02g', field_id)), 
           plot_id = paste0(field_id, '_', sprintf('%02g', plot_id)),
           report_unit = as.character(labelled::to_factor(report_unit)), 
           plot_land_use = as.character(labelled::to_factor(plot_land_use)), 
           measured_plot_area_ha = round(measured_plot_area / 10000, 4)) |>
    filter(plot_land_use == '1. Cultivated' )
)

eth_unit_conversion <- eth_unit_conversion |>
  mutate(woreda_id = paste0(region, '_', sprintf('%02g', zone), '_', sprintf('%02g', woreda)),
         report_unit = case_when(local_unit == 3 ~ '3. Timad',
                                 local_unit == 4 ~ '4. Boy',
                                 local_unit == 5 ~ '5. Senga',
                                 local_unit == 6 ~ '6. Kert',
                                 .default = NA),
         conversion = conversion / 10000)  # from sq_meter to ha. Note that some woreda did not have the conversion!

eth_raw <- eth_raw |>
  left_join(
    eth_unit_conversion |>
      select(woreda_id, report_unit, conversion)  ) |>
  mutate(conversion = case_when(report_unit == '1. Hectare' ~ 1,  # from Moti and Gebre' s email exchange
                                report_unit == '2. Square Meters' ~ 1 / 10000,
                                report_unit == '7. Tilm' ~ 204.4169 / 10000,
                                report_unit == '8. Medeb' ~ 69.28191 / 10000,
                                report_unit == '9. Rope(Gemed)' ~ 1,
                                report_unit == '10. Ermija' ~ 6176.3808 / 10000,
                                report_unit == '11. Other (Specify)' ~ NA,
                                
                                report_unit == '3. Timad' & is.na(conversion) ~ 0.0000161, # some woreda were not in the conversion file; these are country averages
                                report_unit == '4. Boy' & is.na(conversion) ~ 0.00000291,
                                report_unit == '5. Senga' & is.na(conversion) ~ 0.0000120,
                                report_unit == '6. Kert' & is.na(conversion) ~ 0.0000199,
                                .default = conversion),
         reported_area_ha = reported_area * conversion)

eth_raw <- inner_join(
  eth_raw,
  ea_data |>
    as_tibble() |>
    select(ea_id, lon_mod, lat_mod) |>
    group_by(ea_id) |>
    summarize(x = lon_mod, y = lat_mod) |>
    distinct() |>
    mutate(country = 'Ethiopia', year = 2018,
           ea_id = as.character(ea_id) ) ) |>
  ungroup() |> 
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
           reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(eth_raw, file = '../data/processed/Ethiopia_2018_raw.csv')

#######################################################################
# Get LSMS data from Ethiopia 2021
# in Ethiopia 2021, GPS coordinates of the Enumeration Areas are lat_mod and long_mod in the ETH_HouseholdGeovariables_Y5 csv file
# in Ethiopia 2021, household size can be derived from s1q01 (relation to the head of household), s1q02 (sex), and s1q04 (is XXX a new member) and s1q05 (is XXX still a member of the household) in sect1_hh_w5 file
# in Ethiopia 2021, field number ==> s2q02 + parcel_id in sect2ppw5, 
# in Ethiopia 2021, plot number can be derived from  parcel_id, field_id in sect3_pp_w5
# in Ethiopia 2021, farmer-reported plot size ==> s3q02a in sect3_pp_w5
# in Ethiopia 2021, farmer-reported plot size ==> s3q02b in sect3_pp_w5
# in Ethiopia 2021, was plot measured with GPS ==>  s3q07 in sect3_pp_w5
# in Ethiopia 2021, GPS measured plot size (ha) ==> s3q08/10000 in sect3_pp_w5
# in Ethiopia 2021, crop code was not available, but the status of the plot(cultivated or not) ==> s3q03 in sect3_pp_w5
# in Ethiopia 2021, household ID and holder ID refer to saq08 and saq09, but also has __id variables in  sect3_pp_w5

eth_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Ethiopia_2021', full.names = T)
eth_zip <- eth_fold[grep('Stata.zip$', eth_fold, ignore.case = T)]
eth_file_list <- unzip(eth_zip, list = T)$Name
eth_sel_files <- eth_file_list[grep('eth_householdgeovariables_y5|sect1_hh_w5|sect2ppw5|sect3_pp_w5|ET_local_area_unit_conversion', eth_file_list)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(eth_zip, files = gsub('^./', '', eth_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir)[grep('sect1_hh_w5', dir(temporary_dir))]
plot_roster <- dir(temporary_dir)[grep('sect3_pp_w5', dir(temporary_dir))]
ea_characteristics <- dir(temporary_dir)[grep('eth_householdgeovariables_y5', dir(temporary_dir))]

hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

eth_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(ea_id, household_id, s1q01, s1q02, saq01, saq02, saq03) |>
    filter(!is.na(s1q02)) |> # Sex must be filled in
    mutate(ea_id = as.character(ea_id), 
           woreda_id = as.character(paste0(substr(saq01, 1, 1), '_', saq02, '_', saq03)), 
           farm_id = gsub('^s|^0', '', household_id)) |>
    group_by(ea_id, woreda_id, farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    as_tibble() |>
    select(ea_id, household_id, holder_id, parcel_id, field_id, s3q02a, s3q02b, s3q07, s3q08, s3q03) |>
    rename(farm_id = household_id, field_id = parcel_id, plot_id = field_id,
           reported_area = s3q02a, report_unit = s3q02b, measured_plot = s3q07, 
           plot_land_use = s3q03, measured_plot_area = s3q08) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = gsub('^s|^0', '', substr(holder_id, 1, 18)),
           field_id = paste0(farm_id, '_', sprintf('%02g', field_id)), 
           plot_id = paste0(field_id, '_', sprintf('%02g', plot_id)),
           plot_land_use = as.character(labelled::to_factor(plot_land_use)),
           report_unit = as.character(labelled::to_factor(report_unit)),
           measured_plot_area_ha = round(measured_plot_area / 10000, 4)) |>
    filter(plot_land_use == '1. Cultivated' )
)

# Use unit_conversion from 2018
eth_raw <- eth_raw |>
  left_join(
    eth_unit_conversion |>
      select(woreda_id, report_unit, conversion)  ) |>
  mutate(conversion = case_when(report_unit == '1. Hectare' ~ 1,  # from Moti and Gebre' s email exchange
                                report_unit == '2. Square Meters' ~ 1 / 10000,
                                report_unit == '7. Tilm' ~ 204.4169 / 10000,
                                report_unit == '8. Medeb' ~ 69.28191 / 10000,
                                report_unit == '9. Rope(Gemed)' ~ 1,
                                report_unit == '10. Ermija' ~ 6176.3808 / 10000,
                                report_unit == '11. Other (Specify)' ~ NA,
                                
                                report_unit == '3. Timad' & is.na(conversion) ~ 0.0000161, # some woreda were not in the conversion file; these are country averages
                                report_unit == '4. Boy' & is.na(conversion) ~ 0.00000291,
                                report_unit == '5. Senga' & is.na(conversion) ~ 0.0000120,
                                report_unit == '6. Kert' & is.na(conversion) ~ 0.0000199,
                                .default = conversion),
         reported_area_ha = reported_area * conversion)

eth_raw <- inner_join(
  eth_raw,
  ea_data |>
    rename(x = lon_dd_mod, y = lat_dd_mod) |>
    mutate(farm_id = gsub('^s|^0', '', household_id),
           country = 'Ethiopia', year = 2021) ) |>
  ungroup() |> 
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(eth_raw, file = '../data/processed/Ethiopia_2021_raw.csv')
#######################################################################
# Get LSMS data from Ethiopia 2015
# in Ethiopia 2015, GPS coordinates of the Enumeration Areas are lat_mod and long_mod in the ETH_HouseholdGeovariables_Y4 csv file
# in Ethiopia 2015, household size can be derived from hh_s1q03  (sex),
# in Ethiopia 2015, field number ==> s2q02 + parcel_id in sect2ppw3, 
# in Ethiopia 2015, plot number can be derived from  parcel_id, field_id in sect3_pp_w3
# in Ethiopia 2015, farmer-reported plot size ==> s3q02a in sect3_pp_w3
# in Ethiopia 2015, farmer-reported plot size ==> s3q02b in sect3_pp_w3
# in Ethiopia 2015, was plot measured with GPS ==>  s3q07 in sect3_pp_w3
# in Ethiopia 2015, GPS measured plot size (ha) ==> s3q08/10000 in sect3_pp_w3
# in Ethiopia 2015, crop code was not available, but the status of the plot(cultivated or not) ==> s3q03 in sect3_pp_w3
# in Ethiopia 2015, household ID and holder ID refer to saq08 and saq09, but also has __id variables in  sect3_pp_w3

eth_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Ethiopia_2015', full.names = T)
eth_zip <- eth_fold[grep('Stata.zip$', eth_fold, ignore.case = T)]
eth_file_list <- unzip(eth_zip, list = T)$Name
eth_sel_files <- eth_file_list[grep('ETH_HouseholdGeovar|sect1_hh_w3|sect2ppw3|sect3_pp_w3', eth_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(eth_zip, files = gsub('^./', '', eth_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('sect1_hh_w3', dir(temporary_dir, recursive = T))]
plot_roster <- dir(temporary_dir, recursive = T)[grep('sect3_pp_w3', dir(temporary_dir, recursive = T))]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('ETH_HouseholdGeovars_y3\\.', dir(temporary_dir, recursive = T))]

hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

eth_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(ea_id, ea_id2, household_id, hh_s1q03) |>
    filter(!is.na(hh_s1q03)) |> # Sex must be filled in
    mutate(ea_id = as.character(ea_id), 
           farm_id = gsub('^s|^0', '', household_id)) |>
    group_by(ea_id, ea_id2, farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    as_tibble() |>
    select(ea_id, household_id, holder_id, parcel_id, field_id, pp_s3q02_a, pp_s3q02_c, pp_s3q04, pp_s3q05_a, pp_s3q03) |>
    rename(farm_id = household_id, field_id = parcel_id, plot_id = field_id,
           reported_area = pp_s3q02_a, report_unit = pp_s3q02_c, measured_plot = pp_s3q04, 
           plot_land_use = pp_s3q03, measured_plot_area = pp_s3q05_a) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = gsub('^s|^0', '', farm_id),
           field_id = paste0(farm_id, '_', sprintf('%02g', field_id)), 
           plot_id = paste0(field_id, '_', sprintf('%02g', plot_id)),
           measured_plot_area_ha = round(measured_plot_area / 10000, 4),
           report_unit = as.character(labelled::to_factor(report_unit)),
           plot_land_use = case_when(plot_land_use == 1 ~ 'CULTIVATED',
                                     plot_land_use != 1 ~ 'Uncultivated')) |>
    filter(plot_land_use == 'CULTIVATED' )
)

eth_raw <- eth_raw |>
  mutate(report_unit = case_when(report_unit == 1 ~ '1. Hectare',
                                 report_unit == 2 ~ '2. Square Meters',
                                 report_unit == 3 ~ '3. Timad',
                                 report_unit == 4 ~ '4. Boy',
                                 report_unit == 5 ~ '5. Senga',
                                 report_unit == 6 ~ '6. Kert',
                                 report_unit == 7 ~ '7. Tilm',
                                 report_unit == 8 ~ '8. Medeb',
                                 report_unit == 9 ~ '9. Rope(Gemed)',
                                 report_unit == 10 ~ '10. Ermija',
                                 report_unit == 11 ~ '11. Other (Specify)',
                                 .default = NA)) |>
  left_join(
    eth_unit_conversion |>
      select(woreda_id, report_unit, conversion)  ) |>
  mutate(conversion = case_when(report_unit == '1. Hectare' ~ 1,  # from Moti and Gebre' s email exchange
                                report_unit == '2. Square Meters' ~ 1 / 10000,
                                report_unit == '7. Tilm' ~ 204.4169 / 10000,
                                report_unit == '8. Medeb' ~ 69.28191 / 10000,
                                report_unit == '9. Rope(Gemed)' ~ 1,
                                report_unit == '10. Ermija' ~ 6176.3808 / 10000,
                                report_unit == '11. Other (Specify)' ~ NA,
                                
                                report_unit == '3. Timad' & is.na(conversion) ~ 0.0000161, # some woreda were not in the conversion file; these are country averages
                                report_unit == '4. Boy' & is.na(conversion) ~ 0.00000291,
                                report_unit == '5. Senga' & is.na(conversion) ~ 0.0000120,
                                report_unit == '6. Kert' & is.na(conversion) ~ 0.0000199,
                                .default = conversion),
         reported_area_ha = reported_area * conversion) |>
  distinct(paste0(plot_id, reported_area), .keep_all = T)

eth_raw <- inner_join(
  eth_raw,
  ea_data |>
    as_tibble() |>
    select(ea_id2, lon_dd_mod, lat_dd_mod) |>
    group_by(ea_id2) |>
    summarize(x = lon_dd_mod, y = lat_dd_mod) |>
    distinct() |>
    mutate(country = 'Ethiopia', year = 2015 )) |>
  ungroup() |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(eth_raw, file = '../data/processed/Ethiopia_2015_raw.csv')
#######################################################################
# Get LSMS data from Ethiopia 2013
# in Ethiopia 2013, GPS coordinates of the Enumeration Areas are lat_mod and long_mod in the ETH_HouseholdGeovariables_Y4 csv file
# in Ethiopia 2013, household size can be derived from hh_s1q03  (sex),
# in Ethiopia 2013, field number ==> s2q02 + parcel_id in sect2ppw2, 
# in Ethiopia 2013, plot number can be derived from  parcel_id, field_id in sect3_pp_w2
# in Ethiopia 2013, farmer-reported plot size ==> s3q02a in sect3_pp_w2
# in Ethiopia 2013, farmer-reported plot size ==> s3q02b in sect3_pp_w2
# in Ethiopia 2013, was plot measured with GPS ==>  s3q07 in sect3_pp_w2
# in Ethiopia 2013, GPS measured plot size (ha) ==> s3q08/10000 in sect3_pp_w2
# in Ethiopia 2013, crop code was not available, but the status of the plot(cultivated or not) ==> s3q03 in sect3_pp_w2
# in Ethiopia 2013, household ID and holder ID refer to saq08 and saq09, but also has __id variables in  sect3_pp_w2

eth_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Ethiopia_2013', full.names = T)
eth_zip <- eth_fold[grep('STATA.zip$', eth_fold, ignore.case = T)]
eth_file_list <- unzip(eth_zip, list = T)$Name
eth_sel_files <- eth_file_list[grep('ETH_HouseholdGeovar|sect1_hh_w2|sect2ppw2|sect3_pp_w2', eth_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(eth_zip, files = gsub('^./', '', eth_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('sect1_hh_w2', dir(temporary_dir, recursive = T))]
plot_roster <- dir(temporary_dir, recursive = T)[grep('sect3_pp_w2', dir(temporary_dir, recursive = T))]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('ETH_HouseholdGeovar', dir(temporary_dir, recursive = T))]

hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

eth_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(ea_id, ea_id2, household_id, hh_s1q03) |>
    filter(!is.na(hh_s1q03)) |> # Sex must be filled in
    mutate(ea_id = as.character(ea_id), 
           farm_id = gsub('^s|^0', '', household_id)) |>
    group_by(ea_id, ea_id2, farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    as_tibble() |>
    select(ea_id, household_id, holder_id, parcel_id, field_id, pp_s3q02_a, pp_s3q02_c, pp_s3q04, pp_s3q05_a, pp_s3q03) |>
    rename(farm_id = household_id, field_id = parcel_id, plot_id = field_id,
           reported_area = pp_s3q02_a, report_unit = pp_s3q02_c, measured_plot = pp_s3q04, 
           plot_land_use = pp_s3q03, measured_plot_area = pp_s3q05_a) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = gsub('^s|^0', '', farm_id),
           field_id = paste0(farm_id, '_', sprintf('%02g', field_id)), 
           plot_id = paste0(field_id, '_', sprintf('%02g', plot_id)),
           measured_plot_area_ha = round(measured_plot_area / 10000, 4),
           plot_land_use = as.character(labelled::to_factor(plot_land_use)),
           measured_plot = as.character(labelled::to_factor(measured_plot))) |>
    filter(plot_land_use == 'Cultivated' )
)

eth_raw <- eth_raw |>
  mutate(report_unit = case_when(report_unit == 1 ~ '1. Hectare',
                                 report_unit == 2 ~ '2. Square Meters',
                                 report_unit == 3 ~ '3. Timad',
                                 report_unit == 4 ~ '4. Boy',
                                 report_unit == 5 ~ '5. Senga',
                                 report_unit == 6 ~ '6. Kert',
                                 report_unit == 7 ~ '7. Tilm',
                                 report_unit == 8 ~ '8. Medeb',
                                 report_unit == 9 ~ '9. Rope(Gemed)',
                                 report_unit == 10 ~ '10. Ermija',
                                 report_unit == 11 ~ '11. Other (Specify)',
                                 .default = NA)) |>
  left_join(
    eth_unit_conversion |>
      select(woreda_id, report_unit, conversion)  ) |>
  mutate(conversion = case_when(report_unit == '1. Hectare' ~ 1,  # from Moti and Gebre' s email exchange
                                report_unit == '2. Square Meters' ~ 1 / 10000,
                                report_unit == '7. Tilm' ~ 204.4169 / 10000,
                                report_unit == '8. Medeb' ~ 69.28191 / 10000,
                                report_unit == '9. Rope(Gemed)' ~ 1,
                                report_unit == '10. Ermija' ~ 6176.3808 / 10000,
                                report_unit == '11. Other (Specify)' ~ NA,
                                
                                report_unit == '3. Timad' & is.na(conversion) ~ 0.0000161, # some woreda were not in the conversion file; these are country averages
                                report_unit == '4. Boy' & is.na(conversion) ~ 0.00000291,
                                report_unit == '5. Senga' & is.na(conversion) ~ 0.0000120,
                                report_unit == '6. Kert' & is.na(conversion) ~ 0.0000199,
                                .default = conversion),
         reported_area_ha = reported_area * conversion) |>
  distinct(paste0(plot_id, reported_area), .keep_all = T)

eth_raw <- inner_join(
  eth_raw,
  ea_data |>
    as_tibble() |>
    select(ea_id2, lon_dd_mod, lat_dd_mod) |>
    group_by(ea_id2) |>
    summarize(x = lon_dd_mod, y = lat_dd_mod) |>
    distinct() |>
    mutate(country = 'Ethiopia', year = 2013 )) |>
  ungroup() |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(eth_raw, file = '../data/processed/Ethiopia_2013_raw.csv')
#######################################################################
# Get LSMS data from Ethiopia 2011
# in Ethiopia 2011, GPS coordinates of the Enumeration Areas are lat_mod and long_mod in the ETH_HouseholdGeovariables_Y4 csv file
# in Ethiopia 2011, household size can be derived from hh_s1q03  (sex),
# in Ethiopia 2011, field number ==> s2q02 + parcel_id in sect2ppw1, 
# in Ethiopia 2011, plot number can be derived from  parcel_id, field_id in sect3_pp_w1
# in Ethiopia 2011, farmer-reported plot size ==> s3q02a in sect3_pp_w1
# in Ethiopia 2011, farmer-reported plot size ==> s3q02b in sect3_pp_w1
# in Ethiopia 2011, was plot measured with GPS ==>  s3q07 in sect3_pp_w1
# in Ethiopia 2011, GPS measured plot size (ha) ==> s3q08/10000 in sect3_pp_w1
# in Ethiopia 2011, crop code was not available, but the status of the plot(cultivated or not) ==> s3q03 in sect3_pp_w1
# in Ethiopia 2011, household ID and holder ID refer to saq08 and saq09, but also has __id variables in  sect3_pp_w1

eth_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Ethiopia_2011', full.names = T)
eth_zip <- eth_fold[grep('Stata8.zip$', eth_fold, ignore.case = T)]
eth_file_list <- unzip(eth_zip, list = T)$Name
eth_sel_files <- eth_file_list[grep('ETH_HouseholdGeovar|sect1_hh_w1|sect2ppw1|sect3_pp_w1', eth_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(eth_zip, files = gsub('^./', '', eth_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('sect1_hh_w1', dir(temporary_dir, recursive = T))]
plot_roster <- dir(temporary_dir, recursive = T)[grep('sect3_pp_w1', dir(temporary_dir, recursive = T))]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('ETH_HouseholdGeovar', dir(temporary_dir, recursive = T))]

hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

eth_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(ea_id, household_id, hh_s1q03) |>
    filter(!is.na(hh_s1q03)) |> # Sex must be filled in
    mutate(ea_id = as.character(ea_id), 
           farm_id = gsub('^s|^0', '', household_id)) |>
    group_by(ea_id, farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    as_tibble() |>
    select(ea_id, household_id, holder_id, parcel_id, field_id, pp_s3q02_a, pp_s3q02_c, pp_s3q04, pp_s3q05_a, pp_s3q03) |>
    rename(farm_id = household_id, field_id = parcel_id, plot_id = field_id,
           reported_area = pp_s3q02_a, report_unit = pp_s3q02_c, measured_plot = pp_s3q04, 
           plot_land_use = pp_s3q03, measured_plot_area = pp_s3q05_a) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = gsub('^s|^0', '', farm_id),
           field_id = paste0(farm_id, '_', sprintf('%02g', field_id)), 
           plot_id = paste0(field_id, '_', sprintf('%02g', plot_id)),
           measured_plot_area_ha = round(measured_plot_area / 10000, 4),
           measured_plot = as.character(labelled::to_factor(measured_plot)),
           plot_land_use = as.character(labelled::to_factor(plot_land_use))) |>
    filter(plot_land_use %in% c('Purestand', 'Mixed crop') )
)

eth_raw <- eth_raw |>
  mutate(report_unit = case_when(report_unit == 1 ~ '1. Hectare',
                                 report_unit == 2 ~ '2. Square Meters',
                                 report_unit == 3 ~ '3. Timad',
                                 report_unit == 4 ~ '4. Boy',
                                 report_unit == 5 ~ '5. Senga',
                                 report_unit == 6 ~ '6. Kert',
                                 report_unit == 7 ~ '7. Tilm',
                                 report_unit == 8 ~ '8. Medeb',
                                 report_unit == 9 ~ '9. Rope(Gemed)',
                                 report_unit == 10 ~ '10. Ermija',
                                 report_unit == 11 ~ '11. Other (Specify)',
                                 .default = NA)) |>
  left_join(
    eth_unit_conversion |>
      select(woreda_id, report_unit, conversion)  ) |>
  mutate(conversion = case_when(report_unit == '1. Hectare' ~ 1,  # from Moti and Gebre' s email exchange
                                report_unit == '2. Square Meters' ~ 1 / 10000,
                                report_unit == '7. Tilm' ~ 204.4169 / 10000,
                                report_unit == '8. Medeb' ~ 69.28191 / 10000,
                                report_unit == '9. Rope(Gemed)' ~ 1,
                                report_unit == '10. Ermija' ~ 6176.3808 / 10000,
                                report_unit == '11. Other (Specify)' ~ NA,
                                
                                report_unit == '3. Timad' & is.na(conversion) ~ 0.0000161, # some woreda were not in the conversion file; these are country averages
                                report_unit == '4. Boy' & is.na(conversion) ~ 0.00000291,
                                report_unit == '5. Senga' & is.na(conversion) ~ 0.0000120,
                                report_unit == '6. Kert' & is.na(conversion) ~ 0.0000199,
                                .default = conversion),
         reported_area_ha = reported_area * conversion) |>
  distinct(paste0(plot_id, reported_area), .keep_all = T)

eth_raw <- inner_join(
  eth_raw,
  ea_data |>
    as_tibble() |>
    select(ea_id, LON_DD_MOD, LAT_DD_MOD) |>
    group_by(ea_id) |>
    summarize(x = LON_DD_MOD, y = LAT_DD_MOD) |>
    distinct() |>
    mutate(country = 'Ethiopia', year = 2011,
           ea_id = as.character(ea_id) ) ) |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(eth_raw, file = '../data/processed/Ethiopia_2011_raw.csv')
#######################################################################################
# Get LSMS data from Malawi 2019

# in Malawi 2019, GPS coordinates of the Enumeration Areas are ea_lat_mod and ea_long_mod in the householdgeovariables_ihs5.dta file
# in Malawi 2019, household size is given as hhsize in  hh_mod_a_filt.dta
# in Malawi 2019, field number ==> gardenid, (mind the case_id)
# in Malawi 2019, plot number ==> plotid  
# in Malawi 2019, farmer-reported plot size ==> ag_c04a  in ag_mod_c
# in Malawi 2019, farmer-reported plot unit ==> ag_c04b  in ag_mod_c
# in Malawi 2019, was plot measured with GPS ==>  retrieve from ag_c06 (number of satelites racked) during GPS measurement in ag_mod_c 
# in Malawi 2019, GPS measured plot size (ha) ==> ag_c04c/2.47  in ag_mod_c
# in Malawi 2019, crop code was not attached to plot, but plot land use was given as ag_d14 in ag_mod_d, 
# in Malawi 2019, household ID is a combination of ea_id, case_id, +++ garden_id, plotid, etc.... (sometimes hhid)

mwi_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Malawi_2019', full.names = T)
mwi_zip <- mwi_fold[grep('Stata.zip$', mwi_fold, ignore.case = T)]
mwi_file_list <- unzip(mwi_zip, list = T)$Name
mwi_sel_files <- mwi_file_list[grep('householdgeovariables_ihs5|hh_mod_a_filt|ag_mod_c|ag_mod_d', mwi_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(mwi_zip, files = gsub('^./', '', mwi_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('hh_mod_a_filt', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('ag_mod_c', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster <- dir(temporary_dir, recursive = T)[grep('ag_mod_d', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('householdgeovariables_ihs5', dir(temporary_dir, recursive = T), ignore.case = T)]

hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

mwi_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(ea_id, case_id, HHID, hhsize) |>
    filter(!is.na(hhsize)) |> # HHSIZE must be filled in
    mutate(ea_id = as.character(ea_id), 
           farm_id = as.character(HHID),
           hh_size = hhsize) |>
    group_by(ea_id, farm_id) ,
  plot_data |>
    as_tibble() |>
    select(case_id, HHID, gardenid, plotid, ag_c04a, ag_c04b, ag_c06, ag_c04c) |>
    rename(farm_id = HHID, field_id = gardenid, plot_id = plotid,
           reported_area = ag_c04a, report_unit = ag_c04b, measured_plot = ag_c06, 
           measured_plot_area = ag_c04c) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_', field_id), 
           plot_id = paste0(field_id, '_', plot_id),
           report_unit = as.character(labelled::to_factor(report_unit)),
           reported_area_ha = case_when(report_unit == 'HECTARE' ~ reported_area,
                                        report_unit == 'ACRE' ~ reported_area / 2.47,
                                        report_unit == 'SQUARE METERS' ~ reported_area / 10000,
                                        .default = NA),
           measured_plot = case_when(!is.na(measured_plot) ~ 'GPS-measured',
                                     is.na(measured_plot) ~ 'not measured',
                                     .default = NA),
           measured_plot_area_ha = round(measured_plot_area / 2.47, 4)) 
)

mwi_raw <- inner_join(
  mwi_raw,
  crop_data |>
    select(case_id, HHID, gardenid, plotid, ag_d14) |>
    rename(farm_id = HHID, field_id = gardenid, plot_id = plotid, plot_land_use = ag_d14) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_', sprintf('%02s', field_id)), 
           plot_id = paste0(field_id, '_', sprintf('%02s', plot_id)),
           plot_land_use = as.character(labelled::to_factor(plot_land_use)) ) |>
    filter(plot_land_use == 'Cultivated')
)

mwi_raw <- inner_join(
  mwi_raw,
  ea_data |>
    as_tibble() |>
    select(ea_id, ea_lon_mod, ea_lat_mod) |>
    group_by(ea_id) |>
    summarize(x = ea_lon_mod, y = ea_lat_mod) |>
    distinct() |>
    mutate(country = 'Malawi', year = 2019,
           ea_id = as.character(ea_id) ) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(mwi_raw, file = '../data/processed/Malawi_2019_raw.csv')
#######################################################################################
# Get LSMS data from Malawi 2016

# in Malawi 2016, GPS coordinates of the Enumeration Areas are ea_lat_mod and ea_long_mod in the householdgeovariables_ihs5.dta file
# in Malawi 2016, household size is given as hhsize in  hh_mod_a_filt.dta
# in Malawi 2016, field number ==> gardenid, (mind the case_id)
# in Malawi 2016, plot number ==> plotid  
# in Malawi 2016, farmer-reported plot size ==> ag_c04a  in ag_mod_c
# in Malawi 2016, farmer-reported plot unit ==> ag_c04b  in ag_mod_c
# in Malawi 2016, was plot measured with GPS ==>  retrieve from ag_c06 (number of satelites racked) during GPS measurement in ag_mod_c 
# in Malawi 2016, GPS measured plot size (ha) ==> ag_c04c/2.47  in ag_mod_c
# in Malawi 2016, crop code was not attached to plot, but plot land use was given as ag_d14 in ag_mod_d, 
# in Malawi 2016, household ID is a combination of ea_id, case_id, +++ garden_id, plotid, etc.... (sometimes hhid)

mwi_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Malawi_2016', full.names = T)
mwi_zip <- mwi_fold[grep('STATA14.zip$', mwi_fold, ignore.case = T)]
mwi_file_list <- unzip(mwi_zip, list = T)$Name
mwi_sel_files <- mwi_file_list[grep('householdgeovariables|hh_mod_a_filt|hh_mod_b|ag_mod_c|ag_mod_d', mwi_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(mwi_zip, files = gsub('^./', '', mwi_sel_files), exdir = temporary_dir)

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('hh_mod_a_filt', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('hh_mod_b', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('ag_mod_c', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster <- dir(temporary_dir, recursive = T)[grep('ag_mod_d', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('householdgeovariable', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

mwi_raw <- inner_join(
  inner_join(
    hh_data |>
      as_tibble() |>
      select(case_id, hhid, hh_b03) |>
      filter(!is.na(hh_b03)) |> # Sex must be filled in
      mutate(farm_id = as.character(hhid)) |>
      group_by(farm_id) |>
      summarize(hh_size = n()),
    plot_data |>
      as_tibble() |>
      select(case_id, hhid, gardenid, plotid, ag_c04a, ag_c04b, ag_c06, ag_c04c) |>
      rename(farm_id = hhid, field_id = gardenid, plot_id = plotid,
             reported_area = ag_c04a, report_unit = ag_c04b, measured_plot = ag_c06, 
             measured_plot_area = ag_c04c) |>
      mutate(farm_id = as.character(farm_id),
             field_id = paste0(farm_id, '_', field_id), 
             plot_id = paste0(field_id, '_', plot_id),
             reported_area_ha = case_when(report_unit == 1 ~ reported_area / 2.47,  # acre
                                          report_unit == 2 ~ reported_area,         # ha
                                          report_unit == 3 ~ reported_area / 10000, # sq_m
                                          .default = NA),
             report_unit = as.character(labelled::to_factor(report_unit)),
             measured_plot = case_when(!is.na(measured_plot) ~ 'GPS-measured',
                                       is.na(measured_plot) ~ 'not measured',
                                       .default = NA),
             measured_plot_area_ha = round(measured_plot_area / 2.47, 4)) ),
  cluster_data |>
    select(ea_id, case_id, hhid) |>
    mutate(ea_id = as.character(ea_id),
           farm_id = as.character(hhid))
)

mwi_raw <- inner_join(
  mwi_raw,
  crop_data |>
    select(case_id, hhid, gardenid, plotid, ag_d14) |>
    rename(farm_id = hhid, field_id = gardenid, plot_id = plotid, plot_land_use = ag_d14) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_', sprintf('%02s', field_id)), 
           plot_id = paste0(field_id, '_', sprintf('%02s', plot_id)),
           plot_land_use = as.character(labelled::to_factor(plot_land_use))) |>
    filter(plot_land_use == 'Cultivated')  # 1 == 'Cultivated'
)

mwi_raw <- inner_join(
  mwi_raw,
  ea_data |>
    as_tibble() |>
    select(hhid, lon_modified, lat_modified) |>
    mutate(farm_id = hhid) |>
    group_by(farm_id) |>
    summarize(x = lon_modified, y = lat_modified) |>
    distinct() |>
    mutate(country = 'Malawi', year = 2016) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(mwi_raw, file = '../data/processed/Malawi_2016_raw.csv')
#######################################################################################
# Get LSMS data from Malawi 2013

# in Malawi 2013, GPS coordinates of the Enumeration Areas are ea_lat_mod and ea_long_mod in the householdgeovariables_ihs5.dta file
# in Malawi 2013, household size is given as hhsize in  hh_mod_a_filt.dta
# in Malawi 2013, field number ==> gardenid, (mind the case_id)
# in Malawi 2013, plot number ==> plotid  
# in Malawi 2013, farmer-reported plot size ==> ag_c04a  in ag_mod_c
# in Malawi 2013, farmer-reported plot unit ==> ag_c04b  in ag_mod_c
# in Malawi 2013, was plot measured with GPS ==>  retrieve from ag_c06 (number of satelites racked) during GPS measurement in ag_mod_c 
# in Malawi 2013, GPS measured plot size (ha) ==> ag_c04c/2.47  in ag_mod_c
# in Malawi 2013, crop code was not attached to plot, but plot land use was given as ag_d14 in ag_mod_d, 
# in Malawi 2013, household ID is a combination of ea_id, case_id, +++ garden_id, plotid, etc.... (sometimes hhid)

mwi_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Malawi_2013', full.names = T)
mwi_zip <- mwi_fold[grep('Stata.zip$', mwi_fold, ignore.case = T)]
mwi_file_list <- unzip(mwi_zip, list = T)$Name
mwi_sel_files <- mwi_file_list[grep('householdgeovariables|hh_mod_a_filt|hh_mod_b|ag_mod_c|ag_mod_d', mwi_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(mwi_zip, files = gsub('^./', '', mwi_sel_files), exdir = temporary_dir)

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('hh_mod_a_filt_13', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('hh_mod_b_13', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('ag_mod_c_13', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster <- dir(temporary_dir, recursive = T)[grep('ag_mod_d_13', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('householdgeovariables_ihps_13', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

mwi_raw <- inner_join(
  inner_join(
    hh_data |>
      select(y2_hhid, hhsize) |>
      rename(farm_id =y2_hhid, hh_size = hhsize) |>
      filter(!is.na(hh_size)),  # Sex must be filled in
    plot_data |>
      select( y2_hhid, ag_c03_2, ag_c00, ag_c04a, ag_c04b, ag_c06, ag_c04c) |>
      rename(farm_id = y2_hhid, field_id = ag_c03_2, plot_id = ag_c00,
             reported_area = ag_c04a, report_unit = ag_c04b, measured_plot = ag_c06, 
             measured_plot_area = ag_c04c) |>
      mutate(farm_id = as.character(farm_id),
             field_id = paste0(farm_id, '_', field_id), 
             plot_id = paste0(farm_id, '_', plot_id),
             reported_area_ha = case_when(report_unit == 1 ~ reported_area / 2.47,  # acre
                                          report_unit == 2 ~ reported_area,         # ha
                                          report_unit == 3 ~ reported_area / 10000, # sq_m
                                          .default = NA),
             report_unit = as.character(labelled::to_factor(report_unit)),
             measured_plot = case_when(!is.na(measured_plot) ~ 'GPS-measured',
                                       is.na(measured_plot) ~ 'not measured',
                                       .default = NA),
             measured_plot_area_ha = round(measured_plot_area / 2.47, 4)) ),
  cluster_data |>
    select(ea_id,  y2_hhid) |>
    mutate(ea_id = as.character(ea_id),
           farm_id = as.character(y2_hhid))
)

mwi_raw <- inner_join(
  mwi_raw,
  crop_data |>
    select( y2_hhid, ag_d00, ag_d14) |>
    rename(farm_id = y2_hhid, plot_id = ag_d00, plot_land_use = ag_d14) |>
    mutate(farm_id = as.character(farm_id),
           plot_id = paste0(farm_id, '_', sprintf('%02s', plot_id)),
           plot_land_use = as.character(labelled::to_factor(plot_land_use))) |>
    filter(plot_land_use == 'CULTIVATED') 
)

mwi_raw <- inner_join(
  mwi_raw,
  ea_data |>
    as_tibble() |>
    select(y2_hhid, LON_DD_MOD, LAT_DD_MOD) |>
    mutate(farm_id = y2_hhid) |>
    group_by(farm_id) |>
    summarize(x = LON_DD_MOD, y = LAT_DD_MOD) |>
    distinct() |>
    mutate(country = 'Malawi', year = 2013) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha) |>
  mutate(plot_id = paste0(field_id, '_', sprintf('%02s', substr(plot_id, 10, nchar(plot_id)))) )

write_csv(mwi_raw, file = '../data/processed/Malawi_2013_raw.csv')
#######################################################################################
# Get LSMS data from Malawi 2010

# in Malawi 2010, GPS coordinates of the Enumeration Areas are ea_lat_mod and ea_long_mod in the householdgeovariables_ihs5.dta file
# in Malawi 2010, household size is given as hhsize in  hh_mod_a_filt.dta
# in Malawi 2010, field number ==> gardenid, (mind the case_id)
# in Malawi 2010, plot number ==> plotid  
# in Malawi 2010, farmer-reported plot size ==> ag_c04a  in ag_mod_c
# in Malawi 2010, farmer-reported plot unit ==> ag_c04b  in ag_mod_c
# in Malawi 2010, was plot measured with GPS ==>  retrieve from ag_c06 (number of satelites racked) during GPS measurement in ag_mod_c 
# in Malawi 2010, GPS measured plot size (ha) ==> ag_c04c/2.47  in ag_mod_c
# in Malawi 2010, crop code was not attached to plot, but plot land use was given as ag_d14 in ag_mod_d, 
# in Malawi 2010, household ID is a combination of ea_id, case_id, +++ garden_id, plotid, etc.... (sometimes hhid)

mwi_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Malawi_2013', full.names = T)
mwi_zip <- mwi_fold[grep('Stata.zip$', mwi_fold, ignore.case = T)]
mwi_file_list <- unzip(mwi_zip, list = T)$Name
mwi_sel_files <- mwi_file_list[grep('householdgeovariables|hh_mod_a_filt|hh_mod_b|ag_mod_c|ag_mod_d', mwi_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(mwi_zip, files = gsub('^./', '', mwi_sel_files), exdir = temporary_dir)

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('hh_mod_a_filt_10', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('hh_mod_b_10', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('ag_mod_c_10', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster <- dir(temporary_dir, recursive = T)[grep('ag_mod_d_10', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('householdgeovariables_ihs3_rerelease_10', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

# this chunk is only to get the District + Traditional areas for the 2004 dataset that missed the geovariables
mwi_dist_ta <- enframe(labelled::val_labels(labelled::set_value_labels(cluster_data)$hh_a02b))
mwi_dist <- enframe(labelled::val_labels(labelled::set_value_labels(cluster_data)$hh_a01))
mwi_dist_ta <- mwi_dist |>
  rename(district = name, value_dist = value) |>
  inner_join(mwi_dist_ta |>
               rename(ta = name, code = value) |> 
               mutate(value_dist = as.integer(substr(code, 1, 3))) ) |>
  select(!value_dist) |>
  inner_join(cluster_data |>
               select(ea_id, hh_a01, hh_a02b) |>
               rename(district = hh_a01, ta = hh_a02b) |>
               mutate(district = as.character(labelled::to_factor(district)),
                      ta = as.character(labelled::to_factor(ta))) ) |>
  inner_join(ea_data |> 
               select(ea_id, lon_modified, lat_modified) |>
               rename(x = lon_modified, y = lat_modified)) |>
  distinct(); rm(mwi_dist)
  
mwi_raw <- inner_join(
  inner_join(
    hh_data |>
      select(HHID, hh_b03) |>
      mutate(farm_id = as.character(HHID)) |>
      filter(!is.na(hh_b03)) |>  # Sex must be filled in
      group_by(farm_id) |>
      summarize(hh_size = n()),
    plot_data |>
      select( HHID, ag_c00, ag_c04a, ag_c04b, ag_c05d_os, ag_c04c) |>
      rename(farm_id = HHID, plot_id = ag_c00,
             reported_area = ag_c04a, report_unit = ag_c04b, measured_plot = ag_c05d_os, 
             measured_plot_area = ag_c04c) |>
      mutate(farm_id = as.character(farm_id),
             field_id = paste0(farm_id, '_X'), 
             plot_id = paste0(field_id, '_', plot_id),
             measured_plot = case_when(measured_plot %in% c('TOO FAR FROM HOUSEHOLD', 'WATER LODGED') ~ 'not measured',
                                       .default = 'GPS-measured'),
             reported_area_ha = case_when(report_unit == 1 ~ reported_area / 2.47,  # acre
                                          report_unit == 2 ~ reported_area,         # ha
                                          report_unit == 3 ~ reported_area / 10000, # sq_m
                                          .default = NA),
             report_unit = as.character(labelled::to_factor(report_unit)),
             measured_plot_area_ha = round(measured_plot_area / 2.47, 4)) ),
  cluster_data |>
    select(ea_id,  HHID) |>
    mutate(ea_id = as.character(ea_id),
           farm_id = as.character(HHID))
)

mwi_raw <- inner_join(
  mwi_raw,
  crop_data |>
    select( HHID, ag_d00, ag_d14) |>
    rename(farm_id = HHID, plot_id = ag_d00, plot_land_use = ag_d14) |>
    mutate(farm_id = as.character(farm_id),
           plot_id = paste0(farm_id, '_X_', sprintf('%02s', plot_id)),
           plot_land_use = as.character(labelled::to_factor(plot_land_use))) |>
    filter(plot_land_use == 'Cultivated')  #1.cultivated, 2. rented-out, 3.given for free, 4.fallow
)

mwi_raw <- inner_join(
  mwi_raw,
  ea_data |>
    as_tibble() |>
    select(HHID, lon_modified, lat_modified) |>
    mutate(farm_id = as.character(HHID)) |>
    group_by(farm_id) |>
    summarize(x = lon_modified, y = lat_modified) |>
    distinct() |>
    mutate(country = 'Malawi', year = 2010) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(mwi_dist_ta, file = '../data/processed/Malawi_Districts_TA.csv')
write_csv(mwi_raw, file = '../data/processed/Malawi_2010_raw.csv')
#######################################################################################
# Get LSMS data from Malawi 2004

mwi_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Malawi_2004', full.names = T)
mwi_zip <- mwi_fold[grep('Stata8.zip$', mwi_fold, ignore.case = T)]
mwi_file_list <- unzip(mwi_zip, list = T)$Name
mwi_sel_files <- mwi_file_list[grep('ihs2_ea_data|sec_a|sec_o|sec_r', mwi_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(mwi_zip, files = gsub('^./', '', mwi_sel_files), exdir = temporary_dir)

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('sec_aa.dta', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('sec_ab', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('sec_o.dta', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster <- dir(temporary_dir, recursive = T)[grep('sec_r.dta', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster))

mwi_raw <- inner_join(
  hh_data |>
    select(case_id, ea, psu, hhsize) |>
    rename(farm_id = case_id, ea_id = ea, hh_size = hhsize) |>
    mutate(farm_id = as.character(farm_id), ea_id = as.character(ea_id)) |>
    filter(!is.na(hh_size)) |>
    distinct(),
  plot_data |>
    select(case_id, plotid, o05a, o05b, o08b) |>
    rename(farm_id = case_id, plot_id = plotid, reported_area = o05a, report_unit = o05b, crop_code = o08b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_', sprintf('%02g', plot_id)),
           report_unit = as.character(labelled::to_factor(report_unit)),
           reported_area_ha = case_when(report_unit == 'Acre' ~ reported_area / 2.47,  # acre
                                        report_unit == 'Hectare' ~ reported_area,         # ha
                                        report_unit == 'Square meters' ~ reported_area / 10000, # sq_m
                                        .default = NA),
           plot_land_use = case_when(crop_code %in% 1:27 ~ 'CULTIVATED',
                                     crop_code == 0 ~ 'uncultivated',
                                     .default = NA),
           measured_plot = NA, measured_plot_area_ha = NA ) |>
    filter(plot_land_use == 'CULTIVATED')
 )

# Extracting coordinates from TA in GADM
mwi_gadm_level2 <- geodata::gadm("Malawi", level = 2, path = paste0(input_path, '/gadm/Malawi/level2') )
mwi2 <- terra::unwrap(readRDS(paste0(input_path, '/gadm/Malawi/level2/gadm/gadm41_mwi_2_pk.rds')))
mwi2_admin <- terra::as.data.frame(mwi2)

mwi2_centroids <- terra::centroids(mwi2)
mwi_ta_coords <- terra::crds(mwi2_centroids) # as.data.frame(mwi_centroids, xy = T) did not work
mwi_ta_list <- mwi2_admin |>
  select(starts_with('NAME_')) |>
  bind_cols(mwi_ta_coords) |>
  rename(district = NAME_1, ta = NAME_2)

# align GADM data and 2010 survey data (districts and TA names). GADM is the reference
mwi_dist_ta <- read_csv('../data/processed/Malawi_Districts_TA.csv')
mwi_dist_ta <- mwi_dist_ta |>
  mutate(district = gsub('Blanytyre', 'Blantyre', district),
         district = gsub('Nkhatabay', 'Nkhata Bay', district),
         district = gsub('Nkhota kota', 'Nkhotakota', district) )

# Transfer TA names and coords from 2010 survey to cluster_data in 2004 (there are still missing TA since 2010 EAs were a subset of 2004) before adding manually
mwi_updated_cluster <- cluster_data |>
  mutate(district = as.character(labelled::to_factor(dist)),
         code = as.numeric(paste0(dist, sprintf('%02g', ta))) ) |>
  mutate(district = case_when(code %in% c(10501:10509) ~ 'Mzimba',
                              code %in% c(10531:10546) ~ 'Mzuzu City',
                              code %in% c(20601:20615) ~ 'Lilongwe',
                              code %in% c(20631:20687) ~ 'Lilongwe City',
                              code %in% c(30301:30306) ~ 'Zomba',
                              code %in% c(30331:30342) ~ 'Zomba City',
                              code %in% c(30501:30508) ~ 'Blantyre',
                              code %in% c(30531:30554) ~ 'Blantyre City',
                              .default = district)) |>
  select(district, code) |>
  full_join(mwi_dist_ta |> 
              select(district, ta, code, x, y) |>
              mutate(code = case_when(code > 10730 & code < 10800 ~ code - 200,          # Align code of Mzuzu City to match 2004
                                      code > 21030 & code < 21100 ~ code - 400,          # Align code of Lilongwe City to match 2004
                                      code > 31430 & code < 31500 ~ code - 1100,         # Align code of Zomba to match 2004
                                      code > 31530 & code < 31600 ~ code - 1000,         # Align code of Blantyre City to match 2004
                                      .default = code)) ) |>
  distinct()
  
# Assigning TA names following codes provided in 'IH2 Enum Manual'

mwi_updated_cluster |>
  filter(is.na(ta)) |>
  arrange(code, district, ta) |>
  View()

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Chitipa' & mwi_updated_cluster$code == 10105] <- 'TA Kameme'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Chitipa' & mwi_updated_cluster$code == 10120] <- 'Chipita Boma'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Karonga' & mwi_updated_cluster$code == 10205] <- "S/C Mwirang'ombe"

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nkhata Bay' & mwi_updated_cluster$code == 10306] <- 'Mankhambira'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nkhata Bay' & mwi_updated_cluster$code == 10309] <- 'Mkondowe'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Rumphi' & mwi_updated_cluster$code == 10402] <- 'Mwmamlowe'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Rumphi' & mwi_updated_cluster$code == 10403] <- 'Mwahenga'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Rumphi' & mwi_updated_cluster$code == 10407] <- 'Mwankhunikira'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Rumphi' & mwi_updated_cluster$code == 10408] <- 'Katumbi'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Rumphi' & mwi_updated_cluster$code == 10409] <- 'Zolokere'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Rumphi' & mwi_updated_cluster$code == 10420] <- 'Rumphi'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mzimba' & mwi_updated_cluster$code == 10504] <- 'S/C Jaravikuba Munthali'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mzimba' & mwi_updated_cluster$code == 10508] <- 'S/C Khosolo Gwaza Jere'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mzuzu City' & mwi_updated_cluster$code == 10531] <- 'Nkhorongo Ward'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mzuzu City' & mwi_updated_cluster$code == 10545] <- 'Msongwe Ward'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Kasungu' & mwi_updated_cluster$code == 20101] <- 'TA Kaluluma'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Kasungu' & mwi_updated_cluster$code == 20102] <- 'S/C Simlemba'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Kasungu' & mwi_updated_cluster$code == 20103] <- "S/C M'nyanja"
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Kasungu' & mwi_updated_cluster$code == 20105] <- 'TA Kaomba'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Kasungu' & mwi_updated_cluster$code == 20108] <- 'S/C Njombwa'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Kasungu' & mwi_updated_cluster$code == 20109] <- 'S/C Chilowamatambe'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nkhotakota' & mwi_updated_cluster$code == 20201] <- 'TA Kanyenda'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nkhotakota' & mwi_updated_cluster$code == 20202] <- 'S/C Kafuzila'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nkhotakota' & mwi_updated_cluster$code == 20203] <- 'TA Malenga Chanzi'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nkhotakota' & mwi_updated_cluster$code == 20220] <- 'Nkhotakota Boma'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Ntchisi' & mwi_updated_cluster$code == 20304] <- 'S/C Nthondo'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Ntchisi' & mwi_updated_cluster$code == 20320] <- 'Ntchisi Boma'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Dowa' & mwi_updated_cluster$code == 20401] <- 'TA Dzoole'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Dowa' & mwi_updated_cluster$code == 20403] <- 'S/C Kayembe'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Dowa' & mwi_updated_cluster$code == 20420] <- 'Dowa Boma'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Dowa' & mwi_updated_cluster$code == 20421] <- 'Mponela Urban'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Salima' & mwi_updated_cluster$code == 20502] <- 'TA Karonga'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Salima' & mwi_updated_cluster$code == 20505] <- 'TA Ndindi'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Salima' & mwi_updated_cluster$code == 20510] <- 'S/C Msosa'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Lilongwe' & mwi_updated_cluster$code == 20606] <- 'TA Khongoni'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Lilongwe' & mwi_updated_cluster$code == 20609] <- 'S/C Mtema'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Lilongwe' & mwi_updated_cluster$code == 20611] <- 'S/C Tsabango'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mchinji' & mwi_updated_cluster$code == 20703] <- 'TA Zulu'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Ntcheu' & mwi_updated_cluster$code == 20901] <- 'TA Phambala'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Ntcheu' & mwi_updated_cluster$code == 20902] <- 'TA Mpando'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Ntcheu' & mwi_updated_cluster$code == 20904] <- 'S/C Makwangwala'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Ntcheu' & mwi_updated_cluster$code == 20905] <- 'S/C Champiti'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Ntcheu' & mwi_updated_cluster$code == 20907] <- 'TA Chakhumbira'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Lilongwe City' & mwi_updated_cluster$code == 20631] <- 'Area 1'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Lilongwe City' & mwi_updated_cluster$code == 20633] <- 'Area 3'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Lilongwe City' & mwi_updated_cluster$code == 20638] <- 'Area 8'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Lilongwe City' & mwi_updated_cluster$code == 20654] <- 'Area 24'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Lilongwe City' & mwi_updated_cluster$code == 20668] <- 'Area 38'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Lilongwe City' & mwi_updated_cluster$code == 20682] <- 'Area 52'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Lilongwe City' & mwi_updated_cluster$code == 20684] <- 'Area 54'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Lilongwe City' & mwi_updated_cluster$code == 20685] <- 'Area 55'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mangochi' & mwi_updated_cluster$code == 30106] <- 'S/C Chowe'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Machinga' & mwi_updated_cluster$code == 30201] <- 'TA Liwonde'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Machinga' & mwi_updated_cluster$code == 30203] <- 'TA Kawinga'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Machinga' & mwi_updated_cluster$code == 30204] <- 'S/C Chamba'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Machinga' & mwi_updated_cluster$code == 30205] <- 'S/C Mposa'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Machinga' & mwi_updated_cluster$code == 30207] <- 'S/C Chikweo'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Machinga' & mwi_updated_cluster$code == 30209] <- 'S/C Chiwalo'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Machinga' & mwi_updated_cluster$code == 30210] <- 'TA Nyambi'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Zomba' & mwi_updated_cluster$code == 30301] <- 'TA Kuntumanji'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Zomba' & mwi_updated_cluster$code == 30304] <- 'TA Chikowi'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Chiradzulu' & mwi_updated_cluster$code == 30401] <- 'TA Mpama'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Chiradzulu' & mwi_updated_cluster$code == 30402] <- 'TA Likoswe'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Chiradzulu' & mwi_updated_cluster$code == 30404] <- 'TA Nkalo'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Blantyre' & mwi_updated_cluster$code == 30506] <- 'TA Kuntaja'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Blantyre' & mwi_updated_cluster$code == 30507] <- 'TA Machinjili'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Blantyre' & mwi_updated_cluster$code == 30508] <- 'TA Somba'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mwanza' & mwi_updated_cluster$code == 30601] <- 'TA Dambe'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mwanza' & mwi_updated_cluster$code == 30603] <- 'TA Kanduku'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mwanza' & mwi_updated_cluster$code == 30604] <- 'TA Nthache'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mwanza' & mwi_updated_cluster$code == 30605] <- 'TA Symon'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mwanza' & mwi_updated_cluster$code == 30606] <- 'TA Ngozi'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Thyolo' & mwi_updated_cluster$code == 30701] <- 'TA Nsabwe'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Thyolo' & mwi_updated_cluster$code == 30702] <- 'S/C Thukuta'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Thyolo' & mwi_updated_cluster$code == 30707] <- 'TA Kapichi'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mulanje' & mwi_updated_cluster$code == 30802] <- 'S/C Laston Njema'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Mulanje' & mwi_updated_cluster$code == 30820] <- 'Mulanje Boma'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Phalombe' & mwi_updated_cluster$code == 30902] <- 'TA Nazombe'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Chikwawa' & mwi_updated_cluster$code == 31003] <- 'TA Chapananga'


mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nsanje' & mwi_updated_cluster$code == 31101] <- 'TA Ndamera'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nsanje' & mwi_updated_cluster$code == 31102] <- 'TA Chimombo'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nsanje' & mwi_updated_cluster$code == 31104] <- 'TA Mlolo'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nsanje' & mwi_updated_cluster$code == 31105] <- 'TA Tengani'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nsanje' & mwi_updated_cluster$code == 31106] <- 'S/C Mbenje'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Nsanje' & mwi_updated_cluster$code == 31107] <- 'TA Malemia'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Balaka' & mwi_updated_cluster$code == 31202] <- 'TA Kalembo'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Zomba City' & mwi_updated_cluster$code == 30334] <- 'Chikamveka Ward'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Zomba City' & mwi_updated_cluster$code == 30340] <- 'Zomba Central Ward'

mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Blantyre City' & mwi_updated_cluster$code == 30531] <- 'Michiru Ward'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Blantyre City' & mwi_updated_cluster$code == 30534] <- 'Nkolokoti Ward'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Blantyre City' & mwi_updated_cluster$code == 30535] <- 'Ndirande North Ward'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Blantyre City' & mwi_updated_cluster$code == 30537] <- 'Ndirande West Ward'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Blantyre City' & mwi_updated_cluster$code == 30541] <- 'Blantyre West Ward'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Blantyre City' & mwi_updated_cluster$code == 30548] <- 'Limbe East Ward'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Blantyre City' & mwi_updated_cluster$code == 30551] <- 'Soche East Ward'
mwi_updated_cluster$ta[mwi_updated_cluster$district == 'Blantyre City' & mwi_updated_cluster$code == 30553] <- 'Nancholi Ward'

mwi_ea_available <- mwi_updated_cluster |>
  filter(!is.na(x), !is.na(y)) |>
  group_by(district, ta, code) |>
  summarize(x = mean(x, na.rm = T), y = mean(y, na.rm = T))

mwi_ea_gadm <- mwi_updated_cluster |>
  filter(is.na(x) | is.na(y)) |>
  select(!c(x, y)) |>
  mutate(district = gsub(' City', '', district),
         district = gsub('Mzuzu', 'Mzimba', district),
         ta = gsub('^S/C', 'SC', ta))

# Apply a fuzzy match algo
# step 1.1: read through 'check_list' and identify potential misspelling
list1 <- sort(unique(tolower(mwi_ea_gadm$ta)))
list2 <- sort(unique(tolower(mwi_ta_list$ta)))
distance_matrix <- stringdist::stringdistmatrix(list1, list2, method = 'jw')
rownames(distance_matrix) <- list1
colnames(distance_matrix) <- list2

check_list <- one_row <- data.frame()
for(i  in seq_along(list1)){
  ii <- distance_matrix[i,]
  j <- min(ii[ii > 0], na.rm = T)
  ifelse(j < 0.2, one_row <- cbind.data.frame(lsms = list1[i], gadm = names(ii[ii == j ])), h <- 0) # for looser match, please increase the upper limit of j,
  check_list <- bind_rows(check_list, one_row)
}
check_list <- check_list |>
  distinct() 

check_list_lsms <- inner_join(
  check_list,
  mwi_ea_gadm |> 
    mutate(lsms = tolower(ta)) ) |>
  distinct() |>
  arrange(lsms)

check_list_gadm <- inner_join(
  check_list,
  mwi_ta_list |>
    mutate (gadm = tolower(ta))) |>
  distinct() |>
  arrange(lsms)

check_list_2004 <- check_list_lsms |>
  inner_join(check_list_gadm |>
               select(!ta)) |>
  distinct()
print(check_list_2004) 

misspelled_names <- c('Chipita Boma', 'Katumbi', 'Mwahenga', 'Mwankhunikira', 'Rumphi')
corrected_names <- c('Chitipa Boma', 'TA Katumbi', 'SC Mwahenga', 'SC Mwankhunikira', 'Rumphi Boma')

mwi_found_ta <- check_list_2004 |>
  filter(ta %in% misspelled_names) 

mwi_ea <- bind_rows(
  mwi_ea_available,
  mwi_found_ta |>
    select(district, ta, code, x, y)
)

ea_data <- mwi_ea |>
  inner_join(
    cluster_data |> 
      rename(farm_id = case_id) |>
      mutate(code = as.numeric(paste0(dist, sprintf('%02g', ta))) ) |> 
      select(farm_id, code)
  )

mwi_raw <- inner_join(
  mwi_raw,
  ea_data |>
    select(farm_id, x, y) |>
    mutate(country = 'Malawi', year = 2004) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(mwi_raw, file = '../data/processed/Malawi_2004_raw.csv')
#######################################################################################
# Get LSMS data from Nigeria 2018

# in Nigeria GHSP 2018, GPS coordinates of the Enumeration Areas ==> lon_dd_mod and lat_dd_mod in nga_householdgeovars_y4
# in Nigeria GHSP 2018, household size ==> caan be retrieved from s1q2 sex of xxx in sect1_plantingw4
# in Nigeria GHSP 2018, field number ==> NOT AVAILABLE
# in Nigeria GHSP 2018, plot number ==> plotid in  sect11a1_plantingw4  \\\\ plotid   in sect11f_plantingw4 
# in Nigeria GHSP 2018, farmer-reported plot size ==> s11aq4aa   in  sect11a1_plantingw4
# in Nigeria GHSP 2018, farmer-reported plot unit ==> s11aq4ab   in  sect11a1_plantingw4
# in Nigeria GHSP 2018, was plot measured with GPS ==> s11aq4a in  sect11a1_plantingw4 
# in Nigeria GHSP 2018, GPS measured plot size (ha) ==> s11aq4c/10000   in  sect11a1_plantingw4
# in Nigeria GHSP 2018, crop code ==> Did anyone cultivate this plot s11b1q27 in  sect11a1_plantingw4  \\\\ cropcode in  sect11f_plantingw4
# in Nigeria GHSP 2018, household ID ==>  is a combination of zone, state, lga, ea, hhid  +++ plotid   \\\\  hhid in sect11f_plantingw4

nga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_GHSP_Nigeria_2018', full.names = T)
nga_zip <- nga_fold[grep('Stata12.zip$', nga_fold, ignore.case = T)]
nga_file_list <- unzip(nga_zip, list = T)$Name
nga_sel_files <- nga_file_list[grep('nga_householdgeovars_y4|sect1_plantingw4|sect11a1_plantingw4', nga_file_list)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(nga_zip, files = gsub('^./', '', nga_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('sect1_plantingw4', dir(temporary_dir, recursive = T))]
plot_roster <- dir(temporary_dir, recursive = T)[grep('sect11a1_plantingw4', dir(temporary_dir, recursive = T))]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('nga_householdgeovars_y4', dir(temporary_dir, recursive = T))]

hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

nga_unit_conversion <- data.frame(
  zone_id = 1:6,
  heaps = c(12, 16, 11, 19, 21, 12) / 100000,
  ridges = c(0.0027, 0.004, 0.00494, 0.0023, 0.0023, 0.00001),
  stands = c(6, 16, 4, 4, 13, 41) / 100000,
  acres = 1 / 2.47,
  hectares = 1,
  square_meters = 1/ 1000) |>
  pivot_longer(cols = c('heaps', 'ridges', 'stands', 'acres', 'hectares', 'square_meters'),
               names_to = 'report_unit',
               values_to = 'conversion')

nga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(ea, hhid, s1q2) |>
    filter(!is.na(s1q2)) |> # Sex must be filled in
    mutate(ea_id = as.character(ea), 
           farm_id = as.character(hhid)) |>
    group_by(ea_id, farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    as_tibble() |>
    select(zone, ea, hhid, plotid, s11aq4aa, s11aq4b, s11aq4a, s11aq4c, s11b1q27) |>
    rename(zone_id = zone, ea_id = ea, farm_id = hhid, plot_id = plotid,
           reported_area = s11aq4aa, report_unit = s11aq4b, measured_plot = s11aq4a, 
           plot_land_use = s11b1q27, measured_plot_area = s11aq4c) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           report_unit = case_when(report_unit == 1 ~ 'heaps',
                                   report_unit == 2 ~ 'ridges',
                                   report_unit == 3 ~ 'stands',
                                   report_unit == 5 ~ 'acres',
                                   report_unit == 6 ~ 'hectares',
                                   report_unit == 7 ~ 'square_meters',
                                   .default = NA),
           plot_land_use = case_when(plot_land_use == 1 ~ 'Cultivated',
                                     plot_land_use == 2 ~ 'Uncultivated',
                                     .default = NA),
           measured_plot = case_when(measured_plot == 1 ~ 'GPS-measured',
                                     measured_plot == 2 ~ 'not measured',
                                     .default = NA),
           measured_plot_area_ha = round(measured_plot_area / 10000, 4)) |>
    filter(plot_land_use == 'Cultivated' ) 
)

nga_raw <- nga_raw |>
  inner_join(nga_unit_conversion) |>
  mutate(reported_area_ha = reported_area * conversion)
  
nga_raw <- inner_join(
  nga_raw,
  ea_data |>
    as_tibble() |>
    select(hhid, lon_dd_mod, lat_dd_mod) |>
    rename(farm_id = hhid) |>
    group_by(farm_id) |>
    summarize(x = lon_dd_mod, y = lat_dd_mod) |>
    distinct() |>
    mutate(country = 'Nigeria', year = 2018,
           farm_id = as.character(farm_id) ) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(nga_raw, file = '../data/processed/Nigeria_2018_raw.csv')
#######################################################################################
# Get LSMS data from Nigeria 2015

nga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_GHSP_Nigeria_2015', full.names = T)
nga_zip <- nga_fold[grep('Stata.zip$', nga_fold, ignore.case = T)]
nga_file_list <- unzip(nga_zip, list = T)$Name
nga_sel_files <- nga_file_list[grep('nga_householdgeovars_y3|sect1_plantingw3|sect11a1_plantingw3|sect11f_plantingw3', nga_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(nga_zip, files = gsub('^./', '', nga_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('sect1_plantingw3', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('sect11a1_plantingw3', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster <- dir(temporary_dir, recursive = T)[grep('sect11f_plantingw3', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('nga_householdgeovars_y3', dir(temporary_dir, recursive = T), ignore.case = T)]

hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

nga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(ea, hhid, s1q2) |>
    filter(!is.na(s1q2)) |> # Sex must be filled in
    mutate(ea_id = as.character(ea), 
           farm_id = as.character(hhid)) |>
    group_by(ea_id, farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    as_tibble() |>
    select(ea, hhid, plotid, s11aq4a, s11aq4b, s11aq4c, s11aq4a1) |>
    rename(ea_id = ea, farm_id = hhid, plot_id = plotid,
           reported_area = s11aq4a, report_unit = s11aq4b, measured_plot = s11aq4a1, 
           measured_plot_area = s11aq4c) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           report_unit = case_when(report_unit == 1 ~ 'heaps',
                                   report_unit == 2 ~ 'ridges',
                                   report_unit == 3 ~ 'stands',
                                   report_unit == 5 ~ 'acres',
                                   report_unit == 6 ~ 'hectares',
                                   report_unit == 7 ~ 'square meters',
                                   .default = NA),
           measured_plot = case_when(measured_plot == 1 ~ 'GPS-measured',
                                     measured_plot == 2 ~ 'not measured',
                                     .default = NA),
           measured_plot_area_ha = round(measured_plot_area / 10000, 4)) 
    
)

nga_raw <- inner_join(
  nga_raw,
  crop_data |>
    select(ea, hhid, plotid, cropcode) |>
    rename(ea_id = ea, farm_id = hhid, plot_id = plotid,) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           plot_land_use = case_when(!is.na(cropcode) ~ 'Cultivated',
                                     .default = NA) ) |>
    filter(plot_land_use == 'Cultivated' ) 
)

nga_raw <- nga_raw |>
  inner_join(nga_unit_conversion) |>
  mutate(reported_area_ha = reported_area * conversion)

nga_raw <- inner_join(
  nga_raw,
  ea_data |>
    as_tibble() |>
    select(hhid, LON_DD_MOD, LAT_DD_MOD) |>
    rename(farm_id = hhid) |>
    group_by(farm_id) |>
    summarize(x = LON_DD_MOD, y = LAT_DD_MOD) |>
    distinct() |>
    mutate(country = 'Nigeria', year = 2015,
           farm_id = as.character(farm_id) ) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(nga_raw, file = '../data/processed/Nigeria_2015_raw.csv')
#######################################################################################
# Get LSMS data from Nigeria 2012

nga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_GHSP_Nigeria_2012', full.names = T)
nga_zip <- nga_fold[grep('STATA.zip$', nga_fold, ignore.case = T)]
nga_file_list <- unzip(nga_zip, list = T)$Name
nga_sel_files <- nga_file_list[grep('nga_householdgeovars_y2|sect1_plantingw2|sect11a1_plantingw2|sect11f_plantingw2', nga_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(nga_zip, files = gsub('^./', '', nga_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('sect1_plantingw2', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('sect11a1_plantingw2', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster <- dir(temporary_dir, recursive = T)[grep('sect11f_plantingw2', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('nga_householdgeovars_y2', dir(temporary_dir, recursive = T), ignore.case = T)]

hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

nga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(ea, hhid, s1q2) |>
    filter(!is.na(s1q2)) |> # Sex must be filled in
    mutate(ea_id = as.character(ea), 
           farm_id = as.character(hhid)) |>
    group_by(ea_id, farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    as_tibble() |>
    select(ea, hhid, plotid, s11aq4a, s11aq4b, s11aq4c) |>
    rename(ea_id = ea, farm_id = hhid, plot_id = plotid,
           reported_area = s11aq4a, report_unit = s11aq4b,  
           measured_plot_area = s11aq4c) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           report_unit = case_when(report_unit == 1 ~ 'heaps',
                                   report_unit == 2 ~ 'ridges',
                                   report_unit == 3 ~ 'stands',
                                   report_unit == 5 ~ 'acres',
                                   report_unit == 6 ~ 'hectares',
                                   report_unit == 7 ~ 'square meters',
                                   .default = NA),
           measured_plot = 'unspecified',
           measured_plot_area_ha = round(measured_plot_area / 10000, 4)) 
  
)

nga_raw <- inner_join(
  nga_raw,
  crop_data |>
    select(ea, hhid, plotid, cropcode) |>
    rename(ea_id = ea, farm_id = hhid, plot_id = plotid,) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           plot_land_use = case_when(!is.na(cropcode) ~ 'Cultivated',
                                     .default = NA) ) |>
    filter(plot_land_use == 'Cultivated' ) 
)

nga_raw <- nga_raw |>
  inner_join(nga_unit_conversion) |>
  mutate(reported_area_ha = reported_area * conversion)

nga_raw <- inner_join(
  nga_raw,
  ea_data |>
    as_tibble() |>
    select(hhid, LON_DD_MOD, LAT_DD_MOD) |>
    rename(farm_id = hhid) |>
    group_by(farm_id) |>
    summarize(x = LON_DD_MOD, y = LAT_DD_MOD) |>
    distinct() |>
    mutate(country = 'Nigeria', year = 2012,
           farm_id = as.character(farm_id) ) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(nga_raw, file = '../data/processed/Nigeria_2012_raw.csv')
#######################################################################################
# Get LSMS data from Nigeria 2010

nga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_GHSP_Nigeria_2010', full.names = T)
nga_zip <- nga_fold[grep('STATA.zip$', nga_fold, ignore.case = T)]
nga_file_list <- unzip(nga_zip, list = T)$Name
nga_sel_files <- nga_file_list[grep('nga_householdgeovariables_y1|sect1_plantingw1|sect11a1_plantingw1|sect11f_plantingw1', nga_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(nga_zip, files = gsub('^./', '', nga_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('sect1_plantingw1', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('sect11a1_plantingw1', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster <- dir(temporary_dir, recursive = T)[grep('sect11f_plantingw1', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('nga_householdgeovariables_y1', dir(temporary_dir, recursive = T), ignore.case = T)]

hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

nga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(ea, hhid, s1q2) |>
    filter(!is.na(s1q2)) |> # Sex must be filled in
    mutate(ea_id = as.character(ea), 
           farm_id = as.character(hhid)) |>
    group_by(ea_id, farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    as_tibble() |>
    select(ea, hhid, plotid, s11aq4a, s11aq4b, s11aq4d) |>
    rename(ea_id = ea, farm_id = hhid, plot_id = plotid,
           reported_area = s11aq4a, report_unit = s11aq4b,  
           measured_plot_area = s11aq4d) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           report_unit = case_when(report_unit == 1 ~ 'heaps',
                                   report_unit == 2 ~ 'ridges',
                                   report_unit == 3 ~ 'stands',
                                   report_unit == 5 ~ 'acres',
                                   report_unit == 6 ~ 'hectares',
                                   report_unit == 7 ~ 'square meters',
                                   .default = NA),
           measured_plot = 'unspecified',
           measured_plot_area_ha = round(measured_plot_area / 10000, 2) ) 
  
)

nga_raw <- inner_join(
  nga_raw,
  crop_data |>
    select(ea, hhid, plotid, cropcode) |>
    rename(ea_id = ea, farm_id = hhid, plot_id = plotid,) |>
    mutate(ea_id = as.character(ea_id), 
           farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           plot_land_use = case_when(!is.na(cropcode) ~ 'Cultivated',
                                     .default = NA) ) |>
    filter(plot_land_use == 'Cultivated' ) 
)

nga_raw <- nga_raw |>
  inner_join(nga_unit_conversion) |>
  mutate(reported_area_ha = reported_area * conversion)

nga_raw <- inner_join(
  nga_raw,
  ea_data |>
    as_tibble() |>
    select(hhid, lon_dd_mod, lat_dd_mod) |>
    rename(farm_id = hhid) |>
    group_by(farm_id) |>
    summarize(x = lon_dd_mod, y = lat_dd_mod) |>
    distinct() |>
    mutate(country = 'Nigeria', year = 2010,
           farm_id = as.character(farm_id) ) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(nga_raw, file = '../data/processed/Nigeria_2010_raw.csv')
#######################################################################################
# Get LSMS data from Tanzania 2014

# in Tanzania 2014, GPS coordinates of the Enumeration Areas ==> lat_modified and lon_modified in npsy4.ea.offset
# in Tanzania 2014, household size can be derived from ==> hh_b02 (sex) in hh_sec_b
# in Tanzania 2014, field number ==> NOT AVAILABLE
# in Tanzania 2014, plot number ==> plotnum in  ag_sec_2a AND in ag_sec_2b
# in Tanzania 2014, farmer-reported plot area ==> ag2a_04 (in acres)  in ag_sec_2a AND     ag_2b_15 in ag_sec_2b (for new plots)
# in Tanzania 2014, farmer reported plot unit ==> ACRES
# in Tanzania 2014, was plot measured with GPS ==> ag2a_07  in ag_sec_2a    AND    ag_2b_18 in ag_sec_2b (new plots)
# in Tanzania 2014, GPS measured plot size (ha) ==> ag2a_09 / 2.47 in ag_sec_2a   AND ag_2b_20 / 2.47 in ag_sec_2b (new plots)
# in Tanzania 2014, crop code was not available, ag3a_03  in ag_sec_3a (land-use of plot in long rainy season 2014), (maybe ag_3a_40) and ag3a_76 (land-use in short season, 2013) in ag_sec_3a  AND ag_3b_03 (new plots, long rainy season) in ag_sec_3b possibly ag_3b_40???
# in Tanzania 2014, household ID ==>  is a combination of  y4_hhid, clusterid (clustered enumeration area) in hh_sec_a     and then plotnum (ag_sec_02 and ag_sec_03), sometimes refers to prevplot_id

tza_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Tanzania_2014', full.names = T)
tza_zip <- tza_fold[grep('Stata11.zip$', tza_fold, ignore.case = T)]
tza_file_list <- unzip(tza_zip, list = T)$Name
tza_sel_files <- tza_file_list[grep('npsy4.ea.offset|hh_sec_a|hh_sec_b|ag_sec_2a|ag_sec_2b|ag_sec_3a|ag_sec_3b', tza_file_list)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(tza_zip, files = gsub('^./', '', tza_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('hh_sec_a', dir(temporary_dir, recursive = T))]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('hh_sec_b', dir(temporary_dir, recursive = T))]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('ag_sec_2a', dir(temporary_dir, recursive = T))]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('ag_sec_2b', dir(temporary_dir, recursive = T))]
crop_roster_1 <- dir(temporary_dir, recursive = T)[grep('ag_sec_3a', dir(temporary_dir, recursive = T))]
crop_roster_2 <- dir(temporary_dir, recursive = T)[grep('ag_sec_3b', dir(temporary_dir, recursive = T))]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('npsy4.ea.offset', dir(temporary_dir, recursive = T))]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_2))
crop1_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_1))
crop2_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

plot_data <- full_join(
  plot1_data |>
    as_tibble() |>
    select(y4_hhid, plotnum, ag2a_04, ag2a_07, ag2a_09) |>
    rename(farm_id = y4_hhid, plot_id = plotnum,
           reported_area = ag2a_04, measured_plot = ag2a_07, 
           measured_plot_area = ag2a_09) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = as.character(labelled::to_factor(measured_plot)),
           measured_plot = case_when(measured_plot == 'yes' ~ 'GPS-measured',
                                     measured_plot == 'no' ~ 'not measured',
                                     .default = NA),
           reported_area_ha = reported_area / 2.47,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 4)), 
  plot2_data |>
    as_tibble() |>
    select(y4_hhid, plotnum, ag2b_15, ag2b_18, ag2b_20) |>
    rename(farm_id = y4_hhid, plot_id = plotnum,
           reported_area = ag2b_15, measured_plot = ag2b_18, 
           measured_plot_area = ag2b_20) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = as.character(labelled::to_factor(measured_plot)),
           measured_plot = case_when(measured_plot == 'yes' ~ 'GPS-measured',
                                      measured_plot == 'no' ~ 'not measured',
                                      .default = NA),
           reported_area_ha = reported_area / 2.47,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 4)) ) |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  distinct(plot_id, .keep_all = T)


tza_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(y4_hhid, hh_b02) |>
    filter(!is.na(hh_b02)) |> # Sex must be filled in
    mutate(farm_id = as.character(y4_hhid)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data
)

tza_plot_use <- inner_join(
  crop1_data |>
    select(y4_hhid, plotnum, ag3a_03, ag3a_76) |>
    rename(farm_id = y4_hhid, plot_id = plotnum,
           plot_land_use_1 = ag3a_03, plot_land_use_2 = ag3a_76) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           plot_land_use_1 = as.character(labelled::to_factor(plot_land_use_1)),
           plot_land_use_2 = as.character(labelled::to_factor(plot_land_use_2)) ),
  crop2_data |>
    select(y4_hhid, plotnum, ag3b_03) |>
    rename(farm_id = y4_hhid, plot_id = plotnum, 
           plot_land_use_3 = ag3b_03) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           plot_land_use_3 = as.character(labelled::to_factor(plot_land_use_3))) ) |>
  as_tibble() |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'cultivated' | 
                                     plot_land_use_2 == 'cultivated' | 
                                     plot_land_use_3 == 'cultivated' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)))

tza_raw <- inner_join(
  tza_raw,
  tza_plot_use |>
    select(farm_id, plot_id, plot_land_use) ) |>
  filter(plot_land_use == 'CULTIVATED')

ea_data <- cluster_data |>
  select(y4_hhid, clusterid) |>
  inner_join(ea_data |> 
               select(clusterid, lat_modified, lon_modified)) |>
  group_by(clusterid, y4_hhid) |>
  summarize(x = lon_modified, y = lat_modified) |>
  rename(ea_id = clusterid, farm_id = y4_hhid) |>
  distinct()

tza_raw <- inner_join(
  tza_raw,
  ea_data |>
    mutate(country = 'Tanzania', year = 2014,
           farm_id = as.character(farm_id), 
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(tza_raw, file = '../data/processed/Tanzania_2014_raw.csv')
#######################################################################################
# Get LSMS data from Tanzania 2012

# in Tanzania 2012, GPS coordinates of the Enumeration Areas ==> lat_dd_mod and lon_dd_mod in HouseholdGeovars_Y3
# in Tanzania 2012, household size can be derived from ==> hh_b02 (sex) in HH_SEC_B
# in Tanzania 2012, field number ==> NOT AVAILABLE
# in Tanzania 2012, plot number ==> plotnum in  ag_sec_2a AND in ag_sec_2b
# in Tanzania 2012, farmer-reported plot area ==> ag2a_04 (in acres)  in ag_sec_2a AND     ag_2b_15 in ag_sec_2b (for new plots)
# in Tanzania 2012, farmer reported plot unit ==> ACRES
# in Tanzania 2012, was plot measured with GPS ==> ag2a_07  in ag_sec_2a    AND    ag_2b_18 in ag_sec_2b (new plots)
# in Tanzania 2012, GPS measured plot size (ha) ==> ag2a_09 / 2.47 in ag_sec_2a   AND ag_2b_20 / 2.47 in ag_sec_2b (new plots)
# in Tanzania 2012, crop code was not available, ag3a_03  in ag_sec_3a (land-use of plot in long rainy season 2012), (maybe ag_3a_40) and ag3a_76 (land-use in short season, 2013) in ag_sec_3a  AND ag_3b_03 (new plots, long rainy season) in ag_sec_3b possibly ag_3b_40???
# in Tanzania 2012, household ID ==>  is a combination of  y4_hhid, clusterid (clustered enumeration area) in hh_sec_a     and then plotnum (ag_sec_02 and ag_sec_03), sometimes refers to prevplot_id

tza_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Tanzania_2012', full.names = T)
tza_zip <- tza_fold[grep('STATA8_English_labels.zip$', tza_fold, ignore.case = T)]
tza_file_list <- unzip(tza_zip, list = T)$Name
tza_sel_files <- tza_file_list[grep('HouseholdGeovars_Y3|HH_SEC_A|HH_SEC_B|AG_SEC_2A|AG_SEC_2B|AG_SEC_3A|AG_SEC_3B', tza_file_list)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(tza_zip, files = gsub('^./', '', tza_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('HH_SEC_A', dir(temporary_dir, recursive = T))]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('HH_SEC_B', dir(temporary_dir, recursive = T))]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('AG_SEC_2A', dir(temporary_dir, recursive = T))]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('AG_SEC_2B', dir(temporary_dir, recursive = T))]
crop_roster_1 <- dir(temporary_dir, recursive = T)[grep('AG_SEC_3A', dir(temporary_dir, recursive = T))]
crop_roster_2 <- dir(temporary_dir, recursive = T)[grep('AG_SEC_3B', dir(temporary_dir, recursive = T))]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('HouseholdGeovars_Y3', dir(temporary_dir, recursive = T))]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_2))
crop1_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_1))
crop2_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

plot_data <- full_join(
  plot1_data |>
    as_tibble() |>
    select(y3_hhid, plotnum, ag2a_04, ag2a_07, ag2a_09) |>
    rename(farm_id = y3_hhid, plot_id = plotnum,
           reported_area = ag2a_04, measured_plot = ag2a_07, 
           measured_plot_area = ag2a_09) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = as.character(labelled::to_factor(measured_plot)),
           measured_plot = case_when(measured_plot == 'yes' ~ 'GPS-measured',
                                     measured_plot == 'no' ~ 'not measured',
                                     .default = NA),
           reported_area_ha = reported_area / 2.47,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 4)), 
  plot2_data |>
    as_tibble() |>
    select(y3_hhid, plotnum, ag2b_15, ag2b_18, ag2b_20) |>
    rename(farm_id = y3_hhid, plot_id = plotnum,
           reported_area = ag2b_15, measured_plot = ag2b_18, 
           measured_plot_area = ag2b_20) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = as.character(labelled::to_factor(measured_plot)),
           measured_plot = case_when(measured_plot == 'yes' ~ 'GPS-measured',
                                     measured_plot == 'no' ~ 'not measured',
                                     .default = NA),
           reported_area_ha = reported_area / 2.47,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 4)) ) |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  distinct(plot_id, .keep_all = T)


tza_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(y3_hhid, hh_b02) |>
    filter(!is.na(hh_b02)) |> # Sex must be filled in
    mutate(farm_id = as.character(y3_hhid)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data
)

tza_plot_use <- inner_join(
  crop1_data |>
    select(y3_hhid, plotnum, ag3a_03, ag3a_76) |>
    rename(farm_id = y3_hhid, plot_id = plotnum,
           plot_land_use_1 = ag3a_03, plot_land_use_2 = ag3a_76) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           plot_land_use_1 = as.character(labelled::to_factor(plot_land_use_1)),
           plot_land_use_2 = as.character(labelled::to_factor(plot_land_use_2)) ),
  crop2_data |>
    select(y3_hhid, plotnum, ag3b_03) |>
    rename(farm_id = y3_hhid, plot_id = plotnum, 
           plot_land_use_3 = ag3b_03) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           plot_land_use_3 = as.character(labelled::to_factor(plot_land_use_3)) ) ) |>
  as_tibble() |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' | 
                                     plot_land_use_2 == 'CULTIVATED' | 
                                     plot_land_use_3 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)))

tza_raw <- inner_join(
  tza_raw,
  tza_plot_use |>
    select(farm_id, plot_id, plot_land_use) ) |>
  filter(plot_land_use == 'CULTIVATED')

ea_data <- cluster_data |>
  select(y3_hhid, clusterid) |>
  inner_join(
    ea_data |>
      select(y3_hhid, lon_dd_mod, lat_dd_mod) ) |>
  rename(farm_id = y3_hhid, ea_id = clusterid) |>
  group_by(ea_id, farm_id) |>
  summarize(x = lon_dd_mod, y = lat_dd_mod) |>
  distinct()

tza_raw <- inner_join(
  tza_raw,
  ea_data |>
    mutate(country = 'Tanzania', year = 2012,
           farm_id = as.character(farm_id),
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(tza_raw, file = '../data/processed/Tanzania_2012_raw.csv')
#######################################################################################
# Get LSMS data from Tanzania 2010

tza_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Tanzania_2010', full.names = T)
tza_zip <- tza_fold[grep('STATA8.zip$', tza_fold, ignore.case = T)]
tza_file_list <- unzip(tza_zip, list = T)$Name
tza_sel_files <- tza_file_list[grep('HH.Geovariables_Y2|HH_SEC_A|HH_SEC_B|AG_SEC2A|AG_SEC2B|AG_SEC3A|AG_SEC3B', tza_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(tza_zip, files = gsub('^./', '', tza_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('HH_SEC_A', dir(temporary_dir, recursive = T))]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('HH_SEC_B', dir(temporary_dir, recursive = T))]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('AG_SEC2A', dir(temporary_dir, recursive = T))]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('AG_SEC2B', dir(temporary_dir, recursive = T))]
crop_roster_1 <- dir(temporary_dir, recursive = T)[grep('AG_SEC3A', dir(temporary_dir, recursive = T))]
crop_roster_2 <- dir(temporary_dir, recursive = T)[grep('AG_SEC3B', dir(temporary_dir, recursive = T))]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('HH.Geovariables_Y2', dir(temporary_dir, recursive = T))]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_2))
crop1_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_1))
crop2_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

plot_data <- full_join(
  plot1_data |>
    as_tibble() |>
    select(y2_hhid, plotnum, ag2a_04, ag2a_07, ag2a_09) |>
    rename(farm_id = y2_hhid, plot_id = plotnum,
           reported_area = ag2a_04, measured_plot = ag2a_07, 
           measured_plot_area = ag2a_09) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = case_when(measured_plot == 1 ~ 'GPS-measured',
                                     measured_plot == 2 ~ 'not measured',
                                     .default = NA),
           reported_area_ha = reported_area / 2.47,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 4)), 
  plot2_data |>
    as_tibble() |>
    select(y2_hhid, plotnum, ag2b_15, ag2b_18, ag2b_20) |>
    rename(farm_id = y2_hhid, plot_id = plotnum,
           reported_area = ag2b_15, measured_plot = ag2b_18, 
           measured_plot_area = ag2b_20) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = case_when(measured_plot == 1 ~ 'GPS-measured',
                                     measured_plot == 2 ~ 'not measured',
                                     .default = NA),
           reported_area_ha = reported_area / 2.47,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 4)) ) |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  distinct(plot_id, .keep_all = T)

tza_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(y2_hhid, hh_b02) |>
    filter(!is.na(hh_b02)) |> # Sex must be filled in
    mutate(farm_id = as.character(y2_hhid)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data
)

tza_plot_use <- inner_join(
  crop1_data |>
    select(y2_hhid, plotnum, ag3a_03, ag3a_76) |>
    rename(farm_id = y2_hhid, plot_id = plotnum,
           plot_land_use_1 = ag3a_03, plot_land_use_2 = ag3a_76) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           plot_land_use_1 = case_when(plot_land_use_1 == 1 ~ 'CULTIVATED',
                                       plot_land_use_1 == 2 ~ 'rented out',
                                       plot_land_use_1 == 3 ~ 'given out',
                                       plot_land_use_1 == 4 ~ 'fallow',
                                       plot_land_use_1 == 5 ~ 'forest',
                                       plot_land_use_1 == 6 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_2 = case_when(plot_land_use_2 == 1 ~ 'CULTIVATED',
                                       plot_land_use_2 == 2 ~ 'rented out',
                                       plot_land_use_2 == 3 ~ 'given out',
                                       plot_land_use_2 == 4 ~ 'fallow',
                                       plot_land_use_2 == 5 ~ 'forest',
                                       plot_land_use_2 == 6 ~ 'other_specify',
                                       .default = NA)),
  crop2_data |>
    select(y2_hhid, plotnum, ag3b_03) |>
    rename(farm_id = y2_hhid, plot_id = plotnum, 
           plot_land_use_3 = ag3b_03) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           plot_land_use_3 = case_when(plot_land_use_3 == 1 ~ 'CULTIVATED',
                                       plot_land_use_3 == 2 ~ 'rented out',
                                       plot_land_use_3 == 3 ~ 'given out',
                                       plot_land_use_3 == 4 ~ 'fallow',
                                       plot_land_use_3 == 5 ~ 'forest',
                                       plot_land_use_3 == 6 ~ 'other_specify',
                                       .default = NA)) ) |>
  as_tibble() |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' | 
                                     plot_land_use_2 == 'CULTIVATED' | 
                                     plot_land_use_3 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)))

tza_raw <- inner_join(
  tza_raw,
  tza_plot_use |>
    select(farm_id, plot_id, plot_land_use) ) |>
  filter(plot_land_use == 'CULTIVATED')

ea_data <- cluster_data |>
  select(y2_hhid, clusterid) |>
  inner_join(
    ea_data |>
      select(y2_hhid, lon_modified, lat_modified) ) |>
  rename(farm_id = y2_hhid, ea_id = clusterid) |>
  group_by(ea_id, farm_id) |>
  summarize(x = lon_modified, y = lat_modified) |>
  distinct()

tza_raw <- inner_join(
  tza_raw,
  ea_data |>
    mutate(country = 'Tanzania', year = 2010,
           farm_id = as.character(farm_id),
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(tza_raw, file = '../data/processed/Tanzania_2010_raw.csv')
#######################################################################################
# Get LSMS data from Tanzania 2008

tza_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Tanzania_2008', full.names = T)
tza_zip <- tza_fold[grep('STATA_English_labels.zip$', tza_fold, ignore.case = T)]
tza_file_list <- unzip(tza_zip, list = T)$Name
tza_sel_files <- tza_file_list[grep('HH.Geovariables_Y1|SEC_A_T|HH_SEC_B_C_D_E1_F_G1_U|SEC_B|SEC_2A|SEC_2B|SEC_3A|SEC_3B', tza_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(tza_zip, files = gsub('^./', '', tza_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('SEC_A_T', dir(temporary_dir, recursive = T))]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('SEC_B_C_D_E1_F_G1_U', dir(temporary_dir, recursive = T))]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('SEC_2A', dir(temporary_dir, recursive = T))]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('SEC_2B', dir(temporary_dir, recursive = T))]
crop_roster_1 <- dir(temporary_dir, recursive = T)[grep('SEC_3A', dir(temporary_dir, recursive = T))]
crop_roster_2 <- dir(temporary_dir, recursive = T)[grep('SEC_3B', dir(temporary_dir, recursive = T))]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('HH.Geovariables_Y1', dir(temporary_dir, recursive = T))]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_2))
crop1_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_1))
crop2_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))


plot_data <- full_join(
  plot1_data |>
    as_tibble() |>
    select(hhid, plotnum, s2aq4, area) |>
    rename(farm_id = hhid, plot_id = plotnum,
           reported_area = s2aq4,  
           measured_plot_area = area) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           reported_area_ha = reported_area / 2.47,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 4)), 
  plot2_data |>
    as_tibble() |>
    select(hhid, plotnum, s2bq9, area) |>
    rename(farm_id = hhid, plot_id = plotnum,
           reported_area = s2bq9, 
           measured_plot_area = area) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           reported_area_ha = reported_area / 2.47,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 4)) ) |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  distinct(plot_id, .keep_all = T)

tza_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(hhid, sbq2) |>
    filter(!is.na(sbq2)) |> # Sex must be filled in
    mutate(farm_id = as.character(hhid)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data
)

tza_plot_use <- inner_join(
  crop1_data |>
    select(hhid, plotnum, s3aq3, s3aq74) |>
    rename(farm_id = hhid, plot_id = plotnum,
           plot_land_use_1 = s3aq3, plot_land_use_2 = s3aq74) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           plot_land_use_1 = case_when(plot_land_use_1 == 1 ~ 'CULTIVATED',
                                       plot_land_use_1 == 2 ~ 'rented out',
                                       plot_land_use_1 == 3 ~ 'given out',
                                       plot_land_use_1 == 4 ~ 'fallow',
                                       plot_land_use_1 == 5 ~ 'forest',
                                       plot_land_use_1 == 6 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_2 = case_when(plot_land_use_2 == 1 ~ 'CULTIVATED',
                                       plot_land_use_2 == 2 ~ 'rented out',
                                       plot_land_use_2 == 3 ~ 'given out',
                                       plot_land_use_2 == 4 ~ 'fallow',
                                       plot_land_use_2 == 5 ~ 'forest',
                                       plot_land_use_2 == 6 ~ 'other_specify',
                                       .default = NA)),
  crop2_data |>
    select(hhid, plotnum, s3bq3) |>
    rename(farm_id = hhid, plot_id = plotnum, 
           plot_land_use_3 = s3bq3) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           plot_land_use_3 = case_when(plot_land_use_3 == 1 ~ 'CULTIVATED',
                                       plot_land_use_3 == 2 ~ 'rented out',
                                       plot_land_use_3 == 3 ~ 'given out',
                                       plot_land_use_3 == 4 ~ 'fallow',
                                       plot_land_use_3 == 5 ~ 'forest',
                                       plot_land_use_3 == 6 ~ 'other_specify',
                                       .default = NA)) ) |>
  as_tibble() |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' | 
                                     plot_land_use_2 == 'CULTIVATED' | 
                                     plot_land_use_3 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)))

tza_raw <- inner_join(
  tza_raw,
  tza_plot_use |>
    select(farm_id, plot_id, plot_land_use) ) |>
  filter(plot_land_use == 'CULTIVATED')

ea_data <- cluster_data |>
  select(hhid, clusterid) |>
  inner_join(
    ea_data |>
      select(hhid, lon_modified, lat_modified) ) |>
  rename(farm_id = hhid, ea_id = clusterid) |>
  group_by(ea_id, farm_id) |>
  summarize(x = lon_modified, y = lat_modified) |>
  distinct()

tza_raw <- inner_join(
  tza_raw,
  ea_data |>
    mutate(country = 'Tanzania', year = 2008,
           farm_id = as.character(farm_id),
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(tza_raw, file = '../data/processed/Tanzania_2008_raw.csv')
#######################################################################################
# Get LSMS data from Tanzania 2019

# in Tanzania 2019, GPS coordinates of the Enumeration Areas ==> cm_gps__Latitude and cm_gps__Longitude in cm_sec_a file
# in Tanzania 2019, household size can be derived from ==> hh_b02 (sex) in hh_sec_b
# in Tanzania 2019, field number ==> NOT AVAILABLE
# in Tanzania 2019, plot number ==> a combination of plot_id and ag_01_01 (previous plot id that is in round 4) in  ag_sec_01
# in Tanzania 2019, farmer-reported plot area ==> ag2a_04 (in acres, unclear whether reported or measured )  in ag_sec_02
# in Tanzania 2019, farmer reported plot unit ==> ACRES
# in Tanzania 2019, was plot measured with GPS ==> ag2a_06  in ag_sec_02
# in Tanzania 2019, GPS measured plot size (ha) ==> ag2a_09 / 2.47 in ag_sec_02
# in Tanzania 2019, crop code was not available, ag3a_03  in AG_SEC_3A (land-use of plot in long rainy season) and ag3a_76 (land-use in short season) in AG_SEC_3A  and (for new plots? only in long season) ag3b_03 in AG_SEC_3B  \\\\\new vs existing plot? \\\ ag3a_03 (land-use in ag_sec_03a) and ag3a_07 (crop) in long rainy season, and ag3b_03 and ag3b_07 for short rainy season
# in Tanzania 2019, household ID ==>  is a combination of ssd_id??,  y5_hhid, cm_a05 (enumeration area in cm_sec_a) and then plot_id or plotnum (ag_sec_02 and ag_sec_03), sometimes refers to prevplot_id

tza_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Tanzania_2019', full.names = T)
tza_zip <- tza_fold[grep('Stata12.zip$', tza_fold, ignore.case = T)]
tza_file_list <- unzip(tza_zip, list = T)$Name
tza_sel_files <- tza_file_list[grep('npssdd.panel.key|CM_SEC_A|HH_SEC_A|HH_SEC_B|AG_SEC_01|AG_SEC_02|AG_SEC_3A|AG_SEC_3B', tza_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(tza_zip, files = gsub('^./', '', tza_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('hh_sec_a', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('hh_sec_b\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_3 <- dir(temporary_dir, recursive = T)[grep('npssdd.panel.key', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('ag_sec_02', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster_1 <- dir(temporary_dir, recursive = T)[grep('ag_sec_3a\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster_2 <- dir(temporary_dir, recursive = T)[grep('ag_sec_3b\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('cm_sec_a', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
tracking_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_3))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop1_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_1))
crop2_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

# and to retrieve EA locations from previous surveys
hh_2014 <- read_csv('../data/processed/Tanzania_2014_raw.csv')
hh_2012 <- read_csv('../data/processed/Tanzania_2012_raw.csv')
hh_2010 <- read_csv('../data/processed/Tanzania_2010_raw.csv')
hh_2008 <- read_csv('../data/processed/Tanzania_2008_raw.csv')

tza_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(sdd_hhid, hh_b02) |>
    filter(!is.na(hh_b02)) |> # Sex must be filled in
    mutate(farm_id = as.character(sdd_hhid)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    as_tibble() |>
    select(sdd_hhid, plotnum, ag2a_04, ag2a_06, ag2a_09) |>
    rename(farm_id = sdd_hhid, plot_id = plotnum,
           reported_area = ag2a_04, measured_plot = ag2a_06, 
           measured_plot_area = ag2a_09) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           report_unit = 'acres',
           measured_plot = as.character(labelled::to_factor(measured_plot)),
           measured_plot = case_when(measured_plot == 'YES' ~ 'GPS-measured',
                                     measured_plot == 'NO' ~ 'not measured',
                                     .default = NA),
           reported_area_ha = reported_area / 2.47,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 4)) 
)

tza_plot_use <- inner_join(
  crop1_data |>
    select(sdd_hhid, plotnum, ag3a_03, ag3a_76) |>
    rename(farm_id = sdd_hhid, plot_id = plotnum,
           plot_land_use_1 = ag3a_03, plot_land_use_2 = ag3a_76) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           plot_land_use_1 = as.character(labelled::to_factor(plot_land_use_1)),
           plot_land_use_2 = as.character(labelled::to_factor(plot_land_use_2)),
           plot_land_use_2 = case_when(plot_land_use_2 == 'YES' ~ 'CULTIVATED',
                                       plot_land_use_2 == 'NO' ~ 'uncultivated',
                                       .default = NA)),
  crop2_data |>
    select(sdd_hhid, plotnum, ag3b_03) |>
    rename(farm_id = sdd_hhid, plot_id = plotnum, 
           plot_land_use_3 = ag3b_03) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           plot_land_use_3 = as.character(labelled::to_factor(plot_land_use_3)) ) ) |>
  as_tibble() |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' | 
                                     plot_land_use_2 == 'CULTIVATED' | 
                                     plot_land_use_3 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)))

tza_raw <- inner_join(
  tza_raw,
  tza_plot_use |>
    select(farm_id, plot_id, plot_land_use) ) |>
  filter(plot_land_use == 'CULTIVATED') |>
  inner_join(
    cluster_data |>
      select(sdd_hhid, clusterid) |>
      rename(farm_id = sdd_hhid, ea_id = clusterid)
  )

# Getting ea_id coordinates from previous waves (2008, 2010, 2012)
tza_previous_waves <- bind_rows(
  hh_2008 |>
    select(farm_id, ea_id, x, y) |>
    rename(y1_hhid = farm_id),
  hh_2010 |>
    select(farm_id, ea_id, x, y) |>
    rename(y2_hhid = farm_id),
  hh_2012 |>
    select(farm_id, ea_id, x, y) |>
    rename(y3_hhid = farm_id),
  hh_2014 |>
    select(farm_id, ea_id, x, y) |>
    rename(y4_hhid = farm_id)
)

tza_raw  <- left_join(
  tza_raw |>
    mutate(country = 'Tanzania', year = 2019,
           ea_id = str_pad(ea_id, width = 9, side = 'left', pad = '0'),
           ea_id = paste0(substr(ea_id, 1, 2), '-', substr(ea_id, 3, 3), '-', substr(ea_id, 4, 6), '-', substr(ea_id, 7, 9)),
           ea_id = gsub('_', '', ea_id)),
  tza_previous_waves |>
    mutate(ea_id = str_pad(as.character(ea_id), width = 8, side = 'left', pad = '0'),
           ea_id = paste0(substr(ea_id, 1, 2), '-', substr(ea_id, 3, 3), '-0', substr(ea_id, 4, 5), '-', substr(ea_id, 6, 8)),
           ea_id = gsub('_', '',ea_id)) ) |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha) |>
  distinct(plot_id, .keep_all = T)

  
write_csv(tza_raw, file = '../data/processed/Tanzania_2019_raw.csv')
#######################################################################################
# Get LSMS data from Tanzania 2020

# in Tanzania 2020, GPS coordinates of the Enumeration Areas ==> cm_gps__Latitude and cm_gps__Longitude in cm_sec_a file
# in Tanzania 2020, household size can be derived from ==> hh_b02 (sex) in hh_sec_b
# in Tanzania 2020, field number ==> NOT AVAILABLE
# in Tanzania 2020, plot number ==> a combination of plot_id and ag_01_01 (previous plot id that is in round 4) in  ag_sec_01
# in Tanzania 2020, farmer-reported plot area ==> ag2a_04 (in acres, unclear whether reported or measured )  in ag_sec_02
# in Tanzania 2020, farmer reported plot unit ==> ACRES
# in Tanzania 2020, was plot measured with GPS ==> ag2a_06  in ag_sec_02
# in Tanzania 2020, GPS measured plot size (ha) ==> ag2a_09 / 2.47 in ag_sec_02
# in Tanzania 2020, crop code was not available, ag3a_03  in AG_SEC_3A (land-use of plot in long rainy season) and ag3a_76 (land-use in short season) in AG_SEC_3A  and (for new plots? only in long season) ag3b_03 in AG_SEC_3B  \\\\\new vs existing plot? \\\ ag3a_03 (land-use in ag_sec_03a) and ag3a_07 (crop) in long rainy season, and ag3b_03 and ag3b_07 for short rainy season
# in Tanzania 2020, household ID ==>  is a combination of ssd_id??,  y5_hhid, cm_a05 (enumeration area in cm_sec_a) and then plot_id or plotnum (ag_sec_02 and ag_sec_03), sometimes refers to prevplot_id

tza_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Tanzania_2020', full.names = T)
tza_zip <- tza_fold[grep('Stata14.zip$', tza_fold, ignore.case = T)]
tza_file_list <- unzip(tza_zip, list = T)$Name
tza_sel_files <- tza_file_list[grep('npsy5.panel.key|CM_SEC_A|HH_SEC_A|HH_SEC_B|AG_SEC_01|AG_SEC_02|AG_SEC_3A|AG_SEC_3B', tza_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(tza_zip, files = gsub('^./', '', tza_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('hh_sec_a', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('hh_sec_b\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_3 <- dir(temporary_dir, recursive = T)[grep('npsy5.panel.key', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('ag_sec_02', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster_1 <- dir(temporary_dir, recursive = T)[grep('ag_sec_3a\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster_2 <- dir(temporary_dir, recursive = T)[grep('ag_sec_3b\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('cm_sec_a', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
tracking_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_3))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop1_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_1))
crop2_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

# and to retrieve EA locations from previous surveys
hh_2019 <- read_csv('../data/processed/Tanzania_2019_raw.csv')
hh_2014 <- read_csv('../data/processed/Tanzania_2014_raw.csv')
hh_2012 <- read_csv('../data/processed/Tanzania_2012_raw.csv')
hh_2010 <- read_csv('../data/processed/Tanzania_2010_raw.csv')
hh_2008 <- read_csv('../data/processed/Tanzania_2008_raw.csv')

tza_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(y5_hhid, hh_b02) |>
    filter(!is.na(hh_b02)) |> # Sex must be filled in
    mutate(farm_id = as.character(y5_hhid)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    as_tibble() |>
    select(y5_hhid, plot_id, ag2a_04, ag2a_06, ag2a_09) |>
    rename(farm_id = y5_hhid, 
           reported_area = ag2a_04, measured_plot = ag2a_06, 
           measured_plot_area = ag2a_09) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           report_unit = 'acres',
           measured_plot = as.character(labelled::to_factor(measured_plot)),
           measured_plot = case_when(measured_plot == 'YES' ~ 'GPS-measured',
                                     measured_plot == 'NO' ~ 'not measured',
                                     .default = NA),
           reported_area_ha = reported_area / 2.47,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 4)) 
)

tza_plot_use <- inner_join(
  crop1_data |>
    select(y5_hhid, plot_id, ag3a_03, ag3a_76) |>
    rename(farm_id = y5_hhid, 
           plot_land_use_1 = ag3a_03, plot_land_use_2 = ag3a_76) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           plot_land_use_1 = as.character(labelled::to_factor(plot_land_use_1)),
           plot_land_use_2 = as.character(labelled::to_factor(plot_land_use_2)),
           plot_land_use_2 = case_when(plot_land_use_2 == 'YES' ~ 'cultivated',
                                       plot_land_use_2 == 'NO' ~ 'uncultivated',
                                       .default = NA)),
  crop2_data |>
    select(y5_hhid, plot_id, ag3b_03) |>
    rename(farm_id = y5_hhid, 
           plot_land_use_3 = ag3b_03) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
           plot_land_use_3 = as.character(labelled::to_factor(plot_land_use_3)) ) ) |>
  as_tibble() |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'cultivated' | 
                                     plot_land_use_2 == 'cultivated' | 
                                     plot_land_use_3 == 'cultivated' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)))

tza_raw <- inner_join(
  tza_raw,
  tza_plot_use |>
    select(farm_id, plot_id, plot_land_use) ) |>
  filter(plot_land_use == 'CULTIVATED') |>
  inner_join(
    cluster_data |>
      select(y5_hhid, clusterid) |>
      rename(farm_id = y5_hhid, ea_id = clusterid)
  )

# Getting ea_id coordinates from previous waves (2014 only, for there was a miss-match with 2019)
tza_cluster_gps <- inner_join(
  tracking_data |>
    select(y5_hhid, y4_hhid) |>
    mutate(y5_hhid = paste0(substr(y5_hhid, 1, 9), 0, substr(y5_hhid, 10, 11))),
  hh_2014 |>
    select(farm_id, ea_id, x, y) |>
    rename(y4_hhid = farm_id) |>
    mutate(ea_id = as.character(ea_id)) ) |> 
  select(ea_id, x, y) |>
  distinct()

tza_raw  <- left_join(
  tza_raw |>
    mutate(country = 'Tanzania', year = 2020,
           ea_id = str_pad(ea_id, width = 12, side = 'left', pad = '0')),
  tza_cluster_gps |>
    mutate(ea_id = str_pad(as.character(ea_id), width = 12, side = 'left', pad = '0')) ) |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha) |>
  distinct(plot_id, .keep_all = T) 

write_csv(tza_raw, file = '../data/processed/Tanzania_2020_raw.csv')
#######################################################################################
# Get LSMS data from Uganda 2019

# in Uganda 2019, GPS coordinates of the Enumeration Areas are not available. Will use s1aq04a in gsec1 (parish names) in combination with GADM.
# in Uganda 2019, household size ==> can be retrieved from  h2q3 (sex) in gsec2
# in Uganda 2019, field number ==> NA
# in Uganda 2019, plot number ==> parcelID in agsec1
# in Uganda 2019, farmer-reported plot area ==>  s2aq5 in agsec2a AND s2aq05 in agsec2b
# in Uganda 2019, farmer-reported plot unit ==> ACRES
# in Uganda 2019, was plot measured with GPS ==>  ???
# in Uganda 2019, GPS measured plot size (ha) ==> s2aq4 in agsec2a  AND  s2aq04 in agsec2b  (plot rented in or access through other user rights)
# in Uganda 2019, crop code ==> can be retrieved through s2aq11a (land-use in season 2 in 2018) and s2aq11b (land-use in season 1 in 2019) in agsec2a AND a2bq12a and a2bq12b in agsec2b
# in Uganda 2019, household ID ==>  is a combination of Final_EA_code (enumeration area), hhid +++ then parcelID, pltid

# parishes <- tolower(sort(unique(cluster_data$s1aq04a))), but failed to capture 220 of them in GADM/NAME_4
# sub_counties <- tolower(sort(unique(cluster_data$s1aq03a)))

uga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Uganda_2019', full.names = T)
uga_zip <- uga_fold[grep('Stata14.zip$', uga_fold, ignore.case = T)]
uga_file_list <- unzip(uga_zip, list = T)$Name
uga_sel_files <- uga_file_list[grep('gsec1|gsec2|agsec1|agsec2a|agsec2b|csec1a', uga_file_list)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(uga_zip, files = gsub('^./', '', uga_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('/gsec1\\.', dir(temporary_dir, recursive = T))]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('/gsec2\\.', dir(temporary_dir, recursive = T))]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('agsec2a', dir(temporary_dir, recursive = T))]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('agsec2b', dir(temporary_dir, recursive = T))]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('csec1a', dir(temporary_dir, recursive = T))]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))
uga_ea_2019_data <- ea_data

plot_data <- full_join(
  plot1_data |>
    select(hhid, parcelID, s2aq4, s2aq5, s2aq11a, s2aq11b) |>
    rename(farm_id = hhid, plot_id = parcelID,
           reported_area = s2aq5,
           measured_plot_area = s2aq4,
           plot_land_use_1 = s2aq11a, plot_land_use_2 = s2aq11b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_1 = case_when(plot_land_use_1 == 1 ~ 'CULTIVATED',
                                       plot_land_use_1 == 2 ~ 'CULTIVATED',
                                       plot_land_use_1 == 3 ~ 'rented_out',
                                       plot_land_use_1 == 4 ~ 'CULTIVATED',
                                       plot_land_use_1 == 5 ~ 'fallow',
                                       plot_land_use_1 == 6 ~ 'grazing_land',
                                       plot_land_use_1 == 7 ~ 'woodlot',
                                       plot_land_use_1 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_2 = case_when(plot_land_use_2 == 1 ~ 'CULTIVATED',
                                       plot_land_use_2 == 2 ~ 'CULTIVATED',
                                       plot_land_use_2 == 3 ~ 'rented_out',
                                       plot_land_use_2 == 4 ~ 'CULTIVATED',
                                       plot_land_use_2 == 5 ~ 'fallow',
                                       plot_land_use_2 == 6 ~ 'grazing_land',
                                       plot_land_use_2 == 7 ~ 'woodlot',
                                       plot_land_use_2 == 96 ~ 'other_specify',
                                       .default = NA)), 
  plot2_data |>
    select(hhid, parcelID, s2aq04, s2aq05, a2bq12a, a2bq12b) |>
    rename(farm_id = hhid, plot_id = parcelID,
           reported_area = s2aq05, 
           measured_plot_area = s2aq04,
           plot_land_use_3 = a2bq12a, plot_land_use_4 = a2bq12b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_3 = case_when(plot_land_use_3 == 1 ~ 'CULTIVATED',
                                       plot_land_use_3 == 2 ~ 'CULTIVATED',
                                       plot_land_use_3 == 3 ~ 'rented_out',
                                       plot_land_use_3 == 5 ~ 'fallow',
                                       plot_land_use_3 == 6 ~ 'grazing_land',
                                       plot_land_use_3 == 7 ~ 'woodlot',
                                       plot_land_use_3 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_4 = case_when(plot_land_use_4 == 1 ~ 'CULTIVATED',
                                       plot_land_use_4 == 2 ~ 'CULTIVATED',
                                       plot_land_use_4 == 3 ~ 'rented_out',
                                       plot_land_use_4 == 5 ~ 'fallow',
                                       plot_land_use_4 == 6 ~ 'grazing_land',
                                       plot_land_use_4 == 7 ~ 'woodlot',
                                       plot_land_use_4 == 96 ~ 'other_specify',
                                       .default = NA)) ) |>
  as_tibble() |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' | 
                                     plot_land_use_2 == 'CULTIVATED' | 
                                     plot_land_use_3 == 'CULTIVATED' | 
                                     plot_land_use_4 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)),
         reported_area_ha = reported_area / 2.47  ) 

uga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(hhid, h2q3) |>
    filter(!is.na(h2q3)) |> # Sex must be filled in
    mutate(farm_id = as.character(hhid)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    filter(plot_land_use == 'CULTIVATED')
)

# Since GPS coordinates of EA were not given, the centroid of the parish/ward is used instead
# however, approx. 220 parish names were not found in GADM

# calculate centroid positions for parish/wards in GADM
# uga_gadm_level4 <- geodata::gadm("Uganda", level = 4, path = paste0(input_path, '/gadm/Uganda/level4') )
ug4 <- terra::unwrap(readRDS(paste0(input_path, '/gadm/Uganda/level4/gadm/gadm41_UGA_4_pk.rds')))
ug4_centroids <- terra::centroids(ug4)
uga_parish_coords <- terra::crds(ug4_centroids) # as.data.frame(uga_centroids, xy = T) did not work
ug4_admin <- terra::as.data.frame(ug4)
uga_parish_list <- ug4_admin |>
  select(starts_with('NAME_'))
uga_parish_list <- uga_parish_list |>
  bind_cols(uga_parish_coords) |>
  rename(district = NAME_1, county = NAME_2, sub_county = NAME_3, parish = NAME_4) 

# uga_gadm_level3 <- geodata::gadm("Uganda", level = 3, path = paste0(input_path, '/gadm/Uganda/level3') )
ug3 <- terra::unwrap(readRDS(paste0(input_path, '/gadm/Uganda/level3/gadm/gadm41_UGA_3_pk.rds')))
ug3_centroids <- terra::centroids(ug3)
uga_sub_county_coords <- terra::crds(ug3_centroids) # as.data.frame(uga_centroids, xy = T) did not work
ug3_admin <- terra::as.data.frame(ug3)
uga_sub_county_list <- ug3_admin |>
  select(starts_with('NAME_'))
uga_sub_county_list <- uga_sub_county_list |>
  bind_cols(uga_sub_county_coords) |>
  rename(district = NAME_1, county = NAME_2, sub_county = NAME_3) 

uga_lsms_parishes <- cluster_data |>
  select(hhid, district, s1aq02a, s1aq03a, s1aq04a) |>
  rename(county = s1aq02a, sub_county = s1aq03a, parish = s1aq04a) |>
  mutate(parish_match = tolower(parish)) |>
  left_join(uga_parish_list |>
              mutate(parish_match = tolower(parish)),
            by = 'parish_match') |>
  select(hhid, x, y, starts_with('distr'), starts_with('county'), starts_with('sub_county'), starts_with('parish')) |>
  distinct(hhid, .keep_all = T)

# Parishes that match perfectly across datasets (LSMS vs GADM)
found_parishes <- uga_lsms_parishes |>
  filter(!is.na(x)) |>
  distinct(hhid, .keep_all = T)

# List of parishes with initial mismatch (and save it as lost_parishes_2019 to ease the search in other waves)
lost_parishes <- uga_lsms_parishes |>
  filter(is.na(x) | is.na(y) )
lost_parishes_2019 <- lost_parishes

# step 1.1: read through 'check_list' and identify potential misspelling
list1 <- sort(unique(tolower(lost_parishes_2019$parish_match)))
list2 <- sort(unique(tolower(uga_parish_list$parish)))
distance_matrix <- stringdist::stringdistmatrix(list1, list2, method = 'jw')
rownames(distance_matrix) <- list1
colnames(distance_matrix) <- list2

check_list <- one_row <- data.frame()
for(i  in seq_along(list1)){
  ii <- distance_matrix[i,]
  j <- min(ii[ii > 0], na.rm = T)
  ifelse(j > 0 & j < 0.2, one_row <- cbind.data.frame(lsms = list1[i], gadm = names(ii[ii == j ])), h <- 0) # for looser match, please increase the upper limit of j,
  check_list <- bind_rows(check_list, one_row)
}
check_list <- check_list |>
  filter(lsms != gadm) |>
  distinct() 

check_list_lsms <- inner_join(
  check_list,
  lost_parishes |>
    select(ends_with('.x'), parish_match) |>
    rename(lsms = parish_match) ) |>
  distinct()

check_list_gadm <- inner_join(
  check_list,
  uga_parish_list |>
    select(!c(x, y)) |>
    rename(district.y = district, county.y = county, sub_county.y = sub_county, gadm = parish) |>
    mutate (gadm = tolower(gadm))) |>
  distinct()

check_list_2019 <- check_list_lsms |>
  inner_join(check_list_gadm) |>
  select(starts_with('dist'), starts_with('county'), starts_with('sub_county'), lsms, gadm) |>
  mutate(lsms = toupper(lsms)) |>
  distinct(.keep_all = T)
print(check_list_2019) #enlarge your left panel to see the entire table in a nice format. Look the whole line and search for matches between x and y (may not be the same level). Apply some intuition!

# step 1.2: to get the misspelled names, manually check across these 2 datasets (and fill in the misspelled/correct_names pairs below)
lost_parishes |>
  distinct(parish_match, .keep_all = T) |>
  arrange(parish_match) |>
  View()

uga_parish_list |>
  select(!c(x, y)) |>
  filter(tolower(substr(parish, 1, 1)) == 'a') |> # change a to b, c, etc
  arrange(parish)

mispelled_parish_names <- c('ABAR WEST', 'ABERIDWOGO', 'ABIRA EAST', 'ACHUNGI', 'ADELLOGO', 'AGURURU', 'ANYOMOREM', 'ATIGOLWOK', 
                            'ADOLLO', 'AMIIABERIDWOGO', 'ANEPKIDI', 'ANGWETANGWET', 'APA', 'ARWOTOMITO',
                            'BARONGER GO DOWN', 'BUJANGA', 'BUMULISYA', 'BUSUNJU TOWN BOARD', 'BUWENGE WEST', 
                            'BARAPWO', 'BISON MAGURIA', 'BUGUSEGE', 'BUKIBETI', 'BUWUNI RURAL', 
                            'CHEGERE', 'DOG APIO', 'IDUDI TOWN BOARD', 'IGGWE', 'INDUSTRIAL', 'IRIAGA', 
                            'CAMPSWAHILI', 'DIMA', 'GGULU', 'GWENGDIYA', 'IREDA WEST', 
                            'KAKOGE WARD', 'KALIRO', 'KASHOZI EAST', 'KASHOZI WEST', 'KATANGA WARD', 'KIRYOKYA', 
                            'KAMOR', 'KASIMERI', 'KIHUUBA', 'KIJJUMBA', 'KIJUGUTA', 'KIJJUMBA', 'KIJUMBA WEST', 'KIJUNA', 'KIMAANYA', 'KIRAARO', 'KITOROGYA',
                            'KOTIDO RURAL', 'KYEBANDO I ', 'KYENGERA TOWN BOARD', 
                            'LUBYA', 'MATUGGA', 'NAMAGABI', 'NAMPAOGWE', 'NANTABULIRIRWA', 'NJERU WEST WARD', 'NTENJERU',
                            'LOMERUMA', 'LOSILANG PARISH', 'LTOJO', 'LWABAKOBA', 'MADI KILOC', 'MAKERERE UNIVERSITY', 'MBIRIIZI', 'MUGARUSTYA', 'MUTUNGO I',
                            'NAKULABYE I', 'NAMTHINI', 'NANKULABYE I', 'NINDYE', 'NSUUBE KAUGA', 'NTAAWO', 'NTAYIGIRWA',  'NYAMASA PARISH', 'NYARWIMUKA', 
                            'OKAPI', 'OMACH', 'PUGWENYI', 'RAGEM LOWER', 'RUHIRA', 'VILLAMARIA', 
                            
                            'NYAKASANGA I WARD', 'OLD CAMP-SWAHILI', 'OMOLADYANG', 'ORUPU', 'PECE PRISON', 
                            'TEGWANA', 'VANGUARD', 'WESTLAND B', # add for the other years and save it
                            'KYOGA', 'MITUKULA', 'NICU PARISH')  
corrected_parish_names <-c('Abar', 'Aberidwogo-Omera', 'Abira', 'Acungi', 'Adelogo', 'Agururu A Ward', 'Anyomerem', 'Atigo-Lwok', 
                           'Padolo', 'Amii', 'Anepkide', 'Angwecibange', 'Appa', 'Arwot- Omito',
                           'Bar Onger', 'Bujaga', 'Bumulisha', 'Busunju', 'Buwenge West Ward', 
                           'Bar Apwo', 'Bison-Maguria Ward', 'Bugube', 'Bukiabi',  'Buwuni',
                           'Cegere', 'Dogapio', 'Idudi', 'Igwe', 'Industrial Ward', 'Iriaga Ward',
                           'Kampswahili', 'Diima', 'GGulu Ward', 'Gwendiya', 'Ireda-West', 
                           'Kakoge', 'Kaliiro', 'Kashozi', 'Kashozi', 'Katanga/Township', 'Kiryookya',
                           'Kamoru', 'Kathimeri', 'Kihuba', 'Kijumba', 'Kijuguta Ward', 'Kijumba', 'Kijumba', 'Kijjuna','Kimanya', 'Kiraro', 'Kitonya',
                           'Kotido Central', 'Kyebando', 'Kyengera', 
                           'Lubia', 'Matuga', 'Namagabi Ward', 'Nampongwe', 'Nantabulirwa', 'Njeru West', 'Ntenjeru Ward', 
                           'Lomerima', 'Losilang', 'Itojo', 'Lwabakooba', 'Madi-Kiloc', 'Makerere Ii', 'Mbirizi', 'Mugarutsya', 'Mutungo', 
                           'Nakulabye', 'Namthin', 'Nakulabye', 'Nnindye', 'Nsuube-Kauga', 'Ntaawo Ward', 'Ntaigirwa', 'Nyamasa', 'Nyarwimuka/Kanyamisekuru', 
                           'Ojapi', 'Omac', 'Pugwinyi', 'Ragem', 'Ruhiira', 'Villa-Maria', 
                           
                           'Nyakasanga I', 'Old Campswahili', 'Omolo-Adyang', 'Orupo', 'Pece Prisons Ward', 
                           'Tegwana Ward', 'Vanguard Ward', 'Westland', # add for the other years and save it
                           'Kioga', 'Mitukula Ward', 'Nicu')  
parish_name_match <- cbind(lsms = mispelled_parish_names, gadm = corrected_parish_names); rm(mispelled_parish_names, corrected_parish_names)
pnm <- parish_name_match[,'lsms']

parish_name_match <- read_csv('../data/raw/received/Uganda_proposed_matching_names-jvs.csv')
parish_name_match <- parish_name_match |>
  rename(lsms = parish.LSMS, gadm = parish.GADM) |>
  filter(matching == 'yes')
pnm <- parish_name_match[,'lsms']

# Parish names that were misspelled in LSMS (considering GADM as reference)
lost_par_1 <- lost_parishes |> 
  filter(parish_match %in% tolower(pnm)) |>
  inner_join(parish_name_match |>
               as_tibble() |>
               mutate(parish_match = tolower(lsms), parish = tolower(gadm)) ) |>
  select(hhid, parish) |>
  inner_join(uga_parish_list |>
               select(x, y, parish) |>
               mutate(parish = tolower(parish)) ) |>
  distinct(hhid, .keep_all = T)


# Step 2:Using sub-county in LSMS to match with GADM's sub-county
lost_par_2 <- lost_parishes |> 
  filter(!(hhid %in% unique(c(found_parishes$hhid,  lost_par_1$hhid))) ) |>
  select(hhid, district.x, county.x, sub_county.x) |>
  mutate(sub_county = tolower(sub_county.x)) |>
  inner_join(uga_sub_county_list |>
               mutate(sub_county = tolower(sub_county))) |>
  select(hhid, x, y, starts_with('distr'), starts_with('county'), starts_with('sub_county') ) |>
  distinct(hhid, .keep_all = T)

# Put together the found parishes, the misspelled ones, and the one for which the sub_county was retrieved
uga_2019_gps <- bind_rows(
  found_parishes |>
    select(hhid, x, y),
  lost_par_1 |>
    select(hhid, x, y),
  lost_par_2 |>
    select(hhid, x, y) ) |>
  distinct(hhid, .keep_all = T) 

ea_data <- ea_data |>
  select(Final_EA_code, starts_with('s1aq')) |>
  rename(ea_id = Final_EA_code, district_code = s1aq01b, county_code = s1aq02b,
         sub_county_code = s1aq03b, parish_code = s1aq04b) |>
  inner_join(cluster_data |>
               select(hhid, ends_with('2018')) |>
               rename(district_code = dc_2018, county_code = cc_2018,
                      sub_county_code = sc_2018, parish_code = pc_2018) |>
               mutate(district_code = as.character(district_code),
                      county_code = as.character(county_code),
                      sub_county_code = as.character(sub_county_code),
                      parish_code = as.character(parish_code))) |>
  inner_join(uga_2019_gps  ) |>
  distinct(ea_id, hhid, x, y) 

uga_raw <- inner_join(
  uga_raw,
  ea_data |>
    mutate(country = 'Uganda', year = 2019,
           farm_id = as.character(hhid),
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(uga_raw, file = '../data/processed/Uganda_2019_raw.csv')
#######################################################################################
# Get LSMS data from Uganda 2018

# in Uganda 2018, GPS coordinates of the Enumeration Areas are not available. Will use s1aq04a in gsec1 (parish names) in combination with GADM.
# in Uganda 2018, household size ==> can be retrieved from  h2q3 (sex) in gsec2
# in Uganda 2018, field number ==> NA
# in Uganda 2018, plot number ==> parcelID in agsec1
# in Uganda 2018, farmer-reported plot area ==>  s2aq5 in agsec2a AND s2aq05 in agsec2b
# in Uganda 2018, farmer-reported plot unit ==> ACRES
# in Uganda 2018, was plot measured with GPS ==>  ???
# in Uganda 2018, GPS measured plot size (ha) ==> s2aq4 in agsec2a  AND  s2aq04 in agsec2b  (plot rented in or access through other user rights)
# in Uganda 2018, crop code ==> can be retrieved through s2aq11a (land-use in season 2 in 2017) and s2aq11b (land-use in season 1 in 2018) in agsec2a AND a2bq12a and a2bq12b in agsec2b
# in Uganda 2018, household ID ==>  is a combination of Final_EA_code (enumeration area), hhid +++ then parcelID, pltid

uga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Uganda_2018', full.names = T)
uga_zip <- uga_fold[grep('CSV.zip$', uga_fold, ignore.case = T)] # the CSV is used here because the STATA12.zip structure is not friendly
uga_file_list <- unzip(uga_zip, list = T)$Name
uga_sel_files <- uga_file_list[grep('gsec1.csv|gsec2.csv|agsec1.csv|agsec2a.csv|agsec2b.csv|csec1a.csv', uga_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(uga_zip, files = gsub('^./', '', uga_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('/gsec1', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('/gsec2', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('agsec2a', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('agsec2b', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('csec1a', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- read_csv(paste0(temporary_dir, '/', household_roster_1))
hh_data <- read_csv(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- read_csv(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- read_csv(paste0(temporary_dir, '/', plot_roster_2))
ea_data <- read_csv(paste0(temporary_dir, '/', ea_characteristics))
uga_ea_2018_data <- ea_data

plot_data <- full_join(
  plot1_data |>
    select(hhid, parcelID, s2aq4, s2aq5, s2aq11a, s2aq11b) |>
    rename(farm_id = hhid, plot_id = parcelID,
           reported_area = s2aq5,
           measured_plot_area = s2aq4,
           plot_land_use_1 = s2aq11a, plot_land_use_2 = s2aq11b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_1 = case_when(plot_land_use_1 == 1 ~ 'CULTIVATED',
                                       plot_land_use_1 == 2 ~ 'CULTIVATED',
                                       plot_land_use_1 == 3 ~ 'rented_out',
                                       plot_land_use_1 == 4 ~ 'CULTIVATED',
                                       plot_land_use_1 == 5 ~ 'fallow',
                                       plot_land_use_1 == 6 ~ 'grazing_land',
                                       plot_land_use_1 == 7 ~ 'woodlot',
                                       plot_land_use_1 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_2 = case_when(plot_land_use_2 == 1 ~ 'CULTIVATED',
                                       plot_land_use_2 == 2 ~ 'CULTIVATED',
                                       plot_land_use_2 == 3 ~ 'rented_out',
                                       plot_land_use_2 == 4 ~ 'CULTIVATED',
                                       plot_land_use_2 == 5 ~ 'fallow',
                                       plot_land_use_2 == 6 ~ 'grazing_land',
                                       plot_land_use_2 == 7 ~ 'woodlot',
                                       plot_land_use_2 == 96 ~ 'other_specify',
                                       .default = NA)), 
  plot2_data |>
    select(hhid, parcelID, s2aq04, s2aq05, a2bq12a, a2bq12b) |>
    rename(farm_id = hhid, plot_id = parcelID,
           reported_area = s2aq05, 
           measured_plot_area = s2aq04,
           plot_land_use_3 = a2bq12a, plot_land_use_4 = a2bq12b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'), 
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_3 = case_when(plot_land_use_3 == 1 ~ 'CULTIVATED',
                                       plot_land_use_3 == 2 ~ 'CULTIVATED',
                                       plot_land_use_3 == 3 ~ 'rented_out',
                                       plot_land_use_3 == 5 ~ 'fallow',
                                       plot_land_use_3 == 6 ~ 'grazing_land',
                                       plot_land_use_3 == 7 ~ 'woodlot',
                                       plot_land_use_3 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_4 = case_when(plot_land_use_4 == 1 ~ 'CULTIVATED',
                                       plot_land_use_4 == 2 ~ 'CULTIVATED',
                                       plot_land_use_4 == 3 ~ 'rented_out',
                                       plot_land_use_4 == 5 ~ 'fallow',
                                       plot_land_use_4 == 6 ~ 'grazing_land',
                                       plot_land_use_4 == 7 ~ 'woodlot',
                                       plot_land_use_4 == 96 ~ 'other_specify',
                                       .default = NA)) ) |>
  as_tibble() |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' | 
                                     plot_land_use_2 == 'CULTIVATED' | 
                                     plot_land_use_3 == 'CULTIVATED' | 
                                     plot_land_use_4 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)),
         reported_area_ha = reported_area / 2.47 ) 


uga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(hhid, h2q3) |>
    filter(!is.na(h2q3)) |> # Sex must be filled in
    mutate(farm_id = as.character(hhid)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    filter(plot_land_use == 'CULTIVATED')
)

# Since GPS coordinates of EA were not given, the centroid of the parish/ward is used instead
# however, approx. 220 parish names were not found in GADM

# calculate centroid positions for parish/wards in GADM
# uga_gadm_level4 <- geodata::gadm("Uganda", level = 4, path = paste0(input_path, '/gadm/Uganda/level4') )
ug4 <- terra::unwrap(readRDS(paste0(input_path, '/gadm/Uganda/level4/gadm/gadm41_UGA_4_pk.rds')))
ug4_centroids <- terra::centroids(ug4)
uga_parish_coords <- terra::crds(ug4_centroids) # as.data.frame(uga_centroids, xy = T) did not work
ug4_admin <- terra::as.data.frame(ug4)
uga_parish_list <- ug4_admin |>
  select(starts_with('NAME_'))
uga_parish_list <- uga_parish_list |>
  bind_cols(uga_parish_coords) |>
  rename(district = NAME_1, county = NAME_2, sub_county = NAME_3, parish = NAME_4) 

# uga_gadm_level3 <- geodata::gadm("Uganda", level = 3, path = paste0(input_path, '/gadm/Uganda/level3') )
ug3 <- terra::unwrap(readRDS(paste0(input_path, '/gadm/Uganda/level3/gadm/gadm41_UGA_3_pk.rds')))
ug3_centroids <- terra::centroids(ug3)
uga_sub_county_coords <- terra::crds(ug3_centroids) # as.data.frame(uga_centroids, xy = T) did not work
ug3_admin <- terra::as.data.frame(ug3)
uga_sub_county_list <- ug3_admin |>
  select(starts_with('NAME_'))
uga_sub_county_list <- uga_sub_county_list |>
  bind_cols(uga_sub_county_coords) |>
  rename(district = NAME_1, county = NAME_2, sub_county = NAME_3) 

uga_lsms_parishes <- cluster_data |>
  select(hhid, district_2018, county_2018, subcounty_2018, parish_2018) |>
  rename(district = district_2018, county = county_2018, sub_county = subcounty_2018, parish = parish_2018) |>
  mutate(parish_match = tolower(parish)) |>
  left_join(uga_parish_list |>
              mutate(parish_match = tolower(parish)),
            by = 'parish_match') |>
  select(hhid, x, y, starts_with('distr'), starts_with('county'), starts_with('sub_county'), starts_with('parish')) |>
  distinct(hhid, .keep_all = T)

# Parishes that match perfectly across datasets (LSMS vs GADM)
found_parishes <- uga_lsms_parishes |>
  filter(!is.na(x)) |>
  distinct(hhid, .keep_all = T)

# List of parishes with initial mismatch (and save it as lost_parishes_2018 to ease the search in other waves)
lost_parishes <- uga_lsms_parishes |>
  filter(is.na(x) | is.na(y) )
lost_parishes_2018 <- lost_parishes |>
  filter(!parish_match %in% lost_parishes_2019$parish_match)

# step 1.1: read through 'check_list' and identify potential misspelling
list1 <- sort(unique(tolower(lost_parishes_2018$parish_match)))
list2 <- sort(unique(tolower(uga_parish_list$parish)))
distance_matrix <- stringdist::stringdistmatrix(list1, list2, method = 'jw')
rownames(distance_matrix) <- list1
colnames(distance_matrix) <- list2

check_list <- one_row <- data.frame()
for(i  in seq_along(list1)){
  ii <- distance_matrix[i,]
  j <- min(ii[ii > 0], na.rm = T)
  ifelse(j > 0 & j < 0.2, one_row <- cbind.data.frame(lsms = list1[i], gadm = names(ii[ii == j ])), h <- 0) # for looser match, please increase the upper limit of j,
  check_list <- bind_rows(check_list, one_row)
}
check_list <- check_list |>
  filter(lsms != gadm) |>
  distinct() 

check_list_lsms <- inner_join(
  check_list,
  lost_parishes |>
    select(ends_with('.x'), parish_match) |>
    rename(lsms = parish_match) ) |>
  distinct()

check_list_gadm <- inner_join(
  check_list,
  uga_parish_list |>
    select(!c(x, y)) |>
    rename(district.y = district, county.y = county, sub_county.y = sub_county, gadm = parish) |>
    mutate (gadm = tolower(gadm))) |>
  distinct()

check_list_2018 <- check_list_lsms |>
  inner_join(check_list_gadm) |>
  select(starts_with('dist'), starts_with('county'), starts_with('sub_county'), lsms, gadm) |>
  mutate(lsms = toupper(lsms)) |>
  distinct(.keep_all = T)
print(check_list_2018) #enlarge your left panel to see the entire table in a nice format. Look the whole line and search for matches between x and y (may not be the same level). Apply some intuition!

# step 1.2: to get the misspelled names, manually check across these 2 datasets (and fill in the misspelled/correct_names pairs below)
lost_parishes |>
  distinct(parish_match, .keep_all = T) |>
  arrange(parish_match) |>
  View()

uga_parish_list |>
  select(!c(x, y)) |>
  filter(tolower(substr(parish, 1, 1)) == 'a') |> # change a to b, c, etc
  arrange(parish)

# Get misspelled/corrected names from 2019 (Only MITUKULA /Mitukula Ward was added after copy-pasting 2019)
mispelled_parish_names <- c('ABAR WEST', 'ABERIDWOGO', 'ABIRA EAST', 'ACHUNGI', 'ADELLOGO', 'AGURURU', 'ANYOMOREM', 'ATIGOLWOK', 
                            'ADOLLO', 'AMIIABERIDWOGO', 'ANEPKIDI', 'ANGWETANGWET', 'APA', 'ARWOTOMITO',
                            'BARONGER GO DOWN', 'BUJANGA', 'BUMULISYA', 'BUSUNJU TOWN BOARD', 'BUWENGE WEST', 
                            'BARAPWO', 'BISON MAGURIA', 'BUGUSEGE', 'BUKIBETI', 'BUWUNI RURAL', 
                            'CHEGERE', 'DOG APIO', 'IDUDI TOWN BOARD', 'IGGWE', 'INDUSTRIAL', 'IRIAGA', 
                            'CAMPSWAHILI', 'DIMA', 'GGULU', 'GWENGDIYA', 'IREDA WEST', 
                            'KAKOGE WARD', 'KALIRO', 'KASHOZI EAST', 'KASHOZI WEST', 'KATANGA WARD', 'KIRYOKYA', 
                            'KAMOR', 'KASIMERI', 'KIHUUBA', 'KIJJUMBA', 'KIJUGUTA', 'KIJJUMBA', 'KIJUMBA WEST', 'KIJUNA', 'KIMAANYA', 'KIRAARO', 'KITOROGYA',
                            'KOTIDO RURAL', 'KYEBANDO I ', 'KYENGERA TOWN BOARD', 
                            'LUBYA', 'MATUGGA', 'NAMAGABI', 'NAMPAOGWE', 'NANTABULIRIRWA', 'NJERU WEST WARD', 'NTENJERU',
                            'LOMERUMA', 'LOSILANG PARISH', 'LTOJO', 'LWABAKOBA', 'MADI KILOC', 'MAKERERE UNIVERSITY', 'MBIRIIZI',  'MUGARUSTYA', 'MUTUNGO I',
                            'NAKULABYE I', 'NAMTHINI', 'NANKULABYE I', 'NINDYE', 'NSUUBE KAUGA', 'NTAAWO', 'NTAYIGIRWA',  'NYAMASA PARISH', 'NYARWIMUKA', 
                            'OKAPI', 'OMACH', 'PUGWENYI', 'RAGEM LOWER', 'RUHIRA', 'VILLAMARIA', 
                            
                            'NYAKASANGA I WARD', 'OLD CAMP-SWAHILI', 'OMOLADYANG', 'ORUPU', 'PECE PRISON', 
                            'TEGWANA', 'VANGUARD', 'WESTLAND B', # add for the other years and save it
                            'KYOGA', 'MITUKULA', 'NICU PARISH')  

corrected_parish_names <-c('Abar', 'Aberidwogo-Omera', 'Abira', 'Acungi', 'Adelogo', 'Agururu A Ward', 'Anyomerem', 'Atigo-Lwok', 
                           'Padolo', 'Amii', 'Anepkide', 'Angwecibange', 'Appa', 'Arwot- Omito',
                           'Bar Onger', 'Bujaga', 'Bumulisha', 'Busunju', 'Buwenge West Ward', 
                           'Bar Apwo', 'Bison-Maguria Ward', 'Bugube', 'Bukiabi',  'Buwuni',
                           'Cegere', 'Dogapio', 'Idudi', 'Igwe', 'Industrial Ward', 'Iriaga Ward',
                           'Kampswahili', 'Diima', 'GGulu Ward', 'Gwendiya', 'Ireda-West', 
                           'Kakoge', 'Kaliiro', 'Kashozi', 'Kashozi', 'Katanga/Township', 'Kiryookya',
                           'Kamoru', 'Kathimeri', 'Kihuba', 'Kijumba', 'Kijuguta Ward', 'Kijumba', 'Kijumba', 'Kijjuna','Kimanya', 'Kiraro', 'Kitonya',
                           'Kotido Central', 'Kyebando', 'Kyengera', 
                           'Lubia', 'Matuga', 'Namagabi Ward', 'Nampongwe', 'Nantabulirwa', 'Njeru West', 'Ntenjeru Ward', 
                           'Lomerima', 'Losilang', 'Itojo', 'Lwabakooba', 'Madi-Kiloc', 'Makerere Ii', 'Mbirizi',  'Mugarutsya', 'Mutungo', 
                           'Nakulabye', 'Namthin', 'Nakulabye', 'Nnindye', 'Nsuube-Kauga', 'Ntaawo Ward', 'Ntaigirwa', 'Nyamasa', 'Nyarwimuka/Kanyamisekuru', 
                           'Ojapi', 'Omac', 'Pugwinyi', 'Ragem', 'Ruhiira', 'Villa-Maria', 
                           
                           'Nyakasanga I', 'Old Campswahili', 'Omolo-Adyang', 'Orupo', 'Pece Prisons Ward', 
                           'Tegwana Ward', 'Vanguard Ward', 'Westland', # add for the other years and save it
                           'Kioga', 'Mitukula Ward', 'Nicu')  
parish_name_match <- cbind(lsms = mispelled_parish_names, gadm = corrected_parish_names); rm(mispelled_parish_names, corrected_parish_names)
pnm <- parish_name_match[,'lsms']

parish_name_match <- read_csv('../data/raw/received/Uganda_proposed_matching_names-jvs.csv')
parish_name_match <- parish_name_match |>
  rename(lsms = parish.LSMS, gadm = parish.GADM) |>
  filter(matching == 'yes')
pnm <- parish_name_match[,'lsms']

# Parish names that were misspelled in LSMS (considering GADM as reference)
lost_par_1 <- lost_parishes |> 
  filter(parish_match %in% tolower(pnm)) |>
  inner_join(parish_name_match |>
               as_tibble() |>
               mutate(parish_match = tolower(lsms), parish = tolower(gadm)) ) |>
  select(hhid, parish) |>
  inner_join(uga_parish_list |>
               select(x, y, parish) |>
               mutate(parish = tolower(parish)) ) |>
  distinct(hhid, .keep_all = T)


# Step 2:Using sub-county in LSMS to match with GADM's sub-county
lost_par_2 <- lost_parishes |> 
  filter(!(hhid %in% unique(c(found_parishes$hhid,  lost_par_1$hhid))) ) |>
  select(hhid, district.x, county.x, sub_county.x) |>
  mutate(sub_county = tolower(sub_county.x)) |>
  inner_join(uga_sub_county_list |>
               mutate(sub_county = tolower(sub_county))) |>
  select(hhid, x, y, starts_with('distr'), starts_with('county'), starts_with('sub_county') ) |>
  distinct(hhid, .keep_all = T)

# Put together the found parishes, the misspelled ones, and the one for which the sub_county was retrieved
uga_2018_gps <- bind_rows(
  found_parishes |>
    select(hhid, x, y),
  lost_par_1 |>
    select(hhid, x, y),
  lost_par_2 |>
    select(hhid, x, y) ) |>
  distinct(hhid, .keep_all = T) 

ea_data <- ea_data |>
  select(EA_code, starts_with('c1aq'), s1aq04b) |>
  rename(ea_id = EA_code, district_code = c1aq01b, county_code = c1aq02b,
         sub_county_code = c1aq03b, parish_code = s1aq04b) |>
  inner_join(cluster_data |>
               select(hhid, ends_with('2018')) |>
               rename(district_code = dc_2018, county_code = cc_2018,
                      sub_county_code = sc_2018, parish_code = pc_2018) |>
               mutate(county_code = as.character(county_code),
                      sub_county_code = as.character(sub_county_code),
                      parish_code = as.character(parish_code))) |>
  inner_join(uga_2018_gps  ) |>
  distinct(ea_id, hhid, x, y) 

uga_raw <- inner_join(
  uga_raw,
  ea_data |>
    mutate(country = 'Uganda', year = 2018,
           farm_id = as.character(hhid),
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(uga_raw, file = '../data/processed/Uganda_2018_raw.csv')
#######################################################################################
# Get LSMS data from Uganda 2015

# in Uganda 2015, GPS coordinates of the Enumeration Areas are not available. Will use s1aq04a in gsec1 (parish names) in combination with GADM.
# in Uganda 2015, household size ==> can be retrieved from  h2q3 (sex) in gsec2
# in Uganda 2015, field number ==> NA
# in Uganda 2015, plot number ==> parcelID in agsec1
# in Uganda 2015, farmer-reported plot area ==>  s2aq5 in agsec2a AND s2aq05 in agsec2b
# in Uganda 2015, farmer-reported plot unit ==> ACRES
# in Uganda 2015, was plot measured with GPS ==>  ???
# in Uganda 2015, GPS measured plot size (ha) ==> s2aq4 in agsec2a  AND  s2aq04 in agsec2b  (plot rented in or access through other user rights)
# in Uganda 2015, crop code ==> can be retrieved through s2aq11a (land-use in season 2 in 2017) and s2aq11b (land-use in season 1 in 2015) in agsec2a AND a2bq12a and a2bq12b in agsec2b
# in Uganda 2015, household ID ==>  is a combination of Final_EA_code (enumeration area), hhid +++ then parcelID, pltid

uga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Uganda_2015', full.names = T)
uga_zip <- uga_fold[grep('STATA8.zip$', uga_fold, ignore.case = T)]
uga_file_list <- unzip(uga_zip, list = T)$Name
uga_sel_files <- uga_file_list[grep('gsec1.csv|gsec2.csv|agsec1.csv|agsec2a.csv|agsec2b.csv|csec1a.csv', uga_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(uga_zip, files = gsub('^./', '', uga_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('/gsec1\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('/gsec2', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('agsec2a\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('agsec2b', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('csec1a', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))
uga_ea_2015_data <- ea_data

plot_data <- full_join(
  plot1_data |>
    select(HHID, parcelID, a2aq4, a2aq5, a2aq11a, a2aq11b) |>
    rename(farm_id = HHID, plot_id = parcelID,
           reported_area = a2aq5,
           measured_plot_area = a2aq4,
           plot_land_use_1 = a2aq11a, plot_land_use_2 = a2aq11b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_1 = case_when(plot_land_use_1 == 1 ~ 'CULTIVATED',
                                       plot_land_use_1 == 2 ~ 'CULTIVATED',
                                       plot_land_use_1 == 3 ~ 'rented_out',
                                       plot_land_use_1 == 4 ~ 'CULTIVATED',
                                       plot_land_use_1 == 5 ~ 'fallow',
                                       plot_land_use_1 == 6 ~ 'grazing_land',
                                       plot_land_use_1 == 7 ~ 'woodlot',
                                       plot_land_use_1 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_2 = case_when(plot_land_use_2 == 1 ~ 'CULTIVATED',
                                       plot_land_use_2 == 2 ~ 'CULTIVATED',
                                       plot_land_use_2 == 3 ~ 'rented_out',
                                       plot_land_use_2 == 4 ~ 'CULTIVATED',
                                       plot_land_use_2 == 5 ~ 'fallow',
                                       plot_land_use_2 == 6 ~ 'grazing_land',
                                       plot_land_use_2 == 7 ~ 'woodlot',
                                       plot_land_use_2 == 96 ~ 'other_specify',
                                       .default = NA)),
  plot2_data |>
    select(HHID, parcelID, a2bq4, a2bq5, a2bq12a, a2bq12b) |>
    rename(farm_id = HHID, plot_id = parcelID,
           reported_area = a2bq5,
           measured_plot_area = a2bq4,
           plot_land_use_3 = a2bq12a, plot_land_use_4 = a2bq12b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_3 = case_when(plot_land_use_3 == 1 ~ 'CULTIVATED',
                                       plot_land_use_3 == 2 ~ 'CULTIVATED',
                                       plot_land_use_3 == 3 ~ 'rented_out',
                                       plot_land_use_3 == 5 ~ 'fallow',
                                       plot_land_use_3 == 6 ~ 'grazing_land',
                                       plot_land_use_3 == 7 ~ 'woodlot',
                                       plot_land_use_3 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_4 = case_when(plot_land_use_4 == 1 ~ 'CULTIVATED',
                                       plot_land_use_4 == 2 ~ 'CULTIVATED',
                                       plot_land_use_4 == 3 ~ 'rented_out',
                                       plot_land_use_4 == 5 ~ 'fallow',
                                       plot_land_use_4 == 6 ~ 'grazing_land',
                                       plot_land_use_4 == 7 ~ 'woodlot',
                                       plot_land_use_4 == 96 ~ 'other_specify',
                                       .default = NA)) ) |>
  as_tibble() |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' |
                                     plot_land_use_2 == 'CULTIVATED' |
                                     plot_land_use_3 == 'CULTIVATED' |
                                     plot_land_use_4 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)),
         reported_area_ha = reported_area / 2.47 )


uga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(hhid, h2q3) |>
    filter(!is.na(h2q3)) |> # Sex must be filled in
    mutate(farm_id = as.character(hhid)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    filter(plot_land_use == 'CULTIVATED')
)

# Since GPS coordinates of EA were not given, the centroid of the parish/ward is used instead
# however, approx. 220 parish names were not found in GADM

# calculate centroid positions for parish/wards in GADM
# uga_gadm_level4 <- geodata::gadm("Uganda", level = 4, path = paste0(input_path, '/gadm/Uganda/level4') )
ug4 <- terra::unwrap(readRDS(paste0(input_path, '/gadm/Uganda/level4/gadm/gadm41_UGA_4_pk.rds')))
ug4_centroids <- terra::centroids(ug4)
uga_parish_coords <- terra::crds(ug4_centroids) # as.data.frame(uga_centroids, xy = T) did not work
ug4_admin <- terra::as.data.frame(ug4)
uga_parish_list <- ug4_admin |>
  select(starts_with('NAME_'))
uga_parish_list <- uga_parish_list |>
  bind_cols(uga_parish_coords) |>
  rename(district = NAME_1, county = NAME_2, sub_county = NAME_3, parish = NAME_4) 

# uga_gadm_level3 <- geodata::gadm("Uganda", level = 3, path = paste0(input_path, '/gadm/Uganda/level3') )
ug3 <- terra::unwrap(readRDS(paste0(input_path, '/gadm/Uganda/level3/gadm/gadm41_UGA_3_pk.rds')))
ug3_centroids <- terra::centroids(ug3)
uga_sub_county_coords <- terra::crds(ug3_centroids) # as.data.frame(uga_centroids, xy = T) did not work
ug3_admin <- terra::as.data.frame(ug3)
uga_sub_county_list <- ug3_admin |>
  select(starts_with('NAME_'))
uga_sub_county_list <- uga_sub_county_list |>
  bind_cols(uga_sub_county_coords) |>
  rename(district = NAME_1, county = NAME_2, sub_county = NAME_3) 

uga_lsms_parishes <- cluster_data |>
  select(HHID, district_name, subcounty_name, parish_name) |>
  rename(district = district_name,  sub_county = subcounty_name, parish = parish_name) |>
  mutate(parish_match = tolower(parish)) |>
  left_join(uga_parish_list |>
              mutate(parish_match = tolower(parish)),
            by = 'parish_match') |>
  select(HHID, x, y, starts_with('distr'), starts_with('sub_county'), starts_with('parish')) |>
  distinct(HHID, .keep_all = T)

# Parishes that match perfectly across datasets (LSMS vs GADM)
found_parishes <- uga_lsms_parishes |>
  filter(!is.na(x)) |>
  distinct(HHID, .keep_all = T)

# List of parishes with initial mismatch (and save it as lost_parishes_2015 to ease the search in other waves)
lost_parishes <- uga_lsms_parishes |>
  filter(is.na(x) | is.na(y) )
lost_parishes_2015 <- lost_parishes |>
  filter(!(parish_match %in% c(lost_parishes_2019$parish_match, lost_parishes_2018$parish_match)))

# step 1.1: read through 'check_list' and identify potential misspelling
list1 <- sort(unique(tolower(lost_parishes_2015$parish_match)))
list2 <- sort(unique(tolower(uga_parish_list$parish)))
distance_matrix <- stringdist::stringdistmatrix(list1, list2, method = 'jw')
rownames(distance_matrix) <- list1
colnames(distance_matrix) <- list2

check_list <- one_row <- data.frame()
for(i  in seq_along(list1)){
  ii <- distance_matrix[i,]
  j <- min(ii[ii > 0], na.rm = T)
  ifelse(j > 0 & j < 0.2, one_row <- cbind.data.frame(lsms = list1[i], gadm = names(ii[ii == j ])), h <- 0) # for looser match, please increase the upper limit of j,
  check_list <- bind_rows(check_list, one_row)
}
check_list <- check_list |>
  filter(lsms != gadm) |>
  distinct() 

check_list_lsms <- inner_join(
  check_list,
  lost_parishes |>
    select(ends_with('.x'), parish_match) |>
    rename(lsms = parish_match) ) |>
  distinct()

check_list_gadm <- inner_join(
  check_list,
  uga_parish_list |>
    select(!c(x, y)) |>
    rename(district.y = district, county.y = county, sub_county.y = sub_county, gadm = parish) |>
    mutate (gadm = tolower(gadm))) |>
  distinct()

check_list_2015 <- check_list_lsms |>
  inner_join(check_list_gadm) |>
  select(starts_with('dist'), starts_with('county'), starts_with('sub_county'), lsms, gadm) |>
  mutate(lsms = toupper(lsms)) |>
  distinct(.keep_all = T)
print(check_list_2015) #enlarge your left panel to see the entire table in a nice format. Look the whole line and search for matches between x and y (may not be the same level). Apply some intuition!

# step 1.2: to get the misspelled names, manually check across these 2 datasets (and fill in the misspelled/correct_names pairs below)
lost_parishes |>
  distinct(parish_match, .keep_all = T) |>
  arrange(parish_match) |>
  View()

uga_parish_list |>
  select(!c(x, y)) |>
  filter(tolower(substr(parish, 1, 1)) == 'a') |> # change a to b, c, etc
  arrange(parish)

# Get misspelled/corrected names from 2018 (a few more were added)
mispelled_parish_names <- c('ABAR WEST', 'ABERIDWOGO', 'ABIRA EAST', 'ACHUNGI', 'ADELLOGO', 'AGURURU', 'ANYOMOREM', 'ATIGOLWOK', 
                            'ADOLLO', 'AMIIABERIDWOGO', 'ANEPKIDI', 'ANGWETANGWET', 'APA', 'ARWOTOMITO',
                            'BARONGER GO DOWN', 'BUJANGA', 'BUMULISYA', 'BUSUNJU TOWN BOARD', 'BUWENGE WEST', 
                            'BARAPWO', 'BISON MAGURIA', 'BUGUSEGE', 'BUKIBETI', 'BUWUNI RURAL', 
                            'CHEGERE', 'DOG APIO', 'IDUDI TOWN BOARD', 'IGGWE', 'INDUSTRIAL', 'IRIAGA', 
                            'CAMPSWAHILI', 'DIMA', 'GGULU', 'GWENGDIYA', 'IREDA WEST', 
                            'KAKOGE WARD', 'KALIRO', 'KASHOZI EAST', 'KASHOZI WEST', 'KATANGA WARD', 'KIRYOKYA', 
                            'KAMOR', 'KASIMERI', 'KIHUUBA', 'KIJJUMBA', 'KIJUGUTA', 'KIJJUMBA', 'KIJUMBA WEST', 'KIJUNA', 'KIMAANYA', 'KIRAARO', 'KITOROGYA',
                            'KOTIDO RURAL', 'KYEBANDO I ', 'KYENGERA TOWN BOARD', 
                            'LUBYA', 'MATUGGA', 'NAMAGABI', 'NAMPAOGWE', 'NANTABULIRIRWA', 'NJERU WEST WARD', 'NTENJERU',
                            'LOMERUMA', 'LOSILANG PARISH', 'LTOJO', 'LWABAKOBA', 'MADI KILOC', 'MAKERERE UNIVERSITY', 'MBIRIIZI', 'MITUKULA', 'MUGARUSTYA', 'MUTUNGO I',
                            'NAKULABYE I', 'NAMTHINI', 'NANKULABYE I', 'NINDYE', 'NSUUBE KAUGA', 'NTAAWO', 'NTAYIGIRWA',  'NYAMASA PARISH', 'NYARWIMUKA', 
                            'OKAPI', 'OMACH', 'PUGWENYI', 'RAGEM LOWER', 'RUHIRA', 'VILLAMARIA', 
                            
                            'NYAKASANGA I WARD', 'OLD CAMP-SWAHILI', 'OMOLADYANG', 'ORUPU', 'PECE PRISON', 
                            'TEGWANA', 'VANGUARD', 'WESTLAND B', # add for the other years and save it
                            'KYOGA', 'MITUKULA', 'NICU PARISH',
                            
                            'ADONYIMO', 'AWINDIRI WARD', 'AYA-YIA', 'BAZAAR WARD', 'BIJAABA PARISH', 'BUGUMBA WARD', 'BULIIGO WARD', 'BULOWOOZA',
                            'BUSOWA RURAL', 'BUSUNJU WARD', 'BUWENGE SOUTHWARD', 'BUWENGE WESTWARD', 'BUWUNI TB', 'INDUSRIAL AREA', 'KAKWOKO', 'KANGONDO', 
                            'KASENSERO TOWN BOARD', 'KIRONGO  WARD', 'KIRYOKA', 'KISEKENDE WARD', 'KISOJJO WARD', 'KISUURA PARISH', 'KOTIDO CENTRAL WARD', 
                            'KOTIDO EAST WARD', 'KYALIWAJJALA', 'KYAMULIBWA TOWN  BOARD', 'KYEGERA', 'KYONGERA', 'LOBUNEIT WARD', 'MADI- KILOC', 'MALUKHU WARD',
                            'MASESE WARD', 'MATEETE CENTRAL', 'MPONDWE WARD ', 'MUSUUBIRO', 'NABIRUMBA I', 'NALUWERERE WARD', 'NAMTHIN WARD', 'NAMUSALE',
                            'NAMUTUMBA CENTRAL WARD', 'NANSANA EAST WARD', 'NANSANA WEST WARD', 'NANTABULIRIRWA WARD', 'NAWAPANDA', 'NKUNGULUNTALE', 
                            'NSUUBE KAUGA WARD', 'NTAWO WARD', 'NYAKARONGO PARISH', 'NYAKASANGA II WARD', 'NYANTSIMBO WARD', 'NYARUBUNGO II', 'SWAHILI CHIN',
                            'OSUGURO WARD', 'RUTOMA', 'SOUTH CENTRAL WARD', 'TOROMA TB' )  

corrected_parish_names <-c('Abar', 'Aberidwogo-Omera', 'Abira', 'Acungi', 'Adelogo', 'Agururu A Ward', 'Anyomerem', 'Atigo-Lwok', 
                           'Padolo', 'Amii', 'Anepkide', 'Angwecibange', 'Appa', 'Arwot- Omito',
                           'Bar Onger', 'Bujaga', 'Bumulisha', 'Busunju', 'Buwenge West Ward', 
                           'Bar Apwo', 'Bison-Maguria Ward', 'Bugube', 'Bukiabi',  'Buwuni',
                           'Cegere', 'Dogapio', 'Idudi', 'Igwe', 'Industrial Ward', 'Iriaga Ward',
                           'Kampswahili', 'Diima', 'GGulu Ward', 'Gwendiya', 'Ireda-West', 
                           'Kakoge', 'Kaliiro', 'Kashozi', 'Kashozi', 'Katanga/Township', 'Kiryookya',
                           'Kamoru', 'Kathimeri', 'Kihuba', 'Kijumba', 'Kijuguta Ward', 'Kijumba', 'Kijumba', 'Kijjuna','Kimanya', 'Kiraro', 'Kitonya',
                           'Kotido Central', 'Kyebando', 'Kyengera', 
                           'Lubia', 'Matuga', 'Namagabi Ward', 'Nampongwe', 'Nantabulirwa', 'Njeru West', 'Ntenjeru Ward', 
                           'Lomerima', 'Losilang', 'Itojo', 'Lwabakooba', 'Madi-Kiloc', 'Makerere Ii', 'Mbirizi', 'Mitukula Ward', 'Mugarutsya', 'Mutungo', 
                           'Nakulabye', 'Namthin', 'Nakulabye', 'Nnindye', 'Nsuube-Kauga', 'Ntaawo Ward', 'Ntaigirwa', 'Nyamasa', 'Nyarwimuka/Kanyamisekuru', 
                           'Ojapi', 'Omac', 'Pugwinyi', 'Ragem', 'Ruhiira', 'Villa-Maria', 
                           
                           'Nyakasanga I', 'Old Campswahili', 'Omolo-Adyang', 'Orupo', 'Pece Prisons Ward', 
                           'Tegwana Ward', 'Vanguard Ward', 'Westland', # add for the other years and save it
                           'Kioga', 'Mitukula Ward', 'Nicu',
                           
                           'Adonyoimo', 'Awindiri', 'Ayayia', 'Bazaar', 'Bijaaba', 'Bugumba', 'Buligo', 'Bulowoza',
                           'Busowa', 'Busunju', 'Buwenge South Ward', 'Buwenge West Ward', 'Buwuni', 'Industrial Area', 'Kakwokwo', 'Kangodo',
                           'Kasensero', 'Kirongo', 'Kiryookya', 'Kisekende', 'Kisojjo', 'Kisuura', 'Kotido Central',
                           'Kotido East', 'Kyaliwajala', 'Kyamulibwa', 'Kyengera', 'Kyogera', 'Lobuneit', 'Madi-Kiloc', 'Malukhu',
                           'Masese', 'Mateete', 'Mpondwe', 'Musubiro', 'Nabirumba', 'Naluwerere', 'Namthin', 'Namusaale', 
                           'Namutumba', 'Nansana', 'Nansana', 'Nantabulirwa', 'Nawampanda', 'Nkungulutale',
                           'Nsuube-Kauga', 'Ntaawo Ward', 'Nyakarongo', 'Nyakasanga Ii', 'Nyantsimbo', 'Nyarubungo', 'Old Campswahili',
                           'Osuguro', 'Rutooma', 'South Central', 'Toroma' )  
parish_name_match <- cbind(lsms = mispelled_parish_names, gadm = corrected_parish_names); rm(mispelled_parish_names, corrected_parish_names)
pnm <- parish_name_match[,'lsms']

parish_name_match <- read_csv('../data/raw/received/Uganda_proposed_matching_names-jvs.csv')
parish_name_match <- parish_name_match |>
  rename(lsms = parish.LSMS, gadm = parish.GADM) |>
  filter(matching == 'yes')
pnm <- parish_name_match[,'lsms']


# Parish names that were misspelled in LSMS (considering GADM as reference)
lost_par_1 <- lost_parishes |> 
  filter(parish_match %in% tolower(pnm)) |>
  inner_join(parish_name_match |>
               as_tibble() |>
               mutate(parish_match = tolower(lsms), parish = tolower(gadm)) ) |>
  select(HHID, parish) |>
  inner_join(uga_parish_list |>
               select(x, y, parish) |>
               mutate(parish = tolower(parish)) ) |>
  distinct(HHID, .keep_all = T)


# Step 2:Using sub-county in LSMS to match with GADM's sub-county
lost_par_2 <- lost_parishes |> 
  filter(!(HHID %in% unique(c(found_parishes$HHID,  lost_par_1$HHID))) ) |>
  select(HHID, district.x, sub_county.x) |>
  mutate(sub_county = tolower(sub_county.x)) |>
  inner_join(uga_sub_county_list |>
               mutate(sub_county = tolower(sub_county))) |>
  select(HHID, x, y, starts_with('distr'), starts_with('county'), starts_with('sub_county') ) |>
  distinct(HHID, .keep_all = T)

# Put together the found parishes, the misspelled ones, and the one for which the sub_county was retrieved
uga_2015_gps <- bind_rows(
  found_parishes |>
    select(HHID, x, y),
  lost_par_1 |>
    select(HHID, x, y),
  lost_par_2 |>
    select(HHID, x, y) ) |>
  distinct(HHID, .keep_all = T) 

cluster_data <- cluster_data |>
  select(HHID, ea) |>
  rename(ea_id = ea) |>
  inner_join(uga_2015_gps  ) |>
  distinct(ea_id, HHID, x, y) 

uga_raw <- inner_join(
  uga_raw,
  cluster_data |>
    mutate(country = 'Uganda', year = 2015,
           farm_id = as.character(HHID),
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(uga_raw, file = '../data/processed/Uganda_2015_raw.csv')
#######################################################################################
# Get LSMS data from Uganda 2013

# in Uganda 2013, GPS coordinates of the Enumeration Areas are not available. Will use s1aq04a in gsec1 (parish names) in combination with GADM.
# in Uganda 2013, household size ==> can be retrieved from  h2q3 (sex) in gsec2
# in Uganda 2013, field number ==> NA
# in Uganda 2013, plot number ==> parcelID in agsec1
# in Uganda 2013, farmer-reported plot area ==>  s2aq5 in agsec2a AND s2aq05 in agsec2b
# in Uganda 2013, farmer-reported plot unit ==> ACRES
# in Uganda 2013, was plot measured with GPS ==>  ???
# in Uganda 2013, GPS measured plot size (ha) ==> s2aq4 in agsec2a  AND  s2aq04 in agsec2b  (plot rented in or access through other user rights)
# in Uganda 2013, crop code ==> can be retrieved through s2aq11a (land-use in season 2 in 2017) and s2aq11b (land-use in season 1 in 2013) in agsec2a AND a2bq12a and a2bq12b in agsec2b
# in Uganda 2013, household ID ==>  is a combination of Final_EA_code (enumeration area), hhid +++ then parcelID, pltid

uga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Uganda_2013', full.names = T)
uga_zip <- uga_fold[grep('STATA8.zip$', uga_fold, ignore.case = T)]
uga_file_list <- unzip(uga_zip, list = T)$Name
uga_sel_files <- uga_file_list[grep('gsec1.|gsec2.|agsec1.|agsec2a.|agsec2b.|csec1a.', uga_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(uga_zip, files = gsub('^./', '', uga_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('/gsec1\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('/gsec2', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('agsec2a\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('agsec2b', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('csec1a', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))
uga_ea_2013_data <- ea_data

plot_data <- full_join(
  plot1_data |>
    select(hh, parcelID, a2aq4, a2aq5, a2aq11a, a2aq11b) |>
    rename(farm_id = hh, plot_id = parcelID,
           reported_area = a2aq5,
           measured_plot_area = a2aq4,
           plot_land_use_1 = a2aq11a, plot_land_use_2 = a2aq11b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_1 = case_when(plot_land_use_1 == 1 ~ 'CULTIVATED',
                                       plot_land_use_1 == 2 ~ 'CULTIVATED',
                                       plot_land_use_1 == 3 ~ 'rented_out',
                                       plot_land_use_1 == 4 ~ 'CULTIVATED',
                                       plot_land_use_1 == 5 ~ 'fallow',
                                       plot_land_use_1 == 6 ~ 'grazing_land',
                                       plot_land_use_1 == 7 ~ 'woodlot',
                                       plot_land_use_1 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_2 = case_when(plot_land_use_2 == 1 ~ 'CULTIVATED',
                                       plot_land_use_2 == 2 ~ 'CULTIVATED',
                                       plot_land_use_2 == 3 ~ 'rented_out',
                                       plot_land_use_2 == 4 ~ 'CULTIVATED',
                                       plot_land_use_2 == 5 ~ 'fallow',
                                       plot_land_use_2 == 6 ~ 'grazing_land',
                                       plot_land_use_2 == 7 ~ 'woodlot',
                                       plot_land_use_2 == 96 ~ 'other_specify',
                                       .default = NA)),
  plot2_data |>
    select(hh, parcelID, a2bq4, a2bq5, a2bq12a, a2bq12b) |>
    rename(farm_id = hh, plot_id = parcelID,
           reported_area = a2bq5,
           measured_plot_area = a2bq4,
           plot_land_use_3 = a2bq12a, plot_land_use_4 = a2bq12b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_3 = case_when(plot_land_use_3 == 1 ~ 'CULTIVATED',
                                       plot_land_use_3 == 2 ~ 'CULTIVATED',
                                       plot_land_use_3 == 3 ~ 'rented_out',
                                       plot_land_use_3 == 5 ~ 'fallow',
                                       plot_land_use_3 == 6 ~ 'grazing_land',
                                       plot_land_use_3 == 7 ~ 'woodlot',
                                       plot_land_use_3 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_4 = case_when(plot_land_use_4 == 1 ~ 'CULTIVATED',
                                       plot_land_use_4 == 2 ~ 'CULTIVATED',
                                       plot_land_use_4 == 3 ~ 'rented_out',
                                       plot_land_use_4 == 5 ~ 'fallow',
                                       plot_land_use_4 == 6 ~ 'grazing_land',
                                       plot_land_use_4 == 7 ~ 'woodlot',
                                       plot_land_use_4 == 96 ~ 'other_specify',
                                       .default = NA)) ) |>
  as_tibble() |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' |
                                     plot_land_use_2 == 'CULTIVATED' |
                                     plot_land_use_3 == 'CULTIVATED' |
                                     plot_land_use_4 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)),
         reported_area_ha = reported_area / 2.47 )


uga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(HHID, h2q3) |>
    filter(!is.na(h2q3)) |> # Sex must be filled in
    mutate(farm_id = as.character(HHID)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    filter(plot_land_use == 'CULTIVATED')
)

# Since GPS coordinates of EA were not given, the centroid of the parish/ward is used instead
# however, approx. 220 parish names were not found in GADM

# calculate centroid positions for parish/wards in GADM
# uga_gadm_level4 <- geodata::gadm("Uganda", level = 4, path = paste0(input_path, '/gadm/Uganda/level4') )
ug4 <- terra::unwrap(readRDS(paste0(input_path, '/gadm/Uganda/level4/gadm/gadm41_UGA_4_pk.rds')))
ug4_centroids <- terra::centroids(ug4)
uga_parish_coords <- terra::crds(ug4_centroids) # as.data.frame(uga_centroids, xy = T) did not work
ug4_admin <- terra::as.data.frame(ug4)
uga_parish_list <- ug4_admin |>
  select(starts_with('NAME_'))
uga_parish_list <- uga_parish_list |>
  bind_cols(uga_parish_coords) |>
  rename(district = NAME_1, county = NAME_2, sub_county = NAME_3, parish = NAME_4) 

# uga_gadm_level3 <- geodata::gadm("Uganda", level = 3, path = paste0(input_path, '/gadm/Uganda/level3') )
ug3 <- terra::unwrap(readRDS(paste0(input_path, '/gadm/Uganda/level3/gadm/gadm41_UGA_3_pk.rds')))
ug3_centroids <- terra::centroids(ug3)
uga_sub_county_coords <- terra::crds(ug3_centroids) # as.data.frame(uga_centroids, xy = T) did not work
ug3_admin <- terra::as.data.frame(ug3)
uga_sub_county_list <- ug3_admin |>
  select(starts_with('NAME_'))
uga_sub_county_list <- uga_sub_county_list |>
  bind_cols(uga_sub_county_coords) |>
  rename(district = NAME_1, county = NAME_2, sub_county = NAME_3) 

uga_lsms_parishes <- cluster_data |>
  select(HHID,  h1aq3b, h1aq4b) |>
  rename(sub_county = h1aq3b, parish = h1aq4b) |>
  mutate(parish_match = tolower(parish)) |>
  left_join(uga_parish_list |>
              mutate(parish_match = tolower(parish)),
            by = 'parish_match') |>
  select(HHID, x, y, starts_with('sub_county'), starts_with('parish')) |>
  distinct(HHID, .keep_all = T)

# Parishes that match perfectly across datasets (LSMS vs GADM)
found_parishes <- uga_lsms_parishes |>
  filter(!is.na(x)) |>
  distinct(HHID, .keep_all = T)

# List of parishes with initial mismatch (and save it as lost_parishes_2013 to ease the search in other waves)
lost_parishes <- uga_lsms_parishes |>
  filter(is.na(x) | is.na(y) )
lost_parishes_2013 <- lost_parishes |>
  filter(!(parish_match %in% c(lost_parishes_2019$parish_match, lost_parishes_2018$parish_match, lost_parishes_2015$parish_match)))

# step 1.1: read through 'check_list' and identify potential misspelling
list1 <- sort(unique(tolower(lost_parishes_2013$parish_match)))
list2 <- sort(unique(tolower(uga_parish_list$parish)))

distance_matrix <- stringdist::stringdistmatrix(list1, list2, method = 'jw')
rownames(distance_matrix) <- list1

colnames(distance_matrix) <- list2

check_list <- one_row <- data.frame()
for(i  in seq_along(list1)){
  ii <- distance_matrix[i,]
  j <- min(ii[ii > 0], na.rm = T)
  ifelse(j > 0 & j < 0.2, one_row <- cbind.data.frame(lsms = list1[i], gadm = names(ii[ii == j ])), h <- 0) # for looser match, please increase the upper limit of j,
  check_list <- bind_rows(check_list, one_row)
}
check_list <- check_list |>
  filter(lsms != gadm) |>
  distinct() 

check_list_lsms <- inner_join(
  check_list,
  lost_parishes |>
    select(ends_with('.x'), parish_match) |>
    rename(lsms = parish_match) ) |>
  distinct()

check_list_gadm <- inner_join(
  check_list,
  uga_parish_list |>
    select(!c(x, y)) |>
    rename(district.y = district, county.y = county, sub_county.y = sub_county, gadm = parish) |>
    mutate (gadm = tolower(gadm))) |>
  distinct()

check_list_2013 <- check_list_lsms |>
  inner_join(check_list_gadm) |>
  select(starts_with('dist'), starts_with('county'), starts_with('sub_county'), lsms, gadm) |>
  mutate(lsms = toupper(lsms)) |>
  distinct(.keep_all = T)
print(check_list_2013) #enlarge your left panel to see the entire table in a nice format. Look the whole line and search for matches between x and y (may not be the same level). Apply some intuition!

# step 1.2: to get the misspelled names, manually check across these 2 datasets (and fill in the misspelled/correct_names pairs below)
lost_parishes |>
  distinct(parish_match, .keep_all = T) |>
  arrange(parish_match) |>
  View()

uga_parish_list |>
  select(!c(x, y)) |>
  filter(tolower(substr(parish, 1, 1)) == 'a') |> # change a to b, c, etc
  arrange(parish)

# Get misspelled/corrected names from 2018 (a few more were added)
mispelled_parish_names <- c('ABAR WEST', 'ABERIDWOGO', 'ABIRA EAST', 'ACHUNGI', 'ADELLOGO', 'AGURURU', 'ANYOMOREM', 'ATIGOLWOK', 
                            'ADOLLO', 'AMIIABERIDWOGO', 'ANEPKIDI', 'ANGWETANGWET', 'APA', 'ARWOTOMITO',
                            'BARONGER GO DOWN', 'BUJANGA', 'BUMULISYA', 'BUSUNJU TOWN BOARD', 'BUWENGE WEST', 
                            'BARAPWO', 'BISON MAGURIA', 'BUGUSEGE', 'BUKIBETI', 'BUWUNI RURAL', 
                            'CHEGERE', 'DOG APIO', 'IDUDI TOWN BOARD', 'IGGWE', 'INDUSTRIAL', 'IRIAGA', 
                            'CAMPSWAHILI', 'DIMA', 'GGULU', 'GWENGDIYA', 'IREDA WEST', 
                            'KAKOGE WARD', 'KALIRO', 'KASHOZI EAST', 'KASHOZI WEST', 'KATANGA WARD', 'KIRYOKYA', 
                            'KAMOR', 'KASIMERI', 'KIHUUBA', 'KIJJUMBA', 'KIJUGUTA', 'KIJJUMBA', 'KIJUMBA WEST', 'KIJUNA', 'KIMAANYA', 'KIRAARO', 'KITOROGYA',
                            'KOTIDO RURAL', 'KYEBANDO I ', 'KYENGERA TOWN BOARD', 
                            'LUBYA', 'MATUGGA', 'NAMAGABI', 'NAMPAOGWE', 'NANTABULIRIRWA', 'NJERU WEST WARD', 'NTENJERU',
                            'LOMERUMA', 'LOSILANG PARISH', 'LTOJO', 'LWABAKOBA', 'MADI KILOC', 'MAKERERE UNIVERSITY', 'MBIRIIZI', 'MITUKULA', 'MUGARUSTYA', 'MUTUNGO I',
                            'NAKULABYE I', 'NAMTHINI', 'NANKULABYE I', 'NINDYE', 'NSUUBE KAUGA', 'NTAAWO', 'NTAYIGIRWA',  'NYAMASA PARISH', 'NYARWIMUKA', 
                            'OKAPI', 'OMACH', 'PUGWENYI', 'RAGEM LOWER', 'RUHIRA', 'VILLAMARIA', 
                            
                            'NYAKASANGA I WARD', 'OLD CAMP-SWAHILI', 'OMOLADYANG', 'ORUPU', 'PECE PRISON', 
                            'TEGWANA', 'VANGUARD', 'WESTLAND B', # add for the other years and save it
                            'KYOGA', 'MITUKULA', 'NICU PARISH',
                            
                            'ADONYIMO', 'AWINDIRI WARD', 'AYA-YIA', 'BAZAAR WARD', 'BIJAABA PARISH', 'BUGUMBA WARD', 'BULIIGO WARD', 'BULOWOOZA',
                            'BUSOWA RURAL', 'BUSUNJU WARD', 'BUWENGE SOUTHWARD', 'BUWENGE WESTWARD', 'BUWUNI TB', 'INDUSRIAL AREA', 'KAKWOKO', 'KANGONDO', 
                            'KASENSERO TOWN BOARD', 'KIRONGO  WARD', 'KIRYOKA', 'KISEKENDE WARD', 'KISOJJO WARD', 'KISUURA PARISH', 'KOTIDO CENTRAL WARD', 
                            'KOTIDO EAST WARD', 'KYALIWAJJALA', 'KYAMULIBWA TOWN  BOARD', 'KYEGERA', 'KYONGERA', 'LOBUNEIT WARD', 'MADI- KILOC', 'MALUKHU WARD',
                            'MASESE WARD', 'MATEETE CENTRAL', 'MPONDWE WARD ', 'MUSUUBIRO', 'NABIRUMBA I', 'NALUWERERE WARD', 'NAMTHIN WARD', 'NAMUSALE',
                            'NAMUTUMBA CENTRAL WARD', 'NANSANA EAST WARD', 'NANSANA WEST WARD', 'NANTABULIRIRWA WARD', 'NAWAPANDA', 'NKUNGULUNTALE', 
                            'NSUUBE KAUGA WARD', 'NTAWO WARD', 'NYAKARONGO PARISH', 'NYAKASANGA II WARD', 'NYANTSIMBO WARD', 'NYARUBUNGO II', 'SWAHILI CHIN',
                            'OSUGURO WARD', 'RUTOMA', 'SOUTH CENTRAL WARD', 'TOROMA TB',
                            
                            'ATYAK', 'BUMUDU', 'KAMYOKYA I', 'KANYUM WARD', 'KASENYI -CALTEX  WARD', 'KAYUNGA-LUBONA', 'KYABAZALA', 'LABOURLINE WARD', 
                            'MALENGA WARD', 'NAKAZZADDE WARD', 'NEW CAMP  SWAHILI JUU', 'NORTH CENTRAL', 'OCOKICAN' )

corrected_parish_names <-c('Abar', 'Aberidwogo-Omera', 'Abira', 'Acungi', 'Adelogo', 'Agururu A Ward', 'Anyomerem', 'Atigo-Lwok', 
                           'Padolo', 'Amii', 'Anepkide', 'Angwecibange', 'Appa', 'Arwot- Omito',
                           'Bar Onger', 'Bujaga', 'Bumulisha', 'Busunju', 'Buwenge West Ward', 
                           'Bar Apwo', 'Bison-Maguria Ward', 'Bugube', 'Bukiabi',  'Buwuni',
                           'Cegere', 'Dogapio', 'Idudi', 'Igwe', 'Industrial Ward', 'Iriaga Ward',
                           'Kampswahili', 'Diima', 'GGulu Ward', 'Gwendiya', 'Ireda-West', 
                           'Kakoge', 'Kaliiro', 'Kashozi', 'Kashozi', 'Katanga/Township', 'Kiryookya',
                           'Kamoru', 'Kathimeri', 'Kihuba', 'Kijumba', 'Kijuguta Ward', 'Kijumba', 'Kijumba', 'Kijjuna','Kimanya', 'Kiraro', 'Kitonya',
                           'Kotido Central', 'Kyebando', 'Kyengera', 
                           'Lubia', 'Matuga', 'Namagabi Ward', 'Nampongwe', 'Nantabulirwa', 'Njeru West', 'Ntenjeru Ward', 
                           'Lomerima', 'Losilang', 'Itojo', 'Lwabakooba', 'Madi-Kiloc', 'Makerere Ii', 'Mbirizi', 'Mitukula Ward', 'Mugarutsya', 'Mutungo', 
                           'Nakulabye', 'Namthin', 'Nakulabye', 'Nnindye', 'Nsuube-Kauga', 'Ntaawo Ward', 'Ntaigirwa', 'Nyamasa', 'Nyarwimuka/Kanyamisekuru', 
                           'Ojapi', 'Omac', 'Pugwinyi', 'Ragem', 'Ruhiira', 'Villa-Maria', 
                           
                           'Nyakasanga I', 'Old Campswahili', 'Omolo-Adyang', 'Orupo', 'Pece Prisons Ward', 
                           'Tegwana Ward', 'Vanguard Ward', 'Westland', # add for the other years and save it
                           'Kioga', 'Mitukula Ward', 'Nicu',
                           
                           'Adonyoimo', 'Awindiri', 'Ayayia', 'Bazaar', 'Bijaaba', 'Bugumba', 'Buligo', 'Bulowoza',
                           'Busowa', 'Busunju', 'Buwenge South Ward', 'Buwenge West Ward', 'Buwuni', 'Industrial Area', 'Kakwokwo', 'Kangodo',
                           'Kasensero', 'Kirongo', 'Kiryookya', 'Kisekende', 'Kisojjo', 'Kisuura', 'Kotido Central',
                           'Kotido East', 'Kyaliwajala', 'Kyamulibwa', 'Kyengera', 'Kyogera', 'Lobuneit', 'Madi-Kiloc', 'Malukhu',
                           'Masese', 'Mateete', 'Mpondwe', 'Musubiro', 'Nabirumba', 'Naluwerere', 'Namthin', 'Namusaale', 
                           'Namutumba', 'Nansana', 'Nansana', 'Nantabulirwa', 'Nawampanda', 'Nkungulutale',
                           'Nsuube-Kauga', 'Ntaawo Ward', 'Nyakarongo', 'Nyakasanga Ii', 'Nyantsimbo', 'Nyarubungo', 'Old Campswahili',
                           'Osuguro', 'Rutooma', 'South Central', 'Toroma',
                           
                           'Atiak', 'Bummudu', 'Kamwokya I', 'Kanyum', 'Kasenyi-Caltex', 'Kayunga', 'Kyabazaala', 'Labour Line Ward', 
                           'Malenga', 'Nakazadde', 'New Campswahili', 'North Central Ward', 'Ochokican')  
parish_name_match <- cbind(lsms = mispelled_parish_names, gadm = corrected_parish_names); rm(mispelled_parish_names, corrected_parish_names)
pnm <- parish_name_match[,'lsms']

parish_name_match <- read_csv('../data/raw/received/Uganda_proposed_matching_names-jvs.csv')
parish_name_match <- parish_name_match |>
  rename(lsms = parish.LSMS, gadm = parish.GADM) |>
  filter(matching == 'yes')
pnm <- parish_name_match[,'lsms']

# Parish names that were misspelled in LSMS (considering GADM as reference)
lost_par_1 <- lost_parishes |> 
  filter(parish_match %in% tolower(pnm)) |>
  inner_join(parish_name_match |>
               as_tibble() |>
               mutate(parish_match = tolower(lsms), parish = tolower(gadm)) ) |>
  select(HHID, parish) |>
  inner_join(uga_parish_list |>
               select(x, y, parish) |>
               mutate(parish = tolower(parish)) ) |>
  distinct(HHID, .keep_all = T)

# Step 2:Using sub-county in LSMS to match with GADM's sub-county
lost_par_2 <- lost_parishes |> 
  filter(!(HHID %in% unique(c(found_parishes$HHID,  lost_par_1$HHID))) ) |>
  select(HHID, sub_county.x) |>
  mutate(sub_county = tolower(sub_county.x)) |>
  inner_join(uga_sub_county_list |>
               mutate(sub_county = tolower(sub_county))) |>
  select(HHID, x, y, starts_with('county'), starts_with('sub_county') ) |>
  distinct(HHID, .keep_all = T)

# Put together the found parishes, the misspelled ones, and the one for which the sub_county was retrieved
uga_2013_gps <- bind_rows(
  found_parishes |>
    select(HHID, x, y),
  lost_par_1 |>
    select(HHID, x, y),
  lost_par_2 |>
    select(HHID, x, y) ) |>
  distinct(HHID, .keep_all = T) 

cluster_data <- cluster_data |>
  select(HHID, ea) |>
  rename(ea_id = ea) |>
  inner_join(uga_2013_gps  ) |>
  distinct(ea_id, HHID, x, y) 

uga_raw <- inner_join(
  uga_raw,
  cluster_data |>
    mutate(country = 'Uganda', year = 2013,
           farm_id = as.character(HHID),
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(uga_raw, file = '../data/processed/Uganda_2013_raw.csv')
#######################################################################################
# Get LSMS data from Uganda 2011

uga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Uganda_2011', full.names = T)
uga_zip <- uga_fold[grep('Stata.zip$', uga_fold, ignore.case = T)]
uga_file_list <- unzip(uga_zip, list = T)$Name
uga_sel_files <- uga_file_list[grep('gsec1.|gsec2.|agsec1.|agsec2a.|agsec2b.|csec1.', uga_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(uga_zip, files = gsub('^./', '', uga_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('^gsec1\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('^gsec2\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('agsec2a\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('agsec2b', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('csec1\\.', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

plot_data <- full_join(
  plot1_data |>
    select(HHID, parcelID, a2aq4, a2aq5, a2aq11a, a2aq11b) |>
    rename(farm_id = HHID, plot_id = parcelID,
           reported_area = a2aq5,
           measured_plot_area = a2aq4,
           plot_land_use_1 = a2aq11a, plot_land_use_2 = a2aq11b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_1 = case_when(plot_land_use_1 == 1 ~ 'CULTIVATED',
                                       plot_land_use_1 == 2 ~ 'CULTIVATED',
                                       plot_land_use_1 == 3 ~ 'rented_out',
                                       plot_land_use_1 == 4 ~ 'CULTIVATED',
                                       plot_land_use_1 == 5 ~ 'fallow',
                                       plot_land_use_1 == 6 ~ 'grazing_land',
                                       plot_land_use_1 == 7 ~ 'woodlot',
                                       plot_land_use_1 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_2 = case_when(plot_land_use_2 == 1 ~ 'CULTIVATED',
                                       plot_land_use_2 == 2 ~ 'CULTIVATED',
                                       plot_land_use_2 == 3 ~ 'rented_out',
                                       plot_land_use_2 == 4 ~ 'CULTIVATED',
                                       plot_land_use_2 == 5 ~ 'fallow',
                                       plot_land_use_2 == 6 ~ 'grazing_land',
                                       plot_land_use_2 == 7 ~ 'woodlot',
                                       plot_land_use_2 == 96 ~ 'other_specify',
                                       .default = NA)),
  plot2_data |>
    select(HHID, parcelID, a2bq4, a2bq5, a2bq12a, a2bq12b) |>
    rename(farm_id = HHID, plot_id = parcelID,
           reported_area = a2bq5,
           measured_plot_area = a2bq4,
           plot_land_use_3 = a2bq12a, plot_land_use_4 = a2bq12b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_3 = case_when(plot_land_use_3 == 1 ~ 'CULTIVATED',
                                       plot_land_use_3 == 2 ~ 'CULTIVATED',
                                       plot_land_use_3 == 3 ~ 'rented_out',
                                       plot_land_use_3 == 5 ~ 'fallow',
                                       plot_land_use_3 == 6 ~ 'grazing_land',
                                       plot_land_use_3 == 7 ~ 'woodlot',
                                       plot_land_use_3 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_4 = case_when(plot_land_use_4 == 1 ~ 'CULTIVATED',
                                       plot_land_use_4 == 2 ~ 'CULTIVATED',
                                       plot_land_use_4 == 3 ~ 'rented_out',
                                       plot_land_use_4 == 5 ~ 'fallow',
                                       plot_land_use_4 == 6 ~ 'grazing_land',
                                       plot_land_use_4 == 7 ~ 'woodlot',
                                       plot_land_use_4 == 96 ~ 'other_specify',
                                       .default = NA)) ) |>
  as_tibble() |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' |
                                     plot_land_use_2 == 'CULTIVATED' |
                                     plot_land_use_3 == 'CULTIVATED' |
                                     plot_land_use_4 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)),
         reported_area_ha = reported_area / 2.47 )

uga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(HHID, h2q3) |>
    filter(!is.na(h2q3)) |> # Sex must be filled in
    mutate(farm_id = as.character(HHID)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    filter(plot_land_use == 'CULTIVATED')
)

#  GPS coordinates of EA were provided

ea_data <- ea_data |>
  select(comm, c1aq5, starts_with('GPS_'), starts_with('cgps')) |>
  rename(ea_id = c1aq5, ea_2005 = comm) |>
  mutate(facility = ifelse(!is.na(cgpsdlt2), 'service',
                           ifelse(!is.na(cgpsdlt3), 'edu',
                                  ifelse(!is.na(cgpsdlt4), 'health',
                                         ifelse(!is.na(cgpsdlt5), 'works', NA)))),
         dms_lat = case_when(facility == 'service' ~ cgpsdlt2 + cgpsmlt2_min / 60 + cgpsmlt2_sec / 3600,
                             facility == 'edu' ~ cgpsdlt3 + cgpsmlt3_min / 60 + cgpsmlt3_sec / 3600,
                             facility == 'health' ~ cgpsdlt4 + cgpsmlt4_min / 60 + cgpsmlt4_sec / 3600,
                             facility == 'works' ~ cgpsdlt5 + cgpsmlt5_min / 60 + cgpsmlt5_sec / 3600,
                             .default = NA),
         dir_lat = case_when(facility == 'service' ~ cgpsn2,
                             facility == 'edu' ~ cgpsn3,
                             facility == 'health' ~ cgpsn4,
                             facility == 'works' ~ as.integer(cgpsn5),
                             .default = NA),
         x = case_when(facility == 'service' ~ cgpsdlg2 + cgpsmlg2_min / 60 + cgpsmlg2_sec / 3600,
                       facility == 'edu' ~ cgpsdlg3 + cgpsmlg3_min / 60 + cgpsmlg3_sec / 3600,
                       facility == 'health' ~ cgpsdlg4 + cgpsmlg4_min / 60 + cgpsmlg4_sec / 3600,
                       facility == 'works' ~ cgpsdlg5 + cgpsmlg5_min / 60 + cgpsmlg5_sec / 3600,
                       .default = NA),
         y = case_when(dir_lat == 1 ~ dms_lat,
                       dir_lat == 2 ~  - dms_lat,
                       .default = NA)) |>
  select(ea_id, ea_2005, x, y) |>
  na.omit()

ea_data <- ea_data |>
  inner_join(cluster_data |>
               select(HHID, comm) |>
               rename(ea_2005 = comm))

uga_raw <- inner_join(
  uga_raw,
  ea_data |>
    mutate(country = 'Uganda', year = 2011,
           farm_id = as.character(HHID),
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(uga_raw, file = '../data/processed/Uganda_2011_raw.csv')
#######################################################################################
# Get LSMS data from Uganda 2010

uga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Uganda_2010', full.names = T)
uga_zip <- uga_fold[grep('STATA12.zip$', uga_fold, ignore.case = T)]
uga_file_list <- unzip(uga_zip, list = T)$Name
uga_sel_files <- uga_file_list[grep('gsec1.|gsec2.|agsec1.|agsec2a.|agsec2b.|geovars', uga_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(uga_zip, files = gsub('^./', '', uga_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('^gsec1\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('^gsec2\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('agsec2a\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('agsec2b', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('geovars', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

plot_data <- full_join(
  plot1_data |>
    select(HHID, prcid, a2aq4, a2aq5, a2aq13a, a2aq13b) |>
    rename(farm_id = HHID, plot_id = prcid,
           reported_area = a2aq5,
           measured_plot_area = a2aq4,
           plot_land_use_1 = a2aq13a, plot_land_use_2 = a2aq13b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_1 = case_when(plot_land_use_1 == 1 ~ 'CULTIVATED',
                                       plot_land_use_1 == 2 ~ 'CULTIVATED',
                                       plot_land_use_1 == 3 ~ 'rented_out',
                                       plot_land_use_1 == 4 ~ 'CULTIVATED',
                                       plot_land_use_1 == 5 ~ 'fallow',
                                       plot_land_use_1 == 6 ~ 'grazing_land',
                                       plot_land_use_1 == 7 ~ 'woodlot',
                                       plot_land_use_1 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_2 = case_when(plot_land_use_2 == 1 ~ 'CULTIVATED',
                                       plot_land_use_2 == 2 ~ 'CULTIVATED',
                                       plot_land_use_2 == 3 ~ 'rented_out',
                                       plot_land_use_2 == 4 ~ 'CULTIVATED',
                                       plot_land_use_2 == 5 ~ 'fallow',
                                       plot_land_use_2 == 6 ~ 'grazing_land',
                                       plot_land_use_2 == 7 ~ 'woodlot',
                                       plot_land_use_2 == 96 ~ 'other_specify',
                                       .default = NA)),
  plot2_data |>
    select(HHID, prcid, a2bq4, a2bq5, a2bq15a, a2bq15b) |>
    rename(farm_id = HHID, plot_id = prcid,
           reported_area = a2bq5,
           measured_plot_area = a2bq4,
           plot_land_use_3 = a2bq15a, plot_land_use_4 = a2bq15b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_3 = case_when(plot_land_use_3 == 1 ~ 'CULTIVATED',
                                       plot_land_use_3 == 2 ~ 'CULTIVATED',
                                       plot_land_use_3 == 3 ~ 'rented_out',
                                       plot_land_use_3 == 5 ~ 'fallow',
                                       plot_land_use_3 == 6 ~ 'grazing_land',
                                       plot_land_use_3 == 7 ~ 'woodlot',
                                       plot_land_use_3 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_4 = case_when(plot_land_use_4 == 1 ~ 'CULTIVATED',
                                       plot_land_use_4 == 2 ~ 'CULTIVATED',
                                       plot_land_use_4 == 3 ~ 'rented_out',
                                       plot_land_use_4 == 5 ~ 'fallow',
                                       plot_land_use_4 == 6 ~ 'grazing_land',
                                       plot_land_use_4 == 7 ~ 'woodlot',
                                       plot_land_use_4 == 96 ~ 'other_specify',
                                       .default = NA)) ) |>
  as_tibble() |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' |
                                     plot_land_use_2 == 'CULTIVATED' |
                                     plot_land_use_3 == 'CULTIVATED' |
                                     plot_land_use_4 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)),
         reported_area_ha = reported_area / 2.47 )

uga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(HHID, h2q3) |>
    filter(!is.na(h2q3)) |> # Sex must be filled in
    mutate(farm_id = as.character(HHID)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    filter(plot_land_use == 'CULTIVATED')
)

#  GPS coordinates of EA ???

ea_data <- ea_data |>
  select(HHID, lat_mod, lon_mod) |>
  rename(x = lon_mod, y = lat_mod) |>
  inner_join(cluster_data |>
               select(HHID, comm) |>
               rename(ea_id = comm))

uga_raw <- inner_join(
  uga_raw,
  ea_data |>
    mutate(country = 'Uganda', year = 2010,
           farm_id = as.character(HHID),
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(uga_raw, file = '../data/processed/Uganda_2010_raw.csv')
#######################################################################################
# Get LSMS data from Uganda 2009

uga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Uganda_2009', full.names = T)
uga_zip <- uga_fold[grep('STATA8.zip$', uga_fold, ignore.case = T)]
uga_file_list <- unzip(uga_zip, list = T)$Name
uga_sel_files <- uga_file_list[grep('2009_gsec1.|2009_gsec2.|2009_agsec1.|2009_agsec2a.|2009_agsec2b.|2009_UNPS_geovars', uga_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(uga_zip, files = gsub('^./', '', uga_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('_gsec1\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('_gsec2\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('agsec2a\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('agsec2b', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('geovars', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_2))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

plot_data <- full_join(
  plot1_data |>
    select(Hhid, A2aq2, A2aq4, A2aq5, A2aq13a, A2aq13b) |>
    rename(farm_id = Hhid, plot_id = A2aq2,
           reported_area = A2aq5,
           measured_plot_area = A2aq4,
           plot_land_use_1 = A2aq13a, plot_land_use_2 = A2aq13b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_1 = case_when(plot_land_use_1 == 1 ~ 'CULTIVATED',
                                       plot_land_use_1 == 2 ~ 'CULTIVATED',
                                       plot_land_use_1 == 3 ~ 'rented_out',
                                       plot_land_use_1 == 4 ~ 'CULTIVATED',
                                       plot_land_use_1 == 5 ~ 'fallow',
                                       plot_land_use_1 == 6 ~ 'grazing_land',
                                       plot_land_use_1 == 7 ~ 'woodlot',
                                       plot_land_use_1 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_2 = case_when(plot_land_use_2 == 1 ~ 'CULTIVATED',
                                       plot_land_use_2 == 2 ~ 'CULTIVATED',
                                       plot_land_use_2 == 3 ~ 'rented_out',
                                       plot_land_use_2 == 4 ~ 'CULTIVATED',
                                       plot_land_use_2 == 5 ~ 'fallow',
                                       plot_land_use_2 == 6 ~ 'grazing_land',
                                       plot_land_use_2 == 7 ~ 'woodlot',
                                       plot_land_use_2 == 96 ~ 'other_specify',
                                       .default = NA)),
  plot2_data |>
    select(Hhid, A2bq2, A2bq4, A2bq5, A2bq15a, A2bq15b) |>
    rename(farm_id = Hhid, plot_id = A2bq2,
           reported_area = A2bq5,
           measured_plot_area = A2bq4,
           plot_land_use_3 = A2bq15a, plot_land_use_4 = A2bq15b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_3 = case_when(plot_land_use_3 == 1 ~ 'CULTIVATED',
                                       plot_land_use_3 == 2 ~ 'CULTIVATED',
                                       plot_land_use_3 == 3 ~ 'rented_out',
                                       plot_land_use_3 == 5 ~ 'fallow',
                                       plot_land_use_3 == 6 ~ 'grazing_land',
                                       plot_land_use_3 == 7 ~ 'woodlot',
                                       plot_land_use_3 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_4 = case_when(plot_land_use_4 == 1 ~ 'CULTIVATED',
                                       plot_land_use_4 == 2 ~ 'CULTIVATED',
                                       plot_land_use_4 == 3 ~ 'rented_out',
                                       plot_land_use_4 == 5 ~ 'fallow',
                                       plot_land_use_4 == 6 ~ 'grazing_land',
                                       plot_land_use_4 == 7 ~ 'woodlot',
                                       plot_land_use_4 == 96 ~ 'other_specify',
                                       .default = NA)) ) |>
  as_tibble() |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' |
                                     plot_land_use_2 == 'CULTIVATED' |
                                     plot_land_use_3 == 'CULTIVATED' |
                                     plot_land_use_4 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)),
         reported_area_ha = reported_area / 2.47 )

uga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(HHID, h2q3) |>
    filter(!is.na(h2q3)) |> # Sex must be filled in
    mutate(farm_id = as.character(HHID)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    filter(plot_land_use == 'CULTIVATED')
)

#  GPS coordinates of EA
ea_data <- ea_data |>
  select(HHID, lat_mod, lon_mod) |>
  rename(x = lon_mod, y = lat_mod) |>
  inner_join(cluster_data |>
               select(HHID, comm) |>
               rename(ea_id = comm))

uga_raw <- inner_join(
  uga_raw,
  ea_data |>
    mutate(country = 'Uganda', year = 2009,
           farm_id = as.character(HHID),
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

uga_ea_2009_data <- ea_data; rm(ea_data)

write_csv(uga_raw, file = '../data/processed/Uganda_2009_raw.csv')
#######################################################################################
# Get LSMS data from Uganda 2005

uga_fold <- dir('../data/raw/web_scrapped/survey_data/LSMS_Uganda_2009', full.names = T)
uga_zip <- uga_fold[grep('STATA8.zip$', uga_fold, ignore.case = T)]
uga_file_list <- unzip(uga_zip, list = T)$Name
uga_sel_files <- uga_file_list[grep('2005_gsec1.|2005_gsec2.|2005_agsec1.|2005_agsec2a.|2005_agsec2b.|2009_UNPS_geovars', uga_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(uga_zip, files = gsub('^./', '', uga_sel_files), exdir = temporary_dir)  #It did not work

household_roster_1 <- dir(temporary_dir, recursive = T)[grep('_gsec1\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
household_roster_2 <- dir(temporary_dir, recursive = T)[grep('_gsec2\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_1 <- dir(temporary_dir, recursive = T)[grep('agsec2a\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster_2 <- dir(temporary_dir, recursive = T)[grep('agsec2b\\.', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('geovars', dir(temporary_dir, recursive = T), ignore.case = T)]

cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
plot1_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_1))
plot2_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster_2))

plot_data <- full_join(
  plot1_data |>
    select(Hhid, A2aq2, A2aq4, A2aq5, A2aq13a, A2aq13b) |>
    rename(farm_id = Hhid, plot_id = A2aq2,
           reported_area = A2aq5,
           measured_plot_area = A2aq4,
           plot_land_use_1 = A2aq13a, plot_land_use_2 = A2aq13b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_1 = case_when(plot_land_use_1 == 1 ~ 'CULTIVATED',
                                       plot_land_use_1 == 2 ~ 'CULTIVATED',
                                       plot_land_use_1 == 3 ~ 'rented_out',
                                       plot_land_use_1 == 4 ~ 'CULTIVATED',
                                       plot_land_use_1 == 5 ~ 'fallow',
                                       plot_land_use_1 == 6 ~ 'grazing_land',
                                       plot_land_use_1 == 7 ~ 'woodlot',
                                       plot_land_use_1 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_2 = case_when(plot_land_use_2 == 1 ~ 'CULTIVATED',
                                       plot_land_use_2 == 2 ~ 'CULTIVATED',
                                       plot_land_use_2 == 3 ~ 'rented_out',
                                       plot_land_use_2 == 4 ~ 'CULTIVATED',
                                       plot_land_use_2 == 5 ~ 'fallow',
                                       plot_land_use_2 == 6 ~ 'grazing_land',
                                       plot_land_use_2 == 7 ~ 'woodlot',
                                       plot_land_use_2 == 96 ~ 'other_specify',
                                       .default = NA)),
  plot2_data |>
    select(Hhid, A2bq2, A2bq4, A2bq5, A2bq15a, A2bq15b) |>
    rename(farm_id = Hhid, plot_id = A2bq2,
           reported_area = A2bq5,
           measured_plot_area = A2bq4,
           plot_land_use_3 = A2bq15a, plot_land_use_4 = A2bq15b) |>
    mutate(farm_id = as.character(farm_id),
           field_id = paste0(farm_id, '_X'),
           plot_id = paste0(field_id, '_',  sprintf('%02s', plot_id)),
           report_unit = 'acres',
           measured_plot = NA,
           measured_plot_area_ha = round(measured_plot_area / 2.47, 2),
           plot_land_use_3 = case_when(plot_land_use_3 == 1 ~ 'CULTIVATED',
                                       plot_land_use_3 == 2 ~ 'CULTIVATED',
                                       plot_land_use_3 == 3 ~ 'rented_out',
                                       plot_land_use_3 == 5 ~ 'fallow',
                                       plot_land_use_3 == 6 ~ 'grazing_land',
                                       plot_land_use_3 == 7 ~ 'woodlot',
                                       plot_land_use_3 == 96 ~ 'other_specify',
                                       .default = NA),
           plot_land_use_4 = case_when(plot_land_use_4 == 1 ~ 'CULTIVATED',
                                       plot_land_use_4 == 2 ~ 'CULTIVATED',
                                       plot_land_use_4 == 3 ~ 'rented_out',
                                       plot_land_use_4 == 5 ~ 'fallow',
                                       plot_land_use_4 == 6 ~ 'grazing_land',
                                       plot_land_use_4 == 7 ~ 'woodlot',
                                       plot_land_use_4 == 96 ~ 'other_specify',
                                       .default = NA)) ) |>
  as_tibble() |>
  filter(!(is.na(reported_area) & is.na(measured_plot_area))) |>
  mutate(plot_land_use = case_when(plot_land_use_1 == 'CULTIVATED' |
                                     plot_land_use_2 == 'CULTIVATED' |
                                     plot_land_use_3 == 'CULTIVATED' |
                                     plot_land_use_4 == 'CULTIVATED' ~ 'CULTIVATED',
                                   .default = paste0(plot_land_use_1, '_', plot_land_use_2, '_', plot_land_use_3)),
         reported_area_ha = reported_area / 2.47 )

uga_raw <- inner_join(
  hh_data |>
    as_tibble() |>
    select(HHID, h2q4) |>
    filter(!is.na(h2q4)) |> # Sex must be filled in
    mutate(farm_id = as.character(HHID)) |>
    group_by(farm_id) |>
    summarise(hh_size = n() ),
  plot_data |>
    filter(plot_land_use == 'CULTIVATED')
)

#  GPS coordinates of EA retrieved from 2009 survey
ea_data <- uga_ea_2009_data |>
  select(ea_id, x, y) |>
  inner_join(cluster_data |>
               select(Hhid, Comm) |>
               rename(ea_id = Comm))

uga_raw <- inner_join(
  uga_raw,
  ea_data |>
    mutate(country = 'Uganda', year = 2005,
           farm_id = as.character(Hhid),
           ea_id = as.character(ea_id)) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(uga_raw, file = '../data/processed/Uganda_2005_raw.csv')
#######################################################################
# Get LSMS data from west-African french-speaking countries 2018

# GPS coordinates of the Enumeration Area  ==> coordonnes_gps_Longitude and coordonnes_gps_Latitude in file grappe_gps_****
# Household size can be derived from ==> s01q01 (sex) in s01_me_****
# Field_ID ==>  s16aq02 in file s16a_me_*****   AND    s16cq02 in file s16a_me_*****
# Plot_ID  ==> s16aq03 in file s16a_me_*****    AND    s16cq03 in file s16a_me_*****
# Farmer-reported plot area  ==> s16aq09a in file s16a_me_***** 
# Farmer-reported plot unit  ==> s16aq09b in file s16a_me_***** 
# Was the plot measured with GPS  ==> s16aq45 in file s16a_me_*****  
# GPS measured plot size (ha)  ==> s16aq47 in file s16a_me_****
# Crop code ==> s16cq04 in file s16c_me_***** (for plot land use)
# Household ID ==> is a combination of vague, grappe, menage across files, all in s00_me_***
# EA is supposed to be grappe variable across files

# A function to extract farm size from LSMS in West Africa 
# the function was made in stata and csv because there were discreappancies between STATA and CSV zip
# check Benin 2021  and Burkina  2021
extract_farm_sizes_stata <- function(country_year){
  new_fold <- dir('../data/raw/web_scrapped/survey_data', full.names = T)[grep(country_year, dir('../data/raw/web_scrapped/survey_data', full.names = T), ignore.case = T)]
  my_cty_zip <- dir(new_fold)[grep('stata.*\\.zip$', dir(new_fold), ignore.case = T)]
  my_cty_zip <- paste0(new_fold, '/', my_cty_zip)
  my_file_list <- unzip(my_cty_zip, list =T)$Name
  temporary_dir <- '../data/processed/temporary'
  if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
  dir.create(temporary_dir)
  unzip(my_cty_zip, files = basename(my_file_list[grep('s00_me_|s01_me_|s16a_me_|s16c_me_|grappe_gps', my_file_list)]), exdir = temporary_dir)
  
  household_roster_1 <- dir(temporary_dir, recursive = T)[grep('s00_me_', dir(temporary_dir, recursive = T), ignore.case = T)]
  household_roster_2 <- dir(temporary_dir, recursive = T)[grep('s01_me_', dir(temporary_dir, recursive = T), ignore.case = T)]
  plot_roster <- dir(temporary_dir, recursive = T)[grep('s16a_me_', dir(temporary_dir, recursive = T), ignore.case = T)]
  crop_roster <- dir(temporary_dir, recursive = T)[grep('s16c_me_', dir(temporary_dir, recursive = T), ignore.case = T)]
  ea_characteristics <- dir(temporary_dir, recursive = T)[grep('grappe_gps', dir(temporary_dir, recursive = T), ignore.case = T)]
  
  cluster_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_1))
  hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster_2))
  plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
  crop_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster))
  
  ifelse(length(ea_characteristics) == 0, {
    ea_data <- read_csv(paste0('../data/processed/temp_west_af/ea_data_', substr(country_year, 1, nchar(country_year) - 4), '2018.csv'))
  },{
    ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))
    write_csv(ea_data, file = paste0('../data/processed/temp_west_af/ea_data_', country_year, '.csv'))
  })
  my_cty_raw <- hh_data |>
    select(vague, grappe, menage, s01q01) |>
    filter(!is.na(s01q01)) |> # Sex must be filled in
    mutate(ea_id = as.character(grappe), 
           farm_id = as.character(paste0(sprintf('%04g', grappe), '_', vague, '_', sprintf('%04g', menage)))) |>
    group_by(ea_id, farm_id) |>
    summarise(hh_size = n() ) |>
    inner_join(
      crop_data |>
        select(vague, grappe, menage, s16cq02, s16cq03, s16cq04) |>
        rename(field_id = s16cq02, plot_id = s16cq03, crop_code = s16cq04) |>
        mutate(farm_id = as.character(paste0(sprintf('%04g', grappe), '_', vague, '_', sprintf('%04g', menage))),
               field_id = paste0(farm_id, '_X'), 
               plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
               plot_land_use = case_when(is.na(crop_code) ~ NA,
                                         .default = 'CULTIVATED') ) |>
        filter(plot_land_use == 'CULTIVATED') ) |>
    inner_join(
      plot_data |>
        select(vague, grappe, menage, s16aq02, s16aq03, s16aq09a, s16aq09b, s16aq45, s16aq47) |>
        rename(field_id = s16aq02, plot_id = s16aq03,
               reported_area = s16aq09a, report_unit = s16aq09b, measured_plot = s16aq45, measured_plot_area = s16aq47) |>
        mutate(ea_id = as.character(grappe), 
               farm_id = as.character(paste0(sprintf('%04g', grappe), '_', vague, '_', sprintf('%04g', menage))),
               field_id = paste0(farm_id, '_X'), 
               plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
               report_unit = case_when(report_unit == 1 ~ 'ha',
                                       report_unit == 2 ~ 'sq_meter',
                                       .default = NA),
               measured_plot = case_when(measured_plot == 1 ~ 'GPS-measured',
                                         measured_plot == 2 ~ 'not measured',
                                         .default = NA),
               reported_area_ha = case_when(report_unit == 'ha' ~ reported_area,
                                            report_unit == 'sq_meter' ~ reported_area / 10000),
               measured_plot_area_ha = round(measured_plot_area, 4))
    )
  
  ea_data <- ea_data |>
    select(grappe, vague, coordonnes_gps__Longitude, coordonnes_gps__Latitude) |>
    rename( x = coordonnes_gps__Longitude, y = coordonnes_gps__Latitude) |>
    mutate(ea_id = as.character(grappe) ) |>
    inner_join(
      cluster_data |>
        select(vague, grappe, menage) |>
        mutate(ea_id = as.character(grappe), 
               farm_id = as.character(paste0(sprintf('%04g', grappe), '_', vague, '_', sprintf('%04g', menage)))) |>
        group_by(ea_id, farm_id) ) |>
    ungroup() |>
    select(ea_id, farm_id, x, y) |>
    distinct(farm_id, .keep_all = T)
  
  my_cty_raw <- inner_join(
    my_cty_raw,
    ea_data |>
      select(ea_id, farm_id, x, y) |>
      group_by(farm_id) |>
      distinct() |>
      mutate(country = substr(country_year, 1, nchar(country_year) - 5),
             year = substr(country_year, nchar(country_year) - 3, nchar(country_year)),
             farm_id = as.character(farm_id)) )  |>
    ungroup() |>
    mutate(ea_id = as.character(ea_id)) |>
    select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
           reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)
  my_cty_raw$ea_id <- as.character(my_cty_raw$ea_id)
  write_csv(my_cty_raw, file = paste0('../data/processed/', country_year,'_raw.csv'))
  
  unlink('../data/processed/temporary', recursive = T)
}
extract_farm_sizes_csv <- function(country_year){
  new_fold <- dir('../data/raw/web_scrapped/survey_data', full.names = T)[grep(country_year, dir('../data/raw/web_scrapped/survey_data', full.names = T), ignore.case = T)]
  my_cty_zip <- dir(new_fold)[grep('csv\\.zip$', dir(new_fold), ignore.case = T)]
  my_cty_zip <- paste0(new_fold, '/', my_cty_zip)
  my_file_list <- unzip(my_cty_zip, list =T)$Name
  temporary_dir <- '../data/processed/temporary'
  if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
  dir.create(temporary_dir)
  unzip(my_cty_zip, files = basename(my_file_list[grep('s00_me_|s01_me_|s16a_me_|s16c_me_|grappe_gps', my_file_list)]), exdir = temporary_dir)
  
  household_roster_1 <- dir(temporary_dir, recursive = T)[grep('s00_me_', dir(temporary_dir, recursive = T), ignore.case = T)]
  household_roster_2 <- dir(temporary_dir, recursive = T)[grep('s01_me_', dir(temporary_dir, recursive = T), ignore.case = T)]
  plot_roster <- dir(temporary_dir, recursive = T)[grep('s16a_me_', dir(temporary_dir, recursive = T), ignore.case = T)]
  crop_roster <- dir(temporary_dir, recursive = T)[grep('s16c_me_', dir(temporary_dir, recursive = T), ignore.case = T)]
  ea_characteristics <- dir(temporary_dir, recursive = T)[grep('grappe_gps', dir(temporary_dir, recursive = T), ignore.case = T)]
  
  cluster_data <- read_csv(paste0(temporary_dir, '/', household_roster_1))
  hh_data <- read_csv(paste0(temporary_dir, '/', household_roster_2))
  plot_data <- read_csv(paste0(temporary_dir, '/', plot_roster))
  crop_data <- read_csv(paste0(temporary_dir, '/', crop_roster))
  
  ifelse(length(ea_characteristics) == 0, {
    ea_data <- read_csv(paste0('../data/processed/temp_west_af/ea_data_', substr(country_year, 1, nchar(country_year) - 4), '2018.csv'))
  },{
    ea_data <- read_csv(paste0(temporary_dir, '/', ea_characteristics))
    write_csv(ea_data, file = paste0('../data/processed/temp_west_af/ea_data_', country_year, '.csv'))
  })
  my_cty_raw <- hh_data |>
    select(vague, grappe, menage, s01q01) |>
    filter(!is.na(s01q01)) |> # Sex must be filled in
    mutate(ea_id = as.character(grappe), 
           farm_id = as.character(paste0(sprintf('%04g', grappe), '_', vague, '_', sprintf('%04g', menage)))) |>
    group_by(ea_id, farm_id) |>
    summarise(hh_size = n() ) |>
    inner_join(
      crop_data |>
        select(vague, grappe, menage, s16cq02, s16cq03, s16cq04) |>
        rename(field_id = s16cq02, plot_id = s16cq03, crop_code = s16cq04) |>
        mutate(farm_id = as.character(paste0(sprintf('%04g', grappe), '_', vague, '_', sprintf('%04g', menage))),
               field_id = paste0(farm_id, '_X'), 
               plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
               plot_land_use = case_when(is.na(crop_code) ~ NA,
                                         .default = 'CULTIVATED') ) |>
        filter(plot_land_use == 'CULTIVATED') ) |>
    inner_join(
      plot_data |>
        select(vague, grappe, menage, s16aq02, s16aq03, s16aq09a, s16aq09b, s16aq45, s16aq47) |>
        rename(field_id = s16aq02, plot_id = s16aq03,
               reported_area = s16aq09a, report_unit = s16aq09b, measured_plot = s16aq45, measured_plot_area = s16aq47) |>
        mutate(ea_id = as.character(grappe), 
               farm_id = as.character(paste0(sprintf('%04g', grappe), '_', vague, '_', sprintf('%04g', menage))),
               field_id = paste0(farm_id, '_X'), 
               plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
               report_unit = case_when(report_unit == 1 ~ 'ha',
                                       report_unit == 2 ~ 'sq_meter',
                                       .default = NA),
               measured_plot = case_when(measured_plot == 1 ~ 'GPS-measured',
                                         measured_plot == 2 ~ 'not measured',
                                         .default = NA),
               reported_area_ha = case_when(report_unit == 'ha' ~ reported_area,
                                            report_unit == 'sq_meter' ~ reported_area / 10000),
               measured_plot_area_ha = round(measured_plot_area, 4))
    )
  
  ea_data <- ea_data |>
    select(grappe, vague, coordonnes_gps__Longitude, coordonnes_gps__Latitude) |>
    rename( x = coordonnes_gps__Longitude, y = coordonnes_gps__Latitude) |>
    mutate(ea_id = as.character(grappe) ) |>
    inner_join(
      cluster_data |>
        select(vague, grappe, menage) |>
        mutate(ea_id = as.character(grappe), 
               farm_id = as.character(paste0(sprintf('%04g', grappe), '_', vague, '_', sprintf('%04g', menage)))) |>
        group_by(ea_id, farm_id) ) |>
    ungroup() |>
    select(ea_id, farm_id, x, y) |>
    distinct(farm_id, .keep_all = T)
  
  my_cty_raw <- inner_join(
    my_cty_raw,
    ea_data |>
      select(ea_id, farm_id, x, y) |>
      group_by(farm_id) |>
      distinct() |>
      mutate(country = substr(country_year, 1, nchar(country_year) - 5),
             year = substr(country_year, nchar(country_year) - 3, nchar(country_year)),
             farm_id = as.character(farm_id)) )  |>
    ungroup() |>
    mutate(ea_id = as.character(ea_id)) |>
    select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
           reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)
  my_cty_raw$ea_id <- as.character(my_cty_raw$ea_id)
  write_csv(my_cty_raw, file = paste0('../data/processed/', country_year,'_raw.csv'))
  
  unlink('../data/processed/temporary', recursive = T)
}

temp_west_af <- '../data/processed/temp_west_af'
if(dir.exists(temp_west_af)) unlink(temp_west_af, recursive = T)
dir.create(temp_west_af)

west_af_cty <- expand.grid(cty = c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Guinea_Bissau' , 'Niger', 'Mali', 'Senegal', 'Togo'), yr = c('_2018', '_2021'))
west_af_countries <- with(west_af_cty, paste0(cty, yr)); rm(west_af_cty)
west_af_countries_minus_bfa_2021 <- west_af_countries [west_af_countries != 'Burkina_2021']
sapply(west_af_countries_minus_bfa_2021, extract_farm_sizes_stata)
extract_farm_sizes_csv('Burkina_2021')
#######################################################################
# Get LSMS data from Burkina 2014

# household details are available in emc2014_p1_individu_27022015
# plot size available in emc2014_agri_gps  
# plot land use  and other plot details are available in emc2014_agri_caracteristiques_parcelles
# EA do not have GPS coordinates, will try with 2018 EAs

bfa_fold <- dir('../data/raw/web_scrapped/survey_data', full.names = T)[grep('Burkina_2014', dir('../data/raw/web_scrapped/survey_data', full.names = T), ignore.case = T)]
bfa_zip <- dir(bfa_fold)[grep('STATA8.zip$', dir(bfa_fold))]
bfa_zip <- paste0(bfa_fold, '/', bfa_zip)
bfa_file_list <- unzip(bfa_zip, list =T)$Name
bfa_sel_files <- bfa_file_list[grep('emc2014_p1_individu_27022015|emc2014_agri_gps|emc2014_agri_caracteristiques_parcelles', bfa_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(bfa_zip, files = gsub('^./', '', bfa_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('emc2014_p1_individu_27022015', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('emc2014_agri_gps', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster <- dir(temporary_dir, recursive = T)[grep('emc2014_agri_caracteristiques_parcelles', dir(temporary_dir, recursive = T), ignore.case = T)]

hh_data <- haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster))

bfa_raw <- hh_data |>
  select(zd, menage, B2) |>
  filter(!is.na(B2)) |> # Sex must be filled in
  mutate(ea_id = as.character(zd),
         farm_id = as.character(paste0(sprintf('%04g', zd), '_', sprintf('%04g', menage)))) |>
  group_by(ea_id, farm_id) |>
  summarise(hh_size = n() ) |>
  inner_join(
    crop_data |>
      select(zd, menage, V00, V02, V04, V05B, V06) |>
      rename(field_id = V00, plot_id = V02, plot_land_use = V04, crop_code = V05B, 
             reported_area = V06) |>
      mutate(farm_id = as.character(paste0(sprintf('%04g', zd), '_', sprintf('%04g', menage))),
             field_id = paste0(farm_id, sprintf('%04g', field_id)),
             plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
             plot_land_use = case_when(plot_land_use == 0 ~ 'Uncultivated',
                                       plot_land_use != 0 ~ 'CULTIVATED',
                                       .default = NA),
             report_unit = 'unspecified_ha_as_default',
             reported_area_ha = reported_area ) |>   # based on histogram of reported-area
      filter(plot_land_use == 'CULTIVATED') ) |>
  inner_join(
    plot_data |>
      select(zd, menage, V00, V02, V20, V22) |>
      rename(field_id = V00, plot_id = V02,
             measured_plot = V20, measured_plot_area = V22) |>
      mutate(ea_id = as.character(zd),
             farm_id = as.character(paste0(sprintf('%04g', zd), '_', sprintf('%04g', menage))),
             field_id = paste0(farm_id, sprintf('%04g', field_id)),
             plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
             measured_plot = case_when(measured_plot == 1 ~ 'GPS-measured',
                                       measured_plot == 2 ~ 'not measured',
                                       .default = NA),
             measured_plot_area_ha = round(measured_plot_area, 2) )
  )

# Getting EA IDs from the 2018 survey, based on the assumption that the coding did not change over years, only 585 EAs were captured
# the remaining ~ 300 were not retrieved from GADM because only level 2 was provided in the LSMS (too coarse)
bfa_ea_data <- read_csv(dir('../data/processed/temp_west_af', full.names = T)[grep('Burkina', dir('../data/processed/temp_west_af'), ignore.case = T)])

ea_data <- bfa_ea_data |>
  select(grappe, vague, coordonnes_gps__Longitude, coordonnes_gps__Latitude) |>
  rename( x = coordonnes_gps__Longitude, y = coordonnes_gps__Latitude) |>
  mutate(ea_id = as.character(grappe) ) |>
  select(ea_id, x, y)

bfa_raw <- inner_join(
  bfa_raw,
  ea_data |>
    mutate(country = 'Burkina',
           year = 2014 ) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(bfa_raw, file = paste0('../data/processed/', 'Burkina_2014','_raw.csv'))
#######################################################################
# Get LSMS data from Niger 2014

# HH size given as MS00Q22 in ECVMA2_0P2
# plot details in ECVMA2_AS1P1
ner_fold <- dir('../data/raw/web_scrapped/survey_data', full.names = T)[grep('Niger_2014', dir('../data/raw/web_scrapped/survey_data', full.names = T), ignore.case = T)]
ner_zip <- dir(ner_fold)[grep('STATA8.zip$', dir(ner_fold))]
ner_zip <- paste0(ner_fold, '/', ner_zip)
ner_file_list <- unzip(ner_zip, list =T)$Name
ner_sel_files <- ner_file_list[grep('ECVMA2_0P2|ECVMA2_AS1P1', ner_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(ner_zip, files = gsub('^./', '', ner_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('ECVMA2_0P2', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('ECVMA2_AS1P1', dir(temporary_dir, recursive = T), ignore.case = T)]

hh_data <-haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))

ner_raw <- hh_data |>
  select(GRAPPE, MENAGE, EXTENSION, MS00Q22) |>
  filter(!is.na(MS00Q22)) |> # HH_SIZE must be filled in
  mutate(ea_id = as.character(GRAPPE),
         farm_id = as.character(paste0(sprintf('%04g', GRAPPE), '_', sprintf('%04g', MENAGE), '_', sprintf('%02g', EXTENSION)))) |>
  group_by(ea_id, farm_id) |>
  summarise(hh_size = MS00Q22) |>
  inner_join(
    plot_data |>
      select(GRAPPE, MENAGE, EXTENSION, AS01Q01, AS01Q03, AS01Q06, AS01Q07, AS01Q11, AS01Q38) |>
      rename(field_id = AS01Q01, plot_id = AS01Q03,
             reported_area = AS01Q06, measured_plot = AS01Q11, 
             measured_plot_area = AS01Q07, plot_land_use  = AS01Q38) |>
      mutate(farm_id = as.character(paste0(sprintf('%04g', GRAPPE), '_', sprintf('%04g', MENAGE), '_', sprintf('%02g', EXTENSION))),
             field_id = paste0(farm_id, '_X'),
             plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
             plot_land_use = case_when(plot_land_use == 1 ~ 'CULTIVATED',
                                       plot_land_use == 2 ~ 'Uncultivated',
                                       .default = NA),
             reported_area = case_when(reported_area == 999999 ~ NA,
                                       reported_area == 99 ~ NA,
                                       .default = reported_area),
             report_unit = 'sq_meter',
             reported_area_ha = reported_area / 10000,
             measured_plot = case_when(measured_plot == 1 ~ 'GPS-measured',
                                       measured_plot == 2 ~ 'not measured',
                                       .default = NA),
             measured_plot_area_ha = round(measured_plot_area, 4)) |>
      filter(plot_land_use == 'CULTIVATED') ) 

# based on the assumption that EA coding was maintained from 2011 to 2014
ner_ea_data <- read_csv(dir('../data/processed/temp_west_af', full.names = T)[grep('Niger', dir('../data/processed/temp_west_af'), ignore.case = T)])

ea_data <- ner_ea_data |>
  select(grappe, vague, coordonnes_gps__Longitude, coordonnes_gps__Latitude) |>
  rename( x = coordonnes_gps__Longitude, y = coordonnes_gps__Latitude) |>
  mutate(ea_id = as.character(grappe) ) |>
  select(ea_id, x, y)

ner_raw <- inner_join(
  ner_raw,
  ea_data |>
    mutate(country = 'Niger',
           year = 2014) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(ner_raw, file = paste0('../data/processed/', 'Niger_2014','_raw.csv'))
#######################################################################
# Get LSMS data from Mali 2014

# EA GPS in eaci_geovariables_2014
# HH size given as    in EACIIND_p1
# plot details in     EACIEXPLOI_p1

mli_fold <- dir('../data/raw/web_scrapped/survey_data', full.names = T)[grep('Mali_2014', dir('../data/raw/web_scrapped/survey_data', full.names = T), ignore.case = T)]
mli_zip <- dir(mli_fold)[grep('STATA11.zip$', dir(mli_fold))]
mli_zip <- paste0(mli_fold, '/', mli_zip)
mli_file_list <- unzip(mli_zip, list =T)$Name
mli_sel_files <- mli_file_list[grep('EACIIND_p1|EACIEXPLOI_p1|eaci_geovariables_2014', mli_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(mli_zip, files = gsub('^./', '', mli_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('EACIIND_p1', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('EACIEXPLOI_p1', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('eaci_geovariables_2014', dir(temporary_dir, recursive = T), ignore.case = T)]

hh_data <-haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

mli_raw <- hh_data |>
  select(grappe, menage, extension, s01q01) |>
  filter(!is.na(s01q01)) |> # sex must be filled in
  mutate(ea_id = as.character(grappe),
         farm_id = as.character(paste0(sprintf('%04g', grappe), '_', sprintf('%04g', menage), '_', sprintf('%02g', extension)))) |>
  group_by(ea_id, farm_id) |>
  summarise(hh_size = n()) |>
  inner_join(
    plot_data |>
      select(grappe, menage, extension, s1bq01, s1bq02, s1bq05a, s1bq06, s1bq08b, s1bq10) |>
      rename(field_id = s1bq01, plot_id = s1bq02,
             reported_area = s1bq10, measured_plot = s1bq06, 
             measured_plot_area = s1bq05a, crop_code = s1bq08b) |>
      mutate(farm_id = as.character(paste0(sprintf('%04g', grappe), '_', sprintf('%04g', menage), '_', sprintf('%02g', extension))),
             field_id = paste0(farm_id, field_id),
             plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
             plot_land_use = case_when(crop_code != 999 ~ 'CULTIVATED',
                                       crop_code == 999 ~ 'Uncultivated',
                                       .default = NA),
             reported_area = case_when(reported_area == 999999 ~ NA,
                                       reported_area == 99 ~ NA,
                                       .default = reported_area),
             report_unit = 'ha',
             reported_area_ha = reported_area,
             measured_plot = case_when(measured_plot == 1 ~ 'GPS-measured',
                                       measured_plot == 2 ~ 'not measured',
                                       .default = NA),
             measured_plot_area_ha = round(measured_plot_area, 4)) |>
      filter(plot_land_use == 'CULTIVATED') ) 

ea_data <-ea_data |>
  select(grappe, lon_dd_mod, lat_dd_mod) |>
  rename(ea_id = grappe, x = lon_dd_mod, y = lat_dd_mod) |>
  mutate(ea_id = as.character(ea_id))

mli_raw <- inner_join(
  mli_raw,
  ea_data |>
    mutate(country = 'Mali',
           year = 2014) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(mli_raw, file = paste0('../data/processed/', 'Mali_2014','_raw.csv'))
#######################################################################
# Get LSMS data from Mali 2017

# EA GPS in eaci_geovariables_2017
# Sex given as    in eaci17_s01p1
# plot details in     eaci17_s11bp1
# crop code as  in    eaci17_s11cp1

mli_fold <- dir('../data/raw/web_scrapped/survey_data', full.names = T)[grep('Mali_2017', dir('../data/raw/web_scrapped/survey_data', full.names = T), ignore.case = T)]
mli_zip <- dir(mli_fold)[grep('STATA.zip$', dir(mli_fold))]
mli_zip <- paste0(mli_fold, '/', mli_zip)
mli_file_list <- unzip(mli_zip, list =T)$Name
mli_sel_files <- mli_file_list[grep('eaci17_s01p1|eaci17_s11bp1|eaci17_s11cp1|eaci_geovariables_2017', mli_file_list, ignore.case = T)]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(mli_zip, files = gsub('^./', '', mli_sel_files), exdir = temporary_dir)

household_roster <- dir(temporary_dir, recursive = T)[grep('eaci17_s01p1', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T)[grep('eaci17_s11bp1', dir(temporary_dir, recursive = T), ignore.case = T)]
crop_roster <- dir(temporary_dir, recursive = T)[grep('eaci17_s11cp1', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T)[grep('eaci_geovariables_2017', dir(temporary_dir, recursive = T), ignore.case = T)]

hh_data <-haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
crop_data <- haven::read_dta(paste0(temporary_dir, '/', crop_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))

mli_raw <- hh_data |>
  select(grappe, exploitation, s1q01) |>
  filter(!is.na(s1q01)) |> # sex must be filled in
  mutate(ea_id = as.character(grappe),
         farm_id = as.character(paste0(sprintf('%04g', grappe), '_', sprintf('%04g', exploitation)))) |>
  group_by(ea_id, farm_id) |>
  summarise(hh_size = n()) |>
  inner_join(
    plot_data |>
      select(grappe, exploitation, s11bq01, s11bq02, s11bq03, s11bq04, s11bq07, s11bq11a) |>
      rename(field_id = s11bq01, plot_id = s11bq02,
             reported_area = s11bq11a, measured_plot = s11bq04, 
             measured_plot_area = s11bq07, crop_code = s11bq03) |>
      mutate(farm_id = as.character(paste0(sprintf('%04g', grappe), '_', sprintf('%04g', exploitation))),
             field_id = paste0(farm_id, field_id),
             plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
             plot_land_use = case_when(crop_code != 999 ~ 'CULTIVATED',
                                       crop_code == 999 ~ 'Uncultivated',
                                       .default = NA),
             reported_area = case_when(reported_area == 999999 ~ NA,
                                       reported_area == 99 ~ NA,
                                       .default = reported_area),
             report_unit = 'ha',
             reported_area_ha = reported_area,
             measured_plot = case_when(measured_plot == 1 ~ 'GPS-measured',
                                       measured_plot == 2 ~ 'not measured',
                                       .default = NA),
             measured_plot_area_ha = round(measured_plot_area, 4)) |>
      filter(plot_land_use == 'CULTIVATED') ) 

ea_data <-ea_data |>
  select(grappe, lon_dd_mod, lat_dd_mod) |>
  rename(ea_id = grappe, x = lon_dd_mod, y = lat_dd_mod) |>
  mutate(ea_id = as.character(ea_id))

mli_raw <- inner_join(
  mli_raw,
  ea_data |>
    mutate(country = 'Mali',
           year = 2017) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(mli_raw, file = paste0('../data/processed/', 'Mali_2017','_raw.csv'))
#######################################################################
# Get LSMS data from Ghana 2017

# EA GPS in eaci_geovariables_2012
# Sex given as    in eaci17_s01p1
# plot details in     eaci17_s11bp1
# crop code as  in    eaci17_s11cp1

gha_fold1 <- dir('../data/raw/web_scrapped/survey_data', full.names = T)[grep('Ghana_2012', dir('../data/raw/web_scrapped/survey_data', full.names = T), ignore.case = T)]
gha_zip <- dir(gha_fold1, full.names = T)[grep('\\.zip$', dir(gha_fold1))]
temporary_dir <- '../data/processed/temporary'
if(dir.exists(temporary_dir)) unlink(temporary_dir, recursive = T)
dir.create(temporary_dir)
unzip(gha_zip, exdir = temporary_dir)
gha_fold2 <- dir(temporary_dir, recursive = T, full.names = T)[grep('STATA\\.zip$', dir(temporary_dir, recursive = T))]
unzip(gha_fold2, exdir = temporary_dir)

# sapply(dir(temporary_dir, recursive = T,)[grep('STATA\\/SECTION 10\\/sec10_filters\\.dta', dir(temporary_dir, recursive = T), ignore.case = T)], function(x) {
#   ea_data <- haven::read_dta(paste0(temporary_dir, '/', x))
#   print(paste0('--------------- ', x, '--------------------------'))
#   sapply(ea_data, function(y) print(attr(y, 'label')))
#   View(ea_data)
# 
# })
# 'STATA\\/AGGREGATES\\/GHA_2013_H\\.dta', 'STATA\\/AGGREGATES\\/00_GHA_BASICINFO.dta', 
household_roster <- dir(temporary_dir, recursive = T,)[grep('STATA\\/PARTA\\/SEC1\\.dta', dir(temporary_dir, recursive = T), ignore.case = T)]
plot_roster <- dir(temporary_dir, recursive = T,)[grep('STATA\\/PARTB\\/sec8b.dta', dir(temporary_dir, recursive = T), ignore.case = T)]
ea_characteristics <- dir(temporary_dir, recursive = T,)[grep('STATA/PARTA/SECA.dta', dir(temporary_dir, recursive = T), ignore.case = T)]
gha_codebook <- dir(temporary_dir, recursive = T)[grep('CODEBOOK\\.pdf$', dir(temporary_dir, recursive = T))]

hh_data <-haven::read_dta(paste0(temporary_dir, '/', household_roster))
plot_data <- haven::read_dta(paste0(temporary_dir, '/', plot_roster))
ea_data <- haven::read_dta(paste0(temporary_dir, '/', ea_characteristics))
gha_messy_codes <- tabulapdf::extract_tables(file = paste0(temporary_dir, '/', gha_codebook), method = 'lattice',
                                             pages = 30:31, output = 'tibble', guess = T)
gha_dist_01 <- gha_messy_codes[[1]] |>
  as_tibble() |>
  rename(districts_1 = ...2,
         codes_1 = ...3,
         districts_2 = ...6,
         codes_2 = ...7) |>
  select(contains('_')) |>
  filter(!districts_1 == 'DISTRICT NAME', !is.na(districts_1)) 

gha_dist_02 <- gha_messy_codes[[2]] |>
  as_tibble() |>
  rename(districts_1 = `DISTRICT NAME...2`,
         codes_1 = `DISTRICT CODE...3`,
         districts_2 = `DISTRICT CODE...6`,
         codes_2 = ...7) |>
  select(contains('_')) |>
  filter(!is.na(districts_1))

gha_districts_codes <- bind_rows(
  gha_dist_01 |>
    select(contains('_1')) |>
    rename(district = districts_1, code = codes_1),
  gha_dist_01 |>
    select(contains('_2')) |>
    rename(district = districts_2, code = codes_2),
  gha_dist_02 |>
    select(contains('_1')) |>
    rename(district = districts_1, code = codes_1),
  gha_dist_02 |>
    select(contains('_2')) |>
    rename(district = districts_2, code = codes_2)
) |>
  filter(!is.na(district)) |>
  add_row(district = 'Nkwanta North', code = '0418') |>
  add_row(district = 'Atiwa', code = '0517') |>
  add_row(district = 'Bawku West', code = '0907') |>
  mutate(region = case_when(
    substr(code, 1, 2) == '01' ~ 'Western',
    substr(code, 1, 2) == '02' ~ 'Central',
    substr(code, 1, 2) == '03' ~ 'Greater Accra',
    substr(code, 1, 2) == '04' ~ 'Volta',
    substr(code, 1, 2) == '05' ~ 'Eastern',
    substr(code, 1, 2) == '06' ~ 'Ashanti',
    substr(code, 1, 2) == '07' ~ 'Ahafo',
    substr(code, 1, 2) == '08' ~ 'Northern',
    substr(code, 1, 2) == '09' ~ 'Upper East',
    substr(code, 1, 2) == '10' ~ 'Upper West'
  ));rm(gha_dist_01, gha_dist_02)

gha_gadm2 <- terra::vect(paste0(input_path, '/gadm/Ghana/gadm/gadm41_GHA_2_pk.rds'))
gha_gadm2 <- bind_cols(
  terra::crds(terra::centroids(gha_gadm2)),
  terra::as.data.frame(gha_gadm2) |>
    select(starts_with('NAME_'))
)

gha_dist_01 <- gha_districts_codes |>
  rename(NAME_2 = district) |>
  inner_join(gha_gadm2)

lsms <- gha_districts_codes |>
  rename(NAME_1 = region, NAME_2 = district) |>
  anti_join(gha_gadm2)

gadm <- gha_gadm2 |>
  anti_join(
    gha_districts_codes |>
      rename(NAME_2 = district)
  )

match_fun_2 <- function(a, b) {
  stringdist::stringdist(a, b, method = 'jw') <= 0.25  # may need to adjust the method and threshold
}
gha_dist_02 <- fuzzyjoin::fuzzy_inner_join(
  lsms, gadm, 
  by = c('NAME_1', 'NAME_2'),
  match_fun = match_fun_2
) |> 
  arrange(NAME_2.x, NAME_2.y)

View(gha_dist_02 )
gha_dist_02 <- gha_dist_02[-c(15, 16, 20, 21, 22, 36, 43, 44, 55, 56, 62, 66, 67, 80),]
gha_dist_02 <- gha_dist_02 |>
  distinct(code, .keep_all = T)

gha_dist_01 <- gha_dist_01 |>
  bind_rows(
    gha_dist_02 |>
      rename(NAME_1 = NAME_1.y, NAME_2 = NAME_2.y) |>
      select(x, y, code, NAME_1, NAME_2)
  ); rm(gha_dist_02)

lsms <- gha_districts_codes |>
  rename(NAME_1 = region, NAME_2 = district) |>
  filter(!code %in% unique(gha_dist_01$code)) |>
  arrange(NAME_2)

other_names <- c('Atebubu', 'Bekwai', 'Berekum', 'Bunkpurugu', 'Dormaa',
                 'Kintampo', 'Kwahu Afram Plains North', 'Pru', 
                 'Sawla', 'Sefwi', 'Sene', 'Suhum', 'Sunyani')
gadm <- gha_gadm2 |>
  filter(grepl(paste0(other_names, collapse = '|'), NAME_2, ignore.case = T))

gha_dist_03 <- fuzzyjoin::fuzzy_inner_join(
  lsms, gadm, 
  by = c('NAME_2'),
  match_fun = match_fun_2
) |> 
  arrange(NAME_2.x, NAME_2.y)

gha_dist_03 <- gha_dist_03[-c(6, 7),]
gha_dist_03 <- gha_dist_03 |>
  distinct(code, .keep_all = T)

gha_dist_01 <- gha_dist_01 |>
  bind_rows(
    gha_dist_03 |>
      rename(NAME_1 = NAME_1.y, NAME_2 = NAME_2.y) |>
      select(x, y, code, NAME_1, NAME_2)
  ); rm(gha_dist_03)
# Add Accra Municipal Area (AMA)
gha_dist_01 <- gha_dist_01 |>
  add_row(
    gha_gadm2 |> 
      filter(NAME_2 == 'Accra'),
    code = '0304' # A M A in LSMS codebook
  ) |>
  select(x, y, code, starts_with('NAME_')) |>
  arrange(NAME_1, NAME_2)

gha_raw <- hh_data |>
  select(grappe, exploitation, s1q01) |>
  filter(!is.na(s1q01)) |> # sex must be filled in
  mutate(ea_id = as.character(grappe),
         farm_id = as.character(paste0(sprintf('%04g', grappe), '_', sprintf('%04g', exploitation)))) |>
  group_by(ea_id, farm_id) |>
  summarise(hh_size = n()) |>
  inner_join(
    plot_data |>
      select(grappe, exploitation, s11bq01, s11bq02, s11bq03, s11bq04, s11bq07, s11bq11a) |>
      rename(field_id = s11bq01, plot_id = s11bq02,
             reported_area = s11bq11a, measured_plot = s11bq04, 
             measured_plot_area = s11bq07, crop_code = s11bq03) |>
      mutate(farm_id = as.character(paste0(sprintf('%04g', grappe), '_', sprintf('%04g', exploitation))),
             field_id = paste0(farm_id, field_id),
             plot_id = paste0(field_id, '_',  sprintf('%02g', plot_id)),
             plot_land_use = case_when(crop_code != 999 ~ 'CULTIVATED',
                                       crop_code == 999 ~ 'Uncultivated',
                                       .default = NA),
             reported_area = case_when(reported_area == 999999 ~ NA,
                                       reported_area == 99 ~ NA,
                                       .default = reported_area),
             report_unit = 'ha',
             reported_area_ha = reported_area,
             measured_plot = case_when(measured_plot == 1 ~ 'GPS-measured',
                                       measured_plot == 2 ~ 'not measured',
                                       .default = NA),
             measured_plot_area_ha = round(measured_plot_area, 4)) |>
      filter(plot_land_use == 'CULTIVATED') ) 

ea_data <-ea_data |>
  select(grappe, lon_dd_mod, lat_dd_mod) |>
  rename(ea_id = grappe, x = lon_dd_mod, y = lat_dd_mod) |>
  mutate(ea_id = as.character(ea_id))

gha_raw <- inner_join(
  gha_raw,
  ea_data |>
    mutate(country = 'Ghana',
           year = 2012) )  |>
  select(x, y, country, year, ea_id, farm_id, hh_size, field_id, plot_id,
         reported_area, report_unit, reported_area_ha, plot_land_use, measured_plot, measured_plot_area_ha)

write_csv(gha_raw, file = paste0('../data/processed/', 'Ghana_2012','_raw.csv'))



unlink(temp_west_af, recursive = T)
unlink(temporary_dir, recursive = T)
###########################################################################################