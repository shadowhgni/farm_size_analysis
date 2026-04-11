# Suppl.Fig.03

# load packages
require(tidyverse)
require(patchwork)

# Clean environment
rm(list=ls())

# # Set working directory
# setwd(paste0(here::here(), '/scripts'))

# ------------------------------------------------------------------------------
# Preparation for functions and mapping
input_path <- 'C:/Users/DHOUGNI/OneDrive - CIMMYT/Documents/Harare 2023/Spatial_data_repository'
country <- geodata::world(path=input_path, resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='Réunion' & NAME!='Saint Helena' & NAME!='São Tomé and Príncipe' & NAME!='Seychelles') # keep the mainland + Madagascar only, remove islands
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
pal1 <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))
pal2 <- colorRampPalette(c('#c6dbef','#6baed6','#3182bd', '#08519c', '#08306b'))
pal3 <- colorRampPalette(c('skyblue1', 'blue4'))
pal4 <- colorRampPalette(c('#A1D99B', '#00441B')) # RColorBrewer::brewer.pal(9,'Greens')
pal5 <- colorRampPalette(c('#FFFFCC', '#800026'))
pal6 <- colorRampPalette(c('#F0F921FF', '#0D0887FF'))
pal7 <- colorRampPalette(c('#C7EAE5', '#01665E'))
pal8 <- viridis::plasma(6)
pal9 <- viridis::mako(10)
six_crop_masks <- terra::rast(paste0(input_path, '/landuse/landuse/all_cropland_mask.tif'))

# force terra to use disk-based processing and 50% of RAM (Use this if R crashes because of limited memory)
terra::terraOptions(memfrac = 0.5, todisk = T)
gc()
# ------------------------------------------------------------------------------
# define the countries for which LSMS data are available
sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# Prepare data rasters: lsms and predictions + virtual list of farm sizes
stacked <- terra::rast('../../data/processed/stacked_rasters_africa.tif')
rf_model_predictions <- terra::rast('../../data/processed/rf_model_predictions_SSA.tif')
names(rf_model_predictions) <- 'pred_farm_area_ha'
qrf_model_predictions <- terra::rast('../../data/processed/qrf_100quantiles_predictions_africa.tif')
names(qrf_model_predictions) <- paste0('qrf_q', sprintf('%03g', 1:100))
mask_forest_ssa <- terra::rast('../../data/processed/mask_forest_ssa.tif')
mask_drylands_ssa <- terra::rast('../../data/processed/mask_drylands_ssa.tif')
# lsms data
# lsms_spatial <- readRDS('../../data/processed/lsms_trimmed_95th_africa.rds') 
lsms_oob <- readRDS('lsms_oob.rds') 
lsms_spatial <- lsms_oob
# # ------------------------------------------------------------------------------
# # keep only variables needed in the models
# lsms_spatial <- lsms_spatial |>
#   select(x, y, country, farm_area_ha, cropland, cattle, pop, cropland_per_capita,
#          sand, slope, temperature, rainfall, maizeyield, market) |>
#   na.omit() 

# ------------------------------------------------------------------------------
# load the SSA country boundaries as well as lower admin boundaries for the 6 countries
# Country admin 

ben_distr <- geodata::gadm('Benin', level=3, path=paste0(input_path,'/gadm/Benin'))
bfa_distr <- geodata::gadm('Burkina Faso', level=3, path=paste0(input_path,'/gadm/Burkina'))
civ_distr <- geodata::gadm('CIV', level=4, path=paste0(input_path,'/gadm/Cote_d_Ivoire'))
eth_distr <- geodata::gadm('Ethiopia', level=3, path=paste0(input_path,'/gadm/Ethiopia'))
gha_distr <- geodata::gadm('Ghana', level=2, path=paste0(input_path,'/gadm/Ghana'))
gnb_distr <- geodata::gadm('GNB', level=2, path=paste0(input_path,'/gadm/Guinea_Bissau'))

mwi_distr <- geodata::gadm('Malawi', level=3, path=paste0(input_path,'/gadm/Malawi'))
mli_distr <- geodata::gadm('Mali', level=4, path=paste0(input_path,'/gadm/Mali'))
ner_distr <- geodata::gadm('Niger', level=3, path=paste0(input_path,'/gadm/Niger'))
nga_distr <- geodata::gadm('Nigeria', level=2, path=paste0(input_path,'/gadm/Nigeria'))
rwa_distr <- geodata::gadm('Rwanda', level=4, path=paste0(input_path,'/gadm/Rwanda'))
sen_distr <- geodata::gadm('Senegal', level=4, path=paste0(input_path,'/gadm/Senegal'))
tza_distr <- geodata::gadm('Tanzania', level=3, path=paste0(input_path,'/gadm/Tanzania'))
tgo_distr <- geodata::gadm('Togo', level=3, path=paste0(input_path,'/gadm/Togo'))
uga_distr <- geodata::gadm('Uganda', level=4, path=paste0(input_path,'/gadm/Uganda'))
zmb_distr <- geodata::gadm('Zambia', level=2, path=paste0(input_path,'/gadm/Zambia'))

sixteen_count_distr <- rbind(ben_distr, bfa_distr, civ_distr, eth_distr, gha_distr, gnb_distr, mwi_distr, mli_distr, ner_distr, nga_distr, rwa_distr, sen_distr,  tza_distr, tgo_distr, uga_distr, zmb_distr)

# Getting rasters from the polygons vectors
sixteen_count_grid <- terra::rast(sixteen_count_distr, nrow = 2000, ncol = 2000)
sixteen_count_rast1 <- terra::rasterize(sixteen_count_distr, sixteen_count_grid, field = c('COUNTRY') )
sixteen_count_rast1 <- terra::resample(sixteen_count_rast1, stacked)
names(sixteen_count_rast1) <- 'NAME_0'
sixteen_count_rast2 <- terra::rasterize(sixteen_count_distr, sixteen_count_grid, field = c('NAME_1') )
sixteen_count_rast2 <- terra::resample(sixteen_count_rast2, stacked)
sixteen_count_rast <- c(sixteen_count_rast1, sixteen_count_rast2)

ssa_grid <- terra::rast(ssa, nrow = 2000, ncol = 2000)
ssa_rast <- terra::rasterize(ssa, ssa_grid, field = 'NAME_0')
ssa_rast <- terra::resample(ssa_rast, stacked)

# OLD CODES WITH ERRONEOUS COUNTRY-GADM MATCHING (some enumeration areas fell outside of the country boundaries)
# rf_pred <- c(sixteen_count_rast2, ssa_rast, rf_model_predictions)
# country_predicted <- terra::as.data.frame(rf_pred) |>
#   rename(country = NAME_0, region = NAME_1)
# 
# summary_country <- country_predicted |>
#   filter(!is.na(pred_farm_area_ha), !is.na(country)) |>
#   group_by(country) |>
#   summarize(mean_pred = mean(pred_farm_area_ha, na.rm = T) ) # Use quantile regression for median and variability
# 
# gadm_predicted_level1 <- country_predicted |>                # GADM level 1
#   mutate(country = case_when(grepl('Burkina', country) ~ 'Burkina',
#                              grepl('Ivoire', country) ~ 'Cote_d_Ivoire',
#                              grepl('Bissau', country) ~ 'Guinea_Bissau',
#                              .default = country) ) |>
#   filter(!is.na(pred_farm_area_ha), !is.na(region), country %in% sixteen_countries) |>
#   group_by(country, region) |>
#   summarize(mean_pred = mean(pred_farm_area_ha, na.rm = T))
# 
# gadm_observed_level1 <- lsms_spatial |>
#   select(country, gadm_1, farm_area_ha)
# 
# gadm_level1 <- gadm_predicted_level1 |>
#   rename(gadm_1 = region) |>
#   inner_join(gadm_observed_level1)
# 
# summary_gadm_1 <- gadm_level1 |>
#   group_by(country, gadm_1) |>
#   summarize(farm_area_ha = mean(farm_area_ha, na.rm = T), mean_pred = unique(mean_pred), n_obs = n())
# 
# summary_sixteen_country <- gadm_level1 |>
#   group_by(country) |>
#   summarize(farm_area_ha = mean(farm_area_ha, na.rm = T), mean_pred = mean(mean_pred, na.rm = T), n_obs = n())
# 
# r2_gadm_1 <- round(with(summary_gadm_1, cor(farm_area_ha, mean_pred)^2), 2)
# r2_country <- round(with(summary_sixteen_country, cor(farm_area_ha, mean_pred)^2), 2)
# 
# saveRDS(list(summary_sixteen_country = summary_sixteen_country, summary_gadm_1 = summary_gadm_1,
#              r2_country = r2_country, r2_gadm_1 = r2_gadm_1), 'suppl_fig_03.rds')



lsms_oob_2 <- lsms_oob |> 
  mutate(in_sample_pred = unname(unlist(terra::extract(rf_model_predictions, 
                                                       lsms_oob |>
                                                         select(x, y))$pred_farm_area_ha )))

xx <- lsms_oob_2 |>
  filter(paste0(country, '.', gadm_0) %in% paste0(sixteen_countries, '.', sixteen_country_codes)) |>
  group_by(country, gadm_0) |>
  reframe(lvl = 'country', across(c(farm_area_ha, oob_pred, in_sample_pred), ~ mean(., na.rm = T))) |>
  reframe(lvl = 'gadm_0',
          rsq_oob = round(cor(farm_area_ha, oob_pred, use = 'complete.obs')^2, 2),
          rsq_in_sample = round(cor(farm_area_ha, in_sample_pred, use = 'complete.obs')^2, 2)) |>
  
  bind_rows(
    lsms_oob_2 |>
      filter(paste0(country, '.', gadm_0) %in% paste0(sixteen_countries, '.', sixteen_country_codes)) |>
      group_by(country, gadm_0, gadm_1) |>
      reframe(lvl = 'country', across(c(farm_area_ha, oob_pred, in_sample_pred), ~ mean(., na.rm = T))) |>
      reframe(lvl = 'gadm_1',
              rsq_oob = round(cor(farm_area_ha, oob_pred, use = 'complete.obs')^2, 2),
              rsq_in_sample = round(cor(farm_area_ha, in_sample_pred, use = 'complete.obs')^2, 2)) ,
    
    lsms_oob_2 |>
      filter(paste0(country, '.', gadm_0) %in% paste0(sixteen_countries, '.', sixteen_country_codes)) |>
      group_by(country, gadm_0, gadm_1, gadm_2) |>
      reframe(lvl = 'country', across(c(farm_area_ha, oob_pred, in_sample_pred), ~ mean(., na.rm = T))) |>
      reframe(lvl = 'gadm_2',
              rsq_oob = round(cor(farm_area_ha, oob_pred, use = 'complete.obs')^2, 2),
              rsq_in_sample = round(cor(farm_area_ha, in_sample_pred, use = 'complete.obs')^2, 2)),
    
    lsms_oob_2 |>
      filter(paste0(country, '.', gadm_0) %in% paste0(sixteen_countries, '.', sixteen_country_codes)) |>
      group_by(country, gadm_0, gadm_1, gadm_2, gadm_3) |>
      reframe(lvl = 'country', across(c(farm_area_ha, oob_pred, in_sample_pred), ~ mean(., na.rm = T))) |>
      reframe(lvl = 'gadm_3',
              rsq_oob = round(cor(farm_area_ha, oob_pred, use = 'complete.obs')^2, 2),
              rsq_in_sample = round(cor(farm_area_ha, in_sample_pred, use = 'complete.obs')^2, 2)),
    
    lsms_oob_2 |>
      filter(paste0(country, '.', gadm_0) %in% paste0(sixteen_countries, '.', sixteen_country_codes)) |>
      group_by(country, gadm_0, gadm_1, gadm_2, gadm_3, gadm_4) |>
      reframe(lvl = 'country', across(c(farm_area_ha, oob_pred, in_sample_pred), ~ mean(., na.rm = T))) |>
      reframe(lvl = 'gadm_4',
              rsq_oob = round(cor(farm_area_ha, oob_pred, use = 'complete.obs')^2, 2),
              rsq_in_sample = round(cor(farm_area_ha, in_sample_pred, use = 'complete.obs')^2, 2)),
    
    lsms_oob_2 |>
      filter(paste0(country, '.', gadm_0) %in% paste0(sixteen_countries, '.', sixteen_country_codes)) |>
      group_by(country, gadm_0, gadm_1, gadm_2, gadm_3, gadm_4, x, y) |>
      reframe(lvl = 'country', across(c(farm_area_ha, oob_pred, in_sample_pred), ~ mean(., na.rm = T))) |>
      reframe(lvl = 'enum_area',
              rsq_oob = round(cor(farm_area_ha, oob_pred, use = 'complete.obs')^2, 2),
              rsq_in_sample = round(cor(farm_area_ha, in_sample_pred, use = 'complete.obs')^2, 2))
  )

yy <- xx |>
  filter(lvl != 'gadm_4') |>
  mutate(lvl = c('GADM 0', 'GADM 1', 'GADM 2', 'GADM 3',  'Enum. area'), #'GADM 4',
         rsq_oob = as.character(rsq_oob),
         rsq_in_sample = as.character(rsq_in_sample),
         across(c(rsq_oob, rsq_in_sample), ~ if_else(. == 1, '> 0.99', .))) |>
  rename(
    Level = lvl,
    'OOB R²' = rsq_oob,
    'in-sample R²' = rsq_in_sample
  ) |>
  as.data.frame()
#-------------------------------------------------------------------------------
pdf('Suppl.Fig.03.pdf', width = 9, height = 4.5)
# par(mfrow=c(1, 2), mar = c(3.5, 3.5, 1, 1), xaxs='i', yaxs='i')
layout(matrix(c(1, 2, 3), nrow = 1, ncol = 3), widths = c(1, 1, 0.85))

# plot A
par(mar = c(5, 4, 4, 2) + 0.1)  # Reset margins for first plot

dat00 <- lsms_oob_2 |>
  group_by(country, gadm_0) |>
  reframe(lvl = 'country', n_obs = n(),
          across(c(farm_area_ha, oob_pred, in_sample_pred), ~ mean(., na.rm = T))) |>
  rename(mean_pred = in_sample_pred) |>
  na.omit() |>
  filter(paste0(country, '.', gadm_0) %in% paste0(sixteen_countries, '.', sixteen_country_codes))

dat00$sampl_size <- ifelse(dat00$n_obs < 5000, '< 5000', NA)
dat00$sampl_size <- ifelse(dat00$n_obs >= 5000 & dat00$n_obs < 10000, '5000-10000', dat00$sampl_size)
dat00$sampl_size <- ifelse(dat00$n_obs >= 10000 & dat00$n_obs < 15000, '10000-15000', dat00$sampl_size)
dat00$sampl_size <- ifelse(dat00$n_obs >= 15000 & dat00$n_obs < 20000, '15000-20000', dat00$sampl_size)
dat00$sampl_size <- ifelse(dat00$n_obs >= 20000 , '> 20000', dat00$sampl_size)
plot(dat00$farm_area_ha, dat00$mean_pred, 
     xlim=c(0, 5.75), ylim=c(0, 5.75), col='white', cex.axis=1.2, cex.lab=1.4, las=0, mgp=c(2,0.75,0), #was cex.lab = 1.4
     xlab = 'Reported average farm size (ha)', ylab = 'Predicted average farm size (ha)' )
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "whitesmoke")
grid(nx=8, ny=8, col='lightgrey')
abline(a = 0, b = 1)
abline(a = 0, b = 2, lty = 2)
abline(a = 0, b = 0.5, lty = 3)

colr <- viridis::viridis(5, alpha=0.7)
i <- 1
for(yr in sort(unique(dat00$sampl_size))){
  sbt <- subset(dat00, sampl_size==yr)
  points(sbt$farm_area_ha, sbt$mean_pred, pch=21, cex=3, col='grey20', bg=colr[i])
  i <- i+1
}
for(cty in c('ETH','ZMB', 'BFA', 'CIV')){ 
  sbt <- subset(dat00, gadm_0==cty)
  text(sbt$farm_area_ha, sbt$mean_pred + 0.06, labels = sbt$gadm_0, col='grey10', pos = 3, cex = 1.2)
}
for(cty in c('RWA', 'MWI', 'NGA','NER', 'MLI')){ 
  sbt <- subset(dat00, gadm_0==cty)
  text(sbt$farm_area_ha, sbt$mean_pred - 0.5, labels = sbt$gadm_0, col='grey10', pos = 3, cex = 1.2)
}

legend('bottomright', bty='n', cex=1.3, title='Sample size', legend=sort(unique(dat00$sampl_size)), pch=21, pt.cex=1.7, col='grey20', pt.bg=colr[1:5])
text(0.3, 5.4, 'A)', cex = 1.5)
text(3.65, 4.9, bquote(R^2 > 0.99), cex=1.5)


#-------------------------------------------------------------------------------
# plot B
par(mar = c(5, 4, 4, 0.5) + 0.1)

dat01 <- lsms_oob_2 |>
  group_by(country, gadm_0, gadm_1) |>
  reframe(lvl = 'country', n_obs = n(),
          across(c(farm_area_ha, oob_pred, in_sample_pred), ~ mean(., na.rm = T))) |>
  rename(mean_pred = in_sample_pred) |>
  na.omit() |>
  filter(paste0(country, '.', gadm_0) %in% paste0(sixteen_countries, '.', sixteen_country_codes)) |>
  inner_join(dat00 |> select(country, gadm_0))
dat01$sampl_size <- ifelse(dat01$n_obs < 200, '< 200', NA)
dat01$sampl_size <- ifelse(dat01$n_obs >= 200 & dat01$n_obs < 500, '200-500', dat01$sampl_size)
dat01$sampl_size <- ifelse(dat01$n_obs >= 500 & dat01$n_obs < 1000, '500-1000', dat01$sampl_size)
dat01$sampl_size <- ifelse(dat01$n_obs >= 1000 & dat01$n_obs < 2000, '1000-2000', dat01$sampl_size)
dat01$sampl_size <- ifelse(dat01$n_obs >= 2000 , '> 2000', dat01$sampl_size)
dat01$sampl_size <- factor(dat01$sampl_size, levels = c('< 200', '200-500', '500-1000', '1000-2000', '> 2000'))
plot(dat01$farm_area_ha, dat01$mean_pred, 
     xlim=c(0, 10.75), ylim=c(0, 10.75), col='white', cex.axis=1.2, cex.lab=1.4, las=0, mgp=c(2,0.75,0), #was cex.lab = 1.4
     xlab = 'Reported average farm size (ha)', ylab = 'Predicted average farm size (ha)' )
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "whitesmoke")
grid(nx=8, ny=8, col='lightgrey')


colr <- viridis::viridis(5, alpha=0.7)
i <- 1
for(yr in sort(unique(dat01$sampl_size))){
  sbt <- subset(dat01, sampl_size==yr)
  points(sbt$farm_area_ha, sbt$mean_pred, pch=21, cex=3, col='grey20', bg=colr[i])
  i <- i+1
}

abline(a = 0, b = 1)
abline(a = 0, b = 2, lty = 2)
abline(a = 0, b = 0.5, lty = 3)

legend('bottomright', bty='n', cex=1.3, title='Sample size', legend=sort(unique(dat01$sampl_size)), pch=21, pt.cex=1.7, col='grey20', pt.bg=colr[1:5])
text(0.6, 10, 'B)', cex=1.5)
text(6, 8.5, bquote(R^2== .(round(xx$rsq_in_sample[xx$lvl == 'gadm_1'], 2))), cex=1.5)


#-------------------------------------------------------------------------------
# Panel 3: Table
par(mar = c(0, 0, 2, 0.5))  # Smaller margins for table
plot(0:10, 0:10, type = "n", xlab = "", ylab = "", axes = FALSE)
text(6, 7.5, "Model performance by \naggregation level", line = 1, cex = 1.4)

plotrix::addtable2plot(
  x = 1, y = 4,
  yy,
  bty = "o",
  hlines = TRUE,
  vlines = TRUE,
  cex = 1.2,
  xjust = 0,
  yjust = 1 
)

dev.off()
