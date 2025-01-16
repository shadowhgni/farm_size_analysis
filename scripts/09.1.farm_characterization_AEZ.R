# FARM AND FARM SYSTEM CHARACTERIZATION based on farm size distribution per grid cell

# load packages
require(tidyverse)

# Clean environment
rm(list=ls())

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
# force terra to use disk-based processing and 50% of RAM (Use this if R crashes because of limited memory)
# terra::terraOptions(memfrac = 0.5, todisk = T)

# ------------------------------------------------------------------------------
#define the countries for which LSMS data are available
sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# Prepare data rasters: lsms and predictions + virtual list of farm sizes
stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')
rf_model_predictions <- terra::rast('../data/processed/rf_model_predictions_SSA.tif')
names(rf_model_predictions) <- 'pred_farm_area_ha'
qrf_model_predictions <- terra::rast('../data/processed/qrf_100quantiles_predictions_africa.tif')
names(qrf_model_predictions) <- paste0('qrf_q', sprintf('%03g', 1:100))
mask_forest_ssa <- terra::rast('../data/processed/mask_forest_ssa.tif')
mask_drylands_ssa <- terra::rast('../data/processed/mask_drylands_ssa.tif')

xx <- readRDS('../data/processed/fsize_distribution_resample_long.rds')
theor_farms <- xx$theor_farms
theor_farms_application <- xx$theor_farms_application; rm(xx)

# Prepare lsms data
lsms_spatial <-  readRDS('../data/processed/lsms_trimmed_95th_africa.rds') # this was retrieved from '03.1.pooled_data_for_analysis.r'


# Get the AEZ_5
aez5 <- terra::rast(paste0(input_path, '/AEZ_SSA_IFPRI/AEZ5_CLAS--SSA.tif'))
names(aez5) <- 'aez_class'
lookup <- data.frame(aez_class = 0:5, aez = c('humid', 'sub-humid', 'semi-arid', 'arid', 'tropical highlands', 'sub-tropical'))
levels(aez5) <- lookup

theor_farms_application <- theor_farms_application |>
  rename(individual_farm_size_ha = linear_farm_size_ha) |>
  bind_cols(terra::extract(aez5, 
                           theor_farms_application |>
                             ungroup() |>
                             select(x, y)))
# quick descriptive stat per AEZ
aez_farm_stat <- theor_farms_application |>
  ungroup() |>
  group_by(aez) |>
  summarize(avg = mean(individual_farm_size_ha, na.rm = T),
            med = median(individual_farm_size_ha, na.rm = T),
            std = sd(individual_farm_size_ha, na.rm = T),
            gini = ineq::Gini(individual_farm_size_ha, na.rm = T),
            tot_cropland = sum(individual_farm_size_ha, na.rm = T),
            nb = n() )
aez_farm_stat

cropland_under_0.5 <- theor_farms_application |>
  filter(individual_farm_size_ha < 0.5) |>
  ungroup() |>
  group_by(aez) |> 
  summarize(nb_0.5 = n(),
            cropland_0.5 = sum(individual_farm_size_ha, na.rm = T)) |>
  inner_join(aez_farm_stat) |>
  mutate(prop_0.5 = cropland_0.5 / tot_cropland)

cropland_under_1 <- theor_farms_application |>
  filter(individual_farm_size_ha < 1) |>
  ungroup() |>
  group_by(aez) |> 
  summarize(nb_1 = n(),
            cropland_1 = sum(individual_farm_size_ha, na.rm = T)) |>
  inner_join(aez_farm_stat) |>
  mutate(prop_1 = cropland_1 / tot_cropland)

cropland_under_2 <- theor_farms_application |>
  filter(individual_farm_size_ha < 2) |>
  ungroup() |>
  group_by(aez) |> 
  summarize(nb_2 = n(),
            cropland_2 = sum(individual_farm_size_ha, na.rm = T)) |>
  inner_join(aez_farm_stat) |>
  mutate(prop_2 = cropland_2 / tot_cropland)

aez_farm_stat <- aez_farm_stat |>
  inner_join(cropland_under_0.5) |>
  inner_join(cropland_under_1) |>
  inner_join(cropland_under_2)

saveRDS(aez_farm_stat, file = '../output/tables/cropland_stats_per_aez.rds')
write_csv(aez_farm_stat, file = '../output/tables/cropland_stats_per_aez.csv')

# predicted average farm size per grid cell
simpl_aez <- rf_model_predictions |>
  terra::as.data.frame(xy = T) |>
  bind_cols(
    terra::extract(aez5, terra::crds(rf_model_predictions)) ) |>
  filter(!is.na(pred_farm_area_ha), !is.na(aez))
# -----------------------------------------------------------------------------
# select all AEZ that contribute to + 90% of total cropland in SSA
# Using cropland from SPAM

# later, loop over "harv_area", "prod", and "val_prod"
each_2017_crop <- terra::rast(paste0(input_path, '/spam/spam2017/', dir(paste0(input_path,'/spam/spam2017'))[grep('_P_[A-Z]+_A.tif$', dir(paste0(input_path,'/spam/spam2017')))]) ) # _A is total area (rainfed  + irrigated)
each_2017_crop <- terra::crop(each_2017_crop, ssa, mask = T)
names(each_2017_crop) <- substr(names(each_2017_crop), 20, 23)
# Pick Sudan data from SPAM 2010, and merge with SPAM 2017
each_2010_crop <- terra::rast(paste0(input_path, '/spam/spam2010/', dir(paste0(input_path,'/spam/spam2010'))[grep('_P_[A-Z]+_A.tif$', dir(paste0(input_path,'/spam/spam2010')))]) ) 
names(each_2010_crop) <- substr(names(each_2010_crop), 23, 26)
sudan_mask <- terra::crop(each_2010_crop, subset(ssa, ssa$NAME_0 == 'Sudan'), mask = T)
each_crop <- terra::merge(each_2017_crop, sudan_mask); rm(each_2010_crop, each_2017_crop)
terra::ext(each_crop) <- floor(terra::ext(each_crop))
crop_rel_importance <- each_crop |> 
  terra::as.data.frame() |> 
  summarize(across(everything(), sum, na.rm = T)) |>
  unlist() |>
  sort(decreasing = T)
crop_rel_importance

all_crops <- sum(each_crop, na.rm = T); names(all_crops) <- 'all_crops'
maize_ssa <- each_crop[['MAIZ']]
sorgh_ssa <- each_crop[['SORG']]
pmil_ssa <- each_crop[['PMIL']]
cassa_ssa <- each_crop[['CASS']] 
grou_ssa <- each_crop[['GROU']]
rice_ssa <- each_crop[['RICE']]
beans_ssa <- each_crop[['BEAN']]

millet_ssa <- sum(c(each_crop[['PMIL']], each_crop[['SMIL']]), na.rm = T); names(millet_ssa) <- 'millet'
legm_ssa <- sum(c(each_crop[['BEAN']], each_crop[['CHIC']], each_crop[['COWP']],
                  each_crop[['PIGE']], each_crop[['LENT']], each_crop[['OPUL']], 
                  each_crop[['SOYB']], each_crop[['GROU']] ), na.rm = T); names(legm_ssa) <- 'legumes'

non_food_ssa <- sum(c(each_crop[['SUGC']], each_crop[['COTT']], each_crop[['OFIB']],
                      each_crop[['ACOF']], each_crop[['RCOF']], each_crop[['COCO']],
                      each_crop[['TEAS']], each_crop[['TOBA']]), na.rm = T); names(non_food_ssa) <- 'non_food'
# prepare cattle raster
# cattle <- stacked$cattle
# cattle <- terra::extend(cattle, terra::ext(each_crop)) # not just terra::ext(cattle) <- terra::ext(each_crop)
cattle <- terra::resample(stacked$cattle, each_crop)

cropland_per_aez <- all_crops |>
  terra::as.data.frame(xy = T) |>
  bind_cols(
    terra::extract(aez5, terra::crds(all_crops))
  )

contrib_aez <- cropland_per_aez |>
  group_by(aez) |>
  summarize(absol = sum(all_crops, na.rm = T)) |>
  arrange(desc(absol)) |>
  mutate(rel = absol / sum (absol),
         cumsum = cumsum(rel))

major_aez <-simpl_aez |>
  inner_join(contrib_aez |>
               filter(rel > 0.05) |> # exclude AEZ contributing to less than 5% of total cropland
               select(aez))

df <- c(cattle, all_crops, maize_ssa, sorgh_ssa, pmil_ssa, cassa_ssa, grou_ssa, rice_ssa, beans_ssa, 
        millet_ssa, legm_ssa, non_food_ssa )
df <- c(rf_model_predictions, terra::resample(df, rf_model_predictions))
df <- inner_join(major_aez, na.omit(terra::as.data.frame(df, xy = T))) |>
  rename(maize = MAIZ, sorghum = SORG, pearl_millet = PMIL,
         cassava = CASS, groundnuts = GROU, rice = RICE, beans = BEAN,)

# df$fsize <- round(df$fsize, 1)
# df_agg <- aggregate(df[2], by=list('fsize'=df$fsize), FUN=sum)
# df_agg$cumsum_ha <- cumsum(df_agg$maize)
# df_agg$cumsum_ha <- 100 * df_agg$cumsum_ha / max(df_agg$cumsum_ha)
# df_agg$variable <- 'maize'
# df_agg$aez <- 'all'
# plot(df_agg$fsize, df_agg$cumsum_ha)

df_absolute <- df |> 
  select(!c(x, y)) |>
  group_by(aez, pred_farm_area_ha) |>
  summarize_all(sum, na.rm = T) |>
  arrange(aez, pred_farm_area_ha)

df_rel_per_aez <- df_absolute |> 
  mutate(across(!c(pred_farm_area_ha), function(x) cumsum(x)/sum(x)))

df_rel_all_aez <- df |> 
  select(!c(x, y, aez)) |>
  group_by(pred_farm_area_ha) |>
  summarize_all(sum, na.rm = T) |>
  ungroup() |>
  arrange(pred_farm_area_ha) |>
  mutate(across(!c(pred_farm_area_ha), function(x) cumsum(x)/sum(x)),
         aez = 'all_aez')

df_rel <- df_rel_per_aez |>
  bind_rows(df_rel_all_aez) |>
  mutate(aez = factor(aez, levels = c('all_aez', 'tropical highlands', 'humid', 'sub-humid', 'semi-arid'))) 
df_rel_long <- df_rel |>
  pivot_longer(!c(aez, pred_farm_area_ha), names_to = 'product', values_to = 'value') 


# play with thresholds and product, to find percentage of cropland cultivated in areas where avg farm size is less than x 
exple_50perc_selected_crops <- df_rel_long |> 
  group_by(aez) |> 
  filter(product %in% c('all_crops', 'cassava', 'maize', 'sorghum', 'millet'),
         value > 0.4999, value <0.501) |>
  select(product, aez, value, pred_farm_area_ha) |>
  arrange(product, aez, value) 
View(exple_50perc_selected_crops)

saveRDS(list(crop_rel_importance = crop_rel_importance, df_absolute = df_absolute, df_rel = df_rel, 
             df_rel_long = df_rel_long, exple_50perc_selected_crops = exple_50perc_selected_crops),
        file = '../data/processed/croplands_per_crop_per_aez.rds')


P00 <- ggplot(df_rel_long |>
                filter(product %in% c('cattle', 'all_crops' ), aez == 'all_aez'), 
              aes(pred_farm_area_ha, 100 * value, linetype = product)) + 
  geom_line(linewidth = 0.8, colour = 'black') + 
  labs(x = 'Average farm size, ha', y = 'Cumulative cultivated area or herd size, % ') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 8)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_test() +
  theme(legend.position = c(0.9, 0.25))
P00
png(paste0('../output/graphs/africa_avg_farm_size_cattle_all_crops_all_AEZ.png'), height = 5, width = 7.5, units = 'in', res = 600)
P00
ggsave(paste0('../output/graphs/africa_avg_farm_size_cattle_all_crops_all_AEZ.png'))
dev.off()

P00 <- ggplot(df_rel_long |>
                filter(product %in% c('cattle', 'all_crops'), aez != 'all_aez'), 
              aes(pred_farm_area_ha, 100 * value, linetype = product, colour = aez)) + 
  geom_line(linewidth = 0.8) + 
  labs(x = 'Average farm size, ha', y = 'Cumulative cultivated area or herd size, % ') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 8)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = rev(pal(5))[-4]) +
  theme_test() +
  theme(legend.position = c(0.85, 0.35))
P00
png(paste0('../output/graphs/africa_avg_farm_size_cattle_all_crops_per_AEZ.png'), height = 5, width = 7.5, units = 'in', res = 600)
P00
ggsave(paste0('../output/graphs/africa_avg_farm_size_cattle_all_crops_per_AEZ.png'))
dev.off()

P01a <- ggplot(df_rel_long |>
                filter(product %in% c('maize', 'sorghum', 'millet', 'cassava', 'groundnuts' ) ), 
              aes(pred_farm_area_ha, 100 * value, colour = product)) + 
  geom_line(linewidth = 0.8) + 
  labs(x = 'Average farm size, ha', y = 'Cumulative land area under cultivation, %', colour = 'crop') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 8)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = rev(pal(19))[c(1, 9, 5, 15, 19)]) +
  facet_grid( ~ aez) +
  theme_test() +
  theme(legend.position = c(0.94, 0.2))
P01a
png(paste0('../output/graphs/africa_avg_farm_size_5crops_per_AEZ.png'), height = 5, width = 7.5, units = 'in', res = 600)
P01a
ggsave(paste0('../output/graphs/africa_avg_farm_size_5crops_per_AEZ.png'))
dev.off()

P01a <- ggplot(df_rel_long |>
                 filter(product %in% c('maize', 'sorghum', 'millet', 'cassava'), aez != 'all_aez' ), # groundnut is out
               aes(pred_farm_area_ha, 100 * value, colour = product)) + 
  geom_line(linewidth = 0.8) + 
  labs(x = 'Average farm size, ha', y = 'Cumulative land area under cultivation, %', colour = 'crop') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 4.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = rev(pal(19))[c(1, 5, 15, 19)]) +
  facet_grid( ~ aez) +
  theme_test() +
  theme(legend.position = c(0.15, 0.2))
P01a
png(paste0('../output/graphs/africa_avg_farm_size_4crops_per_AEZ.png'), height = 5, width = 7.5, units = 'in', res = 600)
P01a
ggsave(paste0('../output/graphs/africa_avg_farm_size_4crops_per_AEZ.png'))
dev.off()

P01b <- ggplot(df_rel_long |>
                filter(product %in% c('maize', 'sorghum', 'millet', 'cassava', 'groundnuts'), aez == 'all_aez'), 
              aes(pred_farm_area_ha, 100 * value, colour = product)) + 
  geom_line(linewidth = 0.8) + 
  labs(x = 'Average farm size, ha', y = 'Cumulative land area under cultivation, %', colour = 'crop') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 8)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = rev(pal(19))[c(1, 9, 5, 15, 19)]) +
  theme_test() +
  theme(legend.position = c(0.92, 0.2))
P01b
png(paste0('../output/graphs/africa_avg_farm_size_5crops_all_AEZ.png'), height = 5, width = 7.5, units = 'in', res = 600)
P01b
ggsave(paste0('../output/graphs/africa_avg_farm_size_5crops_all_AEZ.png'))
dev.off()

P01c <- ggplot(df_rel_long |>
                 filter(product %in% c('maize', 'sorghum', 'millet','cassava', 
                                       'legumes', 'non_food'), aez == 'all_aez'), 
               aes(pred_farm_area_ha, 100 * value, colour = product)) + 
  geom_line(linewidth = 0.8) + 
  labs(x = 'Average farm size, ha', y = 'Cumulative land area under cultivation, % ') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 4.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = rev(pal(20))[c(1, 9, 5, 12, 15, 20)]) +
  theme_test() +
  theme(legend.position = c(0.9, 0.3))
P01c
png(paste0('../output/graphs/africa_avg_farm_size_crop_groups_all_AEZ_6crops_no_rice.png'), height = 5, width = 7.5, units = 'in', res = 600)
P01c
ggsave(paste0('../output/graphs/africa_avg_farm_size_crop_groups_all_AEZ_6crops_no_rice.png'))
dev.off()


P01d <- ggplot(df_rel_long |>
                 filter(product %in% c('maize', 'sorghum', 'millet','cassava', 
                                       'legumes', 'non_food'), aez != 'all_aez'), 
               aes(pred_farm_area_ha, 100 * value, colour = product)) + 
  geom_line(linewidth = 0.8) + 
  labs(x = 'Average farm size, ha', y = 'Cumulative land area under cultivation, % ') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 4.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = rev(pal(20))[c(1, 9, 5, 12, 15, 20)]) +
  facet_grid(~ aez) + 
  theme_test() +
  theme(legend.position = c(0.9, 0.3))
P01d
png(paste0('../output/graphs/africa_avg_farm_size_6_sel_crops_per_AEZ.png'), height = 5, width = 7.5, units = 'in', res = 600)
P01d
ggsave(paste0('../output/graphs/africa_avg_farm_size_6_sel_crops_per_AEZ.png'))
dev.off()

# P02 <- patchwork::wrap_plots(P00 + P01, widths = c(0.75, 0.25)) # not satisfied with widths = c(4, 1))
# P02
# P02 <- cowplot::plot_grid(P00, P01, align = 'hv', rel_widths = c(2, 1))
# P02
# pred_mean_aez <- simpl_aez |>
#   group_by(aez) |>
#   summarize(mean_area = mean(pred_farm_area_ha, na.rm = T))
# P00 <- ggplot(simpl_aez, aes(pred_farm_area_ha)) +
#   stat_ecdf(geom = 'line') + 
#   geom_vline(data = pred_mean_aez, aes(xintercept = mean_area), linetype = 'dashed', colour = 'red') + 
#   labs(x = 'Predicted average farm size (ha)', y = 'Cumulative distribution function') + 
#   facet_grid( ~ aez, scales = 'free_x') +
#   theme_bw()
# png(paste0('../output/graphs/africa_avg_farm_size_per_AEZ.png'), height = 5, width = 7.5, units = 'in', res = 600)
# P00
# ggsave(paste0('../output/graphs/africa_avg_farm_size_per_AEZ.png'))
# dev.off()

# dist_per_aez <-  theor_farms |>
#   bind_cols(
#     terra::extract(aez5, with(theor_farms, cbind(x, y))) ) |>
#   filter(!is.na(aez)) 
# long_dist_per_aez <- dist_per_aez |>
#   select(x, y, aez, fitted_trunc_logn) |>
#   unnest_longer(fitted_trunc_logn, values_to = 'pred_farm_area_ha')

# P01 <- ggplot(long_dist_per_aez, aes(pred_farm_area_ha, group = paste(x, '_', y))) +
#   stat_ecdf(geom = 'line', alpha = 0.05, colour = 'lightskyblue1') +
#   facet_grid( ~ aez, scales = 'free_x') + 
#   theme_test()
# png(paste0('../output/graphs/africa_farm_size_dist_per_AEZ.png'), height = 5, width = 7.5, units = 'in', res = 600)
# P01
# ggsave(paste0('../output/graphs/africa_farm_size_dist_per_AEZ.png'))
# dev.off()