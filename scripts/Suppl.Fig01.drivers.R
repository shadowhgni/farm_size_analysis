# Suppl. Fig. 1: drivers of farm size variation across SSA

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
pal <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))
pal2 <- colorRampPalette(c('#c6dbef','#6baed6','#3182bd', '#08519c', '#08306b'))
pal3 <- colorRampPalette(c('lightskyblue1', 'blue4'))
pal4 <- colorRampPalette(c('#A1D99B', '#00441B')) # RColorBrewer::brewer.pal(9,'Greens')
pal5 <- colorRampPalette(c('#FFFFCC', '#800026'))
pal6 <- colorRampPalette(c('#F0F921FF', '#0D0887FF'))
pal7 <- colorRampPalette(c('#C7EAE5', '#01665E'))
pal8 <- viridis::plasma(6)
pal9 <- colorRampPalette(c('#F1605DFF', '#FD9567FF', '#FEC98DFF', '#FCFDBFFF'))
# force terra to use disk-based processing and 50% of RAM (Use this if R crashes because of limited memory)
terra::terraOptions(memfrac = 0.5, todisk = T)
gc()
# ------------------------------------------------------------------------------
#define the countries for which LSMS data are available
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

xx <- readRDS('../../data/processed/fsize_distribution_resample_long.rds')
theor_farms <- xx$theor_farms
theor_farms_application <- xx$theor_farms_application; rm(xx)

# Prepare lsms data
lsms_spatial <-  readRDS('../../data/processed/lsms_trimmed_95th_africa.rds') # this was retrieved from '03.1.pooled_data_for_analysis.r'
stacked$slope <- 100 * stacked$slope

# ------------------------------------------------------------------------------
# # individual plots for each driver
# for(i in names(stacked)){
#   my_range <- case_when(i == 'cropland' ~ c(0, 1000),
#                         i == 'cattle' ~ c(0, 1000),
#                         i == 'pop' ~ c(0, 500),
#                         i == 'cropland_per_capita' ~ c(0, 1000),
#                         i == 'sand' ~ c(0, 1000),
#                         i == 'slope' ~ c(0, 1000),
#                         i == 'temperature' ~ c(0, 30),
#                         i == 'rainfall' ~ c(0, 1000),
#                         i == 'maizeyield' ~ c(0, 1000),
#                         i == 'market' ~ c(0, 1000))
# 
#   my_label <- case_when(i == 'cropland' ~ 'Cropland (ha/100 km2)',
#                         i == 'cattle' ~ paste0(expression('Cattle density (heads / 100 ' ~ km^2), ')'),
#                         i == 'pop' ~ paste0(expression('Pop. density (pers. / 100 ' ~ km^2), ')'),
#                         i == 'cropland_per_capita' ~ 'Cropland per capita (ha/pers)',
#                         i == 'sand' ~ 'Sand content (%)',
#                         i == 'slope' ~ 'Terrain sloe (%)',
#                         i == 'temperature' ~ paste0(expression('Temperature ('~ degree * C ),')' ),
#                         i == 'rainfall' ~ 'Rainfall (mm)',
#                         i == 'maizeyield' ~ paste0(expression('Maize yield (kg '~ ha^{-1}), ')'),
#                         i == 'market' ~ 'Travel time to nearest town (min)')
# 
#   my_tag <- case_when(i == 'cropland' ~ 'A',
#                       i == 'cattle' ~ 'B',
#                       i == 'pop' ~ 'C',
#                       i == 'cropland_per_capita' ~ 'D',
#                       i == 'sand' ~ 'E',
#                       i == 'slope' ~ 'F',
#                       i == 'temperature' ~'G',
#                       i == 'rainfall' ~ 'H',
#                       i == 'maizeyield' ~ 'I',
#                       i == 'market' ~ 'J')
# 
#   my_col <- case_when(i == 'cropland' ~ pal4(10),
#                       i == 'cattle' ~ pal5(10),
#                       i == 'pop' ~ pal6(10),
#                       i == 'cropland_per_capita' ~ pal3(10),
#                       i == 'sand' ~ rev(pal(10)),
#                       i == 'slope' ~ rev(pal(10)),
#                       i == 'temperature' ~ pal5(10),
#                       i == 'rainfall' ~ pal3(10),
#                       i == 'maizeyield' ~ pal(10),
#                       i == 'market' ~ rev(pal(10)))
#   png(paste0('suppl.fig01_', i, '.png'), height = 2.5, width = 2.5, units = 'cm', res = 1000)
#   M00 <-{
#     terra::plot(ssa, col = 'azure', pax = list(cex.axis = 0.8))
#     terra::plot(stacked[[i]], range = my_range, col = my_col, legend = F, cex = 1, axes = F, add = T)
#     terra::plot(ssa, col = NA, add = T)
#     fields::image.plot(legend.only = T,
#                        zlim = my_range,
#                        col = my_col,
#                        legend.lab = '',
#                        legend.args = list(
#                          text = '',
#                          line = 1,
#                          cex = 0.8,
#                          adj = 0),
#                        smallplot = c(0.2, 0.42, 0.4, 0.42),
#                        horizontal = T)
#     text(x = 45, y = 30, labels = my_tag, cex = 2)
#     dev.off()
#   }
# }
# # M01 <- 
# png(paste0('suppl.fig01.', i), height = 2.5, width = 2.5, units = 'cm', res = 1000)
# ggsave('../output/maps/estim_nb_farms_per_grid_cell_classes.png')
# dev.off()



# png('../output/maps/estim_nb_farms_per_grid_cell_classes.png', height = 5, width = 5, units = 'in', res = 600)
# M03 <-{
#   terra::plot(ssa, col = 'azure', main = 'Predicted farm density',
#               panel.first = grid(col = 'gray', lty = 'solid'), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
#   terra::plot(calc_nb_farms$nb_farms, breaks = c(0, 500, 1000, 2000, 5000, 10000, Inf), col = pal2(6), legend = F, cex = 1, axes = F, add = T)
#   legend(-16.5, -10, bty = 'y', cex = 0.7, ncol = 1, box.col = 'white',
#          title  =  expression('Nb. of farms per 100' ~ km^2), legend = c('≤ 500', '501 - 1000', '1001 - 2000', '2001 - 5000', '5001 - 10000', '> 10000'),
#          fill = pal2(6), horiz = FALSE)
#   terra::plot(ssa, axes = F, add = T)
# }
# ggsave('../output/maps/estim_nb_farms_per_grid_cell_classes.png')
# dev.off()

# tm <- tmap::tm_shape(ssa) +
#   tmap::tm_polygons(col = "azure", border.col = NA) +  # Azure fill, no border
#   tmap::tm_shape(stacked[[i]]) +
#   tmap::tm_raster(style = "cont",
#             palette = my_col,
#             breaks = my_range,
#             legend.show = TRUE,
#             legend.format = list(scientific = FALSE)) +
#   tmap::tm_shape(ssa) +
#   tmap::tm_borders(lwd = 0.5) +  # Thin black outline
#   tmap::tm_layout(
#     legend.position = c("left", "bottom"),
#     legend.outside = FALSE,
#     legend.title.size = 0.8,
#     legend.text.size = 0.6,
#     legend.width = 0.5,
#     legend.height = 0.15,
#     legend.bg.color = "white",
#     legend.bg.alpha = 0.7,
#     legend.frame = FALSE,
#     asp = 1
#   ) +
#   tmap::tm_add_legend(
#     type = "colorbar",
#     col = my_col,
#     labels = NULL,
#     is.horizontal = TRUE,
#     width = 0.3,
#     height = 0.02
#   ) +
#   tmap::tm_text(text = my_tag, size = 1.5, just = "right", xmod = 1.5, ymod = 1.5)

# Initialize plot list
tmap_list <- list()

# Set tmap to plot mode
tmap::tmap_mode('plot')
tmap::tmap_options(component.autoscale = FALSE, asp = 1)

for(i in names(stacked)[c(2, 3, 5:10, 1 )]){
  my_range <- case_when(i == 'cropland' ~ c(0, 5000),
                        i == 'cattle' ~ c(0, 3000),
                        i == 'pop' ~ c(0, 400),
                        i == 'cropland_per_capita' ~ c(0, 1000),
                        i == 'sand' ~ c(0, 90),
                        i == 'slope' ~ c(0, 3),
                        i == 'temperature' ~ c(15, 40),
                        i == 'rainfall' ~ c(0, 2000),
                        i == 'maizeyield' ~ c(0, 15000),
                        i == 'market' ~ c(0, 1000))
  
  my_label <- case_when(i == 'cropland' ~ 'SPAM 2017\nCropland\n(ha/100 km²)',
                        i == 'cattle' ~ 'Cattle density\n(heads/100 km²)',
                        i == 'pop' ~ 'Pop. density\n(pers./100 km²)',
                        i == 'cropland_per_capita' ~ 'Cropland per capita\n(ha/pers)',
                        i == 'sand' ~ 'Sand content\n(%)',
                        i == 'slope' ~ 'Terrain slope\n(%)',
                        i == 'temperature' ~ 'Temperature\n(°C)',
                        i == 'rainfall' ~ 'Rainfall\n(mm)',
                        i == 'maizeyield' ~ 'Maize yield\n(kg/ha)',
                        i == 'market' ~ 'Travel time\n(min)')
  
  my_tag <- case_when(i == 'cropland' ~ 'I)',
                      i == 'cattle' ~ 'A)',
                      i == 'pop' ~ 'B)',
                      i == 'cropland_per_capita' ~ 'x',
                      i == 'sand' ~ 'C)',
                      i == 'slope' ~ 'D)',
                      i == 'temperature' ~'E)',
                      i == 'rainfall' ~ 'F)',
                      i == 'maizeyield' ~ 'G)',
                      i == 'market' ~ 'H)')
  
  my_col <- case_when(i == 'cropland' ~ list(pal4(10)),
                      i == 'cattle' ~ list(pal5(10)),
                      i == 'pop' ~ list(pal7(10)),
                      i == 'cropland_per_capita' ~ list(pal3(10)),
                      i == 'sand' ~ list(rev(pal(10))),
                      i == 'slope' ~ list(rev(pal(10))),
                      i == 'temperature' ~ list(rev(pal9(10))),
                      i == 'rainfall' ~ list(pal3(10)),
                      i == 'maizeyield' ~ list(pal(10)),
                      i == 'market' ~ list(rev(pal(10))))[[1]]
  
  # Create tmap with continuous legend and capped colors
  p <- tmap::tm_shape(stacked[[i]]) +
    tmap::tm_raster(
      col.scale = tmap::tm_scale_continuous(
        values = my_col,
        limits = my_range,               # Values above my_range[2] get same color as max
        outliers.trunc = c(TRUE, TRUE),  # Truncate low/high outliers to threshold values
        # n = 2,
        ticks = my_range,
        # labels =  c(my_range[1], paste('≥',my_range[2]))
        labels = my_range
      ),
      col.legend = tmap::tm_legend(
        title = '',
        # width = 5,
        # height = 4,
        frame = FALSE,
        text.size = 1,
        title.size = 0.01,
        title.align = 'left'
        # at = c(min(my_range),
        #        (min(my_range) + max(my_range)) / 2,
        #        max(my_range)), # tick positions
        # labels = c(min(my_range),
        #            (min(my_range) + max(my_range)) / 2,
        #            max(my_range)), # tick labels
        # position = tmap::tm_pos_in("left", "bottom"),
        # position = tmap::tm_pos_in(0.15, 0.1),
      )
    ) +
    tmap::tm_shape(sf::st_as_sf(ssa)) +
    tmap::tm_borders(col = "black", lwd = 0.5) +
    tmap::tm_graticules(
      x = seq(-20, 60, by = 10),   # Longitude lines every 10 degrees
      y = seq(-40, 20, by = 10),   # Latitude lines every 10 degrees
      col = "gray70",
      lwd = 0.3,
      alpha = 0.7,
      labels.size = 0.6,
      labels.col = "gray50"
    ) +
    tmap::tm_layout(
      frame = FALSE,
      bg.color = "azure",
      legend.position = c(0, 0.7),
      legend.frame = FALSE,
      legend.bg.color = 'transparent',
      legend.frame.lwd = 0,
      legend.width = 4.2,
      legend.height = 9
    ) +
    tmap::tm_credits(my_tag, 
               position = tmap::tm_pos_in("right", "top"),
               size = 1.5)
  
  # Store the tmap plot
  tmap_list[[i]] <- p
}

six_crop_masks <- terra::rast(paste0(input_path, '/landuse/landuse/all_cropland_mask.tif'))
for(i in names(six_crop_masks)[c(3, 4, 6)]){
  my_range <- c(0, 5000)
  
  my_label <- if_else(i == 'GEOSURVEY 2015', 'GEOSURV. 2015\nCropland\n(ha/100 km²)',
                      paste0(i, '\nCropland\n(ha/100 km²)'))
  
  my_tag <- case_when(i == 'SPAM 2010' ~ 'Z)',
                      i == 'SPAM 2017' ~ 'Y)',
                      i == 'SPAM 2020' ~ 'J)',
                      i == 'ESA 2020' ~ 'K)',
                      i == 'GLAD 2019' ~ 'X)',
                      i == 'GEOSURVEY 2015' ~ 'L)')
  
  my_col <-pal4(10)
  
  # Create tmap with continuous legend and capped colors
  p <- tmap::tm_shape(six_crop_masks[[i]]) +
    tmap::tm_raster(
      col.scale = tmap::tm_scale_continuous(
        values = my_col,
        limits = my_range,               # Values above my_range[2] get same color as max
        outliers.trunc = c(TRUE, TRUE),  # Truncate low/high outliers to threshold values
        # n = 2
        ticks = my_range,
        labels =  my_range
      ),
      col.legend = tmap::tm_legend(
        title = '',
        # width = 5,
        # height = 4,
        frame = FALSE,
        text.size = 1,
        title.size = 0.01,
        title.align = 'left'
      )
    ) +
    tmap::tm_shape(sf::st_as_sf(ssa)) +
    tmap::tm_borders(col = "black", lwd = 0.5) +
    tmap::tm_graticules(
      x = seq(-20, 60, by = 10),   # Longitude lines every 10 degrees
      y = seq(-40, 20, by = 10),   # Latitude lines every 10 degrees
      col = "gray70",
      lwd = 0.3,
      alpha = 0.7,
      labels.size = 0.6,
      labels.col = "gray50"
    ) +
    tmap::tm_layout(
      frame = FALSE,
      bg.color = "azure",
      legend.position = c(0, 0.7),
      legend.frame = FALSE,
      legend.bg.color = 'transparent',
      legend.frame.lwd = 0,
      legend.width = 4.2,
      legend.height = 9
    ) +
    tmap::tm_credits(my_tag, 
                     position = tmap::tm_pos_in("right", "top"),
                     size = 1.5)
  
  # Store the tmap plot
  tmap_list[[i]] <- p
}

# Combine all plots in a grid
combined_plot <- tmap::tmap_arrange(tmap_list, ncol = 4)

# Save combined plot
tmap::tmap_save(combined_plot, 'Suppl.Fig01.png', 
                width = 10, height = 7, units = 'in', dpi = 1000)
magick::image_write(magick::image_read('Suppl.Fig01.png'), 'Suppl.Fig01.pdf', format = 'pdf')
