# Suppl.Fig.07

# load packages
require(tidyverse)
require(patchwork)

# Clean environment
rm(list=ls())

# # Set working directory
# setwd(paste0(here::here(), '/scripts'))

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

theor_rast <- theor_farms |>
  ungroup() |>
  select(x, y, skew, kurt, 
         ks_trunc_D, ks_trunc_pval, 
         adjusted_logn_mean, adjusted_logn_sd) |>
  terra::rast()
gini <- terra::rast('../../data/processed/gini_raster.tif')
terra::crs(gini) <- terra::crs(theor_farms)

back_avg <- terra::rast('../../data/processed/back_transf_trunc_adj_mean.tif')
terra::crs(back_avg) <- terra::crs(theor_farms)

back_sd <- terra::rast('../../data/processed/back_transf_trunc_adj_sd.tif')
terra::crs(back_sd) <- terra::crs(theor_farms)

selected_rast <- c(theor_rast$skew, gini, 
                   back_avg, back_sd,
                   theor_rast$ks_trunc_D, theor_rast$ks_trunc_pval)
names(selected_rast) <- c('skew', 'gini', 
                          'adjusted_logn_mean', 'adjusted_logn_sd', 
                          'ks_trunc_D', 'ks_trunc_pval')
# Prepare lsms data
lsms_spatial <-  readRDS('../../data/processed/lsms_trimmed_95th_africa.rds') # this was retrieved from '03.1.pooled_data_for_analysis.r'

# ------------------------------------------------------------------------------
# Initialize plot list
tmap_list <- list()

# Set tmap to plot mode
tmap::tmap_mode('plot')
tmap::tmap_options(component.autoscale = FALSE, asp = 1)

for(i in names(selected_rast)){
  my_range <- case_when(i == 'skew' ~ c(0, 6),
                        i == 'gini' ~ c(0.4, 0.6),
                        i == 'ks_trunc_D' ~ c(0, 0.16),
                        i == 'ks_trunc_pval' ~ c(0.05, 1),
                        i == 'adjusted_logn_mean' ~ c(0, 5),
                        i == 'adjusted_logn_sd' ~ c(1, 5) )
  
  my_label <- case_when(i == 'skew' ~ 'Skewness of empirical\nfarm size distribution',
                        i == 'gini' ~ 'Gini coef of empirical\nfarm size distribution',
                        i == 'ks_trunc_D' ~ "Goodness of fit\nKolmogorov's D distance",
                        i == 'ks_trunc_pval' ~ 'Goodness of fit\nP-value',
                        i == 'adjusted_logn_mean' ~ 'Average (back-transformed)\nfarm size (ha)',
                        i == 'adjusted_logn_sd' ~ 'Standard deviation (back-transformed)\nof farm size (ha)' )
  
  my_tag <- case_when(i == 'skew' ~ 'A',
                      i == 'gini' ~ 'B',
                      i == 'ks_trunc_D' ~ 'E',
                      i == 'ks_trunc_pval' ~ 'F',
                      i == 'adjusted_logn_mean' ~ 'C',
                      i == 'adjusted_logn_sd' ~ 'D' )
  
  my_col <- case_when(i == 'skew' ~ rev(pal1(10)),
                      i == 'gini' ~ rev(pal1(10)),
                      i == 'ks_trunc_D' ~ rev(pal1(10)),
                      i == 'ks_trunc_pval' ~ pal1(10),
                      i == 'adjusted_logn_mean' ~ pal1(10),
                      i == 'adjusted_logn_sd' ~ rev(pal1(10)) )
  
  # Create tmap with continuous legend and capped colors
  p <- tmap::tm_shape(selected_rast[[i]]) +
    tmap::tm_raster(
      col.scale = tmap::tm_scale_continuous(
        values = my_col,
        limits = my_range,               # Values above my_range[2] get same color as max
        outliers.trunc = c(TRUE, TRUE),  # Truncate low/high outliers to threshold values
        n = 3
        # ticks = my_range,
        # labels = my_range
      ),
      col.legend = tmap::tm_legend(
        title = my_label,
        # width = 5,
        # height = 4,
        frame = FALSE,
        text.size = 1,
        title.size = 0.01,
        title.align = 'left'
        )
    ) +
    tmap::tm_shape(sf::st_as_sf(ssa), crs = terra::crs(ssa)) +
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
      bg.color = 'whitesmoke',
      legend.position = c('left', 'bottom'),
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
combined_plot <- tmap::tmap_arrange(tmap_list, ncol = 2)

# Save combined plot
tmap::tmap_save(combined_plot, 'Suppl.Fig07.png', 
                width = 7, height = 10, units = 'in', dpi = 1000)
magick::image_write(magick::image_read('Suppl.Fig07.png'), 'Suppl.Fig07.pdf', format = 'pdf')