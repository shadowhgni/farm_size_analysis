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

# # force terra to use disk-based processing and 50% of RAM (Use this if R crashes because of limited memory)
# terra::terraOptions(memfrac = 0.5, todisk = T)
# gc()
# # ------------------------------------------------------------------------------
# # define the countries for which LSMS data are available
# sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
# sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# # ------------------------------------------------------------------------------
# # Prepare data rasters: lsms and predictions + virtual list of farm sizes
# stacked <- terra::rast('../../data/processed/stacked_rasters_africa.tif')
# rf_model_predictions <- terra::rast('../../data/processed/rf_model_predictions_SSA.tif')
# names(rf_model_predictions) <- 'pred_farm_area_ha'
# qrf_model_predictions <- terra::rast('../../data/processed/qrf_100quantiles_predictions_africa.tif')
# names(qrf_model_predictions) <- paste0('qrf_q', sprintf('%03g', 1:100))
# mask_forest_ssa <- terra::rast('../../data/processed/mask_forest_ssa.tif')
# mask_drylands_ssa <- terra::rast('../../data/processed/mask_drylands_ssa.tif')
# # lsms data
# lsms_spatial <- readRDS('../../data/processed/lsms_trimmed_95th_africa.rds') 

# load results from Robert's optim work on HPC
hpc <- readRDS('../../output/tables/RF_optim_summarized_table.rds')

# quick wrangling
long_hpc <- hpc |>
  mutate(mbucket = as.integer(gsub('*.*mbucket\\-', '', gsub('\\.Rds', '', filename)))) |>
  pivot_longer(cols = c(mtry, mbucket, min.node.size), 
               names_to = 'hyper', values_to = 'val')
summ_hpc <- long_hpc |>
  group_by(hyper, val, splitrule) |>
  reframe(avgRMSE = mean(RMSE, na.rm = T), avgRsquared = mean(Rsquared, na.rm = T),
          sdRMSE = sd(RMSE, na.rm = T), sdRsquared = sd(Rsquared, na.rm = T))

P00 <- ggplot(summ_hpc, aes(val)) + 
  geom_ribbon(aes(ymin = avgRsquared - sdRsquared, 
                  ymax = avgRsquared + sdRsquared,
                  fill = splitrule),
              alpha = 0.05, 
              colour = NA) +
  geom_line(aes(y = avgRsquared, colour = splitrule), 
            linetype = 1, linewidth = 0.8) + 
  geom_point(aes(y = avgRsquared, colour = splitrule),
             shape = 16, size = 2, show.legend = F) + 
  geom_ribbon(aes(ymin = (avgRMSE - sdRMSE) / 7.5, 
                  ymax = (avgRMSE + sdRMSE) / 7.5,
                  fill = splitrule),
              alpha = 0.05, 
              colour = NA) +
  geom_line(aes(y = avgRMSE / 7.5, colour = splitrule),
            linetype = 'dashed', linewidth = 0.8) + 
  geom_point(aes(y = avgRMSE / 7.5, colour = splitrule),
             shape = 1, size = 2, show.legend = F) + 
  labs(x = 'Tested value') +
  scale_y_continuous(name = expression('Average ' ~ R^2),
                     sec.axis = sec_axis(~ . * 7.5, name = 'Average RMSE')) +
  scale_colour_manual(values = c('blue', 'red')) +
  scale_fill_manual(values = c('blue', 'red')) +
  facet_grid(~hyper, scales = 'free') + 
  theme_test() +
  theme(legend.position = c(0.9, 0.15),
        strip.background = element_rect(color = NA,  fill = 'white'),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  guides(fill = 'none')
P00

ggsave('Suppl.Fig.04.pdf', P00, width = 9, height = 5, dpi = 1000)