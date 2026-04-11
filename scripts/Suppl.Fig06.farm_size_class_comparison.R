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
# get tables for plotting
xx <- readRDS('../../data/processed/summarized_farm_area_ha_per_class_vs_sarah.rds')
comp_fsize_classes_ha <- xx$comp_fsize_classes_ha
comp_fsize_classes_nb <- xx$comp_fsize_classes_nb; rm(xx)
div_table <- readRDS('Suppl.Fig06_divergence_table.rds')
#--------------------
# Plot A
comp_fsize_classes_nb <- comp_fsize_classes_nb |>
  inner_join(terra::as.data.frame(ssa))

P00 <- ggplot(comp_fsize_classes_nb |>
                pivot_longer(cols = c(nb_farms, pred_nb_farms),
                             names_to = 'category', values_to = 'val') |>
                mutate(category = if_else(grepl('pred', category), 'Predicted', 'Reported')), 
              aes(GID_0, val / 1000000)) +
  geom_col(aes(GID_0, val / 1000000, colour = category, fill = category, group = farm_class,
               width = if_else(category == 'Reported', 0.8, 0.2)), 
           position = position_dodge(width = 0.8), linewidth = 0.8) +
  geom_text(x = 1, y = 8.5, label = 'A)', size = 9, inherit.aes = F) +
  geom_text(data = div_table |> inner_join(comp_fsize_classes_nb |> select(NAME_0, GID_0) |> distinct()),
            aes(x = GID_0, label = round(divergence_nb, 2)), y = 6, size = 5, angle = 60, inherits.aes = F) +
  labs(x = 'Country', y = 'Million farms per farm size class', fill = NULL, colour = NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9))+
  scale_colour_manual(values = c('red3', 'blue4')) + 
  scale_fill_manual(values = c('red3', 'lightskyblue1')) + 
  theme_test() + 
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 12),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.position = c(0.9, 0.9),
        plot.margin = margin(3, 0, 15, 0))

P00


#--------------------
# Plot B
comp_fsize_classes_ha <- comp_fsize_classes_ha |>
  inner_join(terra::as.data.frame(ssa))


P01 <- ggplot(comp_fsize_classes_ha |>
                pivot_longer(cols = c(cropland_ha, pred_cropland_ha),
                             names_to = 'category', values_to = 'val') |>
                mutate(category = if_else(grepl('pred', category), 'Predicted', 'Reported')), 
              aes(GID_0, val / 1000000)) +
  geom_col(aes(GID_0, val / 1000000, colour = category, fill = category, group = farm_class,
               width = if_else(category == 'Reported', 0.8, 0.2)), 
           position = position_dodge(width = 0.8), linewidth = 0.8) +
  geom_text(x = 1, y = 11, label = 'B)', size = 9, inherit.aes = F) +
  geom_text(data = div_table |> inner_join(comp_fsize_classes_ha |> select(NAME_0, GID_0) |> distinct()),
            aes(x = GID_0, label = round(divergence_ha, 2)), y = 8.5, size = 5, angle = 60, inherits.aes = F) +
  labs(x = 'Country', y = 'Million ha cultivated per farm size class', fill = NULL, colour = NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12))+
  scale_colour_manual(values = c('red3', 'blue4')) + 
  scale_fill_manual(values = c('red3', 'lightskyblue1')) + 
  theme_test() + 
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 12),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.position = 'none',
        plot.margin = margin(3, 0, 15, 0))
P01

P02 <- P00 / P01 + patchwork::plot_layout(ncol = 1)

ggsave('Suppl.Fig06.pdf', P02, width = 9, height = 9, units = 'in', dpi = 1000)

# ways of displaying the farm size class (but too small to be legible)
# P01 <- ggplot(comp_fsize_classes_ha |>
#                 pivot_longer(cols = c(cropland_ha, pred_cropland_ha),
#                              names_to = 'category', values_to = 'val') |>
#                 mutate(category = if_else(grepl('pred', category), 'Predicted', 'Reported')), 
#               aes(GID_0, val / 1000000, colour = category, fill = category, group = farm_class)) +
#   geom_col(aes(GID_0, val / 1000000, group = farm_class,
#                width = if_else(category == 'Reported', 0.8, 0.2)), 
#            position = position_dodge(width = 0.8), linewidth = 0.8) +
#   geom_text(aes(y = if_else(farm_class %in% c(1, 5, 20), -0.4, -0.8), 
#                 label = farm_class),  colour = 'black', show.legend = F,
#             position = position_dodge(width = 0.8)) +
#   geom_text(x = 1, y = 9.5, label = 'B)', size = 10, inherit.aes = F) +
#   labs(x = 'Country', y = 'Million ha cultivated  per farm size class', fill = NULL, colour = NULL) +
#   scale_y_continuous(expand = c(NA, 10), limits = c(-1, 10))+
#   scale_colour_manual(values = c('red3', 'blue4')) + 
#   scale_fill_manual(values = c('red3', 'lightskyblue1')) + 
#   theme_test() + 
#   theme(axis.title = element_text(size = 17),
#         axis.text = element_text(size = 12),
#         axis.ticks.x = element_blank(),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 15),
#         legend.position = c(0.95, 0.9),
#         plot.margin = margin(15, 0, 3, 0))

# P01 <- ggplot(comp_fsize_classes_ha |>
#                 pivot_longer(cols = c(cropland_ha, pred_cropland_ha),
#                              names_to = 'category', values_to = 'val') |>
#                 mutate(category = if_else(grepl('pred', category), 'Predicted', 'Reported')) |>
#                 inner_join(comp_fsize_classes_ha |>
#                              group_by(NAME_0, farm_class) |>
#                              reframe(y_lbl = max(cropland_ha, pred_cropland_ha)
#                              )
#                 ), 
#               aes(GID_0, val / 1000000, colour = category, fill = category, group = farm_class)) +
#   geom_col(aes(GID_0, val / 1000000, group = farm_class,
#                width = if_else(category == 'Reported', 0.8, 0.2)), 
#            position = position_dodge(width = 0.8), linewidth = 0.8) +
#   geom_text(aes(y = 0.4 + y_lbl / 1000000, 
#                 label = farm_class),  colour = 'grey', show.legend = F,
#             position = position_dodge(width = 0.8)) +
#   geom_text(x = 1, y = 9.5, label = 'A)', size = 9, inherit.aes = F) +
#   labs(x = 'Country', y = 'Million farms per farm size class', fill = NULL, colour = NULL) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 10))+
#   scale_colour_manual(values = c('red3', 'blue4')) + 
#   scale_fill_manual(values = c('red3', 'lightskyblue1')) + 
#   theme_test() + 
#   theme(axis.title = element_text(size = 17),
#         axis.text = element_text(size = 12),
#         axis.ticks.x = element_blank(),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 15),
#         legend.position = c(0.9, 0.9),
#         plot.margin = margin(3, 0, 15, 0))