# Suppl.Fig.05

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
xx <- readRDS('../../data/processed/cross_validation_graphs.rds')
country_pairs <- xx$country_pairs
country_leave_one_out <- xx$country_leave_one_out; rm(xx)
#--------------------
# Plot A
country_pairs <- country_pairs |>
  inner_join(bind_cols(country = sixteen_countries, GID_0 =sixteen_country_codes) |>
               rename(train_country = country, train_GID_0 = GID_0)) |>
  inner_join(bind_cols(country = sixteen_countries, GID_0 =sixteen_country_codes) |>
               rename(test_country = country, test_GID_0 = GID_0))

P00 <- ggplot(country_pairs,
              aes(train_GID_0, test_GID_0, fill = rf2_test_rsq)) +
  geom_raster() +
  geom_text(aes(label = rf2_test_rsq), size = 5) +
  geom_hline(yintercept = seq(0.5, 16.5, by = 1)) +
  geom_vline(xintercept = seq(0.5, 16.5, by = 1)) +
  geom_text(x = 1, y = 18, label = 'A)', size = 8) +
  labs(x = 'Training dataset', y = 'Validation dataset', fill = bquote(R^2~ '  ')) +
  scale_x_discrete(expand =c(0, 0)) +
  scale_y_discrete(expand =c(0, 0)) +
  scale_fill_continuous(low = 'grey95', high = 'steelblue1',
                        breaks = c(0.25, 0.5, 0.75)) + # try grey95, steelblue1, firebrick4, gold1
  coord_cartesian(clip = 'off') +
  theme_test() +
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = -90, hjust = 1),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.key.width = unit(0.5, 'in'),
        legend.position = 'top',
        legend.justification = 'right',
        legend.direction = 'horizontal',
        title = element_text(size = 14),
        plot.margin = margin(3, 1, 15, 3)
        )
P00

#--------------------
# Plot B
country_leave_one_out <- country_leave_one_out |>
  mutate(model_name = gsub('pred. ', '' , gsub('\\~', 'vs.', model_name)),
         model_name = factor(model_name, levels = c('RF vs. obs.', 'TPS vs. obs.', 'RF vs. TPS')))

P01 <- ggplot(country_leave_one_out) +
  geom_col(aes(code, rsq, colour = model_name,fill = model_name, group = code),
           position = position_dodge2(width = 0.8, preserve = 'single'), width = 0.6) +
  geom_text(x = 1, y = 1.2, label = 'B)', size = 8) +  # family = 'Arial'
  labs(x = 'Country', y = expression(R^2), fill = 'Models  ') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.4)) +
  scale_colour_manual(values = pal2(3)) +
  scale_fill_manual(values = pal2(3)) +
  theme_test() +
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = -60, vjust = 0.5, hjust = 0.1),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = c(0.99, 0.9),
        legend.justification = 'right',
        legend.direction = 'horizontal',
        title = element_text(size = 14),
        plot.margin = margin(15, 3, 3, 3)) + 
  guides(colour = 'none')
P01

P02 <- P00 / P01 + patchwork::plot_layout(ncol = 1, heights = c(2, 1))

ggsave('Suppl.Fig05.pdf', P02, width = 9, height = 9, units = 'in', dpi = 1000)