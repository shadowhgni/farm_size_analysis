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
var_importance_table <- xx$var_importance_table; rm(xx)
var_importance_table <- var_importance_table |>
  mutate(var = case_when(var == 'cropland' ~ 'Cropland',
                              var == 'cattle' ~ 'Cattle density',
                              var == 'pop' ~ 'Population density',
                              var == 'cropland_per_capita' ~ 'Cropland per capita',
                              var == 'sand' ~ 'Sand content',
                              var == 'slope' ~ 'Terrain slope',
                              var == 'temperature' ~ 'Temperature',
                              var == 'rainfall' ~ 'Precipitation',
                              var == 'maizeyield' ~ 'Water-limited maize yield',
                              var == 'market' ~ 'Distance to nearest town')) |>inner_join(bind_cols(country = sixteen_countries, GID_0 = sixteen_country_codes))

var_imp <- read.csv('../../output/tables/etr_variable_importance.csv') |>
  mutate(Variable = case_when(Variable == 'cropland' ~ 'Cropland',
                              Variable == 'cattle' ~ 'Cattle density',
                              Variable == 'pop' ~ 'Population density',
                              Variable == 'cropland_per_capita' ~ 'Cropland per capita',
                              Variable == 'sand' ~ 'Sand content',
                              Variable == 'slope' ~ 'Terrain slope',
                              Variable == 'temperature' ~ 'Temperature',
                              Variable == 'rainfall' ~ 'Precipitation',
                              Variable == 'maizeyield' ~ 'Water-limited maize yield',
                              Variable == 'market' ~ 'Distance to nearest town')) |>
  arrange(-Importance)
#--------------------
# Plot A
P00 <- ggplot(var_imp, aes(reorder(Variable, Importance), 100 * Importance)) + 
  geom_segment(y = 0, aes(yend = 100 * Importance)) +  
  geom_point() + 
  labs(x = 'Feature', y = 'Relative importance (%)') +
  coord_flip() +
  geom_text(x = 1.5, y = 19, label = 'A)', size = 8) + 
  theme_test() + 
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 13, colour = 'grey25'),
        axis.ticks.y = element_blank(),
        title = element_text(size = 14),
        plot.margin = margin(3, 5, 20, 3)
  )
P00
#--------------------
# Plot B
P01 <- ggplot(var_importance_table, aes(GID_0, var, fill = rank)) +
  geom_raster() +
  geom_text(aes(label = rank), size = 6) +
  geom_hline(yintercept = seq(0.5, 9.5, by = 1)) +
  geom_vline(xintercept = seq(0.5, 15.5, by = 1)) +
  geom_text(x = 1, y = 11.5, label = 'B)', size = 8) +
  labs(x = 'Country', y = 'Featrue', fill = 'Rank  ') +
  scale_x_discrete(expand =c(0, 0)) +
  scale_y_discrete(expand =c(0, 0)) +
  scale_fill_continuous(low = 'steelblue1', high = 'grey95', breaks = c(1, 5, 10)) +
  coord_cartesian(clip = 'off') +
  theme_test() +
  theme(axis.title = element_text(size = 17),
        axis.text = element_text(size = 13, colour = 'grey25'),
        axis.text.x = element_text(angle = -90, hjust = 1),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.key.width = unit(0.5, 'in'),
        legend.position = 'top',
        legend.justification = 'right',
        legend.direction = 'horizontal',
        title = element_text(size = 14),
        plot.margin = margin(20, 5, 3, 3)
        )
P01

P02 <- P00 / P01 + patchwork::plot_layout(ncol = 1, heights = c(1, 2))

ggsave('Suppl.Fig08.pdf', P02, width = 9, height = 9, units = 'in', dpi = 1000)