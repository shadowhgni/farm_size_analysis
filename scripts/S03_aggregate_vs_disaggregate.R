# ==============================================================================
# Script: S03_aggregate_vs_disaggregate.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Supplementary Figure 3 - Country vs GADM1 aggregation comparison
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - February 2026
# ==============================================================================


source("00_report_utils.R")
require(tidyverse)
require(patchwork)

# Clean environment
rm(list = setdiff(ls(), c("t0","write_report","capture_output","ci_trees","ci_folds")))
t0 <- proc.time()[["elapsed"]]

# # Set working directory
# setwd(paste0(here::here(), '/scripts'))
dir.create('../output/other_illustr/graphs', recursive = TRUE, showWarnings = FALSE)
dir.create('../output/other_illustr/graphs', recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Preparation for functions and mapping
input_path <- '../data/raw/spatial'
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
lsms_spatial <- readRDS('../../data/processed/lsms_trimmed_95th_africa.rds') 

# cropland comparison
xx <- readRDS('../../output/plot_data/plot_suppl_01_effect_of_source_of_cropland_masks.rds')
pred_cpland_df <- xx$pred_cpland_df
ssa_cropland <- xx$ssa_cropland
ssa_nb_farms <- xx$nb_farms_summarized |>
  mutate(nb_rounded = round(nb_farms / 1000000, 0)); rm(xx)

# ------------------------------------------------------------------------------
# keep only variables needed in the models
lsms_spatial <- lsms_spatial |>
  select(x, y, country, farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>
  na.omit() 

# Check correlation matrix to select relevant drivers

# Custom correlation panel with significance stars
my_cor_stars <- function(data, mapping, size = 5, digits = 2, ...) {
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)
  
  # Pearson correlation
  corr <- cor(x, y, use = "complete.obs")
  
  # Significance test
  test <- cor.test(x, y)
  pval <- test$p.value
  
  # Assign stars based on p-value
  stars <- symnum(pval, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
  
  # Combine correlation value with stars
  label <- paste0(round(corr, digits), stars)
  
  # Centered text output
  GGally::ggally_text(
    label = label,
    mapping = aes(),
    xP = 0.5, yP = 0.5, size = size, ...
  )
}

P00a <- lsms_spatial |>
  select(!c(x, y, country)) |>
  # sample_frac(0.001) |> # unmute only for quick processing!!!
  rename(Y = farm_area_ha, A = cropland, B = cattle, C = pop,  D = cropland_per_capita,
         E = sand, F = slope,  G = temperature, H = rainfall, I = maizeyield, J = market) |>
  GGally::ggpairs(upper = list(continuous = my_cor_stars), 
                  diag = list(continuous = GGally::wrap('densityDiag'))) +
  labs(title = 'A)') + 
  theme_test() +
  theme(
    strip.text.x.top = element_text(size = 14),
    strip.text.y.right = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(size = 10),
    title = element_text(size = 16),
    strip.background = element_rect(color = NA,  fill = 'white')
  )
P00a1 <- patchwork::wrap_elements(GGally::ggmatrix_gtable(P00a))
ggsave('../output/other_illustr/graphs/S03_panel_a1.png', P00a1, width = 9, height = 4.4, dpi = 150)
P00a2 <- magick::image_read('../output/other_illustr/graphs/S03_panel_a1.png')

P00b <- pred_cpland_df |>
  rename_all(toupper) |>
  rename(GEOS.2015 = GEOSURVEY2015) |>
  # sample_frac(0.001) |>  # unmute only for quick processing!!!
  select(!c(X, Y)) |>
  GGally::ggpairs(upper = list(continuous = my_cor_stars), 
                  diag = list(continuous = GGally::wrap('densityDiag'))) +
  labs(title  = 'B)') +
  theme_test() +
  theme(
    strip.text.x.top = element_text(size = 10),
    strip.text.y = element_text(size = 7.3),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(size = 10),
    title = element_text(size = 14),
    # plot.background = element_rect(
    #   color = "black",                       # Border color
    #   size = 1,                             # Border thickness
    #   fill = NA                             # No fill
    # ),
    strip.background = element_rect(color = NA,  fill = 'white')
  )
P00b1 <- patchwork::wrap_elements(GGally::ggmatrix_gtable(P00b))
ggsave('../output/other_illustr/graphs/S03_panel_b1.png', P00b1, width = 6, height = 4.4, dpi = 150)
P00b2 <- magick::image_read('../output/other_illustr/graphs/S03_panel_b1.png')

P00c <- ssa_cropland |>
  inner_join(ssa_nb_farms) |>
  ggplot(aes(source, total / 1000000)) + 
  geom_col() +
  scale_x_discrete(expand = c(0, 0)) +  # Minimal expansion
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(x = 'Source', y = 'Total cropland (million ha)') +
  geom_text(y = 60, aes(label = nb_rounded), size = 6, angle = -90, colour = 'white') +
  geom_text(x = 1, y = 460, label = 'C)', size = 7) +  
  theme_test() + 
  theme(axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 12),
        axis.ticks.x = element_blank(), 
        axis.text.x = element_text(angle = -90, hjust = 0.1, size = 12),
        aspect.ratio = NULL,
        plot.margin = margin(5, 15, 5, 10, 'pt')) 
ggsave('../output/other_illustr/graphs/S03_panel_c.png', P00c, width = 3, height =  4.4, dpi = 150)
P00c2 <- magick::image_read('../output/other_illustr/graphs/S03_panel_c.png')

# Read two images
img1 <- magick::image_read('../output/other_illustr/graphs/S03_panel_a1.png')
img2 <- magick::image_read('../output/other_illustr/graphs/S03_panel_b1.png')
img3 <- magick::image_read('../output/other_illustr/graphs/S03_panel_c.png')

# Convert to raster grobs
grob1 <- grid::rasterGrob(as.raster(img1), interpolate = TRUE)
grob2 <- grid::rasterGrob(as.raster(img2), interpolate = TRUE)
grob3 <- grid::rasterGrob(as.raster(img3), interpolate = TRUE)

# Wrap as patchwork elements
plot1 <- patchwork::wrap_elements(full = grob1)
plot2 <- patchwork::wrap_elements(full = grob2)
plot3 <- patchwork::wrap_elements(full = grob3)

# Combine using patchwork
P00d <- plot2 + plot3 + patchwork::plot_layout(ncol = 2, widths = c(2, 1))
P00e <- plot1 / P00d + patchwork::plot_layout(nrow = 2)
# P00d <- patchwork::wrap_plots((P00b1 + P00c), 
#                               nrow = 1, ncol = 2, widths = c(5, 0.5)) # did not work
  
P01 <-  patchwork::wrap_plots(patchwork::wrap_elements(GGally::ggmatrix_gtable(P00a)) / 
                                (patchwork::wrap_plots(patchwork::wrap_elements(GGally::ggmatrix_gtable(P00b)),
                                                      P00c) +  patchwork::plot_layout(ncol = 2, widths = c(2, 1))) +
                                patchwork::plot_layout(nrow =  2, widths = c(3, 2))) # ugly
ggsave('../output/other_illustr/graphs/Suppl.Fig03.png', P00e , height = 9, width = 8.8, units = 'in', dpi = 300)

# ── Report ────────────────────────────────────────────────────────────────────
elapsed <- proc.time()[["elapsed"]] - t0
write_report(
  "S03_aggregate_vs_disaggregate.R",
  "Supp Fig 3: country vs GADM1 aggregation comparison",
  outputs = list("PNG" = "../output/other_illustr/graphs/Suppl.Fig03.png"),
  elapsed_sec = elapsed
)
message("S03_aggregate_vs_disaggregate.R done in ", round(elapsed,1), "s")
