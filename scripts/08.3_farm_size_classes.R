# ==============================================================================
# Script: 08.3_farm_size_classes.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Classify farms by size category per country and compare with census data
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - February 2026
# ==============================================================================


require(tidyverse)

# Clean environment
rm(list=ls())

# Set working directory
setwd(paste0(here::here(), '/scripts'))
dir.create('../output/other_illustr/maps', recursive = TRUE, showWarnings = FALSE)
dir.create('../output/other_illustr/graphs', recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Preparation for functions and mapping
input_path <- '../data/raw/spatial'
country <- geodata::world(path=input_path, resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='Réunion' & NAME!='Saint Helena' & NAME!='São Tomé and Príncipe' & NAME!='Seychelles') # keep the mainland + Madagascar only, remove islands
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)
pal <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))
pal2 <- colorRampPalette(c('#c6dbef','#6baed6','#3182bd', '#08519c', '#08306b'))
pal3 <- colorRampPalette(c('skyblue1', 'blue4'))

# force terra to use disk-based processing and 20% of RAM (Use this if R crashes because of limited memory)
gc()
# terra::terraOptions(memfrac = 0.2, todisk = T, verbose = F)

# ------------------------------------------------------------------------------
# theor_farms and theor_farms_application are produced by 08.2 — just read them here
xx <- readRDS('../data/processed/fsize_distribution_resample_long.rds')
theor_farms             <- xx$theor_farms
theor_farms_application <- xx$theor_farms_application; rm(xx)

# theor_farms_rast: saved by 08.2 (or synthetic stub) — read directly
# avoids rebuilding from theor_farms which may be empty in CI
theor_farms_rast <- tryCatch(
  terra::rast('../data/processed/farm_size_distribution_parms.tif'),
  error = function(e) { message('CI: farm_size_distribution_parms.tif not found'); NULL }
)


# ------------------------------------------------------------------------------
gc()

# define a threshold to see percentage farms below it (here 0.5, 1, and 2ha)
map_at_threshold <- function(thres) {
  share_farms_below_threshold <- theor_farms_application |>
    group_by(x, y) |>
    summarize(nb_farms_below = sum(linear_farm_size_ha < thres, na.rm = TRUE) / n(),
              .groups = 'drop')
  thr_rast <- tryCatch(terra::rast(share_farms_below_threshold),
                       error = function(e) { message('CI: rast failed for thres=', thres); NULL })
  assign(paste0('share_farms_below_', thres), share_farms_below_threshold, envir = .GlobalEnv)
  if (!is.null(thr_rast))
    terra::writeRaster(thr_rast, file = paste0('../data/processed/prop_farms_below_', thres, '.tif'), overwrite = TRUE)
}
sapply(c(0.5, 1, 2), map_at_threshold)
# e.g. %tage of farms below 0.5, 1, and 2 ha across SSA
nrow(share_farms_below_0.5) / nrow(theor_farms_application)
nrow(share_farms_below_1) / nrow(theor_farms_application)
nrow(share_farms_below_2) / nrow(theor_farms_application)


# # cannot allocate vector of size 532.6 Mb in the following lines, now trying out profvis!
# profvis::profvis({
#   # my code
# })


P00 <- ggplot(theor_farms_application, aes(linear_farm_size_ha)) +
  stat_ecdf(geom = 'line', linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15)) +
  labs(x= 'Predicted average farm size, ha', y = 'ECDF',
       title = 'SSA') +
  theme_test()
P00
png(paste0('../output/other_illustr/africa_ECDF_farm_sizes.png'), height = 5, width = 7.5, units = 'in', res = 600)
P00
ggsave(paste0('../output/other_illustr/africa_ECDF_farm_sizes.png'))
dev.off()

# ------------------------------------------------------------------------------
# regular sampling over grid cells (farms)
# Option A (production): derive coords from raster extent — use lines below
# Option B (CI/stub): use fixed SSA bounding box — comment out A and uncomment B
n_grid <- 15 # distance (arbitrary) between points to select. Increase n_grid to reduce the number of points
# x_coords <- seq(floor(terra::ext(theor_farms_application_rast))[1], floor(terra::ext(theor_farms_application_rast)[2]), n_grid)
# y_coords <- seq(floor(terra::ext(theor_farms_application_rast))[3], floor(terra::ext(theor_farms_application_rast))[4], n_grid)
x_coords <- seq(-10, 40, 10); y_coords <- seq(-30, 10, 10)
regular_sample_coords <- expand.grid(x = x_coords, y = y_coords)

# theor_farms_application is already unnested (linear_farm_size_ha per farm row)
# Filter to the regular sample coordinates directly — no unnest needed
selected_theor_app <- theor_farms_application |>
  ungroup() |>
  filter(round(x, 1) %in% round(regular_sample_coords$x, 1),
         round(y, 1) %in% round(regular_sample_coords$y, 1)) |>
  mutate(unique_coords = paste0(round(x, 0), '_', round(y, 0))) |>
  distinct(unique_coords, .keep_all = TRUE)

# theor_farms |>
#   filter(round(x, 1) %in% round(regular_sample_coords$x, 1),
#          round(y, 1) %in% round(regular_sample_coords$y, 1)) |>
#   distinct(paste0(round(x, 0), '_', round(y, 0)), .keep_all = T) |>
#   View() # 11 grid cells were picked

P01 <- ggplot(selected_theor_app, aes(linear_farm_size_ha, colour = paste0(x, ', ', y))) + 
  stat_ecdf(geom = 'line', linewidth = 0.8) +
  stat_ecdf(data = theor_farms_application, aes(x = linear_farm_size_ha), geom = 'line', linewidth = 1.2, colour = 'red4') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15)) +
  labs(x= 'Predicted individual farm sizes per grid cell, ha', y = 'ECDF',
       title = NULL, colour = 'coordinates (x, y)') +
  scale_colour_manual(values = pal3(15)[3:14]) +
  theme_test() +
  theme(legend.position = c(0.8, 0.35))
P01
png(paste0('../output/other_illustr/africa_ECDF_reg_sampl10_farm_sizes.png'), height = 5, width = 7.5, units = 'in', res = 600)
P01
ggsave(paste0('../output/other_illustr/africa_ECDF_reg_sampl10_farm_sizes.png'))
dev.off()

# ------------------------------------------------------------------------------
gc()
grouped_theor_app <- theor_farms |>
  ungroup() |>
  mutate(avg = unlist(map(pred_farm_sizes, \(x) unlist(mean(x, na.rm = T)))),
         avg_grp = case_when(avg <= 0.5 ~ '< 0.5 ha',
                             avg > 0.5  & avg <= 1 ~ '0.5 - 1 ha',
                             avg > 1  & avg <= 2 ~ '1 - 2 ha',
                             avg > 2  & avg <= 5 ~ '2 - 5 ha',
                             avg > 5 ~ '> 5 ha',
                             .default = NA)) |>
  filter(avg_grp %in% c('< 0.5 ha', '1 - 2 ha', '> 5 ha')) |>
  group_by(x, y, avg_grp) |>
  unnest_longer(pred_farm_sizes)
grouped_theor_app$avg_grp <- factor(grouped_theor_app$avg_grp, levels = c('< 0.5 ha', '1 - 2 ha', '> 5 ha') )
  

grp_avg_theor_app <- grouped_theor_app |>
  ungroup() |>
  group_by(avg_grp, pred_farm_sizes_id) |>
  summarize(min = min(pred_farm_sizes, na.rm = T), 
            central = mean(pred_farm_sizes, na.rm = T), 
            max = max(pred_farm_sizes, na.rm = T)) |>
  mutate(pred_farm_sizes_id = as.integer(gsub('qrf_q', '', pred_farm_sizes_id)))

gc()
P02 <- ggplot() +
  stat_ecdf(data = grouped_theor_app, 
            geom = 'line', aes(pred_farm_sizes , colour = avg_grp, group = cell), 
            linewidth = 0.2, alpha = 0.005) +
  stat_ecdf(data = grp_avg_theor_app, 
            geom = 'line', aes(central , colour = avg_grp), linewidth = 0.8) +
  labs(x= 'Predicted individual farm sizes per grid cell, ha', y = 'ECDF',
       title = NULL, colour = 'Farm size class') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_colour_manual(values = c('#8B0000', '#FFCB00', '#489228')) +
  theme_test() +
  theme(legend.position = c(0.8, 0.35))
P02
png(paste0('../output/other_illustr/africa_ECDF_3selected_groups_ugly.png'), height = 5, width = 7.5, units = 'in', res = 600)
P02
ggsave(paste0('../output/other_illustr/africa_ECDF_3selected_groups_ugly.png'))
dev.off()

P02_data <- ggplot_build(P02)$data
P02_ribbon <- P02_data[[1]] |>
  filter(!is.infinite(x)) |>
  mutate(farm_class = case_when(colour == '#8B0000' ~ '< 0.5 ha',
                                colour == '#FFCB00' ~ '1 - 2 ha',
                                colour == '#489228' ~ '> 5 ha',
                                .default = NA)) |>
  group_by(farm_class, x) |>
  summarize(min = min(y, na.rm = T), max = max(y, na.rm = T))
P02_central <- P02_data[[2]] |>
  filter(!is.infinite(x)) |>
  mutate(farm_class = case_when(colour == '#8B0000' ~ '< 0.5 ha',
                                colour == '#FFCB00' ~ '1 - 2 ha',
                                colour == '#489228' ~ '> 5 ha',
                                .default = NA)) |>
  select(x, y, farm_class)
P02_failed <- ggplot() +
  geom_ribbon(data = P02_ribbon |>
                mutate(x = round(x, 3)) |>
                group_by(farm_class, x) |>
                summarize(min = mean(min, na.rm = T), max = mean(max, na.rm = T)),
              aes(x = x, ymin = min, ymax = max, fill = farm_class), alpha = 0.05) +
  geom_line(data = P02_central |>
              mutate(x = round(x, 3)) |>
              group_by(farm_class, x) |>
              summarize(y = mean(y, na.rm = T)),
            aes(x = x, y = y, colour = farm_class), linewidth = 0.8) +
  labs(x= 'Predicted individual farm sizes per grid cell, ha', y = 'ECDF',
       title = NULL, colour = 'Farm size class') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_fill_manual(values = c('#8B0000', '#FFCB00', '#489228')) +
  scale_colour_manual(values = c('#8B0000', '#FFCB00', '#489228')) +
  theme_test() +
  theme(legend.position = c(0.8, 0.35))
P02_failed
png(paste0('../output/other_illustr/africa_ECDF_3selected_groups_of_farm_sizes.png'), height = 5, width = 7.5, units = 'in', res = 600)
P02_failed
ggsave(paste0('../output/other_illustr/africa_ECDF_3selected_groups_of_farm_sizes.png'))
dev.off()

saveRDS(list(grouped_theor_app = grouped_theor_app, grp_avg_theor_app = grp_avg_theor_app, P02 = P02),
        file = '../output/plot_data/ECDF_3groups.rds')

# --------------------------------------------------------------------------
# maps
png('../output/other_illustr/maps/predicted_lognormal_mean_farm_size_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Location parameter (μ) of log-normal distribution \nfitted to farm sizes  across SSA',
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(theor_farms_rast$adjusted_logn_mean, col = terrain.colors(100), cex = 1, axes  =  F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

png('../output/other_illustr/maps/predicted_lognormal_sd_farm_size_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Dispersion parameter (σ) of log-normal distribution \nfitted to farm sizes across SSA',
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(theor_farms_rast$adjusted_logn_sd, col = terrain.colors(100), cex = 1, axes  =  F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

png('../output/other_illustr/maps/predicted_lognormal_goodness_of_fit_farm_size_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = "Kolmogorov's D for a log-normal distribution \nfitted to farm sizes across SSA",
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(theor_farms_rast$ks_trunc_D, col = terrain.colors(100), cex = 1, axes  =  F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

mask_forest_ssa <- terra::rast('../data/processed/mask_forest_ssa.tif')
mask_drylands_ssa <- terra::rast('../data/processed/mask_drylands_ssa.tif')
back_transformed_trunc_adj_mean <- exp(theor_farms_rast$adjusted_logn_mean + (theor_farms_rast$adjusted_logn_sd / 2))
back_transformed_trunc_adj_mean <- back_transformed_trunc_adj_mean * terra::resample(mask_forest_ssa, back_transformed_trunc_adj_mean)
back_transformed_trunc_adj_mean <- back_transformed_trunc_adj_mean * terra::resample(mask_drylands_ssa, back_transformed_trunc_adj_mean)
terra::writeRaster(back_transformed_trunc_adj_mean, file = '../data/processed/back_transf_trunc_adj_mean.tif', overwrite = T)

png('../output/other_illustr/maps/predicted_back_transf_trunc_lognormal_mean_farm_size_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Back-transformed mean of truncated log-normal \ndistribution fitted to farm sizes  across SSA',
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(back_transformed_trunc_adj_mean,  col = rev(terrain.colors(100)), cex = 1, axes  =  F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

png('../output/other_illustr/maps/predicted_back_transf_trunc_lognormal_mean_farm_size_class_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Back-transformed predicted average farm sizes  across SSA',
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(back_transformed_trunc_adj_mean,  breaks = c(0, 0.5, 1, 1.5, 2, 5, Inf), col = rev(pal(6)), legend = F, cex = 1, axes = F, add = T)
legend(-15, -10, bty = 'y', cex = 0.7, ncol = 1, box.col = "white",
       title = "Farm size", legend = c('< 0.5 ha', '0.5 - 1 ha', '1 - 1.5 ha', '1.5 - 2 ha', '2 - 5 ha', '> 5 ha'),
       fill = pal(6), horiz = FALSE)
terra::plot(ssa, axes = F, add = T)
dev.off()

back_transformed_trunc_adj_sd <- sqrt((exp(theor_farms_rast$adjusted_logn_sd^2) - 1) * exp(2 * theor_farms_rast$adjusted_logn_mean + theor_farms_rast$adjusted_logn_sd^2))
back_transformed_trunc_adj_sd <- back_transformed_trunc_adj_sd * terra::resample(mask_forest_ssa, back_transformed_trunc_adj_sd)
back_transformed_trunc_adj_sd <- back_transformed_trunc_adj_sd * terra::resample(mask_drylands_ssa, back_transformed_trunc_adj_sd)
terra::writeRaster(back_transformed_trunc_adj_sd, file = '../data/processed/back_transf_trunc_adj_sd.tif', overwrite = T)
png('../output/other_illustr/maps/predicted_back_transf_trunc_lognormal_sd_farm_size_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Back-transformed standard deviation of log-normal \ndistribution fitted to farm sizes across SSA',
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(back_transformed_trunc_adj_sd, col = terrain.colors(100), cex = 1, axes  =  F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

png('../output/other_illustr/maps/predicted_back_transf_trunc_lognormal_sd_farm_size_class_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Back-transformed standard deviation \nof farm sizes across SSA',
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(back_transformed_trunc_adj_sd, breaks = c(0, 0.5, 1, 2, 5, 10, Inf), col = pal(6), legend = F, cex = 1, axes  =  F, add = T)
legend(-18, -10, bty = 'y', cex = 0.7, ncol = 1, box.col = "white",
       title = "Range of standard deviation",
       legend = c('< 0.5 ha', '0.5 - 1 ha', '1 - 2 ha', '2 - 5 ha', '5 - 10 ha', '> 10 ha'),
       fill = pal(6), horiz = F)
terra::plot(ssa, axes = F, add = T)
dev.off()

sk <- tryCatch(terra::rast(theor_farms |> ungroup() |> select(x, y, skew)), error = function(e) { message('CI: skew rast skipped'); NULL })
terra::writeRaster(sk, file = '../data/processed/skewness.tif', overwrite = T)
png('../output/other_illustr/maps/predicted_skeweness_farm_size_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = "Skewness of the local farm size distribution \nacross sub-Saharan Africa",
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(sk, col = terrain.colors(100), cex = 1, axes  =  F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

png('../output/other_illustr/maps/predicted_skeweness_farm_size_class_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Skewness of the local farm size distribution \nacross sub-Saharan Africa',
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(sk, breaks = c(0, 0.5, 1, 2, 5, 10, Inf), col = pal(6), legend = F, cex = 1, axes  =  F, add = T)
legend(-18, -10, bty = 'y', cex = 0.7, ncol = 1, box.col = "white",
       title = "Range of standard deviation",
       legend = c('< 0.5 ha', '0.5 - 1 ha', '1 - 2 ha', '2 - 5 ha', '5 - 10 ha', '> 10 ha'),
       fill = pal(6), horiz = F)
terra::plot(ssa, axes = F, add = T)
dev.off()

share_farms_below_0.5_ha <- 100 * terra::rast('../data/processed/prop_farms_below_0.5.tif')
share_farms_below_0.5_ha <- share_farms_below_0.5_ha * terra::resample(mask_forest_ssa, share_farms_below_0.5_ha)
share_farms_below_0.5_ha <- share_farms_below_0.5_ha * terra::resample(mask_drylands_ssa, share_farms_below_0.5_ha)

png('../output/other_illustr/maps/share_farms_below_0.5_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Percentage of farms below 0.5 ha across SSA',
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(100 * terra::rast('../data/processed/prop_farms_below_0.5.tif'), 
            col = terrain.colors(100), cex = 1, axes  =  F, add = T)
terra::plot(ssa, axes = F, add = T)
dev.off()

png('../output/other_illustr/maps/share_farms_below_0.5_classes_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Percentage of farms below 0.5 ha across SSA',
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(share_farms_below_0.5_ha, breaks=c(0, 20, 40, 60, 80, Inf), col = pal(5), legend = F, cex = 1, axes =  F, add = T)
legend(-18, -10, bty='y', cex=0.7, ncol=1, box.col="white",
       title = NULL,
       legend = c('< 20 %', '20 - 40 %', '40 - 60 %', '60 - 80 %',  '> 80 %'),
       fill=pal(5), horiz=F)
terra::plot(ssa, axes = F, add = T)
dev.off()

share_farms_below_1_ha <- 100 * terra::rast('../data/processed/prop_farms_below_1.tif')
share_farms_below_1_ha <- share_farms_below_1_ha * terra::resample(mask_forest_ssa, share_farms_below_1_ha)
share_farms_below_1_ha <- share_farms_below_1_ha * terra::resample(mask_drylands_ssa, share_farms_below_1_ha)

png('../output/other_illustr/maps/share_farms_below_1_classes_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Percentage of farms below 1 ha across SSA',
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(share_farms_below_1_ha, breaks=c(0, 20, 40, 60, 80, Inf), col = pal(5), legend = F, cex = 1, axes =  F, add = T)
legend(-18, -10, bty='y', cex=0.7, ncol=1, box.col="white",
       title = NULL,
       legend = c('< 20 %', '20 - 40 %', '40 - 60 %', '60 - 80 %',  '> 80 %'),
       fill=pal(5), horiz=F)
terra::plot(ssa, axes = F, add = T)
dev.off()

share_farms_below_2_ha <- 100 * terra::rast('../data/processed/prop_farms_below_2.tif')
share_farms_below_2_ha <- share_farms_below_2_ha * terra::resample(mask_forest_ssa, share_farms_below_2_ha)
share_farms_below_2_ha <- share_farms_below_2_ha * terra::resample(mask_drylands_ssa, share_farms_below_2_ha)

png('../output/other_illustr/maps/share_farms_below_2_classes_africa.png', units = "in", width = 5.5, height = 5.5, res = 1000)
terra::plot(ssa, col = 'azure', main = 'Percentage of farms below 2 ha across SSA',
            panel.first = grid(col = "gray", lty = "solid"), pax = list(cex.axis = 1.4), mar  =  c(5, 4, 4, 3.5))
terra::plot(share_farms_below_2_ha, breaks=c(0, 20, 40, 60, 80, Inf), col = pal(5), legend = F, cex = 1, axes =  F, add = T)
legend(-18, -10, bty='y', cex=0.7, ncol=1, box.col="white",
       title = NULL,
       legend = c('< 20 %', '20 - 40 %', '40 - 60 %', '60 - 80 %',  '> 80 %'),
       fill=pal(5), horiz=F)
terra::plot(ssa, axes = F, add = T)
dev.off()

# pal <- colorRampPalette(c('grey98', 'purple4'))
P00 <- ggplot(theor_farms, aes(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)),
                               sqrt((exp(adjusted_logn_sd^2) - 1) * exp(2 * adjusted_logn_mean + adjusted_logn_sd^2)))) +
  geom_point(alpha = 0.05, colour = 'skyblue1') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20)) +
  labs(x= 'Predicted average farm size, ha', y = 'Predicted standard deviation of farm size, ha',
       title = 'SSA') +
  theme_test()
png(paste0('../output/other_illustr/africa_predicted_mean_sd_pts.png'), height = 5, width = 7.5, units = 'in', res = 600)
P00
ggsave(paste0('../output/other_illustr/africa_predicted_mean_sd_pts.png'))
dev.off()

P01 <- ggplot(theor_farms, aes(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)),
                               sqrt((exp(adjusted_logn_sd^2) - 1) * exp(2 * adjusted_logn_mean + adjusted_logn_sd^2)))) +
  geom_density_2d_filled(bins = 10) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_fill_manual(values = pal(10)) +
  labs(x= 'Predicted average farm size, ha', y = 'Predicted standard deviation of farm size, ha',
       title = 'SSA', fill = 'Density of datapoints') +
  theme_test()
png(paste0('../output/other_illustr/africa_predicted_mean_sd.png'), height = 5, width = 7.5, units = 'in', res = 600)
P01
ggsave(paste0('../output/other_illustr/africa_predicted_mean_sd.png'))
dev.off()

P02 <- ggplot(theor_farms, aes(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)),
                               sqrt((exp(adjusted_logn_sd^2) - 1) * exp(2 * adjusted_logn_mean + adjusted_logn_sd^2)) / exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)) )) +
  geom_density_2d_filled(bins = 10) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_fill_manual(values = pal(10)) +
  labs(x= 'Predicted average farm size, ha', y = 'Predicted CV of farm sizes, ha',
       title = 'SSA', fill = 'Density of datapoints') +
  theme_test()
png(paste0('../output/other_illustr/africa_predicted_mean_cv.png'), height = 5, width = 7.5, units = 'in', res = 600)
P02
ggsave(paste0('../output/other_illustr/africa_predicted_mean_cv.png'))
dev.off() 

P03 <- ggplot(theor_farms |>
                mutate(gini = unlist(map(fitted_trunc_logn, function(x) ineq::Gini(unlist(x), na.rm = T)))),
              aes(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)), gini)) +
  geom_point(colour = 'skyblue1') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_fill_manual(values = pal(10)) +
  labs(x= 'Predicted average farm size per grid cell, ha', y = 'Gini coefficient of predicted farm sizes per grid cell',
       title = 'SSA', fill = 'Density of datapoints') +
  theme_test()
png(paste0('../output/other_illustr/africa_predicted_mean_gini.png'), height = 5, width = 7.5, units = 'in', res = 600)
P03
ggsave(paste0('../output/other_illustr/africa_predicted_mean_gini.png'))
dev.off()

# Actual mean and SD truncated to the next 0.1 deg (which is about 10 by 10 km at the eq), with at least 10 datapoints
lsms_spatial <- readRDS('../data/processed/lsms_trimmed_95th_africa.rds')
qrf_model_predictions <- terra::rast('../data/processed/qrf_100quantiles_predictions_africa.tif')
names(qrf_model_predictions) <- paste0('qrf_q', sprintf('%03g', 1:100))
my_points <- terra::as.data.frame(qrf_model_predictions, xy = T)
my_points_cells <- bind_cols(
  cell = terra::cellFromXY(terra::rast(my_points), my_points[, c('x', 'y')]),
  my_points)
cell_id <- as_tibble(terra::cellFromXY(terra::rast(my_points), lsms_spatial[, c('x', 'y')]))
lsms_mean_sd <- lsms_spatial |>
  bind_cols(cell_id |>
              rename(cell = value)) |>
  group_by(cell) |>
  summarize(x = list(x), y = list(y), 
            mean = mean(farm_area_ha, na.rm = T), 
            sd = sd(farm_area_ha, na.rm = T), 
            gini = ineq::Gini(farm_area_ha, na.rm = T),
            nb = n()) |>
  filter(nb > 9)

farm_size_triangle <- cbind.data.frame(x = c(0.1, 0.1, 9.9), y = c(0.15, 0.8, 0.15))
fit.rq <- quantreg::rq(gini ~ mean, lsms_mean_sd, tau = c(0.01, 0.99) )
int1 <- fit.rq$coefficients[1]
slop1 <- fit.rq$coefficients[2]
int2 <- fit.rq$coefficients[3]
slop2 <- fit.rq$coefficients[4]

P02a <- ggplot(lsms_mean_sd ,
               aes(mean, sd)) +
  geom_point() + 
  geom_point(data = theor_farms,
             aes(exp(logn_mean + (logn_sd^2 / 2)),
                 sqrt((exp(logn_sd^2) - 1) * exp(2 * logn_mean + logn_sd^2))),
             alpha = 0.05, colour = 'skyblue1') +
  geom_point(data = theor_farms,
             aes(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)),
                 sqrt((exp(adjusted_logn_sd^2) - 1) * exp(2 * adjusted_logn_mean + adjusted_logn_sd^2))),
             alpha = 0.03, colour = 'lightgreen') +
  geom_point(data = theor_farms,
             aes(as.numeric(map(pred_farm_sizes, function(x) mean(unlist(x), na.rm =T))),
                 as.numeric(map(pred_farm_sizes, function(x) sd(unlist(x), na.rm =T))) ),
             alpha = 0.01, colour = 'pink') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
  labs(x = 'Average farm size, ha', y = 'Standard deviation of farm sizes, ha') +
  theme_test()
P02a
png(paste0('../output/other_illustr/africa_log_vs_trunc_mean_sd.png'), height = 5, width = 7.5, units = 'in', res = 600)
P02a
ggsave(paste0('../output/other_illustr/africa_log_vs_trunc_mean_sd.png'))
dev.off()

P02b <- ggplot(lsms_mean_sd ,
               aes(mean, sd)) +
  geom_point() + 
  geom_point(data = theor_farms,
             aes(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)),
                 sqrt((exp(adjusted_logn_sd^2) - 1) * exp(2 * adjusted_logn_mean + adjusted_logn_sd^2))),
             alpha = 0.05, colour = 'skyblue1') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
  labs(x = 'Average farm size, ha', y = 'Standard deviation of farm sizes, ha') +
  theme_test()
png(paste0('../output/other_illustr/africa_actual_vs_predicted_mean_sd.png'), height = 5, width = 7.5, units = 'in', res = 600)
P02b
ggsave(paste0('../output/other_illustr/africa_actual_vs_predicted_mean_sd.png'))
dev.off()

P02c <- ggplot(lsms_mean_sd ,
               aes(mean, 100 * sd / mean)) +
  geom_point() + 
  geom_point(data = theor_farms,
             aes(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)),
                 100 * sqrt((exp(adjusted_logn_sd^2) - 1) * exp(2 * adjusted_logn_mean + adjusted_logn_sd^2)) / exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)) ),
             alpha = 0.05, colour = 'skyblue1') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  labs(x = 'Average farm size, ha', y = 'Coefficient of variation of farm sizes (%)') +
  theme_test()
png(paste0('../output/other_illustr/africa_actual_vs_predicted_mean_cv.png'), height = 5, width = 7.5, units = 'in', res = 600)
P02c
ggsave(paste0('../output/other_illustr/africa_actual_vs_predicted_mean_cv.png'))
dev.off()

P02d <- ggplot(lsms_mean_sd, aes(mean, gini)) +
  geom_point() + 
  geom_point(data = theor_farms |>
               mutate(gini = unlist(map(fitted_trunc_logn, function(x) ineq::Gini(unlist(x), na.rm = T)))),
             aes(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)), gini),
             alpha = 0.05, colour = 'skyblue1') +
  geom_polygon(data = farm_size_triangle, aes(x = x, y = y), group = 1,
               colour = 'red4', fill = NA, linewidth = 1.2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x = 'Average farm size per grid cell, ha', y = 'Gini coefficient for farm sizes per grid cell') +
  theme_test()
P02d

P02d <- ggplot(lsms_mean_sd, aes(mean, gini)) +
  geom_point() + 
  geom_point(data = theor_farms |>
               mutate(gini = unlist(map(fitted_trunc_logn, function(x) ineq::Gini(unlist(x), na.rm = T)))),
             aes(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)), gini),
             alpha = 0.05, colour = 'skyblue1') +
  # geom_polygon(data = farm_size_triangle, aes(x = x, y = y), group = 1, 
  #              colour = 'red4', fill = NA, linewidth = 1.2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x = 'Average farm size per grid cell, ha', y = 'Gini coefficient for farm sizes per grid cell') +
  theme_test()
P02d
png(paste0('../output/other_illustr/africa_actual_vs_predicted_mean_gini_to_improve.png'), height = 5, width = 7.5, units = 'in', res = 600)
P02d
ggsave(paste0('../output/other_illustr/africa_actual_vs_predicted_mean_gini_to_improve.png'))
dev.off()

P02e <- P02d +
  geom_abline(intercept = int2, slope = slop2, colour = 'red4', linewidth = 1.2) + 
  geom_abline(intercept = int1, slope = slop1, colour = 'red4', linewidth = 1.2)
P02e

# with linear reg
theor_bound_line <- theor_farms |>
  mutate(avg = unlist(map(pred_farm_sizes, \(x) mean(unlist(x), na.rm = T))),
         gini = unlist(map(fitted_trunc_logn, function(x) ineq::Gini(unlist(x), na.rm = T)))) |>
  select(x, y, avg, gini)
avg_bin <- seq(0, 10, 0.1)
avg_bin_table <- tibble()

for(i in seq_along(avg_bin)[-1]){
  sel1 <- theor_bound_line |>
    ungroup() |>
    filter(avg > avg_bin[i -1], avg <= avg_bin[i]) |>
    arrange(desc(gini)) |>
    slice_head(n = 10)
  one_row <- tibble(bin = avg_bin[i], avg = sel1$avg, gini = sel1$gini)
  avg_bin_table <- bind_rows(avg_bin_table, one_row); rm(sel1, one_row)
}
lm.fit <- lm(gini ~ avg, avg_bin_table)
int3 <- lm.fit$coefficients[1]
int3
slop3 <- lm.fit$coefficients[2]
slop3

P02f <- ggplot(theor_bound_line, aes(avg, gini)) +
  geom_density_2d_filled(bins = 10) +
  geom_point(data = lsms_mean_sd,
             aes(x = mean, y = gini), colour = 'grey60', alpha = 0.03) + 
  geom_abline(intercept = int3, slope = slop3, colour = 'red4', linewidth = 1.2) +
  geom_hline(yintercept = 0.25,  colour = 'red4', linewidth = 1.2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 16)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_fill_manual(values = c('white', pal2(10)[-1])) +
  labs(x = 'Average farm size per grid cell, ha', y = 'Gini coefficient for farm sizes per grid cell') +
  theme_test() + 
  theme(legend.position = 'none')
P02f
png(paste0('../output/other_illustr/africa_actual_vs_predicted_mean_gini_with_bound_line.png'), height = 5, width = 7.5, units = 'in', res = 600)
P02f
ggsave(paste0('../output/other_illustr/africa_actual_vs_predicted_mean_gini_with_bound_line.png'))
dev.off()

P02g <- ggplot(theor_bound_line, aes(avg, gini)) +
  geom_point(colour = 'skyblue1') +
  geom_point(data = lsms_mean_sd,
             aes(x = mean, y = gini), colour = 'grey60', alpha = 0.05) + 
  geom_abline(intercept = int3, slope = slop3, colour = 'red4', linewidth = 1.2) +
  geom_hline(yintercept = 0.25,  colour = 'red4', linewidth = 1.2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 16)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_fill_manual(values = pal2(10)) +
  labs(x = 'Average farm size per grid cell, ha', y = 'Gini coefficient for farm sizes per grid cell') +
  theme_test()
P02g
png(paste0('../output/other_illustr/africa_actual_vs_predicted_mean_gini_with_boundline_pts.png'), height = 5, width = 7.5, units = 'in', res = 600)
P02g
ggsave(paste0('../output/other_illustr/africa_actual_vs_predicted_mean_gini_with_boundline_pts.png'))
dev.off()

saveRDS(list(theor_bound_line = theor_bound_line, lsms_mean_sd = lsms_mean_sd, 
             int3 = int3, slop3 = slop3, P02f = P02f, P02g = P02g),
        file = '../output/plot_data/gini_triangle.rds')
# #--------------------------------------------------------------
# # Compare selected grid cells with only large farms against grid cells with mixed farm sizes
# lsms_mean_sd |>
#   filter(mean > 7.5, mean < 8.5, sd < 3.2)
# large_farms_lsms <- terra::crds(lsms_vect) |>
#   bind_cols(terra::as.data.frame(lsms_vect)) |>
#   bind_cols(cell_id |>
#               rename(cell = value)) |>
#   filter(cell %in% c(57701, 58513, 60947, 60136) ) # gadm_4 == 'Toukoroba' corresponds to cells c(57701, 58513, 60947, 60136); but 58513 was selected
# range(large_farms_lsms$x)
# range(large_farms_lsms$y)
# large_farms_pred <- theor_farms |>
#   filter(cell %in% c(57701, 58513, 60947, 60136)) 
# 
# lsms_mean_sd |>
#   filter(mean > 7.5, mean < 8.5, sd > 7)
# mixed_farms_lsms <- terra::crds(lsms_vect) |>
#   bind_cols(terra::as.data.frame(lsms_vect)) |>
#   bind_cols(cell_id |>
#               rename(cell = value)) |>
#   filter(cell %in% c(95810, 93378, 95811, 95001, 95813) ) #gadm_4 == 'Minignan' corresponds to cells c(95810, 93378, 95811, 95001, 95813); 95811
# range(mixed_farms_lsms$x)
# range(mixed_farms_lsms$y)
# mixed_farms_pred <- theor_farms |>
#   filter(cell %in% c(95810, 93378, 95811, 95001, 95813))
# 
# lsms_mean_sd |>
#   filter( mean < 0.5)
# small_farms_lsms <- terra::crds(lsms_vect) |>
#   bind_cols(terra::as.data.frame(lsms_vect)) |>
#   bind_cols(cell_id |>
#               rename(cell = value)) |>
#   filter(cell %in% c(128792, 129605, 128794)) # gadm_3 == 'Kucha corresponds to cells c(128792, 129605, 128794); 128794
# 
# range(small_farms_lsms$x)
# range(small_farms_lsms$y)
# small_farms_pred <- theor_farms |>
#   filter(cell %in% c(128792, 129605, 128794))
# 
# plot_farm_sizes <- bind_rows(
#   data_frame(farm_data = 'actual', farm_type = 'Toukoroba, Mali', farm_area_ha = large_farms_lsms$farm_area_), # large farms, small variation
#   data_frame(farm_data = 'actual', farm_type = 'Minigna', farm_area_ha = mixed_farms_lsms$farm_area_), # large farms , large variation
#   data_frame(farm_data = 'actual', farm_type = 'small', farm_area_ha = small_farms_lsms$farm_area_), # small farms, small variation
#   
#   data_frame(farm_data = 'log predicted', farm_type = 'large', farm_area_ha = unlist(large_farms_pred$fitted_trunc_logn)),
#   data_frame(farm_data = 'log predicted', farm_type = 'mixed', farm_area_ha = unlist(mixed_farms_pred$fitted_trunc_logn)),
#   data_frame(farm_data = 'log predicted', farm_type = 'small', farm_area_ha = unlist(small_farms_pred$fitted_trunc_logn)) ) |>
#   arrange(farm_data, farm_type, farm_area_ha)
# 
# mean_actual <- plot_farm_sizes |>
#   filter(farm_data == 'actual') |> 
#   group_by(farm_type) |>
#   summarize(mean_ha = mean(farm_area_ha, na.rm = T))
# P03 <- ggplot() +
#   stat_ecdf(data = plot_farm_sizes |> 
#               filter(farm_data == 'actual'), 
#             aes(x = farm_area_ha), 
#             geom = 'point') +
#   stat_ecdf(data = plot_farm_sizes |> 
#               filter(farm_data != 'actual'),  
#             aes(x = farm_area_ha),
#             geom = 'line', linewidth = 0.8, colour = 'skyblue1') +
#   geom_vline(data = mean_actual, aes(xintercept = mean_ha), colour = 'red4', linetype = 2) + 
#   labs(x = 'Farm size, ha', y = 'Cumulative distribution function') +
#   facet_grid( ~ farm_type, scales = 'free') +
#   theme_test()
# newtable_a <- ggplot_build(P03)
# newECDFtable1 <- as.data.frame(newtable_a$data[[1]]) |>
#   filter(is.finite(x))
# newtable_a$data[[1]] <- newECDFtable1
# newtable_a <- ggplot_gtable(newtable_a)
# P03 <- ggpubr::as_ggplot(newtable_a)
# P03
# png(paste0('../output/other_illustr/africa_selected_ecdf.png'), height = 5, width = 7.5, units = 'in', res = 600)
# P03
# ggsave(paste0('../output/other_illustr/africa_selected_ecdf.png'))
# dev.off()


# small_farms <- theor_farms |>
#   filter(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)) < 1)
# large_farms <- theor_farms |>
#   filter(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)) > 2.5, # &  exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)) < 3.5 ,
#          sqrt((exp(adjusted_logn_sd^2) - 1) * exp(2 * adjusted_logn_mean + adjusted_logn_sd^2)) < 3)
# mixed_farms <- theor_farms |>
#   filter(exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)) > 2.5, # &  exp(adjusted_logn_mean + (adjusted_logn_sd^2 / 2)) < 3.5 ,
#          sqrt((exp(adjusted_logn_sd^2) - 1) * exp(2 * adjusted_logn_mean + adjusted_logn_sd^2)) > 7)
# 
# small_farms_data <- inner_join(small_farms, share_farms_below_2)
# large_farms_data <- inner_join(large_farms, share_farms_below_2)
# mixed_farms_data <- inner_join(mixed_farms, share_farms_below_2)
# 
# small_farms_map <- terra::rast(small_farms_data |> 
#                                  select(x, y, nb_farms_below))
# large_farms_map <- terra::rast(large_farms_data |> 
#                                  select(x, y, nb_farms_below))
# mixed_farms_map <- terra::rast(mixed_farms_data |>
#                                  select(x, y, nb_farms_below))
# par(mfrow = c(1,2))
# terra::plot(large_farms_map, main = 'large'); terra::plot(ssa, axes = F, add =T)
# terra::plot(mixed_farms_map, main = 'mixed'); terra::plot(ssa, axes = F, add =T)
# # terra::plot(small_farms_map, main = 'small'); terra::plot(ssa, axes = F, add =T)

# train_control <- caret::trainControl(method = 'cv', number = 10, savePredictions = 'all', seeds = 2024)
# tune_grid <- expand.grid(
#   mtry = 10,
#   splitrule = 'extratrees',
#   min.node.size = 40
# )
# 
# rf_full_model <- caret::train(
#   farm_area_ha ~ .,
#   data = lsms_spatial |> na.omit(),
#   method = 'ranger',
#   trainControl = train_control,
#   keep.inbag = T,
#   tuneGrid = tune_grid,
#   importance  = 'permutation', # how to get this in Python??
#   metric = 'RMSE',
#   min.bucket = 20,
#   num.trees = 500
# )
# 
# # model's rsquare
# rf_full_model$finalModel
