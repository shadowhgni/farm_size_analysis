# Random forest to see what explanatory variables are needed to understand variability in farm size across SSA
# Random forest predictions to derive summary values per country (SSA) and provinces/regions in the 6 countries
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
# force terra to use disk-based processing and 20% of RAM (Use this if R crashes because of limited memory)
# terra::terraOptions(memfrac = 0.2, todisk = T)

# ------------------------------------------------------------------------------
# Prepare data: load r_squares from different scripts


# load('../data/processed/2024-11-22.crossval_leave_out_GADM1.rdata')
# gadm_rsq <- mult_rsq |>
#   mutate(across(starts_with('rf'), \(x) as.numeric(x))); rm(mult_rsq)
# load('../data/processed/2024-11-22.cross_val_leave_out_one_country_meanFarm.rdata')
# leave1_rsq <- mult_rsq |>
#   mutate(across(starts_with('rf'), \(x) as.numeric(x))); rm(mult_rsq) # point-based and mean/// country OOB vs tested with other countries 
# load('../data/processed/country_pairwise_comparison_models.rdata')
# mult_rsq <- data.frame()
# for(i in ls(pattern = 'results_pairs_')){
#   one_table <- get(i)
#   mult_rsq <- rbind.data.frame(mult_rsq, one_table)
# }
# 
# regional_rsq <- mult_rsq |>
#   mutate(across(starts_with('rf'), \(x) as.numeric(x)))
# rm(list = ls(pattern = 'results_')); rm(mult_rsq, one_table, i) # point-based and mean/// country OOB vs tested with other countries 
# ------------------------------------------------------------------------------
# heat-map for regional patterns (point-based)
P00 <- ggplot(regional_rsq , aes(train_country, test_country)) +
  geom_raster(aes(fill = rf1_test_rsq)) +
  geom_hline(yintercept = seq(0.5, 14.5, by = 1)) +
  geom_vline(xintercept = seq(0.5, 14.5, by = 1)) +
  geom_text(aes(label = round(rf1_test_rsq, 2))) +
  scale_fill_gradient(low = 'lightskyblue1', high = 'steelblue3') +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))+
  labs(x = 'Training country',
       y = 'Test country' ) +
  theme(legend.position = 'none',
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = -90, hjust = 0.1))
P00
png('../output/graphs/regional_predictability_point_based.png', height = 10, width = 15, units = 'cm', res = 600)
P00
ggsave('../output/graphs/regional_predictability_point_based.png')
dev.off()

# heat-map for regional patterns (point-based)
P01 <- ggplot(regional_rsq , aes(train_country, test_country)) +
  geom_raster(aes(fill = rf2_test_rsq)) +
  geom_hline(yintercept = seq(0.5, 14.5, by = 1)) +
  geom_vline(xintercept = seq(0.5, 14.5, by = 1)) +
  geom_text(aes(label = round(rf2_test_rsq, 2))) +
  scale_fill_gradient(low = 'lightskyblue1', high = 'steelblue3') +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))+
  labs(x = 'Training country',
       y = 'Test country' ) +
  theme(legend.position = 'none',
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = -90, hjust = 0.1))
P01
png('../output/graphs/regional_predictability_mean.png', height = 10, width = 15, units = 'cm', res = 600)
P01
ggsave('../output/graphs/regional_predictability_mean.png')
dev.off()

# countries that can be fairly well predicted from others
regional_rsq |>
  filter(train_country != test_country) |>
  group_by(test_country) |>
  select(rf1_test_rsq, rf2_test_rsq) |>
  summarize(avg_pt_based_test_rsq = mean(rf1_test_rsq, na.rm = T),
            avg_mean_test_rsq = mean(rf2_test_rsq, na.rm = T)) |>
  arrange(-avg_pt_based_test_rsq)

# countries that can be used to predict others
regional_rsq |>
  filter(train_country != test_country) |>
  group_by(train_country) |>
  select(rf1_test_rsq, rf2_test_rsq) |>
  summarize(avg_pt_based_train_rsq = mean(rf1_test_rsq, na.rm = T),
            avg_mean_train_rsq = mean(rf2_test_rsq, na.rm = T)) |>
  arrange(-avg_pt_based_train_rsq)
# ------------------------------------------------------------------------------
# my_country train- my_country test , other_countries train- my_country test (regional_rsq$rf1_test_rsq)
# correlation coef OR R2 for agreement between TPS predictions and OTHR_COUNTRIES predicitions

# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------


# all  graphs
P_NULL <- ggplot() +
  geom_blank() +
  theme_void()

png(paste0('../output/graphs/Fig.S1_2_var_imp.png'), height = 7.5, width = 15, units = 'in', res = 1000)
patchwork::wrap_plots(P00 + labs(title = 'A'), 
                      P_NULL, 
                      P01 + labs(title = 'B'),
                      widths = c(1, 0.1, 1))
ggsave(paste0('../output/graphs/Fig.S1_2_var_imp.png'))
dev.off()