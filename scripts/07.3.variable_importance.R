# Random forest to see what explanatory variables are needed to understand variability in farm size across SSA

# load packages
require(tidyverse)

# Clean environment
rm(list=ls())

# Set working directory
setwd(paste0(here::here(), '/scripts'))

# ------------------------------------------------------------------------------
fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')

# ------------------------------------------------------------------------------
# Prepare data: load RF models and stacked (raster of drivers)
# pooled data RF model
load('../data/processed/2024-11-18.rf_full_model_with_95th_trimmed_data.rdata')
load('../data/processed/2024-11-25.var_importance_RFmodels_fourteen_contries.rdata')


# stacked <- terra::rast('../data/processed/stacked_rasters_africa.tif')
# ------------------------------------------------------------------------------
# compute the variable importance for the pooled data model
vi_SSA <- rf_full_model$finalModel$variable.importance |>
  as_tibble() |> 
  mutate(variable = names(rf_full_model$finalModel$variable.importance)) |> 
  arrange(value) |>
  select(variable, value)
vi_SSA$variable <- factor(vi_SSA$variable,
                          levels = vi_SSA$variable,
                          ordered = F)
r_sq_SSA <- round(rf_full_model$finalModel$r.squared, 2)

P00 <- ggplot(vi_SSA, aes(variable, value)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0))+
  coord_flip() +
  labs(x = 'Drivers of farm size variation (pooled-data model)', y = 'Variable importance') +
  annotate('text', x = 3, y = 2.5, label = bquote(R^2== .(r_sq_SSA)) ) + 
  theme_test()+
  theme(axis.ticks.y = element_blank())
P00
png('../output/graphs/variable_importance_SSA.png', height = 10, width = 15, units = 'cm', res = 600)
P00
ggsave('../output/graphs/variable_importance_SSA.png')
dev.off()

# ------------------------------------------------------------------------------


vi_rank <- mult_var_imp |>
  distinct() |>
  group_by(country) |>
  summarize(variable = variable,
            vi = as.numeric(value)) |>
  mutate(rank = rank(-vi))

P01 <- ggplot(vi_rank, aes(variable, country)) +
  geom_raster(aes(fill = rank)) +
  geom_hline(yintercept = seq(0.5, 14.5, by = 1)) +
  geom_vline(xintercept = seq(0.5, 10.5, by = 1)) +
  geom_text(aes(label = rank)) +
  scale_fill_gradient(low = 'lightskyblue1', high = 'steelblue3') +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))+
  labs(x = 'Driver',
       y = 'Country-specific model' ) +
  theme(legend.position = 'none',
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = -30, hjust = -0.1))
P01
png('../output/graphs/variable_importance_per_country.png', height = 10, width = 15, units = 'cm', res = 600)
P01
ggsave('../output/graphs/variable_importance_per_country.png')
dev.off()

# average rank of variable importance
vi_rank |> 
  ungroup() |> 
  group_by(variable) |> 
  summarize(avg_rank = mean(rank)) |> 
  arrange(desc(avg_rank))

# all variable importance graphs
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