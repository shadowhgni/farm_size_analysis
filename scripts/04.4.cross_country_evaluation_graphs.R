# Random forest to see what explanatory variables are needed to understand variability in farm size across SSA


# load packages
require(tidyverse)

# Clean environment
rm(list=ls())

# Set working directory
setwd(here::here())

# ------------------------------------------------------------------------------

fourteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
fourteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------

# get the table of country_autoevaluation
country_auto_evaluation <- read.csv('../output/tables/country_auto_evaluation_rsquares.csv')
# get the table of one-on-one cross-country evaluation
country_pairs <- read.csv('../output/tables/country_pairwise_point_based_cross_validation.csv')
# get the table of all countries to predict one
country_leave_one_out <- read.csv('../output/tables/country_leave_one_out_point_based_cross_validation.csv')
# get the variable importance table
var_importance_table <- read.csv('../output/tables/country_variable_importance.csv')

# assemble data per country

# heatmap for pairwise comparison of countries, replace OOB r2 with CV r2 (if OOB, comment these lines)
# country_pairs <- country_pairs |>
#   filter(train_country != test_country) |>
#   bind_rows(
#     country_auto_evaluation |>
#       mutate(train_country = country,
#              test_country = country,
#              cty_test_rf_rsq = rf_cv_rsq) |>
#       select(train_country, test_country, cty_test_rf_rsq)
#   )

P00 <- ggplot(country_pairs,
              aes(train_country, test_country, fill = cty_test_rf_rsq)) +
  geom_raster() +
  geom_text(aes(label = cty_test_rf_rsq)) +
  geom_hline(yintercept = seq(0.5, 13.5, by = 1)) +
  geom_vline(xintercept = seq(0.5, 13.5, by = 1)) +
  labs(x = 'Training dataset', y = 'Validation dataset', fill = bquote(R^2)) +
  scale_x_discrete(expand =c(0, 0)) +
  scale_y_discrete(expand =c(0, 0)) +
  scale_fill_continuous(low = 'grey95', high = 'steelblue1') + # try grey95, steelblue1, firebrick4, gold1
  theme_test() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_blank())
png('../output/graphs/country_heatmap_cross_validation.png', height = 7.5, width = 15, units = 'in', res = 1000)
P00
ggsave('../output/graphs/country_heatmap_cross_validation.png')
dev.off()

# barplot for autoevaluation + all-other-countries
dat <- country_leave_one_out |>
  select(!rf_cv_rsq) |>
  inner_join(
    country_auto_evaluation |>
      select(country, rf_cv_rsq, rf_oob_rsq)
  ) |>
  select(country, rf_cv_rsq, rf_oob_rsq, cty_test_rf_rsq) |>
  pivot_longer(
    cols = contains('rsq'),
    names_to = 'type',
    values_to = 'rsq'
  ) |>
  mutate(
    type = case_when(type == 'rf_oob_rsq' ~ 'OOB',
                     type == 'rf_cv_rsq' ~ '10-fold CV',
                     type == 'cty_test_rf_rsq' ~ 'all other countries',
                     .default = NA)
  )
dat$type <- factor(dat$type, levels = c('OOB', '10-fold CV', 'all other countries'))

P01 <- ggplot(dat, aes(country, rsq, fill = type)) +
  geom_bar(stat = 'identity', position = position_dodge(0.8)) + 
  labs(x = 'Country', y = bquote(R^2), fill = 'Procedure') + 
  theme_bw() +
  theme(
    legend.position = c(0.7, 0.9),
    legend.direction = 'horizontal',
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
png('../output/graphs/country_barplot_cross_validation.png', height = 7.5, width = 15, units = 'in', res = 1000)
P01
ggsave('../output/graphs/country_barplot_cross_validation.png')
dev.off()

# heatmap of variable importance
P02 <- ggplot(var_importance_table, aes(country, var, fill = rank)) +
  geom_raster() +
  geom_text(aes(label = rank)) +
  geom_hline(yintercept = seq(0.5, 9.5, by = 1)) +
  geom_vline(xintercept = seq(0.5, 13.5, by = 1)) +
  labs(x = 'Country', y = 'Variable', fill = 'rank') +
  scale_x_discrete(expand =c(0, 0)) +
  scale_y_discrete(expand =c(0, 0)) +
  scale_fill_continuous(low = 'steelblue1', high = 'grey95') + 
  theme_test() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_blank())
png('../output/graphs/country_variable_importance.png', height = 7.5, width = 15, units = 'in', res = 1000)
P02
ggsave('../output/graphs/country_variable_importance.png')
dev.off()

# the three most important variables (1- maizeyield, 2- pop, 3- cattle)
var_importance_table |> 
  group_by(var) |> 
  summarize(avg_rank = mean(rank)) |> 
  arrange(avg_rank)
save(P00, P01, P02, file = '../data/processed/cross_validation_graphs.Rdata')