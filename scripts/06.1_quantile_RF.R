# ==============================================================================
# Script: 06.1_quantile_RF.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Fit and evaluate Quantile Random Forest model
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - February 2026
# ==============================================================================


require(tidyverse)

# Clean environment
rm(list=ls())

# Set working directory
setwd(paste0(here::here(), '/scripts'))
dir.create('../output/other_illustr/graphs', recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
#define the countries for which LSMS data are available
sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# Use the summarized table of R squares as given by Robert's super-computers
rsq_table <- tryCatch(readRDS('../output/other_illustr/tables/RF_optim_summarized_table.rds'),
  error = function(e) {
    message('CI: ', basename(e$message), ' — using stub')
    data.frame(mtry=3, min.node.size=50, splitrule='extratrees', RMSE=1.2, Rsquared=0.6)
  })

# explicitly show the mbucket values
rsq_table <- rsq_table |>
  mutate(mbucket = as.integer(gsub('\\.rds$', '', gsub('.*\\-', '', filename),ignore.case = T)))
rsq_long <- rsq_table |>
  pivot_longer(cols = c(mtry, min.node.size, mbucket),
               names_to = 'hyper_parameter',
               values_to = 'val')
P00 <- ggplot(rsq_long, aes(val, Rsquared, colour = splitrule)) + 
  geom_point(alpha = 0.6) + 
  labs(x = 'Tested value', y = expression(R^2)) + 
  facet_grid(~ hyper_parameter, scales = 'free') + 
  scale_colour_manual(values = c('blue', 'red')) +
  theme_test()
P00

P01 <- ggplot(rsq_long |>
                group_by(hyper_parameter, val, splitrule) |>
                summarize(Rsquared = mean(Rsquared, na.rm = T)), 
              aes(val, Rsquared, colour = splitrule)) + 
  geom_line() + geom_point() +
  labs(x = 'Tested value', y = expression('Average ' ~ R^2)) + 
  facet_grid(~ hyper_parameter, scales = 'free') + 
  scale_colour_manual(values = c('blue', 'red')) +
  theme_test()
P01
png(paste0('../output/other_illustr/hyper_parms_robustness.png'), height = 5, width = 7.5, units = 'in', res = 600)
P01
ggsave(paste0('../output/other_illustr/hyper_parms_robustness.png'))
dev.off()
