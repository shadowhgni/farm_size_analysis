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
# Prepare data: summaries rsquares
leave1_vs_tps <- read.csv('../output/tables/country_one_leave_out_versus_tps.csv')


load('../data/processed/2024-11-22.cross_val_leave_out_one_country_meanFarm.rdata')
leave1_rsq <- mult_rsq |>
  mutate(across(starts_with('rf'), \(x) as.numeric(x))); rm(mult_rsq) # point-based and mean/// country OOB vs tested with other countries 
load('../data/processed/country_pairwise_comparison_models.rdata')
mult_rsq <- data.frame()
for(i in ls(pattern = 'results_pairs_')){
  one_table <- get(i)
  mult_rsq <- rbind.data.frame(mult_rsq, one_table)
}

summary_table <- inner_join(
  mult_rsq |>
    filter(train_country == test_country) |>
    mutate(country = train_country,
           rf1_test_rsq = as.numeric(rf1_test_rsq),
           rf2_test_rsq = as.numeric(rf2_test_rsq)) |>
    select(country, rf2_test_rsq, rf1_test_rsq),
  leave1_vs_tps |>
    select(country, rsq_rf, rsq_rf_vs_tps)
) |>
  rename(avg_country = rf2_test_rsq,
         pt_based_country = rf1_test_rsq,
         pt_based_other. = rsq_rf,
         pt_based_other._vs_TPS = rsq_rf_vs_tps) |>
  pivot_longer(contains('_'),
               names_to = 'r_square',
               values_to = 'val') |>
  mutate(r_square = gsub('avg ', 'average-', gsub('based ', 'based-', gsub('_', ' ', r_square))))

P00 <- ggplot(summary_table, aes(country, val, fill = r_square)) +
  geom_col(colour = 'black', position = position_dodge(width = 0.8))+
  labs(x = 'Country', y = expression(R^2), fill = 'Model') +
  scale_x_discrete(expand =c(0, 0)) +
  scale_y_continuous(expand =c(0, NA)) +
  scale_colour_brewer(palette = 'Set1') +
  theme_test() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = -60, hjust = -0.05))
P00
png('../output/graphs/leave_one_country_out_evaluation_vs_TPS.png', height = 7.5, width = 15, units = 'cm', res = 600)
P00
ggsave('../output/graphs/leave_one_country_out_evaluation_vs_TPS.png')
dev.off()
