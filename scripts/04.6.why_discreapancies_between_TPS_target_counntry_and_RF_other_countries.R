# How can the predictions from TPS_target_counntry and RF_other_countries be in good agreement (fairly high correlation coefficient)
# while RF is unable to predict on unseen data (poor R2) unlike TPS (fairly good R2)


require(tidyverse)
require(patchwork)
# First, correlation between datasets is not STRICTLY additive: this is not an explanation. But a description of what happened

a <- 1:10
old_diff <- 0
large_table <- data.frame()
for(i in 10:30){
  set.seed(2025)
  b <- 2 + 0.5 * a  - 0.01 * a^2 + rnorm(length(a),  0, 0.1 * i)
  print(paste0('========== i = ', i, ' ========'))
  for(j in 1:10){
    print(paste0('--------- j = ', j, ' --------'))
    c <- exp(b) / 1000  + rnorm(length(a), 0, 0.1 * j)
    cor_ab <- cor(a, b)
    cor_ac <- cor(a, c)
    cor_bc <- cor(b, c)
    print(paste0('cor (a, b) =', cor_ab))
    print(paste0('cor (a, c) =', cor_ac))
    print(paste0('cor (b, c) =', cor_bc))
    
    new_diff <- cor_bc - cor_ac
    new_set <- c(i = i, j = j, diff = new_diff)
    large_table <- bind_rows(large_table, c(new_set, cor_ab = cor_ab, cor_ac = cor_ac, cor_bc = cor_bc) )
    print(paste0('new_diff is  ', new_diff))
    ifelse(i == 1 & j == 1, old_set <- new_set, ab <- 10) # initialize old_diff
    
    ifelse(new_diff > old_diff, best_diff <- new_diff, best_diff <- old_diff)
    ifelse((best_diff == new_diff && cor_ab > 0.7), best_set <- new_set, best_set <- old_set)
    old_set <- best_set; old_diff <- best_diff
  }
}
print(best_set)

# visualize 
my_exple <- data.frame(i = c(18, 30, 25), j = 9)
for(ind in 1:nrow(my_exple)){
  set.seed(2025)
  print(paste0('---------- case', ind, ' -------------'))
  i = my_exple$i[ind]; j = my_exple$j[ind]
  bi <- 2 + 0.5 * a  - 0.01 * a^2 + rnorm(length(a),  0, 0.1 * i)
  ci <- exp(b) / 1000  + rnorm(length(a), 0, 0.1 * j)
  df <- data.frame(obs = a, mod1 = bi, mod2 = ci) 
  range(bi); range(ci)
  cor(a, bi); cor(a, ci); cor(bi, ci)
  # with(df, plot(obs, mod1))
  # with(df, plot(obs, mod2))
  # with(df, plot(mod1, mod2))
  
  P01 <- ggplot(df, aes(obs, mod1)) + 
    geom_point(colour = 'blue') + 
    geom_smooth(method = lm, se = F) + 
    annotate('text', x = 6, y = 5, label = bquote(R^2 == .(round(cor(df$obs, df$mod1)^2, 2)))) + 
    theme_test()
  
  P02 <- ggplot(df, aes(obs, mod2)) + 
    geom_point(colour = 'red') + 
    geom_smooth(method = lm, se = F, colour = 'red') + 
    annotate('text', x = 6, y = 5, label = bquote(R^2 == .(round(cor(df$obs, df$mod2)^2, 2)))) +
    theme_test()
  
  P03 <- ggplot(df, aes(mod1, mod2)) + 
    geom_point(colour = 'black') + 
    geom_smooth(method = lm, se = F, colour = 'black') + 
    annotate('text', x = 6, y = 5, label = bquote(cor_coef == .(round(cor(df$mod1, df$mod2), 2)))) +
    theme_test()
  print(P01 + P02 + P03)
}
