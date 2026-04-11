
# fig3 <- readRDS('fig3.a_b_c_farm_characterization.rds')
# fig3a <- fig3$df_cattle_all_crops_per_aez
# fig3b <- fig3$df_6crops_in_groups_all_aez
# fig3c <- fig3$df_4_crops_per_aez
require(tidyverse)
fig3 <- readRDS('2026-01-24.CHINA_croplands_per_crop_per_aez.rds')
fig3a <- fig3$df_rel_long |> filter(product %in% c('all_crops', 'cattle'), aez != 'all_aez')
fig3b <- fig3$df_rel_long |> 
  filter(product %in% c('maize', 'sorghum', 'millet', 'cassava', 'legumes', 'non_food'), 
         aez == 'all_aez') |>
  mutate(product = if_else(product == 'non_food', 'cash crops', product))
fig3c <- fig3$df_rel_long |> filter(product %in% c('maize', 'sorghum', 'millet', 'cassava'), aez != 'all_aez')

pdf("figure-3.pdf", width = 9, height = 8.8)
layout(matrix(c(1,1,2,2,3,4,5,6), nrow=2, byrow=TRUE))
par(mar=c(3.5,3.5,1,1), xaxs='i', yaxs='i')

# plot 1
plot(0, 0, xlim=c(0,8), ylim=c(0,100), xlab='', ylab='', cex.axis=1.2, cex.lab=1.4, las=0, mgp=c(2,0.75,0), col='white')
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "whitesmoke")
grid(nx=8, ny=10, lty=1, col='lightgrey')
i <- 1
for(aez1 in c('tropical highlands', 'humid', 'sub-humid', 'semi-arid')){
  subst1 <- subset(fig3a, aez == aez1)
  lty <- 1
  col <- viridis::viridis(4, direction=1)[i]
  for(ct in c('all_crops', 'cattle')){
    subst <- subset(subst1, product==ct)
    lines(subst$pred_farm_area_ha, subst$value*100, col=col, lwd=3.5, lty=lty)
    lty <- lty + 1
  }
  i <- i + 1
}
legend('bottomright', bty='n', bg='whitesmoke', cex=1.1, lty=c(1,1,1,1,1,2), lwd=3,
       legend=c('Tropical highlands', 'Humid', 'Sub-humid', 'Semi-arid', 'Cropland', 'Cattle'), 
       col=c(viridis::viridis(4, direction=1),1,1))
text(0.5, 93, 'A)', cex=1.5)
title(ylab="Cumulative cultivated area or herd size (%)", cex.lab=1.4, line=2)
title(xlab="Average farm size (ha)", cex.lab=1.4, line=2)
box()

# plot 2
plot(0, 0, xlim=c(0,8), ylim=c(0,100), xlab='', ylab='', cex.axis=1.2, cex.lab=1.4, las=0, mgp=c(2,0.75,0), col='white')
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "whitesmoke")
grid(nx=8, ny=10, lty=1, col='lightgrey')
i <- 1
for(aez1 in c("maize", "sorghum", "millet", "cassava", "legumes", "cash crops")){
  subst1 <- subset(fig3b, product == aez1)
  lty <- 1
  col <- viridis::viridis(6, direction=1)[i]
  lines(subst1$pred_farm_area_ha, subst1$value*100, col=col, lwd=3.5, lty=lty)
  i <- i + 1
}
legend('bottomright', bty='n', bg='whitesmoke', cex=1.1, lty=1, lwd=3,
       legend=c("Maize", "Sorghum", "Millet", "Cassava", "Legumes", "Non-food crops"), 
       col=viridis::viridis(6, direction=1))
text(0.5, 93, 'B)', cex=1.5)
title(ylab="Cumulative crop area (%)", cex.lab=1.4, line=2)
title(xlab="Average farm size (ha)", cex.lab=1.4, line=2)
box()

# plot 3
fig3c_1 <- subset(fig3c, aez=='tropical highlands')
plot(0, 0, xlim=c(0,8), ylim=c(0,100), xlab='', ylab='', cex.axis=1.2, cex.lab=1.4, las=0, mgp=c(2,0.75,0), col='white')
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "whitesmoke")
grid(nx=8, ny=10, lty=1, col='lightgrey')
i <- 1
for(aez1 in c("maize", "sorghum", "millet", "cassava")){
  subst1 <- subset(fig3c_1, product == aez1)
  lty <- 1
  col <- viridis::viridis(4, direction=1)[i]
  lines(subst1$pred_farm_area_ha, subst1$value*100, col=col, lwd=3.5, lty=lty)
  i <- i + 1
}
legend('bottomright', bty='n', bg='whitesmoke', cex=1.1, lty=1, lwd=3,
       title=expression(bold("Tropical\nhighlands")),
       legend=c("Maize", "Sorghum", "Millet", "Cassava"), 
       col=viridis::viridis(4, direction=1))
text(0.8, 93, 'C)', cex=1.5)
title(ylab="Cumulative crop area (%)", cex.lab=1.4, line=2)
title(xlab="Average farm size (ha)", cex.lab=1.4, line=2)
box()

# plot 4
fig3c_1 <- subset(fig3c, aez=='humid')
plot(0, 0, xlim=c(0,8), ylim=c(0,100), xlab='', ylab='', cex.axis=1.2, cex.lab=1.4, las=0, mgp=c(2,0.75,0), col='white')
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "whitesmoke")
grid(nx=8, ny=10, lty=1, col='lightgrey')
i <- 1
for(aez1 in c("maize", "sorghum", "millet", "cassava")){
  subst1 <- subset(fig3c_1, product == aez1)
  lty <- 1
  col <- viridis::viridis(4, direction=1)[i]
  lines(subst1$pred_farm_area_ha, subst1$value*100, col=col, lwd=3.5, lty=lty)
  i <- i + 1
}
legend('bottomright', bty='n', bg='whitesmoke', cex=1.1, lty=1, lwd=3,
       title=expression(bold("Humid")),
       legend=c("Maize", "Sorghum", "Millet", "Cassava"), 
       col=viridis::viridis(4, direction=1))
text(0.8, 93, 'D)', cex=1.5)
title(ylab="Cumulative crop area (%)", cex.lab=1.4, line=2)
title(xlab="Average farm size (ha)", cex.lab=1.4, line=2)
box()

# plot 5
fig3c_1 <- subset(fig3c, aez=='sub-humid')
plot(0, 0, xlim=c(0,8), ylim=c(0,100), xlab='', ylab='', cex.axis=1.2, cex.lab=1.4, las=0, mgp=c(2,0.75,0), col='white')
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "whitesmoke")
grid(nx=8, ny=10, lty=1, col='lightgrey')
i <- 1
for(aez1 in c("maize", "sorghum", "millet", "cassava")){
  subst1 <- subset(fig3c_1, product == aez1)
  lty <- 1
  col <- viridis::viridis(4, direction=1)[i]
  lines(subst1$pred_farm_area_ha, subst1$value*100, col=col, lwd=3.5, lty=lty)
  i <- i + 1
}
legend('bottomright', bty='n', bg='whitesmoke', cex=1.1, lty=1, lwd=3,
       title=expression(bold("Sub-humid")),
       legend=c("Maize", "Sorghum", "Millet", "Cassava"), 
       col=viridis::viridis(4, direction=1))
text(0.8, 93, 'E)', cex=1.5)
title(ylab="Cumulative crop area (%)", cex.lab=1.4, line=2)
title(xlab="Average farm size (ha)", cex.lab=1.4, line=2)
box()

# plot 6
fig3c_1 <- subset(fig3c, aez=='semi-arid')
plot(0, 0, xlim=c(0,8), ylim=c(0,100), xlab='', ylab='', cex.axis=1.2, cex.lab=1.4, las=0, mgp=c(2,0.75,0), col='white')
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "whitesmoke")
grid(nx=8, ny=10, lty=1, col='lightgrey')
i <- 1
for(aez1 in c("maize", "sorghum", "millet", "cassava")){
  subst1 <- subset(fig3c_1, product == aez1)
  lty <- 1
  col <- viridis::viridis(4, direction=1)[i]
  lines(subst1$pred_farm_area_ha, subst1$value*100, col=col, lwd=3.5, lty=lty)
  i <- i + 1
}
legend('bottomright', bty='n', bg='whitesmoke', cex=1.1, lty=1, lwd=3,
       title=expression(bold("Semi-arid")),
       legend=c("Maize", "Sorghum", "Millet", "Cassava"), 
       col=viridis::viridis(4, direction=1))
text(0.8, 93, 'F)', cex=1.5)
title(ylab="Cumulative crop area (%)", cex.lab=1.4, line=2)
title(xlab="Average farm size (ha)", cex.lab=1.4, line=2)
box()

dev.off()