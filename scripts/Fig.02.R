# Fig.02

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
# modified from JOAO

png("Fig.02.png", width = 9, height = 8.8, units = 'in', res = 1000)
par(mfrow=c(2,2), mar=c(3.5,3.5,1,1), xaxs='i', yaxs='i')

# plot 1
fig2a <- terra::rast('fig.2a_quantile_10_fsizes.tif')
pal <- colorRampPalette(c('#8B0000', '#FFCB00', 'forestgreen'))
terra::plot(ssa, mar=c(3.5,3.5,1,1), clip=F, col='white', main='', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.8))
terra::plot(fig2a$qrf_q010, breaks=c(0, 0.1, 0.2, 0.5, 1, 2, Inf), col=pal(6), legend=F, axes=F, add=T)
legend(-15, -5, bty='y', bg='white', cex=1.1, ncol=1, box.col="white",  title=expression(paste('Farm size (q' [10], ')')),
       legend=c('< 0.1 ha', '0.1 - 0.2 ha', '0.2 - 0.5 ha', '0.5 - 1 ha', '1 - 2 ha', '> 2 ha'), fill=pal(6), horiz=F)
terra::plot(ssa, axes=F, add=T)
text(48, 26, 'A)', cex=1.5)

# plot 2
fig2b <- terra::rast('fig.2b_quantile_90_fsizes.tif')
pal <- colorRampPalette(c('#8B0000', '#FFCB00', 'forestgreen'))
terra::plot(ssa, mar=c(3.5,3.5,1,1), clip=F, col='white', main='', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.8))
terra::plot(fig2b$qrf_q090, breaks=c(0, 1, 2, 5, 10, 15, Inf), col=pal(6), legend=F, axes=F, add=T)
legend(-15, -5, bty='y', bg='white', cex=1.1, ncol=1, box.col="white", title=expression(paste('Farm size (q' [90], ')')), 
       legend=c('< 1 ha', '1 - 2 ha', '2 - 5 ha', '5 - 10 ha', '10 - 15 ha', '> 15 ha'), fill=pal(6), horiz=F)
terra::plot(ssa, axes=F, add=T)
text(48, 26, 'B)', cex=1.5)

# ------------------------------------------------------------------------------
# plot 3
# fig2c <- readRDS('sample_virtual_farms.rds')
# xx <- readRDS('../../../data/processed/fsize_distribution_resample_long.rds')
# theor_farms <- xx$theor_farms
# theor_farms_application <- xx$theor_farms_application; rm(xx)
# pred_avg <- terra::rast('../../../data/processed/rf_model_predictions_SSA.tif') |>
#   terra::as.data.frame(xy = T)
# 
# fig2c <- theor_farms_application |>
#   select(x, y, linear_farm_size_ha) |>
#   inner_join(pred_avg) |>
#   rename(farm_size = linear_farm_size_ha, avg_size = rf_predictions_africa)

fig2c <- readRDS('fig2c.rds')

pal <- adjustcolor(c('#8B0000', '#FFCB00', 'forestgreen'), alpha.f = 0.1)
plot(ecdf(fig2c$farm_size[fig2c$avg_size > 5]), col = NA, verticals = F,
     xlim = c(0, 25), main = '', #  xlim was c(0, 15)
     xlab = 'Average farm size per grid cell (ha)', ylab = 'Cumulative probability',
     cex.axis=1.2, cex.lab=1.4, las=0, mgp=c(2,0.75,0), bg = 'whitesmoke' )
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "whitesmoke")
grid(nx=8, ny=8, col='lightgrey')
plot(ecdf(fig2c$farm_size[fig2c$avg_size < 0.5 ]), col='#8B0000', lwd = 3, add = T )
plot(ecdf(fig2c$farm_size[fig2c$avg_size >= 1 & fig2c$avg_size < 2]), col= '#FFCB00', lwd = 3, add = T)
plot(ecdf(fig2c$farm_size[fig2c$avg_size > 5]), col='forestgreen', lwd = 3, add = T)

# Add legend
legend(
  'bottomright',
  legend = c('< 0.5 ha', '1–2 ha', '> 5 ha'),
  col = c('#8B0000', '#FFCB00', 'forestgreen'),
  bg = NA,
  box.col = NA,
  lty = 1,
  lwd = 1.5,
  cex = 1.2,
  pt.cex = 2,
  text.col = 'black',
  title = 'Farm size class'
)

text(23, 0.93, 'C)', cex=1.5)

# ------------------------------------------------------------------------------
# plot 4

fig2d <- readRDS('fig.2d_mean_fsize_gini_coefs.rds')

plot(fig2d$predicted_avg_vs_gini$avg, fig2d$predicted_avg_vs_gini$gini, col='white', xlim=c(0,15), ylim=c(0.1,0.8),
     cex.axis=1.2, cex.lab=1.4, las=0, mgp=c(2,0.75,0),
     xlab='Average farm size per grid cell (ha)', ylab='Gini coefficient of farm size per grid cell')
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "whitesmoke")
grid(nx=8, ny=8, col='lightgrey')
points(fig2d$predicted_avg_vs_gini$avg, fig2d$predicted_avg_vs_gini$gini, cex=0.5, pch=21, col=viridis::viridis(5, alpha=0.1)[2], bg=viridis::viridis(5, alpha=0.1)[2])
points(fig2d$observed_avg_vs_gini$mean, fig2d$observed_avg_vs_gini$gini, cex=0.5, pch=21, col=viridis::viridis(5, alpha=0.1)[4], bg=viridis::viridis(5, alpha=0.1)[4])
car::dataEllipse(fig2d$predicted_avg_vs_gini$avg, fig2d$predicted_avg_vs_gini$gini, 
                 col=viridis::viridis(5, alpha=0.9)[2], 
                 cex = 0,
                 levels = 0.5, 
                 lwd = 2,
                 center.pch = F,
                 add = T)
car::dataEllipse(fig2d$observed_avg_vs_gini$mean, fig2d$observed_avg_vs_gini$gini,
                 col=viridis::viridis(5, alpha=0.9)[4], 
                 cex = 0,
                 levels = 0.5, 
                 lwd = 2,
                 center.pch = F,
                 add = T)
car::dataEllipse(fig2d$predicted_avg_vs_gini$avg, fig2d$predicted_avg_vs_gini$gini, 
                 col=viridis::viridis(5, alpha=0.9)[2], 
                 cex = 0,
                 levels = 0.95, 
                 lty = 2,
                 lwd = 2,
                 center.pch = F,
                 add = T)
car::dataEllipse(fig2d$observed_avg_vs_gini$mean, fig2d$observed_avg_vs_gini$gini,
                 col=viridis::viridis(5, alpha=0.9)[4], 
                 cex = 0,
                 levels = 0.95, 
                 lty = 2,
                 lwd = 2,
                 center.pch = F,
                 add = T)

abline(a=0.7, b=-0.03, col=1, lwd=2)
abline(h=0.2, col=1, lty=2, lwd=2)
legend('topright', bty='n', bg='whitesmoke', cex=1.1, ncol=1, legend=c('Predicted', 'Reported', 'y=0.7-0.03x', 'y=0.2'), pch=c(21,21,NA,NA), lty=c(NA,NA,1,2), lwd=c(NA,NA,2,2), col=c(viridis::viridis(5, alpha=0.7)[c(2,4)],1,1), pt.bg=c(viridis::viridis(5, alpha=0.7)[c(2,4)],NA,NA), pt.cex=1.5, horiz=F)
text(1, 0.75, 'D)', cex=1.5)
box()

dev.off()

magick::image_write(magick::image_read('Fig.02.png'), 'Fig.02.pdf', format = 'pdf')
