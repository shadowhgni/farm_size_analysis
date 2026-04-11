# Fig. 1: drivers of farm size variation across SSA

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
pal <- colorRampPalette(c('darkred', 'orange', 'gold', 'darkolivegreen3', 'darkgreen'))
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
#define the countries for which LSMS data are available
# sixteen_countries <- c('Benin', 'Burkina', 'Cote_d_Ivoire', 'Ethiopia', 'Ghana', 'Guinea_Bissau', 'Malawi', 'Mali', 'Niger', 'Nigeria', 'Rwanda','Senegal', 'Tanzania', 'Togo', 'Uganda', 'Zambia')
# sixteen_country_codes <- c('BEN', 'BFA', 'CIV', 'ETH', 'GHA', 'GNB', 'MWI', 'MLI', 'NER', 'NGA', 'RWA', 'SEN', 'TZA', 'TGO', 'UGA', 'ZMB')
# ------------------------------------------------------------------------------
# Prepare data rasters: lsms and predictions + virtual list of farm sizes
# stacked <- terra::rast('../../../data/processed/stacked_rasters_africa.tif')
# rf_model_predictions <- terra::rast('../../../data/processed/rf_model_predictions_SSA.tif')
# names(rf_model_predictions) <- 'pred_farm_area_ha'
# qrf_model_predictions <- terra::rast('../../../data/processed/qrf_100quantiles_predictions_africa.tif')
# names(qrf_model_predictions) <- paste0('qrf_q', sprintf('%03g', 1:100))
# mask_forest_ssa <- terra::rast('../../../data/processed/mask_forest_ssa.tif')
# mask_drylands_ssa <- terra::rast('../../../data/processed/mask_drylands_ssa.tif')
# ------------------------------------------------------------------------------
# from JOAO
pdf("figure-1.pdf", width = 9, height = 8.8)
par(mfrow=c(2,2), mar=c(3.5,3.5,1,1), xaxs='i', yaxs='i')

# plot 1
fig1a <- terra::rast('../fig.1a_nb_of_farm_per_grid_cell.tif')
# pal <- colorRampPalette(c('#8B0000', '#FFCB00', 'forestgreen'))
pal <-pal4
terra::plot(ssa, mar=c(3.5,3.5,1,1), clip=F, col='white', main='', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.8))
terra::plot(fig1a$spam_2017 / fig1a$pred_farm_area_ha, breaks=c(0, 500, 1000, 2000, 5000, 10000, Inf), col=pal(6), legend=F, axes=F, add=T)
legend(-16, -5, bty='y', bg='white', cex=1.1, ncol=1, box.col="white", title=expression('Farms per 100' ~ km^2), legend=c('< 500', '501 - 1000', '1001 - 2000', '2001 - 5000', '5001 - 10000', '> 10000'), fill=pal(6), horiz=F)
terra::plot(ssa, axes=F, add=T)
text(48, 26, 'A)', cex=1.5)

# plot 2
pal <- colorRampPalette(c('#8B0000', '#FFCB00', 'forestgreen'))
terra::plot(ssa, mar=c(3.5,3.5,1,1), clip=F, col='white', main='', panel.first=grid(col="gray", lty="solid"), pax=list(cex.axis=1.8))
terra::plot(fig1a$pred_farm_area_ha, breaks=c(0, 0.5, 1, 2, 5, Inf), col=pal(6), legend=F, axes=F, add=T)
legend(-15, -5, bty='y', bg='white', cex=1.1, ncol=1, box.col="white", title="Farm size", legend=c('< 0.5 ha', '0.5 - 1 ha', '1 - 1.5 ha', '1.5 - 2 ha', '2 - 5 ha', '> 5 ha'), fill=pal(6), horiz=F)
terra::plot(ssa, axes=F, add=T)
text(48, 26, 'B)', cex=1.5)

# plot 3
fig1c <- readRDS('../fig.1c_comparison_with_sarah_lowder.rds')
fig1c$comp_nb_farms$nb_farms[fig1c$comp_nb_farms$country == 'Rwanda'] <- 1674687
fig1c$r2_sarah <- unname(round(cor(na.omit(bind_cols(fig1c$comp_nb_farms$estim_nb_farms, fig1c$comp_nb_farms$nb_farms)))[2,1]^2, 2))
fig1c$comp_nb_farms$year_class <- ifelse(fig1c$comp_nb_farms$census_year < 1980, '1970s', NA)
fig1c$comp_nb_farms$year_class <- ifelse(fig1c$comp_nb_farms$census_year >= 1980 & fig1c$comp_nb_farms$census_year < 1990, '1980s', fig1c$comp_nb_farms$year_class)
fig1c$comp_nb_farms$year_class <- ifelse(fig1c$comp_nb_farms$census_year >= 1990 & fig1c$comp_nb_farms$census_year < 2000, '1990s', fig1c$comp_nb_farms$year_class)
fig1c$comp_nb_farms$year_class <- ifelse(fig1c$comp_nb_farms$census_year >= 2000 & fig1c$comp_nb_farms$census_year < 2010, '2000s', fig1c$comp_nb_farms$year_class)
fig1c$comp_nb_farms$year_class <- ifelse(fig1c$comp_nb_farms$census_year >= 2010 & fig1c$comp_nb_farms$census_year < 2020, '2010s', fig1c$comp_nb_farms$year_class)
plot(log10(fig1c$comp_nb_farms$nb_farms/1000000), log10(fig1c$comp_nb_farms$estim_nb_farms/1000000),
     xlim=c(-2.5, 1.7), ylim=c(-2.5, 1.7), col='white', cex.axis=1.2, cex.lab=1.35, las=0, mgp=c(2,0.75,0), #was cex.lab = 1.4
     xlab=expression(paste('Million of farms based on census (log'[10],' scale)')), ylab=expression(paste('Million of farms based on predictions (log'[10],' scale)')))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "whitesmoke")
grid(nx=8, ny=8, col='lightgrey')
abline(a=0, b=1)
colr <- viridis::viridis(5, alpha=0.7)
i <- 1
for(yr in sort(unique(fig1c$comp_nb_farms$year_class))){
  sbt <- subset(fig1c$comp_nb_farms, year_class==yr)
  points(log10(sbt$nb_farms/1000000), log10(sbt$estim_nb_farms/1000000), pch=21, cex=3, col='grey20', bg=colr[i])
  i <- i+1
  }
for(cty in c('Angola', 'Benin', 'Botswana', 'Ethiopia', 'Gabon', 'Liberia', 'Malawi', 'Niger', 'Nigeria', 'Rwanda', 'Sierra Leone' )){ 
  sbt <- subset(fig1c$comp_nb_farms, country==cty)
  text(log10(sbt$nb_farms/1000000), log10(sbt$estim_nb_farms/1000000)+0.05, labels = sbt$country, col='grey10', pos = 3, cex = 1.2)
  }
legend('bottomright', bty='n', cex=1.3, title='Census\ndecade', legend=sort(unique(fig1c$comp_nb_farms$year_class)), pch=21, pt.cex=1.7, col='grey20', pt.bg=colr[1:5])
text(-2.2, 1.35, 'C)', cex=1.5)
text(-0.5, 1.2, bquote(R^2== .(round(fig1c$r2_sarah, 2))), cex=1.5)

# plot 4
fig1d <- readRDS('../fig.1d_reported_vs_predicted_fsize.rds')
fig1d <- na.omit(data.frame(x=fig1d$lsms_spatial$farm_area_ha, y=fig1d$lsms_spatial$pred_oob))
dens <- MASS::kde2d(fig1d$x, fig1d$y, lims = c(0, 3, 0, 3), n = 500)
image(dens, col = viridis::mako(20, direction=-1)[c(1:12)], xlim=c(0,3), ylim=c(0,3), 
      xlab = "Reported farm size (ha)", ylab = "Predicted farm size (ha)", main = "",
      cex.axis=1.2, cex.lab=1.4, las=0, mgp=c(2,0.75,0))
# contour(dens, add = TRUE, col='grey')
abline(a=0, b=1, col='black', lwd=2)
abline(a=0, b=2, col='black', lwd=2, lty=2)
abline(a=0, b=0.5, col='black', lwd=2, lty=2)
text(0.2, 2.75, 'D)', cex=1.5)
text(1.85, 2.65, bquote(R^2== .(round(0.54, 2))), cex=1.5)
box()


# Add continuous color legend in bottom-left corner
# Create a small inset plot for the legend
# Define legend position in plot coordinates
legend_x1 <- 2.25
legend_x2 <- 2.4
legend_y1 <- 0.05
legend_y2 <- 0.7

# Create gradient for legend
# Get the grid coordinates
x_breaks <- dens$x
y_breaks <- dens$y

# Find which bin each observation falls into
x_bins <- findInterval(fig1d$x, x_breaks, all.inside = TRUE)
y_bins <- findInterval(fig1d$y, y_breaks, all.inside = TRUE)

# Create count matrix using table
count_table <- table(factor(x_bins, levels = 1:length(x_breaks)),
                     factor(y_bins, levels = 1:length(y_breaks)))

# Convert to matrix
count_matrix <- as.matrix(count_table)
legend_seq <- seq(min(count_matrix, na.rm=TRUE), max(count_matrix, na.rm=TRUE), length.out=100)
legend_matrix <- matrix(legend_seq, ncol=1)

# Add legend border
rect(legend_x1 - 0.01, 0, 3, 0.9, border = "black", lwd = 1, col ='white')

# Add the legend as a small image
image(x = seq(legend_x1, legend_x2, length.out = 2),
      y = seq(legend_y1, legend_y2, length.out = 100),
      z = t(legend_matrix),
      col = viridis::mako(20, direction=-1)[c(1:12)],
      add = TRUE,
      xaxt = "n", yaxt = "n")



# Add legend labels
n_labels <- 5
label_values <- pretty(range(count_matrix, na.rm=TRUE), n = n_labels)
label_positions <- legend_y1 + (label_values - min(count_matrix, na.rm=TRUE)) / 
  (max(count_matrix, na.rm=TRUE) - min(count_matrix, na.rm=TRUE)) * 
  (legend_y2 - legend_y1)

# Add tick marks and labels
for(i in 1:length(label_values[-length(label_values)])) {
  segments(legend_x2, label_positions[i], legend_x2 + 0.05, label_positions[i], lwd = 1)
  text(legend_x2 + 0.1, label_positions[i], 
       format(round(label_values[i]), scientific = FALSE), 
       adj = 0, cex = 1.2)
}

# Add legend title
text(2.25 + (3 - 2.25)/2, legend_y2 + 0.1, 
     "Farms", cex = 1.2)

dev.off()
