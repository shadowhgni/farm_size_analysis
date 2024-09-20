# ------------------------------------------------------------------------------
# Open the file from the folder and set working directory, using the here package
setwd(here::here())

# Clean environment
rm(list=ls())


#############################################################################################################
# Here are stored all the maps that will serve as predictors in the machine-learning (ML) models. Adjust accordingly
input_path <- 'C:/Users/DHOUGNI/OneDrive - CIMMYT/Documents/Harare 2023/Spatial_data_repository'

# load data
years <- terra::rast(Sys.glob(paste0(input_path, '/rainfall/rainfall_yearly/chirps_yearly_rainfall_*.tif')))

# calculate
longterm_tot_avg <- terra::app(years, mean)
longterm_tot_std <- terra::app(years, sd)
longterm_tot_cv  <- longterm_tot_std / longterm_tot_avg; names(longterm_tot_cv) <- 'cv'

# plot
terra::plot(c(longterm_tot_avg, longterm_tot_cv))
terra::plot(longterm_tot_avg)

# save
terra::writeRaster(longterm_tot_avg, paste0(input_path, "/rainfall/rainfall_yearly/#_long_term_rainfall_avg.tif"), overwrite=T)
terra::writeRaster(longterm_tot_cv , paste0(input_path, "/rainfall/rainfall_yearly/#_long_term_rainfall_cv.tif"), overwrite=T)

# ------------------------------------------------------------------------------