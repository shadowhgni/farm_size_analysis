# ------------------------------------------------------------------------------
# Open the file from the folder and set working directory, using the here package
setwd(here::here())

# Clean environment
rm(list=ls())


#############################################################################################################
# Here are stored all the maps that will serve as predictors in the machine-learning (ML) models. Adjust accordingly
input_path <- 'C:/Users/DHOUGNI/OneDrive - CIMMYT/Documents/Harare 2023/Spatial_data_repository'

#############################################################################################################
#define the region of interest: subSaharan Africa, excluding small islands 
country <- geodata::world(path=input_path, resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_ssa <- subset(isocodes, NAME=='Sudan' | UNREGION1=='Middle Africa' | UNREGION1=='Western Africa' | UNREGION1=='Southern Africa' | UNREGION1=='Eastern Africa')
isocodes_ssa <- subset(isocodes_ssa, NAME!='Cabo Verde' & NAME!='Comoros' & NAME!='Mauritius' & NAME!='Mayotte' & NAME!='RC)union' & NAME!='Saint Helena' & NAME!='SC#o TomC) and PrC-ncipe' & NAME!='Seychelles') # keep the mainland + Madagascar only, remove islands
ssa <- subset(country, country$GID_0 %in% isocodes_ssa$ISO3)

# get chirps data
list.files(path = paste0(input_path, '/rainfall/'), pattern="^chirps.*gz$")

# ------------------------------------------------------------------------------
# loop over list of rasters
mylist <- list.files(path = paste0(input_path, '/rainfall/'), pattern="^chirps.*gz$")

# files per year
for(y in c(1981:2023)){
  print(y) 
  print(list.files(path = paste0(input_path, '/rainfall/'), pattern=paste0("^chirps-v2.0.",y,".{1}.*tif.gz$")))
  }

# begin loop over grids

if(!dir.exists(paste0(input_path, '/rainfall/rainfall_monthly')))
  dir.create(paste0(input_path, '/rainfall/rainfall_monthly'))
if(!dir.exists(paste0(input_path, '/rainfall/rainfall_yearly')))
  dir.create(paste0(input_path, '/rainfall/rainfall_yearly'))
for (y in c(1981:2023)){
  rlist <- list()
  s <- terra::rast()
  for (m in c('01', '02', '03', '04', '05', '06', '07','08','09','10','11','12')){
    for (d in 1:3){
      ingridname <- paste0(input_path, "/rainfall/CHIRPS/chirps-v2.0.", y,".", m, ".", d, ".tif")
      print(ingridname)
      try(r <- terra::crop(terra::rast(ingridname), ssa, mask=T))
      r[r<0] <- NA
      names(r) <- paste0("rain_", gsub("\\.", "_", gsub(paste0(input_path, "/rainfall/CHIRPS/chirps-v2.0."), "", gsub(".tif", "", ingridname))))
      rlist[[length(rlist)+1]] <- as.name(names(r))
      s <- c(s, r)
      }
    tmp.tot.mt <- sum(s); names(tmp.tot.mt) <- paste0(y, '_mm')
    terra::writeRaster(tmp.tot.mt, paste0(input_path, '/rainfall/rainfall_monthly/chirps_monthly_rainfall_', y, '_', m, '.tif'), overwrite = T)
    }
  tmp.tot.yr <- sum(s); names(tmp.tot.yr) <- paste0(y, '_mm')
  terra::writeRaster(tmp.tot.yr, paste0(input_path, '/rainfall/rainfall_yearly/chirps_yearly_rainfall_', y, '.tif'), overwrite = T)
  }

# ------------------------------------------------------------------------------
