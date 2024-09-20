# ------------------------------------------------------------------------------
# Open the file from the folder and set working directory, using the here package
setwd(here::here())

# Clean environment
rm(list=ls())

# load packages
require(curl)

#############################################################################################################
# Here are stored all the maps that will serve as predictors in the machine-learning (ML) models. Adjust accordingly
input_path <- 'C:/Users/DHOUGNI/OneDrive - CIMMYT/Documents/Harare 2023/Spatial_data_repository'

# set years, months and dekads
year <- seq(1981, 2024, 1)
month_1 <- seq(1, 9, 1)
month_1 <- paste0(0,month_1)
month_2 <- seq(10, 12, 1)
month <- append(month_1, month_2)
dekad <- seq(1, 3, 1)

# ------------------------------------------------------------------------------

# define the list of files to download
filenames <- c()
for (i in unique(year)){
  for (j in unique(dekad)){
    jan <- paste0("chirps-v2.0.",i,".01.",j,".tif.gz")
    feb <- paste0("chirps-v2.0.",i,".02.",j,".tif.gz")
    mar <- paste0("chirps-v2.0.",i,".03.",j,".tif.gz")
    apr <- paste0("chirps-v2.0.",i,".04.",j,".tif.gz")
    may <- paste0("chirps-v2.0.",i,".05.",j,".tif.gz")
    jun <- paste0("chirps-v2.0.",i,".06.",j,".tif.gz")
    jul <- paste0("chirps-v2.0.",i,".07.",j,".tif.gz")
    aug <- paste0("chirps-v2.0.",i,".08.",j,".tif.gz")
    sep <- paste0("chirps-v2.0.",i,".09.",j,".tif.gz")
    oct <- paste0("chirps-v2.0.",i,".10.",j,".tif.gz")
    nov <- paste0("chirps-v2.0.",i,".11.",j,".tif.gz")
    dec <- paste0("chirps-v2.0.",i,".12.",j,".tif.gz")
    filenames <- append(filenames, c(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec))}}

# download each file in list
url <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_dekad/tifs/'

if(!dir.exists(paste0(input_path, '/rainfall')))
  dir.create(paste0(input_path, '/rainfall'))
downloaded <- Sys.glob(paste0(input_path, '/rainfall/*.gz'))
downloaded <- basename(downloaded)

for (filename in filenames){
  tryCatch({
    print(filename)
    if(!(filename %in% sort(downloaded))){ 
      curl::curl_download(paste(url, filename, sep = ""), paste0(input_path, "/rainfall/", filename, sep = ""))
    }
  },
  error = function(e) cat(paste0('There was a problem....')),
  finally = print('')
  )
}

# ------------------------------------------------------------------------------

# decompress files
if(!dir.exists(paste0(input_path, '/rainfall/CHIRPS/')))
  dir.create(paste0(input_path, '/rainfall/CHIRPS'))

downloaded <- Sys.glob(paste0(input_path, '/rainfall/CHIRPS/*.tif'))
downloaded <- sort(basename(downloaded))
for (i in unique(year)){
 for (m in unique(month)){
   for (j in unique(dekad)){
     filename <- paste0('chirps-v2.0.',i,'.',m,'.',j,'.tif')
     if(!(filename %in% downloaded)) tryCatch(
       {
         R.utils::gunzip(paste0(input_path, '/rainfall/chirps-v2.0.',i,'.',m,'.',j,'.tif.gz'), 
                         remove = F, destname = paste0(input_path, '/rainfall/CHIRPS/', filename), overwrite = T)
         print(paste0('year = ', i, ' // ', 'month = ', m ))
       },
       error = function(e) cat('There was an error'),
       finally = print('-------------')
     )
     }
   }
 }
# ------------------------------------------------------------------------------