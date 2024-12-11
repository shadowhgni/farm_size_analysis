# Open the file from the folder and set working directory, using the here package
setwd(paste0(here::here(), '/scripts'))

# install the following packages if required

# Function to install required packages if not already installed
install_me <- function(packages) {
  # Get the names of any packages that are not installed
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  # Install missing packages
  if(length(missing_packages)) {
    install.packages(missing_packages)
  } else {
    message("All packages are already installed.")
  }
}

# List of required packages
required_packages <- c('tidyverse', 'here', 'curl', 
                       'patchwork', 'ggExtra', 'GGally',
                       'afrilearndata', 'terra', 'geodata',
                       'caret', 'gbm', 'xgboost', 'kernlab', 'e1071',
                       'randomForest', 'quantregForest', 'ranger', 
                       'XNomial', 'EnvStats', 'fitdistrplus')

# Call the function with the list of packages
install_me(required_packages)

# additionally, install these packages from github
remotes::install_github('afrimapr/afrilearndata')
remotes::install_github('ropensci/tabulizer')

### Create the following subfolders in the main folder
# 'scripts', 'data', 'output', and 'validation.'
# in 'data', create 'raw' and 'processed'
# in 'raw', create 'received' and 'web_scrapped'
# in 'output', create 'maps', 'tables', 'graphs'.

# Besides, create another folder (possibly outside the main folder) to store all spatial data that will be downloaded
# The 'input_path' will always refer to this folder.