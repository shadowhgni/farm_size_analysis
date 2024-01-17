# Understanding and predicting the variability of farm size across SSA
# putting maps together in a slide deck

# load package
require(tidyverse)

# Set working directory
setwd('C:/Users/Gebruiker/Documents/Harare 2023/Farm sizes across Africa/scripts')
# Clean environment
rm(list = ls())

# Function to add images of set1 (6 images per slide)

add_images_set1 <- function(ppt, set1, my_country) {
  
  # Add a new slide
  properties <- officer::fp_text(font.size = 14, bold = T)
  slide_title <- officer::ftext(paste0(my_country,' - Biophysical conditions and cropland'),
                       properties)
  ppt <- ppt %>%
    officer::add_slide(layout = 'Title and Content') %>%
    ph_with(text,
            value = fpar(slide_title),
            location = ph_location_type('title'),
            properties
    )
  
  # Define the positions for each image
  positions <- list(
    list(left = 0.0, top = 1.1),
    list(left = 3.3, top = 1.1),
    list(left = 6.6, top = 1.1),
    list(left = 0.0, top = 4),
    list(left = 3.3, top = 4),
    list(left = 6.6, top = 4)
  )
  
  # Loop through image files of set1 and add each to the slide
  for (i in seq_along(set1)) {
    ppt <- ppt %>%
      officer::ph_with(
        external_img(
          src = set1[i],
          width = 1.2),
        location = ph_location(left = positions[[i]]$left,
                               top = positions[[i]]$top),
        use_loc_size = T
      )
  }
  
  return(ppt)
}

add_images_set2 <- function(ppt, set2, my_country) {
  # Add a new slide
  properties <- officer::fp_text(font.size = 14, bold = T)
  slide_title <- officer::ftext(paste0(my_country,' - Socioeconomic conditions and cattle'),
                                  properties)
  ppt <- ppt %>%
    officer::add_slide(layout = 'Title and Content') %>%
    ph_with(text,
            value = fpar(slide_title),
            location = ph_location_type('title'),
            properties
    )
  
  # Define the positions for each image
  positions <- list(
    list(left = 0.5, top = 1.1),
    list(left = 0.5, top = 4.5),
    list(left = 5.0, top = 1.1),
    list(left = 5.0, top = 4.5)
  )
  
  # Loop through image files and add each to the slide
  for (i in seq_along(set2)) {
    ppt <- ppt %>%
      officer::ph_with(
        external_img(
          src = set2[i],
          width = 2.2),
        location = ph_location(left = positions[[i]]$left,
                               top = positions[[i]]$top),
        use_loc_size = T  # change this to resize the image, but will need to define height
      )
  }
  
  return(ppt)
}

# arrange slides per country
country_slides=function(my_country){
  set1 <- paste0('../output/maps/',my_country,'-',
                 c('elevation', 'sand0_30', 'rainfall',
                   'cropland', 'cereals', 'roots'),'.png')
  set2 <-paste0('../output/maps/',my_country,'-',
                c('pop', 'market', 'cattle', 'lsms'),'.png')
  
  add_images_set1(ppt, set1, my_country)
  add_images_set2(ppt, set2, my_country)
}

# list of countries
six_countries=c('Ethiopia', 'Malawi', 'Niger', 'Nigeria', 'Tanzania', 'Uganda')


# Create a PowerPoint document
ppt <- officer::read_pptx()

# fill in the ppt with images created with script 02, located in output/maps folder
sapply(six_countries,country_slides)

# print to draft folder
if(file.exists('../draft/show_expl_var.pptx')){
  file.remove('../draft/show_expl_var.pptx')
}
print(ppt, target='../draft/show_expl_var.pptx')
