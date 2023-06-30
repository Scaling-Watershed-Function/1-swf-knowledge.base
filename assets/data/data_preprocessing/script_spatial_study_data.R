###############################################################################
# Comparison of ER Sed from Field Measurements with River Corridor Model 
# predictions (Son et al., 2022)
# DATA PRE-PROCESSING
###############################################################################
# RESPIRATION DATA
###############################################################################

#By : Francisco Guerrero (Modified from Kyongho Son)
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

# If you don't have the package "librarian", run:
# install.packages("librarian")

librarian::shelf(sp,
                 sf,
                 leaflet,
                 utils,
                 readr,
                 zen4R,
                 tidyverse)
#Note: Verify that the sf package was successfully installed. Otherwise try install.packages("sf)
#and answer "no" to the following prompt:

#Do you want to install from sources the package which needs compilation? (Yes/no/cancel) 

set.seed(2703)

# Model input (contains shape files for the Willamette and Yakima River Basins):

############ loading NHDPlus Data (Shapefiles) ##################################################

# For compatibility with the latest NHDPlus database, the coordinate system for the 
# original shapefiles has to be modified from NAD83 to WGS84. We do so by using the 
# st_transform() function from the package sf

# Input data

shape_data <- "./model_inputs/shapes"

# Shapefiles
# Yakima River Basin
nhd_yrb_stream<-sf::st_transform(st_read(paste(shape_data,"nhd_CR_stream_sub9.shp",sep = "/")),4326)

# Adding basin name and HUC4
nhd_yrb_stream <-  nhd_yrb_stream %>% 
  mutate(Name = "Yakima",
         HUC4 = 1703) 

# CSV file with hydrological data
hydro_dat_url <- "https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/assets/data/raw/230321_pnw_2001_landcover.csv"
nhdp_2_pnw_raw <- read_csv(url(hydro_dat_url),show_col_types = FALSE)

# CSV file with land cover data
land_dat_url <- "https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/assets/data/raw/230321_pnw_2001_landcover.csv"
land_nlcd_raw <- read_csv(url(land_dat_url), show_col_types = FALSE)

hydro_dat_yrb <- filter(nhdp_2_pnw_raw,huc_4=="1703")
              
