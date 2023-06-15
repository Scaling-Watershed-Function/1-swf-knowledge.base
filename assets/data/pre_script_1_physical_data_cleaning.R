################################################################################
# SCALING WATERSHED FUNCTION: DATA CLEANING - PART I WATERSHED & CATCHMENT AREAS
################################################################################

# IMPORTANT: This script is provisional! Final data cleaning script is contingent
# upon matching between NHDPlus 2.1 and NHDPlus HR

#Author: Francisco J. Guerrero

# Loading required packages: 

librarian::shelf(tidyverse,
                 utils,
                 quantreg,
                 gginnards)

# Local Import-Export
raw_data <- "raw"
processed_data <- "processed"

phys_dat_ro <- read_csv(paste(raw_data,"230430_ord_basin_hydrogeom_yrb_wrb.csv", sep = '/'),
                        show_col_types = FALSE)


# Exploring the data via summary

summary(phys_dat_ro)

# We find several variables with zero or missing values.These flow lines do not have a catchment area 
# associated to them because they are not connected to the network (Blodget, comms. pers.). So we will remove thase lines from the 
# dataset. The same applies to flowlines with catchment areas = 0


phys_dat_mod1 <- filter(phys_dat_ro, wshd_area_km2!=0)

phys_dat_mod2 <- filter(phys_dat_mod1, ctch_area_km2!=0)

summary(phys_dat_mod2)

write.csv(phys_dat_mod2,paste(raw_data,"230504_phys_dat_mod_2.csv", sep = "/"),
          row.names = FALSE)


# Summary

# The dataset "230430_ord_basin_hydrogeom_yrb_wrb.csv" had two modifications:

# 1. Removal of Watershed area data with zero values (72 datapoints)

# 2. Removal of catchment area data with zero values (163 datapoints)

















