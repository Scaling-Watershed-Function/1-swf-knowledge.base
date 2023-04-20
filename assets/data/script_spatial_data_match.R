###############################################################################
# Field Spatial Study - YRB Respiration Model Match
###############################################################################

# Author: Francisco J. Guerrero
# Date created: 04-20-2023

librarian::shelf(tidyverse)

# Local import
raw_data <- "raw"
input_data <- "raw/pre-processing/model_inputs" 
processed_data <- "processed"

# Data

spt_dat <- read_csv(paste(raw_data,"230110_yrb_spatial_camp.csv",sep = '/'),
                    show_col_types = FALSE)
fld_dat <- read_csv(paste(raw_data,"combined_results_updated_040623.csv", sep = '/'),
                    show_col_types = FALSE)
mod_dat <- read_csv(paste(raw_data,"230406_son_etal_22_results_zen.csv", sep = '/'),
                    show_col_types = FALSE)
