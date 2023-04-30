################################################################################
# SCALING WATERSHED FUNCTION: DATA CLEANING
################################################################################

#Author: Francisco J. Guerrero

# Loading required packages: 

librarian::shelf(tidyverse,
                 utils)

# Local Import-Export
raw_data <- "raw"
processed_data <- "processed"

phys_dat_ro <- read_csv(paste(raw_data,"230430_ord_basin_hydrogeom_yrb_wrb.csv", sep = '/'),
                        show_col_types = FALSE)


# Exploring the data via summary

summary(phys_dat_ro)


################NEED TO CHANGE TO phys_dat_ro################################################

# We find several variables with zero or missing values: 

# Total Drainage Area

summary(filter(phys_dat,totdasqkm == 0))

# All 72 missing values correspond to first order streams with zero values for catchment
# areas as well, so we will remove these data points.

phys_dat_trm <- (filter(phys_dat,totdasqkm!=0))

summary(phys_dat_trm)

# We have 30 data points with NAs for roughness

summary(filter(phys_dat_trm, is.na(roughness)==TRUE))

# Roughness

# NA values in roughness correspond to flowline_slope = 0. Otherwise, the cover
# a wide range of watershed characteristics

n_plot <- ggplot(data = filter(phys_dat_trm, 
                               is.na(roughness) == FALSE),
                 aes(x = streamorde,
                     y = roughness,
                     color=as.factor(streamorde)))+
  geom_boxplot()
n_plot

# We will replace the missing n values by the average value for the corresponding 
# stream order

phys_dat_trm <- phys_dat_trm %>% 
  group_by(streamorde) %>% 
  mutate(roughness_ord = mean(roughness,na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(roughness = if_else(is.na(roughness),
                             roughness_ord,
                             roughness)) %>% 
  select(-roughness_ord)

# Mean annual flow

summary(filter(phys_dat_trm, mean_ann_flow_m3s==0))

# we have 22 values all corresponding to first order streams, we proceed to remove
# these rows: 

phys_dat_trm <- filter(phys_dat_trm, mean_ann_flow_m3s!=0)

# Stream density

# We have only 1 NA value corresponding to a 1 order stream. We proceed to remove this
# row

phys_dat_trm <- filter(phys_dat_trm, is.na(stream_dens)==FALSE)

# Stream slope & Stream length (we replace zero values with the corresponding flow line values)

summary(phys_dat_trm)

# We have missing values and zeroes for stream slope as well as for flowline slope




phys_dat_trm <- phys_dat_trm %>% 
  mutate(stream_slope = if_else(stream_slope < 0, 
                                flowline_slope,
                                stream_slope),
         stream_lenght_km = if_else(stream_lenght_km== 0,
                                    flowline_lenght_km,
                                    stream_lenght_km))

# Bankfull width, depth, and cross sectional area

summary(filter(phys_dat_trm, bnkfll_width_m == 0))

# Only 5 values corresponding to first order streams. We proceed to remove these rows

phys_dat_trm <- filter(phys_dat_trm, bnkfll_width_m != 0)

summary(phys_dat_trm)
