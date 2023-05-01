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

# We find several variables with zero or missing values. We will start with missing or
# NA values for watershed area since this variable is instrumental for scaling analysis. 

summary(filter(phys_dat_ro, wshd_area_km2 == 0))

# 72 data points with missing values for reach slope and 22 NAs for roughness.All these 
# values corresponding to 1st order streams with also 43 NAs in wshd_stream_dens

phys_dat_trm0 <- filter(phys_dat_ro,wshd_area_km2!=0)

# Checking the summary of the trimmed dataset

summary(phys_dat_trm0)

# We find 30 NAs values for roughness, which we will use in the estimation of residence
# times

summary(filter(phys_dat_trm0,is.na(roughness)==TRUE))










# Total Drainage Area

summary(filter(phys_dat_ro,wshd_area_km2 == 0))

# All 45 NA values correspond to first order with small watershed areas (close to zero).

phys_dat_trm <- (filter(phys_dat_ro,wshd_stream_dens=0))

summary(phys_dat_trm)

# We have 30 data points with NAs for roughness

summary(filter(phys_dat_trm, is.na(roughness)==TRUE))

# Roughness

# NA values in roughness correspond to missing reach_slope values. Otherwise, these NAs cover
# a wide range of watershed characteristics

n_plot <- ggplot(data = filter(phys_dat_trm0, 
                               is.na(roughness) == FALSE),
                 aes(x = as.factor(stream_order),
                     y = roughness,
                     color=as.factor(stream_order)))+
  facet_wrap(~basin,ncol = 2)+
  geom_boxplot()+
  theme(legend.position = "none")
n_plot

# WE observe a consistence decrease of roughness with stream order, so We proceed to 
# replace the missing n values by the average value for the corresponding 
# stream order

phys_dat_trm1 <- phys_dat_trm0 %>% 
  group_by(stream_order,
           basin) %>% 
  mutate(roughness_ord = mean(roughness,na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(roughness = if_else(is.na(roughness),
                             roughness_ord,
                             roughness)) %>% 
  select(-roughness_ord)

# We observe missing values (-9998) for stream slope. Se we proceed to check how 
# many of them are and how those empty values for stream slope are related to the
# dataset

summary(filter(phys_dat_trm1,reach_slope < 0))

# Since these missing values encompass multiple stream orders, we inspect the relationship
# between stream slope and stream order

slope_order <- filter(phys_dat_trm1,reach_slope>=0) %>% 
  select(stream_order,
         basin,
         reach_slope) %>% 
  ggplot(aes(x = as.factor(stream_order),
             y = reach_slope,
             color = as.factor(stream_order)))+
  geom_boxplot()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
slope_order

# We proceed to replace missing slope values with the averge for their correspondent
# stream order

phys_dat_trm2 <- phys_dat_trm1%>% 
  group_by(stream_order,
           basin) %>% 
  mutate(slope_na = if_else(reach_slope<0,
                             NA,
                             reach_slope),
         slope_od = if_else(is.na(slope_na),
                            mean(slope_na,na.rm = TRUE),
                            slope_na)) %>% 
  ungroup() %>% 
  mutate(reach_slope = if_else(reach_slope<0,
                               slope_od,
                               reach_slope))%>% 
  select(-c(slope_na,slope_od))

summary(phys_dat_trm2)




test <- select(phys_dat_trm1,
               comid,
               basin,
               stream_order,
               reach_slope) %>% 
  group_by(stream_order,basin) %>% 
  mutate(slope_na = if_else(reach_slope < 0,
                             NA,
                             reach_slope),
         slope_od = if_else(is.na(slope_na),
                            mean(slope_na,na.rm = TRUE),
                            slope_na))


# Mean annual flow

summary(filter(phys_dat_trm, mean_ann_flow_m3s==0))

# we have 22 values all corresponding to first order streams, we proceed to remove
# these rows: 

phys_dat_trm <- filter(phys_dat_trm, mean_ann_flow_m3s!=0)

# Stream density

# We have only 1 NA value corresponding to a 1 order stream. We proceed to remove this
# row

phys_dat_trm <- filter(phys_dat_trm, is.na(ctch_stream_dens)==FALSE)

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
