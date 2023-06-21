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

phys_dat_ro <- read_csv(paste(raw_data,"230620_ord_basin_hydrogeom_swf.csv", sep = '/'),
                        show_col_types = FALSE)

son_etal_dat <- read_csv(paste(raw_data,"230406_son_etal_22_results_zen.csv", sep = '/'),
                         show_col_types = FALSE)


ntwk_dat <- phys_dat_ro %>% 
  merge(.,
        son_etal_dat %>% 
          filter(.,time_type=="annual"),
        by = "comid",
        all.x = TRUE)

write.csv(ntwk_dat,paste(raw_data,"230620_guerrero_etal_network_swf.csv", sep = "/"),
          row.names = FALSE)

# Exploring the data via summary

summary(phys_dat_ro)

# We find several variables with zero or missing values.These flow lines do not have a catchment area 
# associated to them because they are not connected to the network (Blodget, comms. pers.). So we will remove thase lines from the 
# dataset. The same applies to flowlines with catchment areas = 0

# We will filter our data for stream and rivers only and check whether this takes care of many
# NAs


phys_dat_mod1 <- filter(phys_dat_ro, reach_type=="StreamRiver")

summary(phys_dat_mod1)

phys_dat_mod2 <- filter(phys_dat_mod1, wshd_area_km2!=0 & ctch_area_km2!=0)

summary(phys_dat_mod2)

write.csv(phys_dat_mod2,paste(raw_data,"230620_phys_dat_mod_2.csv", sep = "/"),
          row.names = FALSE)


# Summary

# The dataset "230430_ord_basin_hydrogeom_yrb_wrb.csv" had two modifications:

# 1. Filtering out artificial channels and features (4420 datapoints)

# 2. Removal of catchment/watershed area data with zero values (194 datapoints)



###############################################################################
# Surface area scaling

stream_sa_dat <- phys_dat_ro %>% 
  select(basin,
         comid,
         tocomid,
         wshd_area_km2,
         mean_ann_flow_m3s,
         reach_type,
         reach_length_km,
         bnkfll_width_m) %>% 
  mutate(stream_area_m2 = reach_length_km*1000*bnkfll_width_m) %>% 
  mutate(wilk_bnkfll_width_m = if_else(wshd_area_km2<5,
                                       2.18*wshd_area_km2^0.191,
                                       if_else(wshd_area_km2<336,
                                               1.41*wshd_area_km2^0.462,
                                               if_else(wshd_area_km2>336,
                                                       7.18*wshd_area_km2^0.183,NA)))) %>% 
  mutate(wilk_stream_area_m2 = reach_length_km*1000*wilk_bnkfll_width_m)



library(nhdplusTools)

library(purrr)
library(dplyr)

accm_dat <- stream_sa_dat %>% 
  group_by(basin) %>% 
  select(comid,
         tocomid,
         basin,
         wshd_area_km2,
         mean_ann_flow_m3s,
         reach_type,
         stream_area_m2,
         reach_length_km,
         wilk_stream_area_m2,
         bnkfll_width_m) %>% 
  mutate(across(stream_area_m2:bnkfll_width_m, ~ calculate_arbolate_sum(data.frame(ID = comid,
                                                                                toID = tocomid,
                                                                                length = .x))) %>% 
           set_names(paste0("accm_", names(select(., stream_area_m2:bnkfll_width_m))))) 


p <- ggplot(data = filter(accm_dat, reach_type=="StreamRiver"),
            aes(x = wshd_area_km2,
                y = accm_reach_length_km))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(~basin,ncol = 3)
p

p <- ggplot(data = accm_dat,
            aes(x = wshd_area_km2,
                y = accm_wilk_stream_area_m2,
                color = reach_type))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(basin~reach_type,ncol = 5)
p







