################################################################################
# SCALING WATERSHED FUNCTION: DATA CLEANING - PART II Additional Physical Attributes
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

phys_dat_mod2 <- read_csv(paste(raw_data,"230504_phys_dat_mod_2.csv", sep = '/'),
                        show_col_types = FALSE)


# Exploring the data via summary

summary(phys_dat_mod2)


# Roughness

# NA values in roughness correspond to missing reach_slope values. Otherwise, these NAs cover
# a wide range of watershed characteristics

n_plot <- ggplot(data = filter(phys_dat_mod2, 
                               is.na(roughness) == FALSE),
                 aes(x = as.factor(stream_order),
                     y = roughness,
                     color=as.factor(stream_order)))+
  facet_wrap(~basin,ncol = 2)+
  geom_boxplot()+
  theme(legend.position = "none")
n_plot


# We observe a consistent decrease of roughness with stream order, so We proceed to 
# replace the missing n values by the average value for the corresponding 
# stream order

phys_dat_mod3 <- phys_dat_mod2 %>% 
  group_by(stream_order,
           basin) %>% 
  mutate(roughness_ord = mean(roughness,na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(roughness = if_else(is.na(roughness),
                             roughness_ord,
                             roughness)) %>% 
  select(-roughness_ord)

summary(phys_dat_mod3)

# We observe 52 missing values (-9998) for stream slope. Se we proceed to check how 
# many of them are and how those empty values for stream slope are related to the
# dataset

summary(filter(phys_dat_mod3,reach_slope < 0))

# Since these missing values encompass multiple stream orders, we inspect the relationship
# between stream slope and stream order

slope_order <- filter(phys_dat_mod3,reach_slope>=0) %>% 
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

# We proceed to replace missing slope values with the average for their correspondent
# stream order

phys_dat_mod4 <- phys_dat_mod3%>% 
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

summary(phys_dat_mod4)


# Mean annual flow

summary(filter(phys_dat_mod4, mean_ann_flow_m3s==0))

# we have 94 values all corresponding to first order streams, yet, mean_ann_vel_ms
# is non-zero for these reaches. 

q_plot <- ggplot(data = filter(phys_dat_mod4,
                               mean_ann_flow_m3s>0),
                 aes(x = mean_ann_vel_ms,
                     y = mean_ann_flow_m3s,
                     color = basin))+
  geom_point()+
  scale_y_log10()+
  scale_x_log10()+
  facet_wrap(~basin, ncol = 2)
q_plot


# We replace these missing values with predictions from a regression between 
# mean_ann_flow_m3s on mean_ann_vel_ms and other regressors

q_mod <- lm(log(mean_ann_flow_m3s)~log(mean_ann_vel_ms)+
              wshd_area_km2+
              basin+
              mean_ann_runf_mm,
            data = filter(phys_dat_mod4,
                          mean_ann_flow_m3s>0),
            na.action = na.omit)

summary(q_mod)


phys_dat_mod5 <- phys_dat_mod4 %>% 
  mutate(mean_ann_flow_m3s = if_else(mean_ann_flow_m3s==0,
                                     exp(predict.lm(q_mod,.)),
                                     mean_ann_flow_m3s))

summary(phys_dat_mod5)

# Checking magnitudes
summary(1/phys_dat_mod5$mean_ann_flow_m3s)

# A maximum of 1.139 e+10 indicates the predicted flows are unrealistically low.
# We will replace these values with 1% of the first quantile, in this case
# 0.00042

phys_dat_mod6 <- phys_dat_mod5 %>% 
  mutate(mean_ann_flow_m3s = if_else(mean_ann_flow_m3s < 0.0000042,
                                     0.000042,
                                     mean_ann_flow_m3s))

# Checking magnitudes
summary(1/phys_dat_mod6$mean_ann_flow_m3s)

# We have 51 zero values for bank full width, depth, and cross_sectional area

summary(filter(phys_dat_mod6,bnkfll_width_m==0))

# We replace them with their corresponding predictions from mean annual flow following
# hydraulic geometry w = aQ^b

w_mod <- lm(log(bnkfll_width_m)~log(mean_ann_flow_m3s)+basin,
            data = filter(phys_dat_mod6,
                          bnkfll_width_m>0))
summary(w_mod)

d_mod <- lm(log(bnkfll_depth_m)~log(mean_ann_flow_m3s)+basin,
            data = filter(phys_dat_mod6,
                          bnkfll_depth_m>0))
summary(d_mod)

phys_dat_mod7 <- phys_dat_mod6 %>% 
  mutate(bnkfll_width_m = if_else(bnkfll_width_m==0,
                                  exp(predict.lm(w_mod,.)),
                                  bnkfll_width_m),
         bnkfll_depth_m = if_else(bnkfll_depth_m==0,
                                  exp(predict.lm(d_mod,.)),
                                  bnkfll_depth_m),
         bnkfll_xsec_area_m2 = if_else(bnkfll_xsec_area_m2 ==0,
                                       bnkfll_width_m*bnkfll_depth_m,
                                       bnkfll_xsec_area_m2))

summary(phys_dat_mod7)

# Checking magnitudes

# Finally, have multiple missing values for both wshd stream density and catchment
# stream density. Since we have updated values for both watershed area and 
# catchment area, we can recalculate this missing values: 


phys_dat_mod8 <- phys_dat_mod7 %>% 
  mutate(wshd_stream_dens=tot_stream_length_km/wshd_area_km2,
         ctch_stream_dens=reach_length_km/ctch_area_km2)

# Cummulative values will be recalculated along with other cummulative variables
# as part of the data analysis. 

write.csv(phys_dat_mod8,paste(raw_data,"230504_phys_dat_mod8.csv", sep = "/"),
          row.names = FALSE)
