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

# 72 NAs


# Exploring relationships with other variables that scale with watershed area like tot_
# stream_length

a_plot <- ggplot(data = filter(phys_dat_ro,
                               wshd_area_km2>0),
                 aes(x = tot_stream_length_km,
                     y = wshd_area_km2,
                     color = basin))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point(alpha=0.05)+
  geom_smooth()+
  geom_vline(xintercept = 0.5,
             linetype = "dotted")+
  geom_smooth(data = filter(phys_dat_ro,
                     wshd_area_km2>0&
                     tot_stream_length_km<0.5),
              method = "lm",
              color = "black",
              se = FALSE)+
  facet_wrap(~basin, ncol =2)
a_plot


# 72 data points with missing values for reach slope and 22 NAs for roughness.All these 
# values corresponding to 1st order streams with also 43 NAs in wshd_stream_dens

phys_dat_trm0 <- filter(phys_dat_ro,wshd_area_km2!=0)

# Checking the summary of the trimmed dataset

summary(phys_dat_trm0)

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

# We observe a consistent decrease of roughness with stream order, so We proceed to 
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

# We observe 30 missing values (-9998) for stream slope. Se we proceed to check how 
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


# Mean annual flow

summary(filter(phys_dat_trm2, mean_ann_flow_m3s==0))

# we have 22 values all corresponding to first order streams, yet, mean_ann_vel_ms
# is non-zero for these reaches. 

q_plot <- ggplot(data = filter(phys_dat_trm2,
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
            data = filter(phys_dat_trm2,
                          mean_ann_flow_m3s>0),
            na.action = na.omit)

summary(q_mod)


phys_dat_trm3 <- phys_dat_trm2 %>% 
  mutate(mean_ann_flow_m3s = if_else(mean_ann_flow_m3s==0,
                                     exp(predict.lm(q_mod,.)),
                                     mean_ann_flow_m3s))

summary(phys_dat_trm3)

# We have 8 zero values for bank full width, depth, and cross_sectional area

summary(filter(phys_dat_trm3,bnkfll_width_m==0))

# We replace them with their corresponding predictions from mean annual flow following
# hydraulic geometry w = aQ^b

w_mod <- lm(log(bnkfll_width_m)~log(mean_ann_flow_m3s)+basin,
            data = filter(phys_dat_trm3,
                          bnkfll_width_m>0))
summary(w_mod)

d_mod <- lm(log(bnkfll_depth_m)~log(mean_ann_flow_m3s)+basin,
            data = filter(phys_dat_trm3,
                          bnkfll_depth_m>0))
summary(d_mod)

phys_dat_trm4 <- phys_dat_trm3 %>% 
  mutate(bnkfll_width_m = if_else(bnkfll_width_m==0,
                                     exp(predict.lm(w_mod,.)),
                                     bnkfll_width_m),
         bnkfll_depth_m = if_else(bnkfll_depth_m==0,
                                  exp(predict.lm(d_mod,.)),
                                  bnkfll_depth_m),
         bnkfll_xsec_area_m2 = if_else(bnkfll_xsec_area_m2 ==0,
                                       bnkfll_width_m*bnkfll_depth_m,
                                       bnkfll_xsec_area_m2))

summary(phys_dat_trm4)
