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

# We find several variables with zero or missing values. We will start with missing or
# NA values for watershed area since this variable is instrumental for scaling analysis. 

summary(filter(phys_dat_ro, wshd_area_km2 == 0))

# The minimum value of tot_stream_length for which there are zero values for wshd_area_km2
# is

l_intercept = min(filter(phys_dat_ro, wshd_area_km2 == 0)$tot_stream_length_km)

# The minimum value of wshd_area_km2 for the non-zero dataset is:

a_intercept = min(filter(phys_dat_ro, wshd_area_km2 > 0)$wshd_area_km2)

# Exploring relationships with other variables that scale with watershed area like tot_
# stream_length

w_plot <- ggplot(data = filter(phys_dat_ro,
                               wshd_area_km2>0),
                 aes(x = tot_stream_length_km,
                     y = wshd_area_km2,
                     color = basin))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point(alpha=0.5)+
  geom_smooth()+
  facet_wrap(~basin, ncol =2)
w_plot

# We observe that variability in watershed area is inversely proportional to 
# stream_length. So replacing area values for small streams with the expected
# value given the stream_length_km, would tend to over estimate area values. 


#Envelope data total stream length and watershed area

w_env_dat <- phys_dat_ro %>% 
  select(basin,
         tot_stream_length_km,
         wshd_area_km2) %>% 
  filter(wshd_area_km2>0) %>% 
  group_by(tot_stream_length_km) %>% 
  mutate(avg_area = mean(wshd_area_km2),
         med_area = median(wshd_area_km2),
         max_area = max(wshd_area_km2),
         min_area = min(wshd_area_km2),
         rng_area = max_area - min_area) %>% 
  distinct(tot_stream_length_km, .keep_all = TRUE) %>% 
  filter(rng_area!= 0) %>% 
  select(basin,
         tot_stream_length_km,
         avg_area,
         med_area,
         min_area,
         max_area,
         rng_area)

# Checking relationship between stream length and stat values for watershed area. 

w_stat_plot <- w_env_dat %>% 
  select(-rng_area) %>% 
  gather(key = "a_stats",
         value = "wshd_area_km2",
         c(3:6),
         factor_key = TRUE) %>% 
  ggplot(aes(tot_stream_length_km,
             wshd_area_km2,
             color = a_stats))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point(alpha=0.1)+
  geom_smooth()+
  geom_vline(xintercept = 1.6, #maximum stream length for which there is a whsd_area = 0
             linetype = "dashed")+
  facet_wrap(~basin, ncol = 2)
w_stat_plot

# We observe that the slope of the regression line changes around stream lengths < 5km. 
# Also, that a regression model on the min values would better reflect those changes. 
# This could be improved for larger datasets with a quantile regression model.




w_qr_mod_check <- phys_dat_ro %>% 
  filter(tot_stream_length_km < 1.6 &
           wshd_area_km2!=0) %>% 
  select(basin,
         tot_stream_length_km,
         wshd_area_km2) %>% 
  mutate(w_p_area = predict.rq(w_qr_mod,.)) %>% 
  ggplot(aes(tot_stream_length_km,
             wshd_area_km2,
             color = basin))+
  geom_point(alpha = 0.5)+
  geom_point(aes(tot_stream_length_km,
                 w_p_area),
             alpha = 0.5, 
             color = "gray")+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
w_qr_mod_check


#For this simpler case, we would replace missing values with predictions of a regression of min_area
# on tot_stream_length

# Min area submodel

w_min_mod <- w_env_dat %>% 
  filter(tot_stream_length_km < 1.6) %>% 
  select(basin,
         tot_stream_length_km,
         min_area) %>% 
  mutate(wshd_area_km2 = min_area) %>% 
  lm(log(wshd_area_km2)~log(tot_stream_length_km) + basin,
     data = .)


# Mean area submodel
w_avg_mod <- w_env_dat %>% 
  filter(tot_stream_length_km < 1.6) %>% 
  select(basin,
         tot_stream_length_km,
         avg_area) %>% 
  mutate(wshd_area_km2 = avg_area) %>% 
  lm(log(wshd_area_km2)~log(tot_stream_length_km) + basin,
     data = .)

summary(w_min_mod)
summary(w_avg_mod)

# Let's plot the resulting intercepts along with the data: 

w_stat_plot_i <- delete_layers(w_stat_plot,"GeomPoint")+
  geom_hline(yintercept = exp(-0.47371), linetype = "dashed", color ="darkorchid")+
  geom_hline(yintercept = exp(0.20136), linetype = "dashed", color ="darkorange")
w_stat_plot_i


# The slope for the min values is quite steeper than for mean values. We will replace
# NA's with predictions from the former regression

phys_dat_mod1 <- phys_dat_ro %>% 
  mutate(wshd_area_km2 = if_else(wshd_area_km2==0 &
                                   tot_stream_length_km<1.6,
                                 exp(predict.lm(w_min_mod,.)),
                                     wshd_area_km2))

summary(phys_dat_mod1)

# Checking magniturdes
summary(1/phys_dat_mod1$wshd_area_km2)

# Minimum watershed area is in the order of 1e-4 km2 ~ 0.01 ha

# Catchment areas

# We will explore a similar approach here, since we have complete data about 
# reach lengths

# Let's first check how many 0 values we have

summary(filter(phys_dat_mod1,ctch_area_km2==0))

# We have a total of 163 values. 

# Plotting the relationship between reach length and catchment area

c_plot <- ggplot(data = filter(phys_dat_mod1,
                               ctch_area_km2>0),
                 aes(x = reach_length_km,
                     y = ctch_area_km2,
                     color = basin))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point(alpha=0.05)+
  geom_smooth(method = "lm")+
  facet_wrap(~basin, ncol =2)
c_plot

#Envelope data reach length and catchment area

c_env_dat <- phys_dat_mod1 %>% 
  select(basin,
         reach_length_km,
         ctch_area_km2) %>% 
  filter(ctch_area_km2>0) %>% 
  group_by(reach_length_km) %>% 
  mutate(avg_area = mean(ctch_area_km2),
         med_area = median(ctch_area_km2),
         max_area = max(ctch_area_km2),
         min_area = min(ctch_area_km2),
         rng_area = max_area - min_area) %>% 
  distinct(reach_length_km, .keep_all = TRUE) %>% 
  filter(rng_area!= 0) %>% 
  select(basin,
         reach_length_km,
         avg_area,
         med_area,
         min_area,
         max_area,
         rng_area)

# Checking relationship between stream length and stat values for watershed area. 

c_stat_plot <- c_env_dat %>% 
  select(-rng_area) %>% 
  gather(key = "c_stats",
         value = "ctch_area_km2",
         c(3:6),
         factor_key = TRUE) %>% 
  ggplot(aes(reach_length_km,
             ctch_area_km2,
             color = c_stats))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point(alpha=0.05)+
  geom_smooth()+
  geom_vline(xintercept = 1.6, #maximum reach length for which there is a ctch_area = 0
             linetype = "dashed")+
  facet_wrap(~basin, ncol = 2)
c_stat_plot

# In this case seems more reasonable to compare a model for the minimum and median values

# Min area submodel

c_min_mod <- c_env_dat %>% 
  filter(reach_length_km < 1.6) %>% 
  select(basin,
         reach_length_km,
         min_area) %>% 
  mutate(ctch_area_km2 = min_area) %>% 
  lm(log(ctch_area_km2)~log(reach_length_km) + basin,
     data = .)


# Median area submodel
c_med_mod <- c_env_dat %>% 
  filter(reach_length_km < 1.6) %>% 
  select(basin,
         reach_length_km,
         med_area) %>% 
  mutate(ctch_area_km2 = med_area) %>% 
  lm(log(ctch_area_km2)~log(reach_length_km) + basin,
     data = .)


summary(c_min_mod)
summary(c_med_mod)

# Plotting regression intercepts

c_stat_plot_i <- delete_layers(c_stat_plot,"GeomPoint")+
  geom_hline(yintercept = exp(-0.55331), linetype = "dashed", color ="darkorchid")+
  geom_hline(yintercept = exp(0.03601), linetype = "dashed", color ="darkorange")
c_stat_plot_i

# Both models do a really good job. For consistency, we will use c_min_mod

phys_dat_mod2 <- phys_dat_mod1 %>% 
  mutate(ctch_area_km2 = if_else(ctch_area_km2==0 &
                                 reach_length_km<1.6,
                                 exp(predict.lm(c_min_mod,.)),
                                 ctch_area_km2))

summary(phys_dat_mod2)

# Checking magniturdes
summary(1/phys_dat_mod2$ctch_area_km2)

write.csv(phys_dat_mod2,paste(raw_data,"230504_phys_dat_mod_2.csv", sep = "/"),
          row.names = FALSE)


# Summary

# The dataset "230430_ord_basin_hydrogeom_yrb_wrb.csv" had two modifications:

# 1. Watershed area estimation for channels with length > 1.6 km, for which the original
# data had zero values (72 datapoints)

# 2. Catchment area estimation for channels with length < 1.6 km, for which the original 
# data had zero values (163 datapoints)

# The estimation process used was a linear regression of watershed area on the minimum
# values of stream length. However For heteroscedastic data like these it is better to predict 
# changes in the quantile values than building a model for the mean.

################################## IN PROGRESS ################################################

# The quantile regression process would look like

# Let's start by taking a look at the quantile distribution of watershed area, 
# excluding zero values. 

quantile(filter(phys_dat_ro,wshd_area_km2 > 0)$wshd_area_km2, probs = seq(0,1,0.1))

# Let's take a look at these values on our previous plot

w_q_plot <- delete_layers (w_plot,"GeomPoint")+
  geom_hline(yintercept = 1.325,
             linetype = "dashed")+
  geom_hline(yintercept = 2.169,
             linetype = "dashed")+
  geom_hline(yintercept = 3.225,
             linetype = "dashed")+
  geom_hline(yintercept = 4.869,
             linetype = "dashed")+
  geom_hline(yintercept = 7.751,
             linetype = "dashed")
w_q_plot

w_qr_mod <- phys_dat_ro %>% 
  filter(tot_stream_length_km < 1.6) %>% 
  select(basin,
         tot_stream_length_km,
         wshd_area_km2) %>% 
  rq(wshd_area_km2~tot_stream_length_km + basin,
     tau = 0.35,
     data = .)

summary(w_qr_mod, "nid")

# All slopes significant except for q 95%














