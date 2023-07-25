











test_dat <- rcm_22_model_dat %>% 
  merge(.,
        phys_dat_ro,
        by = "comid",
        all.x = TRUE) %>% 
  select(-tocomid.y) %>% 
  rename(tocomid = tocomid.x)

summary(filter(test_dat, reach_slope > 0))

# 16588 data points

# Testing connectivity

test_dat_connectivity <- test_dat %>% 
  filter(ctch_area_km2 > 0) %>%
  group_by(basin) %>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
test_dat_connectivity

missing_ctch_area <- filter(test_dat, ctch_area_km2 == 0)
summary(missing_ctch_area)


# Checking for redundant variables

p <- ggplot(data = filter(test_dat,
                          ctch_area_km2 > 0),
            aes(x = reach_length_km,
                y = ctch_area_km2))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point()+
  geom_abline()+
  geom_smooth(span = 0.15)+
  facet_wrap(~basin, ncol = 2)
p

library(ggplot2)
library(dplyr)

# Filter the data to remove rows with missing 'ctch_area_km2' values
filtered_data <- filter(test_dat, ctch_area_km2 > 0)

# Fit the loess model
model <- loess(log10(ctch_area_km2) ~ log10(reach_length_km), data = filtered_data, span = 0.15)

# Filter the data to get rows with missing 'ctch_area_km2' values
missing_data <- filter(test_dat, is.na(ctch_area_km2) | ctch_area_km2 < 0)

# Predict the missing 'ctch_area_km2' values using the loess model
predicted_values <- 10^predict(model, newdata = missing_data)

# Update the missing 'ctch_area_km2' values with the predicted values
missing_data$ctch_area_km2 <- predicted_values

# Combine the original data and the data with predicted values
estimated_data <- rbind(filtered_data, missing_data)

# Plot the data with estimated values
p <- ggplot(data = estimated_data,
            aes(x = reach_length_km, y = ctch_area_km2)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point() +
  geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color = "blue") +
  geom_smooth(span = 0.15, color = "red") +
  facet_wrap(~basin, ncol = 2)

print(p)




# remove LENGTHKM, length_m, REACHCODE, tocomid.y,HUC4, FDATE, RESOLUTION, GNIS_ID,
# GNIS_NAME. There are 181 locations for which there are large discrepancies between
# wshd_area_km2 and TotDASqKM, we keep wshd_area_km2 from enhanced nhdplus v. 2.1
# as well as ctch_area_km2 instead of AreaSqKM. We will also rename tocomid.x to tocomid,
# and replace mean annual width values with their corresponding prediction from wshd_area_km2


nsi_rcm_phys_dat <- as.data.frame(nsi_rcm_phys_dat_0 %>% 
                                    select(-geometry)) %>% 
  select(-c(LENGTHKM,
            length_m,
            REACHCODE,
            tocomid.y,
            HUC4,
            FDATE,
            RESOLUTION,
            GNIS_ID,
            GNIS_NAME,
            TotDASqKM,
            AreaSqKM,
            geometry)) %>% 
  mutate(nlogw_m = predict(lm(logw_m~log10(wshd_area_km2)+basin,
                              data = .,
                              na.action = na.omit))) %>% 
  select(-logw_m) %>% 
  rename(tocomid = tocomid.x,
         logw_m = nlogw_m) %>% 
  mutate(stream_sa_m2 = 10^logw_m * reach_length_km*1000) %>% 
  merge(.,
        med_bed_part_dat %>% 
          select(comid,d50_mm),
        by = "comid",
        all.x = TRUE) 

# QA/QC

# Let's check the nsi_rcm_phys_dat for addtional missing values

summary(nsi_rcm_phys_dat)

# We find 29 NAs for roughness, -9999 values for reach slopes, -9999 values for
# accumulated stream slopes, 2 NAs for accumulated stream density, 261 NAs for
# catchment stream density, 2 NAs for wshd_stream_density

# Let's check how many missing values we have for slope

summary(filter(nsi_rcm_phys_dat, reach_slope < 0)) # 29 datapoints. 

# Let's check the effect of removing these points in terms of network integrity

nsi_rcm_ntwk_slope_test <- nsi_rcm_phys_dat %>% 
  filter(reach_slope > 0 & DUP_COMID == 0) %>% 
  group_by(basin) %>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid)*100) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
nsi_rcm_ntwk_slope_test 

# Removing these data points significantly affects the integrity of the network.
# Let's take a look at the spatial location of these points

# Extract the start and end points from the LINESTRING geometries
points_data <- nsi_rcm_phys_dat_0 %>%
  st_cast("POINT")

# Create the leaflet map with polylines and markers
leaflet() %>% 
  addPolylines(data = nsi_rcm_phys_dat_0, weight = 2) %>% 
  addPolylines(data = filter(nsi_rcm_phys_dat_0, reach_slope < 0),
               color = "magenta",
               opacity = 1,
               weight = 9) %>% 
  addMarkers(data = filter(points_data, reach_slope < 0),
             label = ~comid) %>% 
  addProviderTiles("Esri.WorldImagery")


# Function to interpolate missing values (n<50) (e.g., roughness, reach_slope)
interpolate_missing_values <- function(data, column) {
  for (i in seq_len(nrow(data))) {
    # Check if the column value is missing (represented by negative number or NA)
    if (data[[column]][i] < 0 || is.na(data[[column]][i])) {
      # Get the immediate neighbors' mean value
      immediate_mean <- get_immediate_neighbors_mean(data, column, data$comid[i], data$tocomid.x[i])
      
      # If there are no immediate neighbors, replace with the average value for the same 'stream_order'
      if (is.na(immediate_mean)) {
        same_stream_order <- data$stream_order == data$stream_order[i]
        same_order_values <- data[[column]][same_stream_order & data[[column]] >= 0]
        immediate_mean <- mean(same_order_values, na.rm = TRUE)
      }
      
      # Assign the calculated value to the missing value
      data[[column]][i] <- immediate_mean
    }
  }
  return(data)
}

# Replacing missing values in the 'reach_slope' and 'roughness' column
nsi_rcm_phys_dat <- interpolate_missing_values(nsi_rcm_phys_dat, "reach_slope")
nsi_rcm_phys_dat <- interpolate_missing_values(nsi_rcm_phys_dat, "roughness")


# Let's take a look at other missing values to check if we can re-calculate them

filter(nsi_rcm_phys_dat,wshd_stream_dens == 0) #1 case that can be re-calculated
filter(nsi_rcm_phys_dat, is.na(wshd_stream_dens)== TRUE)# 2 cases that can be 
# recalculated

# accm_stream_density needs to be recalculated with calculate_arbolate_sum
# accm_stream_slope needs to be recalculated with...

# We find 261 NAs for catchment stream density: 

missing_ctch_stream_dens <- filter(nsi_rcm_phys_dat, is.na(ctch_stream_dens)== TRUE)
summary(missing_ctch_stream_dens)

# Although reach_lengths are >0, there are zero values cor ctch_area_km2

filter(missing_ctch_stream_dens, ctch_area_km2 == 0) # 89 cases

# Let's take a look at the catchment areas = 0

leaflet() %>% 
  addPolylines(data = nsi_rcm_phys_dat_0, weight = 2) %>% 
  addPolylines(data = filter(nsi_rcm_phys_dat_0, ctch_area_km2 == 0),
               color = "magenta",
               opacity = 1,
               weight = 9) %>% 
  addMarkers(data = filter(points_data, ctch_area_km2 == 0),
             label = ~comid) %>% 
  addProviderTiles("Esri.WorldImagery")






p <- ggplot(data = nsi_rcm_phys_dat,
            aes(x = reach_slope,
                y = roughness,
                color = as.factor(stream_order))) +
  geom_point()+
  scale_x_log10()+
  facet_wrap(~basin, ncol = 2)
p

# Now, the missing 'reach_slope' values represented by negative numbers are replaced with the appropriate averages in the 'nsi_rcm_phys_dat' dataset.


# Now, the missing 'reach_slope' values represented by negative numbers are replaced with the appropriate averages in the 'nsi_rcm_phys_dat' dataset.


# Now, the missing 'reach_slope' values represented by negative numbers are replaced with the appropriate averages.



# Assuming you have the necessary libraries loaded
library(leaflet)
library(dplyr)
library(sf)

# Extract the start and end points from the LINESTRING geometries
points_data <- nsi_rcm_phys_dat_0 %>%
  st_cast("POINT")

# Create the leaflet map with polylines and markers
leaflet() %>% 
  addPolylines(data = nsi_rcm_phys_dat_0, weight = 2) %>% 
  addPolylines(data = filter(nsi_rcm_phys_dat_0, reach_slope < 0),
               color = "magenta",
               opacity = 1,
               weight = 9) %>% 
  addPolylines(data = filter(nsi_rcm_phys_dat_0, comid == 23772710),
               color ="green",
               opacity = 1,
               weight = 9) %>% 
  addMarkers(data = filter(points_data, reach_slope < 0),
             label = ~comid) %>% 
  addProviderTiles("Esri.WorldImagery")

























slope_plot <- ggplot(data = nsi_rcm_phys_dat,
                     aes(x = as.factor(stream_order),
                         y = reach_slope,
                         color = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
slope_plot

nsi_rcm_phys_dat <- nsi_rcm_phys_dat %>% 
  mutate(n_reach_slope = if_else(reach_slope>0,
                                 reach_slope,
                                 (ctch_max_elevation_m - ctch_min_elevation_m)/(reach_slope_length_km*1000)),
         c_reach_slope = if_else(n_reach_slope < 0.00001,
                                 0.00001,
                                 n_reach_slope))

slope_plot <- ggplot(data = nsi_rcm_phys_dat,
                     aes(x = as.factor(stream_order),
                         y = c_reach_slope,
                         color = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  # scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
slope_plot


slope_area_plot <- ggplot(data=(filter(nsi_rcm_phys_dat, reach_slope > 0)),
                          aes(x = stream_sa_m2,
                              y = reach_slope))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
slope_area_plot

slope_mod <- lm(log10(reach_slope)~log10(reach_slope_length_km) + stream_order + basin +
                  # log10(ctch_basin_slope) + log10(ctch_avg_elevation_m) + log10(wshd_avg_elevation_m)+
                  log10(stream_sa_m2) + log10(wshd_area_km2) + log10(roughness),
                data = filter(nsi_rcm_phys_dat, reach_slope >0 & wshd_area_km2 > 0),
                na.action = na.omit)
summary(slope_mod)

slope_mod <- lm(log10(reach_slope) ~ log(reach_slope_length_km),
                data = filter(nsi_rcm_phys_dat, reach_slope >0),
                na.action = na.omit)
summary(slope_mod)


qaqc_nsi_rcm_phys_dat <- nsi_rcm_phys_dat %>% 
  group_by(basin) %>% 
  mutate(q_reach_slope = if_else(reach_slope < 0,
  ))



#Saving processed data
write.csv(nsi_rcm_phys_dat,paste(processed_data,"nsi_rcm_phys_dat.csv", sep = '/'),
          row.names = FALSE) 

hyporheic_predictors <- nsi_rcm_phys_dat %>% 
  mutate(log_wshd_area_km2 = log10(wshd_area_km2),
         log_reach_length_km = log10(reach_length_km),
         log_reach_slope = log10(reach_slope),
         log_mean_ann_flow_m3s = log10(mean_ann_flow_m3s),
         log_mean_ann_vel_ms = log10(mean_ann_vel_ms),
         log_d50_mm = log10(d50_mm)) %>% 
  select(log_wshd_area_km2,
         log_reach_length_km,
         log_reach_slope,
         log_mean_ann_flow_m3s,
         log_mean_ann_vel_ms,
         log_d50_mm,
         sinuosity,
         roughness,
         stream_order,
         logRT_vertical_hz_s,
         logRT_lateral_hz_s,
         logq_hz_vertical_m_div_s,
         logq_hz_lateral_m_div_s)

ggpairs(hyporheic_predictors)


summary(filter(nsi_rcm_phys_dat, reach_slope < 0))