################################################################################
# SCALING WATERSHED FUNCTION: PHYSICAL DATA QA/QC
################################################################################

#Author: Francisco J. Guerrero
gc()
# Loading required packages: 

librarian::shelf(tidyverse,
                 utils,
                 leaflet,
                 sp,
                 sf,
                 nhdplusTools,
                 GGally,
                 htmltools,
                 foreign,
                 data.table)

# Local Import-Export
source_data <- "../../raw_data"
local_data <- "./data"
local_metadata <- "./metadata"

nsi_rcm_phys_dat <- read_csv(paste(local_data,"river_corridors_physical_hyporheic_char.csv", sep = '/'),
                 show_col_types = FALSE)

# Let's take a look at the data

summary(nsi_rcm_phys_dat)

# Variables including zero values

# Calculate the number of zeros in each column
zero_counts <- colSums(nsi_rcm_phys_dat == 0, na.rm = TRUE)

# Create a new dataframe to store the report
zero_report <- data.frame(variable = names(zero_counts), Zeros = zero_counts) %>% 
  filter(Zeros > 0)

# Print the report without row names
print(zero_report, row.names = FALSE)

# We will first remove DUP_COMID that are a special feature of the NSI dataset and test 
# for connectivity

# Testing initial network connectivity

test_dat_connectivity <- nsi_rcm_phys_dat %>% 
  filter(DUP_COMID == 0) %>% 
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

########## DATASET MODIFICATION ################################################ 
# We will drop the DUP_COMIDS == 1 and DUP_ArSqKM from the dataset  
################################################################################

# Let's now take a look at those comid's with zero values for watershed areas. 
# According to Blodgett (2023, pers. coms.) these zero values may correspond to 
# flowlines that are not connected to the network but were found and digitized

summary(filter(nsi_rcm_phys_dat, wshd_area_km2 == 0))

# All the flowlines with zero values in wshd area are first order streams, so let's
# run a connectivity test that ignores these values: 

test_dat_connectivity <- nsi_rcm_phys_dat %>% 
  filter(DUP_COMID == 0 & wshd_area_km2 > 0) %>% 
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

# As suspected, we can remove these values whitout any lost in network connectivity

########## DATASET MODIFICATION ################################################ 
# We will drop comid's with wshd_area_km2 == 0 from the dataset
################################################################################

# Let's now look at the remaining values with catchment areas = 0

summary(filter(nsi_rcm_phys_dat, wshd_area_km2 > 0 & ctch_area_km2 == 0))

# We find 89 additional datapoint with catchment areas = 0. These data points
# encompass multiple stream orders, have reach lengths between 6 to 11.4 m and 
# non-zero values for accumulated stream density, so they are actually drained
# and connected to the network. So, it could be expected that just removing these
# datapoints, would result in reduction of network connectivity:


test_dat_connectivity <- nsi_rcm_phys_dat %>% 
  filter(DUP_COMID == 0 & wshd_area_km2 > 0) %>%
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

# As expected network connectivity decreases significantly in both basins, but more
# so in the YRB

# Let's plot catchment area vs reach_length as it is expected that these two variables
# scale with each other:

# Catchment area ~ Reach length

ctch_rch_plot <- ggplot(data = nsi_rcm_phys_dat %>% 
                          filter(DUP_COMID == 0 & wshd_area_km2 > 0) %>%
                          filter(ctch_area_km2 > 0),
                        aes(x = reach_length_km*1000,
                            y = ctch_area_km2,
                            color = as.factor(stream_order)))+
  geom_point()+
  geom_smooth(span = 0.10)+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
ctch_rch_plot

# Although the relationship is not ideal, it would be good enough to fill the 
# small number of gaps

############################# DATASET MODIFICATION #############################
# Fill gaps for catchment area with loess model:
# model <- loess(log10(ctch_area_km2) ~ log10(reach_length_km), data = ., span = 0.15)
################################################################################


# Variables with missing data

# Calculate the number of NA values in each column
na_counts <- colSums(is.na(nsi_rcm_phys_dat))

# Calculate the number of negative values in each column
negative_counts <- colSums(nsi_rcm_phys_dat < 0, na.rm = TRUE)

# Create a new dataframe to store the report
report_data <- data.frame(
  variable = names(nsi_rcm_phys_dat),
  NA_s = na_counts,
  Negative_s = negative_counts
) %>%
  filter(NA_s > 0 | Negative_s > 0)

# Print the report
print(report_data, row.names = FALSE)

# Besides the gaps we have in the hyporheic characteristics and D50 values, we 
# we also have 43 missing values for stream width (logw_m), 34 missing values 
# in roughness

# We will use a pair of functions to interpolate missing values (n<50) (e.g., roughness, reach_slope)
get_immediate_neighbors_mean <- function(data, column, comid, tocomid) {
  if (is.na(comid) || is.na(tocomid) || comid < 0 || tocomid < 0)
    return(NA)
  
  immediate_neighbors <- c(comid, tocomid)
  values <- data[[column]][data$comid %in% immediate_neighbors & data[[column]] >= 0]
  mean_value <- mean(values, na.rm = TRUE)
  return(mean_value)
}

interpolate_missing_values <- function(data, column) {
  data <- data %>%
    mutate({{column}} := ifelse({{column}} < 0 | is.na({{column}}), NA, {{column}}))
  
  for (i in seq_len(nrow(data))) {
    # Check if the column value is missing (represented by NA)
    if (is.na(data[[column]][i])) {
      # Get the immediate neighbors' mean value
      immediate_mean <- get_immediate_neighbors_mean(data, column, data$comid[i], data$tocomid.x[i])
      
      # If there are no immediate neighbors, replace with the average value for the same 'stream_order'
      if (is.na(immediate_mean)) {
        same_stream_order <- data$stream_order == data$stream_order[i]
        same_order_values <- data[[column]][same_stream_order & !is.na(data[[column]])]
        immediate_mean <- mean(same_order_values, na.rm = TRUE)
      }
      
      # Assign the calculated value to the missing value
      data[[column]][i] <- immediate_mean
    }
  }
  return(data)
}


############################# DATASET MODIFICATION #############################
# Fill gaps for slope and roughness with: interpolate_missing_values()
################################################################################


# In summary: 
# 1. We drop the DUP_COMIDS == 1 and DUP_ArSqKM from the dataset  
# 2. For first order streams with both wshd_basin_slope or ctch_basin_slope = 0,
# we replace these values with the reach slope
# 2. We drop comid's with wshd_area_km2 == 0 from the dataset
# 3. We fill gaps for catchment area with loess model:
# model <- loess(log10(ctch_area_km2) ~ log10(reach_length_km), data = ., span = 0.15)
# 4. We fill gaps for slope and roughness with: interpolate_missing_values()


library(dplyr)

nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_dat %>% 
  filter(DUP_COMID == 0) %>% 
  select(-c(DUP_COMID, DUP_ArSqKM, DUP_Length)) %>% 
  filter(wshd_area_km2 > 0) %>% 
  mutate(wshd_basin_slope = if_else(wshd_basin_slope == 0 & stream_order == 1,
                                    reach_slope,
                                    wshd_basin_slope),
         ctch_basin_slope = if_else(ctch_basin_slope == 0 & stream_order == 1,
                                    reach_slope,
                                    ctch_basin_slope))


###############################################################################
# Fiting the loess model and estimating catchment area from reach lenght

model <- loess(log10(ctch_area_km2) ~ log10(reach_length_km), 
               data = nsi_rcm_phys_qaqc_dat %>% 
                 select(comid,
                        ctch_area_km2,
                        reach_length_km) %>% 
                 filter(ctch_area_km2 >0), 
               span = 0.15)

# Filter the data to get rows with missing 'ctch_area_km2' values
missing_data <- nsi_rcm_phys_qaqc_dat %>% 
  select(comid,
         ctch_area_km2,
         reach_length_km) %>% 
  filter(is.na(ctch_area_km2) | ctch_area_km2 == 0)

# Predict the missing 'ctch_area_km2' values using the loess model
predicted_values <- 10^predict(model, newdata = missing_data)

# Update the missing 'ctch_area_km2' values with the predicted values
missing_data$ctch_area_km2 <- predicted_values

# Combine the original data and the data with predicted values
estimated_ctch_data <- rbind(nsi_rcm_phys_qaqc_dat %>% 
                          select(comid,
                                 ctch_area_km2,
                                 reach_length_km) %>% 
                          filter(ctch_area_km2>0),
                          missing_data)

# Replace original values in the dataset for their estimates:
nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat%>% 
  select(-ctch_area_km2) %>% 
  merge(.,
        estimated_ctch_data %>% 
          select(comid,ctch_area_km2),
        by = "comid",
        all.x = TRUE)

# Let's take a look again at the ctch_basin_slope for stream orders > 1

summary(nsi_rcm_phys_qaqc_dat %>% 
          filter(stream_order>1 & ctch_basin_slope == 0))

# 75 datapoints, now let's check how many of those zeroes correspond to an elevation
# difference of zero

summary(nsi_rcm_phys_qaqc_dat %>% 
          filter(stream_order>1 & ctch_basin_slope == 0) %>% 
          filter(ctch_max_elevation_m - ctch_min_elevation_m!=0)) # we have a total of
# 61 zero values that are not accounted by lack of elevation changes

missing_data <- nsi_rcm_phys_qaqc_dat %>% 
  filter(stream_order>1 & ctch_basin_slope == 0) %>% 
  filter(ctch_max_elevation_m - ctch_min_elevation_m!=0) %>% 
  select(comid,
         ctch_basin_slope,
         ctch_max_elevation_m,
         ctch_min_elevation_m,
         basin,
         ctch_area_km2,
         reach_length_km,
         ctch_avg_elevation_m)


remaining_data <- nsi_rcm_phys_qaqc_dat %>%
  anti_join(missing_data, by = "comid") %>% 
  select(comid,
         ctch_basin_slope,
         ctch_max_elevation_m,
         ctch_min_elevation_m,
         basin,
         ctch_area_km2,
         reach_length_km,
         ctch_avg_elevation_m)


ctch_basin_mod <- lm(log(ctch_basin_slope)~log(ctch_max_elevation_m - ctch_min_elevation_m) + basin +
                       log(ctch_area_km2) + log(reach_length_km) + log(ctch_avg_elevation_m),
     data = nsi_rcm_phys_qaqc_dat %>% 
       filter((ctch_max_elevation_m - ctch_min_elevation_m)>0) %>% 
       filter(stream_order > 1 & ctch_basin_slope !=0),
     na.action = na.omit)
summary(ctch_basin_mod)

# We will fill the in ctch_basin_slope with the model above

# Predict the missing 'ctch_area_km2' values using the loess model
predicted_values <- exp(predict(ctch_basin_mod, newdata = missing_data))

# Update the missing 'ctch_area_km2' values with the predicted values
missing_data$ctch_basin_slope <- predicted_values

# Combine the original data and the data with predicted values
estimated_ctch_data <- rbind(remaining_data,
                             missing_data)

# Replace original values in the dataset for their estimates:
nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat%>% 
  select(-ctch_basin_slope) %>% 
  merge(.,
        estimated_ctch_data %>% 
          select(comid,ctch_basin_slope),
        by = "comid",
        all.x = TRUE) %>% 
  mutate(ctch_basin_slope = if_else(ctch_basin_slope < 0.00000001,
                                    0.00000001,
                                    ctch_basin_slope))


###############################################################################
# Filling gaps for slope and roughness with: interpolate_missing_values()

# Let's start with reach slope, which we expect should be related to ctch_area_km2
# and ctch_basin_slope

p <- ggplot(data = nsi_rcm_phys_qaqc_dat %>% 
              filter(reach_slope>0 & ctch_basin_slope>0),
            aes(x = wshd_basin_slope,
                y = reach_slope,
                color = as.factor(stream_order)))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  # geom_smooth(method = 'lm')+
  facet_wrap(~basin, ncol = 2)+
  theme(legend.position = "none")
p





















