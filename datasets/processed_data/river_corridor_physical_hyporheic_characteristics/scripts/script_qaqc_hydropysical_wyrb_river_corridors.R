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
                 data.table,
                 betareg)

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
# We drop the DUP_COMIDS == 1 and DUP_ArSqKM from the dataset  
################################################################################

nsi_rcm_phys_dat_m1 <-  nsi_rcm_phys_dat %>% 
  filter(DUP_COMID == 0) %>% 
  select(-c(DUP_ArSqKM, DUP_Length, DUP_COMID))

################################################################################

# Let's now take a look at those comid's with zero values for watershed areas. 
# According to Blodgett (2023, pers. coms.) these zero values may correspond to 
# flowlines that are not connected to the network but were found and digitized

summary(filter(nsi_rcm_phys_dat_m1, wshd_area_km2 == 0))

# All the flowlines with zero values in wshd area are first order streams, so let's
# run a connectivity test that ignores these values: 

test_dat_connectivity <- nsi_rcm_phys_dat %>% 
  filter(DUP_COMID == 0 & wshd_area_km2 > 0) %>% 
  filter(stream_order<9) %>% 
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
# We drop comid's with wshd_area_km2 == 0 from the dataset, and reaches with 
# stream order > 8, since these correspond to the Columbia River
################################################################################

nsi_rcm_phys_dat_m2 <-  nsi_rcm_phys_dat_m1 %>% 
  filter(wshd_area_km2 > 0) %>% 
  filter(stream_order < 9)

################################################################################

# Let's now look at the remaining values with catchment areas = 0

summary(filter(nsi_rcm_phys_dat_m2, wshd_area_km2 > 0 & ctch_area_km2 == 0))

# We find 89 additional datapoints with catchment areas = 0. These data points
# encompass multiple stream orders, have reach lengths between 6 to 11.4 m and 
# non-zero values for accumulated stream density, so they are actually drained
# and connected to the network. So, it could be expected that just removing these
# datapoints, would result in reduction of network connectivity:


test_dat_connectivity <- nsi_rcm_phys_dat_m2 %>% 
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

ctch_rch_plot <- ggplot(data = nsi_rcm_phys_dat_m2 %>% 
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

# Fiting the loess model and estimating catchment area from reach length

model <- loess(log10(ctch_area_km2) ~ log10(reach_length_km), 
               data = nsi_rcm_phys_dat_m2 %>% 
                 select(comid,
                        ctch_area_km2,
                        reach_length_km) %>% 
                 filter(ctch_area_km2 >0), 
               span = 0.15)

# Filter the data to get rows with missing 'ctch_area_km2' values
missing_data <- nsi_rcm_phys_dat_m2 %>% 
  select(comid,
         ctch_area_km2,
         reach_length_km) %>% 
  filter(is.na(ctch_area_km2) == TRUE| ctch_area_km2 == 0)


# Predict the missing 'ctch_area_km2' values using the loess model
predicted_values <- 10^predict(model, newdata = missing_data)

# Update the missing 'ctch_area_km2' values with the predicted values
missing_data$ctch_area_km2 <- predicted_values

# Combine the original data and the data with predicted values
estimated_ctch_data <- rbind(nsi_rcm_phys_dat_m2 %>% 
                               select(comid,
                                      ctch_area_km2,
                                      reach_length_km) %>% 
                               filter(ctch_area_km2>0),
                             missing_data)

# Replace original values in the dataset for their estimates:
nsi_rcm_phys_dat_m3 <- nsi_rcm_phys_dat_m2%>% 
  select(-ctch_area_km2) %>% 
  merge(.,
        estimated_ctch_data %>% 
          select(comid,ctch_area_km2),
        by = "comid",
        all.x = TRUE)

summary(nsi_rcm_phys_dat_m3)
################################################################################

# We also find ~ 8 zero values for hydraulic geometry variables including 
# bankfull width, depth, and cross sectional area, as well as mean annual flow. We will 
# check on this values once we have removed first order streams with watershed area = 0.

filter(nsi_rcm_phys_dat_m3, bnkfll_width_m == 0) 

# These values correspond to first order streams, so we remove them from the dataset


############################# DATASET MODIFICATION #############################
# Remove flowlines with bnkfll_width_m = 0
################################################################################

nsi_rcm_phys_dat_m4 <- nsi_rcm_phys_dat_m3 %>% 
  filter(bnkfll_width_m > 0) %>% 
  mutate(bnkfll_xsec_area_m2 = if_else(bnkfll_xsec_area_m2 == 0,
                                       bnkfll_depth_m * bnkfll_width_m,
                                       bnkfll_xsec_area_m2))

################################################################################
# Variables with missing data

# Calculate the number of NA values in each column
na_counts <- colSums(is.na(nsi_rcm_phys_dat_m4))

# Calculate the number of negative values in each column
negative_counts <- colSums(nsi_rcm_phys_dat_m4 < 0, na.rm = TRUE)

# Create a new dataframe to store the report
report_data <- data.frame(
  variable = names(nsi_rcm_phys_dat_m4),
  NA_s = na_counts,
  Negative_s = negative_counts
) %>%
  filter(NA_s > 0 | Negative_s > 0)

# Print the report
print(report_data, row.names = FALSE)

# Besides the gaps we have in the hyporheic variables and D50 values, we 
# we also have 259 NA's for catchment stream_density (probably related to 
# 0 values in catchment area). Negative values in elevation could be expected (?)
# given the resolution of the data, but we have negative values for reach_slope, 
# in this case -99999 (i.e. missing data) and also 29 missing values for roughness. 


############################# DATASET MODIFICATION #############################
# Recalculating catchment stream density
################################################################################

nsi_rcm_phys_dat_m5 <- nsi_rcm_phys_dat_m4 %>% 
  mutate(ctch_stream_dens = if_else(is.na(ctch_stream_dens)== TRUE,
                                    reach_length_km/ctch_area_km2,
                                    ctch_stream_dens))

summary(nsi_rcm_phys_dat_m5)
###############################################################################

# Filling gaps for slope and roughness with: interpolate_missing_values()

# Let's start with reach slope, which we expect should be related to ctch_area_km2
# and ctch_basin_slope

# We will use a pair of functions to interpolate missing values (n<50) (e.g., roughness, reach_slope)
get_immediate_neighbors_median <- function(data, column, comid, tocomid) {
  immediate_neighbors <- c(comid, tocomid)
  values <- data[[column]][data$comid %in% immediate_neighbors & data[[column]] >= 0]
  median_value <- median(values, na.rm = TRUE)
  return(median_value)
}

interpolate_missing_values <- function(data, column) {
  column = sym(column)
  
  data <- data %>%
    mutate(!!column := ifelse(!!column < 0 | is.na(!!column), NA, !!column))
  
  # Replace NA values in 'mean_ann_pcpt_mm' and 'wshd_area_km2' with median
  data$mean_ann_pcpt_mm[is.na(data$mean_ann_pcpt_mm)] <- median(data$mean_ann_pcpt_mm, na.rm = TRUE)
  data$wshd_area_km2[is.na(data$wshd_area_km2)] <- median(data$wshd_area_km2, na.rm = TRUE)
  
  # Create the bins for 'mean_ann_pcpt_mm' and 'wshd_area_km2' if there is more than one unique value
  if(length(unique(data$mean_ann_pcpt_mm)) > 1){
    data$precipitation_bin <- cut2(data$mean_ann_pcpt_mm, g = 10)
  } else {
    data$precipitation_bin <- 1
  }
  
  if(length(unique(data$wshd_area_km2)) > 1){
    data$watershed_area_bin <- cut2(data$wshd_area_km2, g = 10)
  } else {
    data$watershed_area_bin <- 1
  }
  
  for (i in seq_len(nrow(data))) {
    # Check if the column value is missing (represented by NA)
    if (is.na(data[[column]][i])) {
      # Get the immediate neighbors' median value
      immediate_median <- get_immediate_neighbors_median(data, column, data$comid[i], data$tocomid.x[i])
      
      # If there are no immediate neighbors, replace with the median value for the same 'stream_order'
      # and similar 'mean_ann_pcpt_mm' and 'wshd_area_km2'
      if (is.na(immediate_median)) {
        same_bin <- data$stream_order == data$stream_order[i] & 
          data$precipitation_bin == data$precipitation_bin[i] & 
          data$watershed_area_bin == data$watershed_area_bin[i]
        
        same_bin_values <- data[[column]][same_bin & !is.na(data[[column]])]
        
        # Sample a subsample from the set if its size is larger than a specified limit (100 for instance)
        sample_size <- min(length(same_bin_values), 100)
        subsample_values <- sample(same_bin_values, size = sample_size)
        immediate_median <- median(subsample_values, na.rm = TRUE)
      }
      
      # Assign the calculated value to the missing value
      data[[column]][i] <- immediate_median
    }
  }
  return(data)
}

roughness_int <- interpolate_missing_values(data = nsi_rcm_phys_dat_m5 %>% 
                                              select(comid,
                                                     tocomid,
                                                     stream_order,
                                                     roughness),
                                            "roughness")

reach_slope_int <- interpolate_missing_values(data = nsi_rcm_phys_dat_m5 %>% 
                                              select(comid,
                                                     tocomid,
                                                     stream_order,
                                                     reach_slope),
                                            "reach_slope")

summary(roughness_int)
summary(reach_slope_int)

nsi_rcm_phys_dat_m6 <- nsi_rcm_phys_dat_m5 %>%
  select(-c(reach_slope,roughness)) %>% 
  merge(.,
        roughness_int %>% 
          select(comid,
                 roughness),
        by = "comid",
        all.x = TRUE) %>% 
  merge(.,
        reach_slope_int %>% 
          select(comid,
                 reach_slope),
        by = "comid",
        all.x = TRUE)

summary(nsi_rcm_phys_dat_m6)

p <- ggplot(data = nsi_rcm_phys_dat_m6,
            aes(x = wshd_area_km2,
                y = reach_slope,
                color = as.factor(stream_order)))+
  # geom_boxplot()+
  geom_point()+
  scale_x_log10()+
  scale_y_log10(limits = c(0.00000001, 0.001))+
  # geom_smooth(method = 'lm')+
  facet_wrap(basin~stream_order, nrow = 2)+
  theme(legend.position = "none")
p

# We observe a number of datapoints with reach_slope = 0.00000001. These correspond
# to default values assigned at NHDPlus when no other values were available.  Let's
# take a look

summary(filter(nsi_rcm_phys_dat_m6, reach_slope == 0.00000001))

# We find 1383 of these values in this dataset (~ 8%) which tracks with the updated
# version of NHDPlus 2.1.

# Recalculating wshd stream density, and cumulative variables
nsi_rcm_phys_dat_m7 <-  nsi_rcm_phys_dat_m6 %>% 
  mutate(wshd_stream_dens = tot_stream_length_km/wshd_area_km2,
         ctch_stream_dens = reach_length_km/ctch_area_km2) %>% 
  select(-c(accm_basin_area_km2,
            accm_basin_slope,
            accm_stream_slope,
            accm_stream_dens)) %>% 
  mutate(stream_area_m2 = (reach_length_km*bnkfll_width_m)*1000) %>% 
  group_by(basin) %>% 
  mutate(across(c(wshd_stream_dens,
                  tot_stream_length_km,
                  wshd_area_km2,
                  ctch_area_km2,
                  ctch_stream_dens,
                  ctch_basin_slope,
                  reach_slope,
                  stream_area_m2), ~ calculate_arbolate_sum(data.frame(ID = comid,
                                                                       toID = tocomid,
                                                                       length = .x))) %>% 
           set_names(paste0("accm_", names(select(., wshd_stream_dens:stream_area_m2))))) %>% 
  ungroup()

summary(nsi_rcm_phys_dat_m7)

test_dat_connectivity <- nsi_rcm_phys_dat_m7 %>% 
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

# Connectivity in both datasets is 95%

nsi_rcm_phys_qaqc_dat0 <- nsi_rcm_phys_dat_m7

################################################################################

# Sorting and re-ordering columns 

nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat0 %>% 
  select(-c(logRT_vertical_hz_s,
            logRT_lateral_hz_s,
            logq_hz_vertical_m_div_s,
            logq_hz_lateral_m_div_s,
            logw_m,
            length_m,
            huc_2_region_id,
            huc_region_raster_id,
            huc_4_subregion_id,
            logK_m_div_s)) %>% 
  select(comid,
         tocomid,
         reachcode,
         hydroseq,
         from_node,
         to_node,
         basin,
         wshd_area_km2,
         wshd_stream_dens,
         wshd_basin_slope,
         wshd_min_elevation_m,
         wshd_max_elevation_m,
         wshd_avg_elevation_m,
         ctch_area_km2,
         ctch_stream_dens,
         ctch_basin_slope,
         ctch_min_elevation_m,
         ctch_max_elevation_m,
         ctch_avg_elevation_m,
         mean_ann_pcpt_mm,
         mean_ann_temp_dc,
         mean_ann_runf_mm,
         stream_order,
         reach_type,
         reach_length_km,
         reach_slope,
         tot_stream_length_km,
         sinuosity,
         bnkfll_width_m,
         bnkfll_depth_m,
         bnkfll_xsec_area_m2,
         mean_ann_flow_m3s,
         mean_ann_vel_ms,
         roughness,
         d50_m,
         stream_area_m2)

write.csv(nsi_rcm_phys_qaqc_dat,
          paste(local_data,"qaqc_river_corridors_physical_hyporheic_char.csv", sep = '/'),
          row.names = FALSE)

################################################################################

