################################################################################
# NEXSS data gap filling with interpolation across the stream network
################################################################################

# Author: Francisco J. Guerrero

librarian::shelf(tidyverse,
                 ggplot2,
                 sp,
                 sf,
                 leaflet,
                 utils,
                 zen4R,
                 nhdplusTools,
                 htmlwidgets,
                 tidygraph)


# Local import / export paths

raw_data <- "raw" 
processed_data <- "processed"
shapes_data <- "shape_files"


# Reading files from raw data

newest23_pnw_dat <- read_csv(paste(raw_data,"newest23_hyporheic_pnw_data.csv", sep = '/'),
          show_col_types = FALSE)

current20_pnw_dat <- read_csv(paste(raw_data,"current20_hyporheic_pnw_data.csv", sep = '/'),
          show_col_types = FALSE)

# Shapefiles

nis_pnw_stream <- sf::st_transform(st_read(paste(raw_data,"shape_files", "nis_reference","230623_nis_network_ywrb.shp", sep = '/')),4326)

# Merging datasets

hyprh_dat <- enh_nhd21_dat %>%
  filter(huc_4 != "0107") %>%
  rename(streamorder = streamorde,
         usgs_slope = slope,
         usgs_area_km2 = totdasqkm,
         usgs_length_km = lengthkm) %>%
  select(comid,
         tocomid,
         usgs_slope,
         usgs_area_km2,
         usgs_length_km,
         streamorder,
         ftype,
         huc_4) %>%
  mutate(basin = if_else(huc_4 == "1703",
                         "yakima",
                         "willamette")) %>%
  merge(.,
        current20_pnw_dat %>%
          mutate(c_area_km2 = 10^logDA_km2,
                 c_q_m3_s = 10^logQ_m3_div_s,
                 c_RT_lateral_hz_s = 10^logRT_lateral_hz_s,
                 c_RT_vertical_hz_s = 10^logRT_vertical_hz_s,
                 c_q_hz_vertical_m_div_s = 10^logq_hz_vertical_m_div_s,
                 c_q_hz_lateral_m_div_s = 10^logq_hz_lateral_m_div_s,
                 c_w_m = 10^logw_m,
                 c_wbkf_m = 10^logwbkf_m,
                 c_depth_m = 10^logd_m,
                 c_depth_bkf_m = 10^logdbkf_m,
                 c_slope = 10^logSlope,
                 c_k_m_s = 10^logK_m_div_s,
                 c_lenght_km = length_m/1000) %>%
          rename(c_d50_m = D50_m) %>%
          select(comid,
                 c_area_km2,
                 c_q_m3_s,
                 c_RT_lateral_hz_s,
                 c_RT_vertical_hz_s,
                 c_q_hz_lateral_m_div_s,
                 c_q_hz_vertical_m_div_s,
                 c_lenght_km,
                 c_w_m,
                 c_depth_m,
                 c_wbkf_m,
                 c_depth_bkf_m,
                 c_slope,
                 c_d50_m,
                 c_k_m_s),
        by = "comid",
        all.x = TRUE) %>%
  merge(.,
        newest23_pnw_dat %>%
          mutate(n_area_km2 = `DrainageArea[m2]`/1000000,
                 n_length_km = `Length[m]`/1000) %>%
          rename(n_streamorder = streamorder,
                 n_sinuosity = `sinuosity[-]`,
                 n_slope = `slope[-]`,
                 n_d50_m = `D50[m]`,
                 n_w_m = `w[m]`,
                 n_wbkf_m = `wbkf[m]`,
                 n_depth_m = `d[m]`,
                 n_depth_bkf_m = `dbkf[m]`,
                 n_mean_annual_q_m3_s = `Qma[m3/s]`,
                 n_bnkfll_q_m3_s = `Qbf[m3/s]`,
                 n_mean_annual_vel_m_s = `Uma[m/s]`,
                 n_k_m_s = `K[m/s]`,
                 n_porosity_m_s = `poro[m/s]`,
                 n_jx = `Jx[-]`,
                 n_jy = `Jy[-]`,
                 n_jyx = `Jyx[-]`) %>%
          select(comid,
                 FromNode,
                 ToNode,
                 n_streamorder,
                 n_area_km2,
                 n_length_km,
                 n_sinuosity,
                 n_slope,
                 n_d50_m,
                 n_w_m,
                 n_wbkf_m,
                 n_depth_m,
                 n_depth_bkf_m,
                 n_mean_annual_q_m3_s,
                 n_bnkfll_q_m3_s,
                 n_mean_annual_vel_m_s,
                 n_k_m_s,
                 n_porosity_m_s,
                 n_jx,
                 n_jy,
                 n_jyx),
        by = "comid",
        all.x = TRUE)

summary(hyprh_dat)

# The max number of NAs in the newest data (2023) is 1494 and corresponded to mean annual 
# velocity. There are 182 NAs for the other variables in the dataset. 

# The number of NAs in the current dataset (2020) data varies across variables ranging
# from 281 for bankfull width to 3060 lateral hyporheic flow.


# Let's take a look at the stream characteristics for missing D50 values in the 
# Gomez's dataset

summary(filter(hyprh_dat,is.na(n_d50_m)==TRUE))

# The missing values correspond mostly to first order streams, but there is a 7 
# order stream included as well. 

summary(filter(hyprh_dat,is.na(n_d50_m)==TRUE & streamorder > 1))

# Looking at stream networks using leaflet

leaflet(nis_pnw_stream) %>% 
  addPolylines(weight = 2) %>%  
  addProviderTiles("Esri.WorldImagery")

# adding D50 data from Son et al. 2022 and Gomez 2023

nis_pnw_stream_dat <- nis_pnw_stream %>% 
  rename(comid = COMID) %>% 
  merge(.,
        hyprh_dat %>%
          select(comid,
                 tocomid,
                 basin,
                 streamorder,
                 c_d50_m,
                 c_q_m3_s,
                 c_RT_lateral_hz_s,
                 c_RT_vertical_hz_s,
                 c_q_hz_lateral_m_div_s,
                 c_q_hz_vertical_m_div_s,
                 c_lenght_km,
                 c_w_m,
                 c_depth_m,
                 c_wbkf_m,
                 c_depth_bkf_m,
                 c_slope,
                 c_d50_m,
                 c_k_m_s),
        by = "comid",
        all.x = TRUE)

leaflet(nis_pnw_stream_dat) %>% 
  addPolylines(weight = 2) %>%  
  addPolylines(data = filter(nis_pnw_stream_dat, is.na(c_q_hz_lateral_m_div_s)==TRUE),
               weight = 8,
               color = "darkorange",
               opacity = 1) %>%
  addPolylines(data = filter(nis_pnw_stream_dat, is.na(c_k_m_s)==TRUE),
               weight = 4,
               color = "darkorchid",
               opacity = 1) %>% 
  addPolylines(data = filter(nis_pnw_stream_dat, is.na(c_d50_m)==TRUE),
               weight = 2,
               color = "green",
               opacity = 2) %>%
  addProviderTiles("Esri.WorldImagery")


# Checking the spatial pattern of lateral hyporheic flows in the Willamette River

lat_hyp_flow_plot <- nis_pnw_stream_dat %>% 
  select(basin,
         streamorder,
         c_q_hz_lateral_m_div_s) %>% 
  filter(streamorder!= 9) %>% 
  ggplot(aes(x = as.factor(streamorder),
             y = c_q_hz_lateral_m_div_s,
             color = as.factor(streamorder)))+
  scale_y_log10()+
  geom_boxplot()+
  facet_wrap(~basin, ncol = 2)+
  theme(legend.position = "none")
lat_hyp_flow_plot

# Interpolation across the flow network with nhdplusTools

# Read the NHDPlus flowline dataset
w_flowlines <- st_as_sf(filter(nis_pnw_stream_dat,
                               HUC4 == "1709" ))

# Create a tidygraph object from the flowlines dataset
graph <- as_tbl_graph(w_flowlines, directed = TRUE)

# Define the variable to use for filling NA values
desired_variable <- "c_q_hz_lateral_m_div_s"


# Function to fill NA values from upstream and downstream flowlines
fill_na_values <- function(data, var) {
  updated_data <- data
  
  # Find initial NA values
  na_indices <- which(is.na(updated_data[[var]]))
  
  while (length(na_indices) > 0) {
    # Find upstream and downstream flowlines for NA values
    upstream <- filter(updated_data, comid %in% updated_data$tocomid[na_indices])
    downstream <- filter(updated_data, tocomid %in% updated_data$comid[na_indices])
    
    # Find non-NA neighbor values
    neighbor_values <- c(upstream[[var]], downstream[[var]])
    non_na_values <- neighbor_values[!is.na(neighbor_values)]
    
    if (length(non_na_values) > 0) {
      # Calculate the average of non-NA neighbor values for NA indices
      updated_data[[var]][na_indices] <- rep(mean(non_na_values), length(na_indices))
    }
    
    # Find updated NA values
    na_indices <- which(is.na(updated_data[[var]]))
  }
  
  return(updated_data)
}

# Fill NA values in the flowlines dataset
filled_flowlines <- fill_na_values(w_flowlines, desired_variable)


lat_int_hyp_flow_plot <- filled_flowlines %>% 
  select(basin,
         streamorder,
         c_q_hz_lateral_m_div_s) %>% 
  filter(streamorder!= 9) %>% 
  ggplot(aes(x = as.factor(streamorder),
             y = c_q_hz_lateral_m_div_s,
             color = as.factor(streamorder)))+
  scale_y_log10()+
  geom_boxplot()+
  theme(legend.position = "none")
lat_int_hyp_flow_plot

