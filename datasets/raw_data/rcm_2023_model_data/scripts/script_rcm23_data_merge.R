###############################################################################
# Dataset preparation for scaling analysis RCM 2023 on RF filled NEXSS data
###############################################################################

gc()

librarian::shelf(tidyverse,
                 utils,
                 nhdplusTools,
                 sp,
                 sf,
                 leaflet,
                 stringr,
                 GGally,
                 entropy)

# Local Import-Export
source_data <- "../../raw_data"
local_data <- "./data"
export_data <- "/Users/guerrero-fj/Documents/GitHub/1_scaling_watershed_function/2-swf-analytical.engine/scaling_analysis_willamette_yakima_rcm_23/data"

# Loading datasets

# RCM model data version 2023 (resp data estimated with NEXSS gap filling via random forest)
rcm_23_output_dat <- read_csv(paste(local_data,"model_outputs","nhd_stream_annual_resp.csv", sep = '/'),
                               show_col_types = FALSE)

#Hyporheic fluxes and residence times using random forest models (not included in rcm_23_model_dat)
rf_lat_flux_inputs <- read_csv(paste(local_data,"model_inputs","RCM_lateral_flux_inputs.csv", sep = '/'),
                               show_col_types = FALSE)

rf_ver_flux_inputs <- read_csv(paste(local_data,"model_inputs","RCM_vertical_flux_inputs.csv", sep = '/'),
                               show_col_types = FALSE)

rf_lat_rt_inputs <- read_csv(paste(local_data,"model_inputs","RCM_lateral_RT_inputs.csv", sep = '/'),
                             show_col_types = FALSE)

rf_ver_rt_inputs <- read_csv(paste(local_data,"model_inputs","RCM_vertical_RT_inputs.csv", sep = '/'),
                             show_col_types = FALSE)

# Annual averages for substrate concentrations
substrate_inputs <- read_csv(paste(local_data,"model_inputs","RCM_substrate_inputs.csv", sep = '/'),
                             show_col_types = FALSE)

# Removing variables not pertaining to RCM model, and merging with substrate input data

rcm_23_subs_dat <- rcm_23_output_dat %>% 
  select(c(01:34,
           43:44,
           58,
           104:107,
           109,111)) %>% 
  merge(.,
        substrate_inputs,
        by = "comid",
        all.x = TRUE)


# Merging hyporheic datasets

# Exchange fluxes
rf_hyporheic_flux_dat <- rf_lat_flux_inputs %>% 
  merge(.,
        rf_ver_flux_inputs,
        by = "comid",
        all.x = TRUE) %>% 
  mutate(q_hz_lat_ms = 10^logq_hz_lateral_m_div_s_fill,
         q_hz_ver_ms = 10^logq_hz_vertical_m_div_s_fill,
         tot_q_hz_ms = q_hz_lat_ms + q_hz_ver_ms) %>% 
  select(comid,
         q_hz_lat_ms,
         q_hz_ver_ms,
         tot_q_hz_ms)

summary(rf_hyporheic_flux_dat)

# Residence times
rf_hyporheic_rt_dat <- rf_lat_rt_inputs %>% 
  merge(.,
        rf_ver_rt_inputs,
        by = "comid",
        all.x = TRUE) %>% 
  mutate(rt_hz_lat_s = 10^logRT_lateral_hz_s_fill,
         rt_hz_ver_s = 10^logRT_vertical_hz_s_fill,
         tot_rt_hz_s = rt_hz_lat_s + rt_hz_ver_s) %>% 
  select(comid,
         rt_hz_lat_s,
         rt_hz_ver_s,
         tot_rt_hz_s)

summary(rf_hyporheic_rt_dat)

# Exchange Fluxes and Residence Times
rf_hyporheic_dat <- merge(rf_hyporheic_flux_dat,
                          rf_hyporheic_rt_dat,
                          by = "comid",
                          all.x = TRUE)

# Calculating total respiration for rcm_23 

rcm_23_subs_dat$totco2g_day <- with(rcm_23_subs_dat,
                                      totco2_o2g_day + totco2_ang_day)


# Merging all input data and reordering columns
rcm_23_all_inputs_outputs <- rcm_23_subs_dat %>% 
  merge(.,
        rf_hyporheic_dat,
        by = "comid",
        all.x = TRUE) %>% 
  select(c(1:3,
           24,
           23,
           4:22,
           31:32,
           25,
           37,
           34,
           28:30,
           35:36,
           33,
           48:53,
           44:46,
           38,
           47,
           39:43))

# Connectivity test
test_dat_connectivity <- rcm_23_all_inputs_outputs %>% 
  group_by(basin)%>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
test_dat_connectivity

write.csv(rcm_23_all_inputs_outputs,paste(local_data,"rcm_23_model_output_data.csv",sep = '/'),
          row.names = FALSE)

write.csv(rcm_23_all_inputs_outputs,paste(export_data,"rcm_23_model_output_data.csv",sep = '/'),
          row.names = FALSE)

  

