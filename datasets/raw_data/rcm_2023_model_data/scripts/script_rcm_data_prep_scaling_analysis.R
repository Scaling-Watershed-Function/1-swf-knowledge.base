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
                 GGally)

# Local Import-Export
source_data <- "../../raw_data"
local_data <- "./data"

# Loading datasets

# RCM model data version 2022 (without NEXSS gap filling)
rcm_22_model_dat <- read_csv(paste(local_data,"model_inputs","raw_willamette_yakima_data_rf_scaling_analysis_data.csv", sep = '/'),
                             show_col_types = FALSE)

# RCM model data version 2023 (resp data estimated with NEXSS gap filling via random forest)
rcm_23_model_dat <- read_csv(paste(local_data,"model_outputs","nhd_stream_annual_resp.csv", sep = '/'),
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

# Comparing model input data
rcm_23_model_inp_dat <- merge(substrate_inputs,
                              rf_hyporheic_dat,
                              by = "comid",
                              all.x = TRUE)
summary(rcm_23_model_inp_dat)


rcm_22_model_inp_dat <- rcm_22_model_dat %>% 
  select(comid,
         do_stream_mg_l,
         doc_stream_mg_l,
         no3_stream_mg_l,
         tot_q_hz_ms,
         tot_rt_hz_s)

summary(rcm_22_model_inp_dat)

comparison_input_dat <- merge(rcm_23_model_inp_dat,
                              rcm_22_model_inp_dat,
                              by = "comid",
                              all.x = TRUE) %>% 
  select(comid,
         do_stream_mg_l.x,
         doc_stream_mg_l.x,
         no3_stream_mg_l.x,
         tot_q_hz_ms.x,
         tot_rt_hz_s.x,
         do_stream_mg_l.y,
         doc_stream_mg_l.y,
         no3_stream_mg_l.y,
         tot_q_hz_ms.y,
         tot_rt_hz_s.y)

p <- ggplot(data = comparison_input_dat,
            aes(x = doc_stream_mg_l.x,
                y = doc_stream_mg_l.y))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()
p


# Comparing total respiration data

comparison_co2_dat <- rcm_23_model_dat %>% 
  select(comid,
         basin,
         stream_order,
         totco2g_day) %>% 
  rename(totco2g_day_23 = totco2g_day) %>% 
  merge(.,
        rcm_22_model_dat %>% 
          select(comid,
                 totco2g_day) %>% 
          rename(totco2g_day_22 = totco2g_day),
        by = "comid",
        all.x = TRUE)

p <- ggplot(data = comparison_co2_dat,
            aes(x = totco2g_day_22,
                y = totco2g_day_23,
                color = as.factor(stream_order)))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(~basin, ncol = 2)
p

####################Pending edit##################################
# Connectivity test
test_dat_connectivity <- rcm_gap_filled_dat %>% 
  filter(is.na(totco2g_day)==FALSE) %>% 
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

