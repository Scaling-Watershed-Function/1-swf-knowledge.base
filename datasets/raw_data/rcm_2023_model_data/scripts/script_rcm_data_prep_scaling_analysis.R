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
o_rcm_23_model_dat <- read_csv(paste(local_data,"model_outputs","nhd_stream_annual_resp.csv", sep = '/'),
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

comparison_co2_dat <- o_rcm_23_model_dat %>% 
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
        all.x = TRUE) %>% 
  mutate(resp_ratio = totco2g_day_22/totco2g_day_23)

summary(comparison_co2_dat)

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


bp_22 <- ggplot(data = comparison_co2_dat,
                aes(x = as.factor(stream_order),
                    y = totco2g_day_22,
                    color = as.factor(stream_order),
                    fill = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10(limits = c(0.0001,100000000))+
  xlab("Stream order (Strahler)")+
  facet_wrap(~basin, ncol = 2)+
  ggtitle("RCM-22:Total respiration across stream orders (532 zero values)")+
  theme(legend.position = "none")
bp_22

bp_23 <- ggplot(data = comparison_co2_dat,
                aes(x = as.factor(stream_order),
                    y = totco2g_day_23,
                    color = as.factor(stream_order),
                    fill = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10(limits = c(0.0001,100000000))+
  xlab("Stream order (Strahler)")+
  facet_wrap(~basin, ncol = 2)+
  ggtitle("RCM-23:Total respiration across stream orders (no zero values)")+
  theme(legend.position = "none")
bp_23
  
# Respiration ratios

rr_22_23 <- ggplot(data = comparison_co2_dat,
                   aes(x = as.factor(stream_order),
                       y = resp_ratio,
                       color = as.factor(stream_order),
                       fill = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  geom_hline(yintercept = 1.0)+
  geom_hline(yintercept = 44.1)+
  scale_y_log10()+
  xlab("Stream order (Strahler)")+
  facet_wrap(~basin, ncol = 2)+
  ggtitle("RCM-22/23 respiration ratios)")+
  theme(legend.position = "none")
rr_22_23

################################################################################
# assembling newest version or rmc_23 model data
################################################################################

rcm_23_model_in_out_dat <- rcm_23_model_inp_dat %>% 
  rename(do_stream_mg_l_23 = do_stream_mg_l,
         doc_stream_mg_l_23 = doc_stream_mg_l,
         no3_stream_mg_l_23 = no3_stream_mg_l,
         q_hz_lateral_m_s_23 = q_hz_lat_ms,
         q_hz_vertical_m_s_23 = q_hz_ver_ms,
         rt_hz_lateral_s_23 = rt_hz_lat_s,
         rt_hz_vertical_s_23 = rt_hz_ver_s) %>% 
  merge(.,
        o_rcm_23_model_dat %>% 
          select(comid,
                 totco2_anaer_mol,
                 totco2_ang,
                 totco2_ang_day,
                 totco2_ang_m2_day,
                 totco2_o2_mol,
                 totco2_o2g,
                 totco2_o2g_day,
                 totco2_o2g_m2_day,
                 totco2g,
                 totco2g_day,
                 totco2g_m2_day),
        by = "comid",
        all.x = TRUE)





rcm_23_model_dat <- o_rcm_23_model_dat %>% 
  select(-c(tot_q_hz_ms,
            tot_rt_hz_s,
            logrt_total_hz_s,
            logq_hz_total_m_s,
            logRT_lateral_hz_s,
            logRT_vertical_hz_s,
            logq_hz_vertical_m_div_s,
            logq_hz_lateral_m_div_s))




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

