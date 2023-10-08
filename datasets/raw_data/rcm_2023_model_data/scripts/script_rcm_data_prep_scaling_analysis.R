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

################################################################################
# Comparing model input data
################################################################################
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
  scale_y_log10()+
  xlab("Stream order (Strahler)")+
  facet_wrap(~basin, ncol = 2)+
  ggtitle("RCM-22/23 respiration ratios")+
  theme(legend.position = "none")
rr_22_23

################################################################################
# assembling newest version or rmc_23 model data
################################################################################

rcm_23_model_in_out_dat <- rcm_23_model_inp_dat %>% 
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

rcm_23_model_merge <- o_rcm_23_model_dat %>% 
  select(-c(tot_q_hz_ms,
            tot_rt_hz_s,
            logrt_total_hz_s,
            logRT_lateral_hz_s,
            logRT_vertical_hz_s,
            logq_hz_total_m_s,
            logq_hz_vertical_m_div_s,
            logq_hz_lateral_m_div_s,
            logQ_m3_div_s,
            logDA_km2,
            logw_m,
            length_m,
            logwbkf_m,
            logdbkf_m,
            logSlope,
            totco2g_day,
            do_stream_mg_l,
            doc_stream_mg_l,
            no3_stream_mg_l,
            totco2g,
            totco2_o2g_day,
            totco2_ang_day,
            totco2g_m2_day,
            totco2_ang_day,
            totco2_o2g_m2_day,
            totco2_ang_m2_day,
            logtotco2g_m2_day,
            logtotco2_o2g_m2_day,
            logtotco2_ang_m2_day,
            accm_totco2g_day,
            basin_id,
            totco2_o2_mol,
            totco2_o2g,
            totco2_anaer_mol,
            totco2_ang,
            D50_m)) %>% 
  merge(.,
        rcm_23_model_in_out_dat,
        by = "comid",
        all.x = TRUE) %>% 
  mutate(water_exchng_kg_day = 997 * tot_q_hz_ms * 86400 * stream_area_m2)

rcm_23_model_dat <- rcm_23_model_merge %>% 
  select(1:26,
         45:45,
         34:36,
         33:33,
         27:32,
         81:86,
         98:98,
         78:80,
         46:63,
         64:77,
         87:97,
         37:ncol(rcm_23_model_merge))

# Connectivity test
test_dat_connectivity <- rcm_23_model_dat %>% 
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

# Calculating cumulative values
# 
# # Recalculating cumulative variables
# rcm_23_model_dat <- rcm_23_model_dat %>% 
#   mutate(doc_load_kg_d = doc_stream_mg_l*mean_ann_flow_m3s*86400,
#          do_load_kg_d = do_stream_mg_l*mean_ann_flow_m3s*86400,
#          no3_load_kg_d = no3_stream_mg_l*mean_ann_flow_m3s*86400) %>% 
#   group_by(basin) %>% 
#     mutate(across(c(totco2g_day,
#                   totco2_ang_day,
#                   totco2_o2g_day,
#                   tot_stream_length_km,
#                   tot_rt_hz_s,
#                   water_exchng_kg_day,
#                   doc_load_kg_d,
#                   do_load_kg_d,
#                   no3_load_kg_d,
#                   mean_ann_runf_mm), ~ calculate_arbolate_sum(data.frame(ID = comid,
#                                                                        toID = tocomid,
#                                                                        length = .x))) %>% 
#            set_names(paste0("accm_", names(select(., totco2g_day:mean_ann_runf_mm))))) %>% 
#   ungroup()
# 
# # Variability in sediment structure
# 
# qlabel <- c("Q10","Q20","Q30","Q40","Q50","Q60","Q70","Q80+")
# 
# rcm_23_model_dat <- rcm_23_model_dat %>% 
#   mutate(accm_d50_m = calculate_arbolate_sum(data.frame(ID = comid,
#                                                         toID = tocomid,
#                                                         length = d50_m)),
#          inc_comid = 1,
#          accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
#                                                             toID = tocomid,
#                                                             length = inc_comid)),
#          avg_d50_m = accm_d50_m/accm_inc_comid,
#          d50_m_dist = (d50_m - avg_d50_m)^2,
#          tot_d50_m_dist = calculate_arbolate_sum(data.frame(ID = comid,
#                                                            toID = tocomid,
#                                                            length = d50_m_dist)),
#          accm_d50_m_sd = tot_d50_m_dist/(accm_inc_comid)+runif(length(tot_d50_m_dist), min = 0.0000000001, max = 0.00000001),
#          avg_reach_slope = accm_reach_slope/accm_inc_comid,
#          reach_slope_dist = (reach_slope - avg_reach_slope)^2,
#          tot_reach_slope_dist = calculate_arbolate_sum(data.frame(ID = comid,
#                                                             toID = tocomid,
#                                                             length = reach_slope_dist)),
#          accm_reach_slope_sd = tot_reach_slope_dist/(accm_inc_comid)+runif(length(tot_d50_m_dist), min = 0.0000000001, max = 0.00000001),
#          slope_sd_cat = factor(Hmisc::cut2(accm_reach_slope_sd, g = 8),labels = qlabel))
# 
# 
# 
# p <- ggplot(data = rcm_23_model_dat,
#             aes(x = accm_water_exchng_kg_day,
#                 y = accm_totco2_ang_day,
#                 color = accm_d50_m_sd))+
#   geom_point(alpha = 0.5)+
#   scale_x_log10()+
#   scale_y_log10()+
#   facet_wrap(~basin, ncol = 2)
# p
# 
# 
# p <- ggplot(data = rcm_23_model_dat,
#             aes(x = wshd_area_km2,
#                 y = accm_totco2_o2g_day/wshd_area_km2,
#                 color = slope_sd_cat))+
#   geom_point(alpha = 0.5)+
#   scale_x_log10()+
#   scale_y_log10()+
#   geom_abline(slope = 1)+
#   facet_wrap(basin~slope_sd_cat, ncol = 8)
# p
# # Saving the data
# 
# # Removing cumulative columns that do not add new information to respiration patterns:
# 
# rcm_23_model_dat <- rcm_23_model_dat %>% 
#   select(-c(accm_d50_m,
#             accm_d50_m_sd,
#             accm_reach_slope,
#             accm_reach_slope_sd,
#             avg_d50_m,
#             avg_reach_slope,
#             d50_m_dist,
#             reach_slope_dist,
#             tot_d50_m_dist,
#             tot_reach_slope_dist,
#             slope_sd_cat))


# Calculating quantiles

#Calculating the quantiles with Hmisc::cut2, which allows for the inclusion of zeroes

# https://stackoverflow.com/questions/46750635/cut-and-quantile-in-r-in-not-including-zero

qlabel <- c("Q10","Q20","Q30","Q40","Q50","Q60","Q70","Q80+")

rcm_23_model_dat <- rcm_23_model_dat %>% 
  group_by(basin) %>% 
  mutate(ent_cat_w = factor(Hmisc::cut2(w_hrel, g = 8),labels = qlabel),
         ent_cat_c = factor(Hmisc::cut2(c_hrel, g = 8),labels = qlabel),
         rst_cat = factor(Hmisc::cut2(tot_rt_hz_s, g = 8),labels = qlabel),
         hzt_cat = factor(Hmisc::cut2(tot_q_hz_ms, g = 8),labels = qlabel),
         pct_cat = factor(Hmisc::cut2(mean_ann_pcpt_mm, g = 8),labels = qlabel),
         rnf_cat = factor(Hmisc::cut2(mean_ann_runf_mm, g = 8),labels = qlabel),
         d50_cat = factor(Hmisc::cut2(d50_m, g = 8),labels = qlabel),
         accm_hzt_cat = factor(Hmisc::cut2(accm_water_exchng_kg_day, g = 8),labels = qlabel),
         doc_stream_mg_l = ifelse(basin == "yakima", doc_stream_mg_l + runif(length(doc_stream_mg_l), min = 0, max = 0.001), doc_stream_mg_l),
         doc_cat = factor(Hmisc::cut2(doc_stream_mg_l, g = 8, ties.method = "min"), labels = qlabel),
         sto_fct = as.factor(stream_order),
         forest_scp_3 = w_forest_scp + w_water_scp,
         humans_scp_3 = w_human_scp,
         shrubl_scp_3 = w_shrub_scp + w_grass_scp + w_barren_scp,
         frst_cat = factor(Hmisc::cut2(forest_scp_3 , g = 8),labels = qlabel)) %>% 
ungroup() %>% 
  mutate(basin_cat = as.factor(if_else(basin == "yakima",
                                       "Yakima River (Dry)",
                                       "Willamette River (Wet)")))


rcm_23_model_dat <- rcm_23_model_dat %>% 
  group_by(basin) %>% 
  rowwise() %>% 
  mutate(ht_3 = entropy(c(forest_scp_3,
                          shrubl_scp_3,
                          humans_scp_3),
                        unit = "log2"),
         hmax_3 = log(3,2),
         hrel_3 = ht_3/hmax_3,
         hrel_3u = hrel_3 + runif(1, 0.000001, 0.00001)) %>% 
  ungroup() %>% 
  group_by(basin) %>% 
  mutate(hrel_3_cat = factor(Hmisc::cut2(hrel_3u , g = 8),labels = qlabel))


my_mcolors <- c("#ffd6e8","#ffafd2","#ff7eb6","#ee5396",
                "#d62670","#9f1853","#740937","#510224")

p <- ggplot(data = rcm_23_model_dat,
            aes(x = wshd_area_km2,
                y = accm_totco2_o2g_day/wshd_area_km2,
                color = frst_cat))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  # scale_color_manual(values = my_mcolors)+
  scale_color_brewer(palette = "PRGn")+
  geom_abline(slope = 1)+
  facet_wrap(basin~hrel_3_cat, nrow = 2)
p


# Calculating cumulative entropy

test_df <- rcm_23_model_dat %>% 
  select(basin,
         comid,
         tocomid,
         wshd_area_km2,
         stream_order)

associate_area <- function(input_df) {
  result_df <- input_df %>%
    group_by(basin) %>%
    mutate(
      max_area = max(wshd_area_km2),
      down_wshd_area_km2 = case_when(
        tocomid == 0 ~ wshd_area_km2,
        wshd_area_km2 == max_area ~ wshd_area_km2,
        TRUE ~ lag(wshd_area_km2)
      )
    ) %>%
    select(basin,comid, tocomid, down_wshd_area_km2, -max_area) %>%
    ungroup()
  
  return(result_df)
}

# Usage
result <- associate_area(test_df)














write.csv(rcm_23_model_dat,paste(local_data,"rcm_23_model_data.csv",sep = '/'),
          row.names = FALSE)

write.csv(rcm_23_model_dat,paste(export_data,"rcm_23_model_data.csv",sep = '/'),
          row.names = FALSE)
