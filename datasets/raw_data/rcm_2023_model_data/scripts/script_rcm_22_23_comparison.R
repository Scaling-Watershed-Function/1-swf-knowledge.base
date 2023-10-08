###############################################################################
#  RCM 2023 on RF filled NEXSS data vs RCM 2022 Only respiration RF filled data
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

# RCM model data version 2022 (without NEXSS gap filling)
rcm_22_model_dat <- read_csv(paste(local_data,"model_inputs","raw_willamette_yakima_data_rf_scaling_analysis_data.csv", sep = '/'),
                             show_col_types = FALSE)

# RCM model data version 2023 (resp data estimated with NEXSS gap filling via random forest)
rcm_23_model_dat <- read_csv(paste(local_data,"rcm_23_model_output_data.csv",sep = '/'),
                             show_col_types = FALSE)

# Summary of RCM 23 input data
rcm_23_model_inp_dat <- rcm_23_model_dat %>% 
  select(comid,
         do_stream_mg_l,
         doc_stream_mg_l,
         no3_stream_mg_l,
         tot_q_hz_ms,
         tot_rt_hz_s)

summary(rcm_23_model_inp_dat)

# Summary of RCM 22 input data
rcm_22_model_inp_dat <- rcm_22_model_dat %>% 
  select(comid,
         do_stream_mg_l,
         doc_stream_mg_l,
         no3_stream_mg_l,
         tot_q_hz_ms,
         tot_rt_hz_s)

summary(rcm_22_model_inp_dat)


# Combining both datasets
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

# Comparing variables (replacing values for x and y for different comparisons)
p <- ggplot(data = comparison_input_dat,
            aes(x = tot_rt_hz_s.x,
                y = tot_rt_hz_s.y))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()
p

# Observations:

# Substrate concentrations are identical (R2 =1)
# Total exchange flux remains identical for values that were not estimated with RF
# Total residence time remains identical for values that were not estimated with RF

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
