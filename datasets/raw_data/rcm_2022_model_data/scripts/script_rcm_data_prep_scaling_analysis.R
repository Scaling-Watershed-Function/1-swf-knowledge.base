###############################################################################
# Dataset preparation for scaling analysis
###############################################################################

gc()

librarian::shelf(tidyverse,
                 utils,
                 nhdplusTools,
                 sp,
                 sf,
                 leaflet,
                 stringr)

# Local Import-Export
source_data <- "../../raw_data"
local_data <- "./data"

reference_comids <- read_csv(paste(source_data,"enhanced_nhdplus_21","data","enhanced_nhdplus21_ywrb.csv", sep = '/'),
                             show_col_types = FALSE)

nexss_inputs_dat <- read_csv(paste(local_data,"merged_nexss_inputs.csv", sep = '/'),
                             show_col_types = "FALSE")

annual_resp_dat <- read_csv(paste(local_data,"nhd_CR_stream_annual_resp_inputs_outputs.csv", sep = '/'),
                            show_col_types = "FALSE")

resp_gap_filled_wrb_dat0 <- read_csv(paste(local_data,"son_etal_22_wrb_RF_resp_data.csv", sep = '/'),
                                show_col_types = "FALSE") %>% select(-1)

resp_gap_filled_yrb_dat0 <- read_csv(paste(local_data,"son_etal_22_yrb_RF_resp_data.csv", sep = '/'),
                                    show_col_types = "FALSE") %>% select(-1)

nsi_ssn_ntwk_dat <- st_transform(st_read(paste(source_data,"nsi_ssn_network","data","nsi_network_ywrb.shp",sep = "/")),4326)

# Checking connectivity in datasets

reference_comids <- reference_comids %>% 
  select(comid,
         tocomid,
         huc_4) %>% 
  mutate(basin = if_else(huc_4 == "1703",
                         "yakima",
                         "willamette"))

annual_resp_dat <- annual_resp_dat %>% 
  merge(.,
        reference_comids %>% 
          select(comid,basin),
        by = "comid",
        all.x = TRUE)


nexss_inputs_dat <- nexss_inputs_dat %>% 
  merge(.,
        reference_comids %>% 
          select(comid,basin),
        by = "comid",
        all.x = TRUE)

resp_gap_filled_wrb_dat <- resp_gap_filled_wrb_dat0 %>% 
    select(comid,
         totco2g_day_fill) %>% 
  rename(totco2g_day = totco2g_day_fill) 
summary(resp_gap_filled_wrb_dat)

resp_gap_filled_yrb_dat <- resp_gap_filled_yrb_dat0 %>% 
  select(comid,
         totco2g_day_fill) %>% 
  rename(totco2g_day = totco2g_day_fill) 
summary(resp_gap_filled_yrb_dat)

resp_gap_filled_dat <- rbind(resp_gap_filled_wrb_dat,
                             resp_gap_filled_yrb_dat)

resp_dat <- annual_resp_dat %>% 
  merge(.,
        resp_gap_filled_dat,
        by = "comid",
        all.x = TRUE)

summary(resp_dat)

# Connectivity test
test_dat_connectivity <- resp_dat %>% 
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


# Expressing all C fluxes as totco2g_day

stream_area_dat <- nexss_inputs_dat %>% 
  select(comid,
         logw_m,
         length_m) %>% 
  mutate(stream_width_m = 10^logw_m,
         stream_length_m = length_m,
         stream_area_m2 = stream_width_m * stream_length_m) %>% 
  select(comid,
         stream_width_m,
         stream_length_m,
         stream_area_m2)

# Removing NA's for totco2g_day does not affect network connectivity as these values
# correspond to the Columbia River basin. Removing NA's from Yakima does decrease the
# overall connectivity in ~ 10%

willamette_clean <- resp_dat %>% 
  filter(basin == "willamette" & is.na(totco2g_day) == FALSE)

resp_dat_clean <- resp_dat %>% 
  filter(basin == "yakima") %>% 
  rbind(.,
        willamette_clean) %>% 
  merge(.,
        stream_area_dat,
        by = "comid",
        all.x = TRUE)

summary(resp_dat_clean)

# Connectivity test
test_dat_connectivity <- resp_dat_clean %>% 
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

# Relabeling columns and converting all c fluxes to g/day

resp_dat_clean <-  resp_dat_clean %>% 
  mutate(tot_rt_hz_s = 10^logrt_total_hz_s,
         tot_q_hz_ms = 10^logq_hz_total_m_s,
         totco2_o2g_day = 10^logtotco2_o2g_m2_day * stream_area_m2,
         totco2_ang_day = 10^logtotco2_ang_m2_day * stream_area_m2) %>% 
  select(-c(logrt_total_hz_s,
            logq_hz_total_m_s,
            logtotco2_o2g_m2_day,
            logtotco2_ang_m2_day,
            logtotco2g_m2_day))

summary(resp_dat_clean)

# Generating shapefiles

nsi_rcm_ntwk_dat <- nsi_ssn_ntwk_dat %>% 
  rename(comid = COMID) %>% 
  merge(.,
        resp_dat_clean, 
        by = "comid",
        all.x = TRUE) %>% 
  filter(!(basin == "willamette" & is.na(totco2g_day)))


# Checking resulting network

leaflet(nsi_rcm_ntwk_dat) %>% 
  addPolylines(weight = 2) %>% 
  addPolylines(data = filter(nsi_rcm_ntwk_dat,is.na(totco2g_day)==TRUE),
               color = "magenta",
               opacity = 1,
               weight = 9) %>% 
  addProviderTiles("Esri.WorldImagery")


# Saving resulting shape files

# Rename columns so they meet the 10 character length ESRI format
nsi_rcm_ntwk_sto <- nsi_rcm_ntwk_dat %>% 
  rename(pd_ann_do = pred_annual_do,
         pd_ann_doc = pred_annual_doc,
         no3_mgl = no3_conc_mg_l,
         t_co2_gday = totco2g_day,
         strm_wdthm = stream_width_m,
         strm_lgthm = stream_length_m,
         strm_arem2 = stream_area_m2,
         t_rthz_s = tot_rt_hz_s,
         t_qhz_ms = tot_q_hz_ms,
         abco2_gday = totco2_o2g_day,
         anco2_gday = totco2_ang_day)

# Determine the maximum width required for the 'tocomid' field
max_tocomid_width <- max(nchar(as.character(nsi_rcm_ntwk_sto$tocomid)))

# Update the field width for 'tocomid' in 'nsi_rcm_phys_sto'
nsi_rcm_ntwk_sto$tocomid <- format(nsi_rcm_ntwk_sto$tocomid, width = max_tocomid_width)

# Maintaining the numeric format of tocomid
nsi_rcm_ntwk_sto$tocomid <- as.numeric(nsi_rcm_ntwk_sto$tocomid)

# Specify the file path for saving the shapefile
output_file <- paste(local_data,"shapefiles","river_corridors_respiration_geom.shp", sep = '/')

# Write the 'nsi_rcm_phys_sto' data frame to a shapefile
st_write(nsi_rcm_ntwk_sto, 
         dsn = output_file, 
         delete_dsn = TRUE, 
         overwrite_layer = TRUE, 
         delete_layer = TRUE)

write.csv(resp_dat_clean,paste(local_data,"RF_filled_rcm_2022_model_data.csv", sep ='/'),
          row.names = FALSE)



library(sf)

# Saving resulting shape files
nsi_rcm_ntwk_sto <- nsi_rcm_ntwk_dat 

# Check the maximum character length for each column in the dataframe
max_char_length <- sapply(nsi_rcm_ntwk_sto, function(x) max(nchar(as.character(x))))

# Print the maximum character length for each column
print(max_char_length)


# Assuming you want to set the maximum character width to 50
max_char_width <- 50

# Use st_as_sf with dim=TRUE to update the max character width for all columns
nsi_rcm_ntwk_sto <- st_as_sf(nsi_rcm_ntwk_sto, dim = TRUE, max_length = max_char_width)

# Specify the file path for saving the shapefile
output_file <- paste(local_data, "shapefiles", "river_corridors_respiration_geom.shp", sep = '/')

# Write the updated 'nsi_rcm_ntwk_sto' data frame to a shapefile
st_write(nsi_rcm_ntwk_sto, 
         dsn = output_file, 
         delete_dsn = TRUE, 
         overwrite_layer = TRUE, 
         delete_layer = TRUE)


