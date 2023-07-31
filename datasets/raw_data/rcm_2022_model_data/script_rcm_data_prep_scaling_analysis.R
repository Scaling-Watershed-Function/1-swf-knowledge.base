###############################################################################
# Dataset preparation for scaling analysis
###############################################################################

gc()

librarian::shelf(tidyverse,
                 utils)

# Local Import-Export
source_data <- "../../raw_data"
local_data <- "./data"


nexss_inputs_dat <- read_csv(paste(local_data,"merged_nexss_inputs.csv", sep = '/'),
                             show_col_types = "FALSE")

annual_resp_dat <- read_csv(paste(local_data,"nhd_CR_stream_annual_resp_inputs_outputs.csv", sep = '/'),
                            show_col_types = "FALSE")

resp_gap_filled_wrb_dat <- read_csv(paste(local_data,"son_etal_22_wrb_RF_resp_data.csv", sep = '/'),
                                show_col_types = "FALSE") %>% select(-1)

resp_gap_filled_yrb_dat <- read_csv(paste(local_data,"son_etal_22_yrb_RF_resp_data.csv", sep = '/'),
                                    show_col_types = "FALSE") %>% select(-1)

resp_gap_filled_dat <- rbind(resp_gap_filled_wrb_dat,resp_gap_filled_yrb_dat)

# Merging Respiration data
rcm_resp_dat <- resp_gap_filled_dat %>% 
  merge(.,
        annual_resp_dat,
        by = "comid",
        all.x = TRUE) %>% 
  merge(.,
        nexss_inputs_dat %>% 
          select(comid,
                 logw_m,
                 length_m),
        by = "comid",
        all.x = TRUE)


write.csv(rcm_resp_dat,paste(local_data,"RF_filled_rcm_2022_model_data.csv", sep ='/'),
          row.names = FALSE)


