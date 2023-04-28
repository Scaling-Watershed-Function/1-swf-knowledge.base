################################################################################
# SCALING WATERSHED FUNCTION: DATA WRANGLING
################################################################################

#Author: Francisco J. Guerrero

# Loading required packages: 

librarian::shelf(tidyverse,
                 utils)

# Local Import-Export
raw_data <- "raw"
processed_data <- "processed"

# Loading raw data

# Physical Characteristics and Hydrology

# We use the enhanced NHDPlus V.2. as the reference dataset for COMIDs
rcid_dat <- read_csv(paste(raw_data,"230423_enhanced_nhdp_2_yrb_wrb.csv", sep = '/'),
                     show_col_types = FALSE)

# Original dataset citation
#Blodgett, D.L., 2023, Updated CONUS river network attributes based on the E2NHDPlusV2 and NWMv2.1 
#networks (ver. 2.0, February 2023): U.S. Geological Survey data release, https://doi.org/10.5066/P976XCVT.

# Download script: script_enhanced_nhdp2_rselenium.R

# Physical characteristics of the river basins
bchr_dat <-  read_csv(paste(raw_data,"230427_pnw_basin_characteristics.csv", sep = '/'),
                      show_col_types = FALSE)

# Original dataset citation
#Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes for 
#NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream Watersheds 
# for the Conterminous United States (ver. 3.0, January 2021): U.S. Geological Survey data release, 
#https://doi.org/10.5066/F7765D7V.

# Download script: script_nhdp2_basin_charct_rselenium_download.R


# Hydrological data
hydr_dat <- read_csv(paste(raw_data,"230423_main_nhdp_2_yrb_wrb.csv", sep = '/'),
                       show_col_types = FALSE)

#Original dataset citation: 
#Schwarz, G.E., 2019, E2NHDPlusV2_us: Database of Ancillary Hydrologic Attributes 
#and Modified Routing for NHDPlus Version 2.1 Flowlines: U.S. Geological Survey data release, 
#https://doi.org/10.5066/P986KZEM.

#Download script: script_nhdp2_son_etal_22rselenium_download.R

# Merging data to selected variables from 'rcid_dat'

phys_dat <- rcid_dat %>% 
  select(comid,
         lengthkm,
         reachcode,
         totdasqkm,
         arbolatesu,
         hydroseq,
         streamorde,
         slope,
         slopelenkm,
         ftype,
         rpuid,
         vpuid,
         roughness,
         huc_4) %>% 
  merge(.,
        hydr_dat %>% 
          select(ComID,
                 FromNode,
                 ToNode,
                 Divergence,
                 PrecipV,
                 TempV,
                 RunOffV,
                 MAFlowUcfs,
                 MAVelUfps,
                 IncFlwUcfs) %>% 
          rename(comid = ComID,
                 from_node = FromNode,
                 to_node = ToNode,
                 divergence = Divergence,
                 precipt = PrecipV,
                 temp = TempV,
                 runoff = RunOffV,
                 mean_ann_flow = MAFlowUcfs,
                 mean_ann_vel = MAVelUfps,
                 inc_flw = IncFlwUcfs) %>% 
          mutate(mean_ann_pcpt_mm = precipt/100,
                 mean_ann_temp_dc = temp/100,
                 mean_ann_runf_mm = runoff,
                 mean_ann_flow_m3s = mean_ann_flow*0.0283,
                 mean_ann_vel_ms = mean_ann_vel*0.3048,
                 inc_flw_m3s = inc_flw*0.0283) %>% 
          select(comid,
                 from_node,
                 to_node,
                 divergence,
                 mean_ann_pcpt_mm,
                 mean_ann_temp_dc,
                 mean_ann_runf_mm,
                 mean_ann_flow_m3s,
                 mean_ann_vel_ms,
                 inc_flw_m3s),
        by.x = "comid",
        by.y = "comid",
        all.x = TRUE) %>% 
  merge(.,
        bchr_dat %>% 
          select(comid,
                 sinuosity,
                 TOT_STRM_DENS,
                 TOT_BASIN_SLOPE,
                 TOT_ELEV_MIN,
                 TOT_ELEV_MAX,
                 TOT_ELEV_MEAN,
                 TOT_STREAM_LENGTH,
                 CAT_STREAM_LENGTH,
                 TOT_STREAM_SLOPE,
                 CAT_STREAM_SLOPE) %>% 
          rename(stream_dens = TOT_STRM_DENS,
                 basin_slope = TOT_BASIN_SLOPE,
                 min_elevation_m = TOT_ELEV_MIN,
                 max_elevation_m = TOT_ELEV_MAX,
                 avg_elevation_m = TOT_ELEV_MEAN,
                 stream_lenght_km = TOT_STREAM_LENGTH,
                 flowline_lenght_km = CAT_STREAM_LENGTH,
                 stream_slope = TOT_STREAM_SLOPE,
                 flowline_slope = CAT_STREAM_SLOPE) %>% 
          select(comid,
                 sinuosity,
                 stream_dens,
                 basin_slope,
                 min_elevation_m,
                 max_elevation_m,
                 avg_elevation_m,
                 stream_lenght_km,
                 flowline_lenght_km,
                 stream_slope,
                 flowline_slope),
        by.x = "comid",
        by.y = "comid",
        all.x = TRUE)

data_dictionary <- df(variable = c("comid",
                                   "lengthkm",
                                   "reachcode",
                                   "totdasqkm",
                                   "arbolatesu",
                                   "hydroseq",
                                   "streamorde",
                                   "slope",
                                   "slopelenkm",
                                   "ftype",
                                   "rpuid",
                                   "vpuid",
                                   "roughness",
                                   "huc_4", # Blodgett (2023)
                                   "from_node",
                                   "to_node",
                                   "divergence",
                                   "mean_ann_pcpt_mm",
                                   "mean_ann_temp_dc",
                                   "mean_ann_runf_mm",
                                   "mean_ann_flow_m3s",
                                   "mean_ann_vel_ms",
                                   "inc_flw_m3s", # Shwarz (2019)
                                   "sinuosity",
                                   "stream_dens",
                                   "basin_slope",
                                   "min_elevation_m",
                                   "max_elevation_m",
                                   "avg_elevation_m",
                                   "stream_lenght_km",
                                   "flowline_lenght_km",
                                   "stream_slope",
                                   "flowline_slope"), # Wieczorek et al., 2018
                      description = c("Unique feature identifier from NHDPlus source data. USGS defined",
                      ),
                      reference = c(""))



phys_dat_print <- phys_dat %>% 
  select(comid,
         vpuid,
         rpuid,
         huc_4,
         reachcode,
         hydroseq,
         from_node,
         to_node,
         divergence,
         ftype,
         streamorde,
         stream_dens,
         sinuosity,
         roughness,
         stream_lenght_km,
         flowline_lenght_km,
         stream_slope,
         flowline_slope,
         basin_slope,
         min_elevation_m,
         max_elevation_m,
         avg_elevation_m,
         mean_ann_pcpt_mm,
         mean_ann_temp_dc,
         mean_ann_runf_mm,
         mean_ann_flow_m3s,
         mean_ann_vel_ms,
         inc_flw_m3s)






