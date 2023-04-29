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
#networks (ver. 2.0, February 2023): U.S. Geological Survey data release,
#https://doi.org/10.5066/P976XCVT.

#Moore, R.B., McKay, L.D., Rea, A.H., Bondelid, T.R., Price, C.V., Dewald, T.G., and Johnston, 
#C.M., 2019, User's guide for the national hydrography dataset plus (NHDPlus) high 
#resolution: U.S. Geological Survey Open-File Report 2019–1096, 66 p., 
#https://doi.org/10.3133/ofr20191096.


# Download script: script_enhanced_nhdp2_rselenium.R

# Physical characteristics of the river basins
bchr_dat <-  read_csv(paste(raw_data,"230428_pnw_basin_characteristics.csv", sep = '/'),
                      show_col_types = FALSE)

# Original dataset citation
#Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes for 
#NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream Watersheds 
#for the Conterminous United States-Select Basin Characteristics (ver. 3.0, January 2021): U.S. 
#Geological Survey data release, 
#https://www.sciencebase.gov/catalog/item/57976a0ce4b021cadec97890.

#Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes for 
#NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream Watersheds 
#for the Conterminous United States-Bankfull Hydraulic Geometry Related to Physiographic 
#Divisions (ver. 3.0, January 2021): U.S. Geological Survey data release, 
#https://www.sciencebase.gov/catalog/item/5cf02bdae4b0b51330e22b85.

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
                 CAT_STREAM_SLOPE,
                 BANKFULL_WIDTH,
                 BANKFULL_DEPTH,
                 BANKFULL_XSEC_AREA) %>% 
          rename(stream_dens = TOT_STRM_DENS,
                 basin_slope = TOT_BASIN_SLOPE,
                 min_elevation_m = TOT_ELEV_MIN,
                 max_elevation_m = TOT_ELEV_MAX,
                 avg_elevation_m = TOT_ELEV_MEAN,
                 stream_lenght_km = TOT_STREAM_LENGTH,
                 flowline_lenght_km = CAT_STREAM_LENGTH,
                 stream_slope = TOT_STREAM_SLOPE,
                 flowline_slope = CAT_STREAM_SLOPE,
                 bnkfll_width_m = BANKFULL_WIDTH,
                 bnkfll_depth_m = BANKFULL_DEPTH,
                 bnkfll_xsec_area_m2 = BANKFULL_XSEC_AREA) %>% 
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
                 flowline_slope,
                 bnkfll_width_m,
                 bnkfll_depth_m,
                 bnkfll_xsec_area_m2),
        by.x = "comid",
        by.y = "comid",
        all.x = TRUE)


data_dictionary <- data.frame(variable = c("comid",
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
                                   "flowline_slope",
                                   "bnkfll_width_m",
                                   "bnkfll_depth_m",
                                   "bnkfll_xsec_area_m2"), # Wieczorek et al., 2018
                      description = c("Unique feature identifier from NHDPlus source data. USGS defined",
                      "Flowline length. USGS Defined",
                      "Unique flowline identifier. The first eight digits are the Watershed Boundary Dataset(WBD) HUC8.The next six digits are randomly assigned, sequential numbers that are unique within a HUC8.",
                      "Total drainage area recalculated with nhdplus Tools",
                      "the sum of the lengths of all digitized flowlines upstream from the downstream end of the immediate flowline, in kilometers",
                      "Hydrosequence number (assigned in ascending order",
                      "Modified Strahler stream order",
                      "Slope of the flowline from smoothed elevation (unitless)",
                      "Flow-line length used to calculate slope, in km",
                      "An NHDFlowline feature that is part of a series of consecutive flowlines that does not ultimately flow to a coast and has an FType of StreamRiver, Artificial Path, or Connector; otherwise",
                      "Raster Processing Unit ID (For landscape features)",
                      "Vector Processing Unit ID (For flowline features)",
                      "Numeric Manning's N estimate for flowline",
                      "4-Digit Hydrologic Unit Code",
                      "Original NHDPlus V2 from node identifier",
                      "Original NHDPlus V2 to node identifier",
                      "Divergence code (0 for no diversion, 1 for primary pathway, and 2 for secondary pathway).",
                      "Mean annual precipitation in mm -multiplied by 100 in original data",
                      "Mean annual temperature in Degree Celsius -multiplied by 100 in original data",
                      "Mean annual runoff in mm",
                      "Cumulative mean annual flow using unit flow runoff method",
                      "Stream velocity at mean annual flow",
                      "Incremental catchment mean annual flow (unit runoff methods, m3s)",
                      "Flowline reach's sinuosity calculated as the reach length (in meters) divided by its Euclidean distance (straight line in meters). Straight-line length is measured from the beginning node of a reach to the end node of the reach.",
                      "Flowline catchment stream density calculated as all upstream stream lengths (meters) divided by all upstream catchment areas (square kilometers). Upstream is defined by total upstream routing.",
                      "Average slope in percent of all upstream NHDPlusV2 flowline catchments, based on total upstream routing",
                      "Minimum elevation in meters of all upstream NHDPlusV2 flowline catchments, based on total upstream routing.",
                      "Maximum elevation in meters of all upstream NHDPlusV2 flowline catchments, based on total upstream routing.",
                      "Mean elevation in meters of all upstream NHDPlusV2 flowline catchments, based on total upstream routing.",
                      "Total length of all upstream NHDPlusV2 flowlines in kilometers, based on total upstream routing.",
                      "NHDPlus version 2 flowline's length in kilometers taken directly from NHDPlusv2's NHDflowline shapefile's item, LENGTHKM.",
                      "Average slope in percent NHDPlusV2 flowlines, based on total upstream routing.",
                      "NHDPlus version 2 flowline's average slope in percent.",
                      "Estimated bankfull width of NHDPlus version 2.1's flowline reach calculated using Bieger 's regression equation (Bieger et al, 2015)",
                      "Estimated bankfull depth of NHDPlus version 2.1's flowline reach calculated using Bieger 's regression equation (Bieger et al, 2015)",
                      "Estimated bankfull cross sectional area of NHDPlus version 2.1's flowline reach calculated using Bieger 's regression equation (Bieger et al, 2015)"),
                      reference = c("Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Blodgett, 2023; Moore et al., 2019",
                                    "Schwarz, G. E., 2019",
                                    "Schwarz, G. E., 2019",
                                    "Schwarz, G. E., 2019",
                                    "Schwarz, G. E., 2019",
                                    "Schwarz, G. E., 2019",
                                    "Schwarz, G. E., 2019",
                                    "Schwarz, G. E., 2019",
                                    "Schwarz, G. E., 2019",
                                    "Schwarz, G. E., 2019",
                                    "Wieczorek, et al., 2018-Select Basin Characteristics",
                                    "Wieczorek, et al., 2018-Select Basin Characteristics",
                                    "Wieczorek, et al., 2018-Select Basin Characteristics",
                                    "Wieczorek, et al., 2018-Select Basin Characteristics",
                                    "Wieczorek, et al., 2018-Select Basin Characteristics",
                                    "Wieczorek, et al., 2018-Select Basin Characteristics",
                                    "Wieczorek, et al., 2018-Select Basin Characteristics",
                                    "Wieczorek, et al., 2018-Select Basin Characteristics",
                                    "Wieczorek, et al., 2018-Select Basin Characteristics",
                                    "Wieczorek, et al., 2018-Select Basin Characteristics",
                                    "Wieczorek, et al., 2018-Bankfull Hydraulic Geometry",
                                    "Wieczorek, et al., 2018-Bankfull Hydraulic Geometry",
                                    "Wieczorek, et al., 2018-Bankfull Hydraulic Geometry"))


write.csv(phys_dat,paste(raw_data,"230429_basin_char_hydr_geom_yrb_wrb.csv", sep = '/'),
          row.names = FALSE)

write.csv(data_dictionary,paste(raw_data,"230429_dd_basin_char_hydr_geom_yrb_wrb.csv", sep = '/'),
          row.names = FALSE)





