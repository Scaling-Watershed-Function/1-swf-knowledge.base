################################################################################
# SCALING WATERSHED FUNCTION: DATA WRANGLING & GAP FILLING
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

# We use the enhanced NHDPlus V.2. as the reference dataset for COMIDs (Blodgett_23_Network_Attributes)
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

# Physical characteristics of the river basins (Wieczeroek_21_Select_Attributes)
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


# Hydrological data (Schwarz_19_Ancillary_Attributes)
hydr_dat <- read_csv(paste(raw_data,"230423_main_nhdp_2_yrb_wrb.csv", sep = '/'),
                       show_col_types = FALSE)

#Original dataset citation: 
#Schwarz, G.E., 2019, E2NHDPlusV2_us: Database of Ancillary Hydrologic Attributes 
#and Modified Routing for NHDPlus Version 2.1 Flowlines: U.S. Geological Survey data release, 
#https://doi.org/10.5066/P986KZEM.

#Download script: script_nhdp2_son_etal_22rselenium_download.R

# Dataset explorations and variable selection

# Flowline slopes

# In the Enhanced Hydrologic Stream Network Based on the NHDPlus Medium resolution
# dataset (rcid_dat, in our case), the slopes were revised and re-calculated: 

# "NHDPlus slopes determined according to the original NHDPlusV2 method and the 
# revised method in relation to slopes measured at 2,846 sites indexed to NHDPlusV2 
# from the U.S. Environmental Protection Agency's Wadeable Streams Assessment
# and National River and Stream Assessment. WSA, Wadeable Streams Assessment; NRSA, 
# National River and Stream Assessment"

# The major update consisted in a better determination of the benchmark points, 
# mostly in headwater catchments, that could be use as the initial elevation, from 
# which the slopes would be determined in the downstream direction. For more info
# on the revised method go to: https://pubs.usgs.gov/sir/2019/5127/sir20195127.pdf

# Comparing Areas, slopes, and stream lengths between NHDPlus v 2.1 and the Enhanced NHDPlus V. 2.1

slope_comp <- bchr_dat %>% 
  select(comid,
         CAT_BASIN_AREA,
         CAT_STREAM_SLOPE,
         CAT_STREAM_LENGTH,
         TOT_BASIN_AREA,
         TOT_STREAM_LENGTH) %>% 
  merge(.,
        rcid_dat %>% 
          select(comid,
                 areasqkm,
                 slope,
                 lengthkm,
                 slopelenkm,
                 totdasqkm,
                 arbolatesu,
                 streamleve,
                 streamorde,
                 huc_4),
        by= "comid") %>% 
  merge(.,
         hydr_dat %>% 
          select(ComID,
                 CatAreaKm2,
                 SLOPE,
                 LENGTHKM,
                 TotAreaKM2,
                 TotLngthKm),
        by.x = "comid",
        by.y = "ComID",
        all.x = TRUE)


# Area comparison

a_comp_plot <- slope_comp %>% 
  select(comid,
         streamorde,
         CAT_BASIN_AREA,
         areasqkm,
         CatAreaKm2) %>% 
  rename(wcz21_attributes_area = CAT_BASIN_AREA,
         blg23_network_at_area = areasqkm,
         sch19_ancillaryh_area = CatAreaKm2) %>% 
  gather(.,
         key="dataset",
         value = "catchment_area_sqkm",
         factor_key = TRUE,
         c(4:5)) %>% 
  ggplot(aes(x = wcz21_attributes_area,
             y = catchment_area_sqkm,
             color = as.factor(streamorde)))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~dataset,ncol = 2)
a_comp_plot

# It looks like the values for catchment area for both the "Updated CONUS river 
# network attributes based on the E2NHDPlusV2 and NWMv2.1 networks (ver. 2.0, 
# February 2023)" and the E2NHDPlusV2_us: Database of Ancillary Hydrologic 
# Attributes and Modified Routing for NHDPlus Version 2.1 Flowlines are exactly the
# same, and in a number of cases have lower estimations for catchment area. 

# A quick verification: 

p <- ggplot(data = slope_comp,
            aes(x = areasqkm,
                y = CatAreaKm2))+
  geom_point()+
  labs(x = "blg23_network_at_area (km2)",
       y = "sch19_ancillaryh_area (km2)")+
  scale_y_log10()+
  scale_x_log10()+
  geom_abline()
p

# Slope Comparison

# We will start with the most recent updates on NHDPlus v2

p <- ggplot(data = slope_comp,
            aes(x = slope,
                y = SLOPE))+
  geom_point()+
  labs(x = "blg23_network_at_slope",
       y = "sch19_ancillaryh_slope")+
  scale_y_log10()+
  scale_x_log10()+
  geom_abline()
p

# There are large discrepancies between the two datasets, although the slope values
# are within the same order of magnitude.Let's compare these two datasets with the original
# NHDPlus

s_comp_plot <- slope_comp %>% 
  select(comid,
         streamorde,
         CAT_STREAM_SLOPE,
         slope,
         SLOPE) %>% 
  rename(wcz21_attributes_slope = CAT_STREAM_SLOPE,
         blg23_network_at_slope = slope,
         sch19_ancillaryh_slope = SLOPE) %>% 
  gather(.,
         key="dataset",
         value = "flowline_slope",
         factor_key = TRUE,
         c(4:5)) %>% 
  ggplot(aes(x = wcz21_attributes_slope,
             y = flowline_slope,
             color = as.factor(streamorde)))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  geom_hline(yintercept = 0.005, linetype = "dashed")+
  facet_wrap(~dataset,ncol = 2)
s_comp_plot

# The slopes from the blg23_network_at dataset  are in better agreement with the 
# slopes in wzc21_attributes than those in sch_19_ancillaryh dataset. Also, the last
# dataset contains slope values well below the default of 0.00001.

# Now, when comparing blg23 and wzc21 datasets, we observe tends to assign the lowest 
# values for flowlines with low slopes. 


cat_areas <- ggplot(data = slope_comp,
                    aes(x = CAT_BASIN_AREA,
                        y = areasqkm,
                        color = as.factor(streamorde)))+
  geom_point(alpha = 0.5)+
  geom_point(data = slope_comp,
             aes(x = CAT_BASIN_AREA,
                 y = CatAreaKm2),
             inherit.aes = TRUE)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()#+
# facet_wrap(as.factor(huc_4)~as.factor(streamorde))
cat_areas

# Flowline catchment areas fall on the 1:1 line, with a few exceptions, for which
# the estimate in the ENHDPlus 2.1 are lower. 


slopes <- ggplot(data = slope_comp,
                 aes(x = CAT_STREAM_SLOPE,
                     y = slope,
                     color = as.factor(streamorde)))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(~as.factor(huc_4), ncol = 2)
slopes

# Most of the points fall on the 1:1 line between slopes from NHDPlus 2.1 and the 
# ENHDPlus 2.1. However, a larger number of locations appear with lower slopes in the
# ENHDPlus 2.1 dataset. Let's check the approximate value of those locations

nrow(filter(slope_comp,slope<0.00001))

# This is an approximate distribution of with slopes below 0.005
# Slope < 0.005  - 4601 datapoints
# Slope < 0.001  - 2432 datapoints
# Slope < 0.0005 - 2017 datapoints
# Slope < 0.0001 - 1680 datapoints

# These values occur across a wide range of stream orders. Let's take a look at the 
# distribution of slope values across stream orders:

slopes_order <- ggplot(data = slope_comp,
                       aes(x = as.factor(streamorde),
                           y = CAT_STREAM_SLOPE,
                           color = as.factor(streamorde),
                           fill = as.factor(streamorde)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10()+
  facet_wrap(~as.factor(huc_4))
slopes_order


# Checking how slopes vary with stream levels in the ENHDPlus 2.1

slopes_level <- ggplot(data = slope_comp,
                       aes(x = as.factor(streamleve),
                           y = CAT_STREAM_SLOPE,
                           color = as.factor(streamleve),
                           fill = as.factor(streamleve)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10()+
  facet_wrap(~as.factor(huc_4))
slopes_level

# Next steps: To assign slope values that fall below 0.005, the expected value
# from a regression?


slp_mod <- lm(log(slope)~log(CAT_STREAM_SLOPE),
              data = filter(slope_comp, slope > 0.005)) 

summary(slp_mod)

# The residual plot is highly heteroscedastic. Different alternatives? 















# Merging data to selected variables from 'rcid_dat'

phys_dat <- rcid_dat %>% 
  select(comid,
         lengthkm,#flowline length
         reachcode,
         areasqkm,#catchment area
         totdasqkm,#watershed area
         arbolatesu,#total upstream flowline length
         hydroseq,
         streamorde,
         slope,#flowline slope
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
                 CatAreaKm2,
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
                 cat_area_km2 = CatAreaKm2,
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
                 cat_area_km2,
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




p <- ggplot(data = filter(rcid_dat),
            aes(x = arbolatesu,
                y = lengthkm,
                fill = as.factor(streamorde),
                color = as.factor(streamorde)))+
  geom_point(alpha = 0.5)+
  facet_wrap(~as.factor(huc_4), ncol = 2)+
  scale_y_log10()+
  scale_x_log10()
p
  
p <- ggplot(data = filter(phys_dat_trm, flowline_slope > 0),
            aes(x = as.factor(streamorde),
                y = flowline_slope,
                fill = as.factor(streamorde),
                color = as.factor(streamorde)))+
  geom_boxplot(alpha = 0.5)+
  facet_wrap(~as.factor(huc_4), ncol = 2)+
  scale_y_log10()
p




# Exploring the data via summary

summary(phys_dat)

# We find several variables with zero or missing values: 

# Total Drainage Area

summary(filter(phys_dat,totdasqkm == 0))

# All 72 missing values correspond to first order streams with zero values for catchment
# areas as well, so we will remove these data points.

phys_dat_trm <- (filter(phys_dat,totdasqkm!=0))

summary(phys_dat_trm)

# We have 30 data points with NAs for roughness

summary(filter(phys_dat_trm, is.na(roughness)==TRUE))

# Roughness

# NA values in roughness correspond to flowline_slope = 0. Otherwise, the cover
# a wide range of watershed characteristics

n_plot <- ggplot(data = filter(phys_dat_trm, 
                               is.na(roughness) == FALSE),
                 aes(x = streamorde,
                     y = roughness,
                     color=as.factor(streamorde)))+
  geom_boxplot()
n_plot

# We will replace the missing n values by the average value for the corresponding 
# stream order

phys_dat_trm <- phys_dat_trm %>% 
  group_by(streamorde) %>% 
  mutate(roughness_ord = mean(roughness,na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(roughness = if_else(is.na(roughness),
                             roughness_ord,
                             roughness)) %>% 
  select(-roughness_ord)

# Mean annual flow

summary(filter(phys_dat_trm, mean_ann_flow_m3s==0))

# we have 22 values all corresponding to first order streams, we proceed to remove
# these rows: 

phys_dat_trm <- filter(phys_dat_trm, mean_ann_flow_m3s!=0)

# Stream density

# We have only 1 NA value corresponding to a 1 order stream. We proceed to remove this
# row

phys_dat_trm <- filter(phys_dat_trm, is.na(stream_dens)==FALSE)

# Stream slope & Stream length (we replace zero values with the corresponding flow line values)

summary(phys_dat_trm)

# We have missing values and zeroes for stream slope as well as for flowline slope




phys_dat_trm <- phys_dat_trm %>% 
  mutate(stream_slope = if_else(stream_slope < 0, 
                                flowline_slope,
                                stream_slope),
         stream_lenght_km = if_else(stream_lenght_km== 0,
                                    flowline_lenght_km,
                                    stream_lenght_km))

# Bankfull width, depth, and cross sectional area

summary(filter(phys_dat_trm, bnkfll_width_m == 0))

# Only 5 values corresponding to first order streams. We proceed to remove these rows

phys_dat_trm <- filter(phys_dat_trm, bnkfll_width_m != 0)

summary(phys_dat_trm)


data_dictionary <- data.frame(variable = c("comid",
                                   "lengthkm",
                                   "reachcode",
                                   "totdasqkm",
                                   "arbolatesu",
                                   "hydroseq",
                                   "streamorde",
                                   "ftype",
                                   "rpuid",
                                   "vpuid",
                                   "roughness",
                                   "huc_4", # Blodgett (2023)
                                   "from_node",
                                   "to_node",
                                   "cat_area_km2",
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
                      "An NHDFlowline feature that is part of a series of consecutive flowlines that does not ultimately flow to a coast and has an FType of StreamRiver, Artificial Path, or Connector; otherwise",
                      "Raster Processing Unit ID (For landscape features)",
                      "Vector Processing Unit ID (For flowline features)",
                      "Numeric Manning's N estimate for flowline",
                      "4-Digit Hydrologic Unit Code",
                      "Original NHDPlus V2 from node identifier",
                      "Original NHDPlus V2 to node identifier",
                      "Total catchment area (local drainage to flowline)",
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
                                    "Schwarz, G. E., 2019",
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

write.csv(phys_dat_trm,paste(raw_data,"230429_trm_basin_char_hydr_geom_yrb_wrb.csv", sep = '/'),
          row.names = FALSE)

write.csv(data_dictionary,paste(raw_data,"230429_dd_basin_char_hydr_geom_yrb_wrb.csv", sep = '/'),
          row.names = FALSE)

g = 9.8

test_dat <- phys_dat_trm %>% 
  select(comid,
         bnkfll_depth_m,
         mean_ann_vel_ms,
         bnkfll_width_m,
         flowline_slope,
         flowline_lenght_km,
         sinuosity) %>% 
  mutate(darcy = (8*g*bnkfll_depth_m*flowline_slope)/(mean_ann_vel_ms^2))



