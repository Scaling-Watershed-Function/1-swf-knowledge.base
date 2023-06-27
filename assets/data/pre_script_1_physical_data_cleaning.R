################################################################################
# SCALING WATERSHED FUNCTION: DATA CLEANING - PART I WATERSHED & CATCHMENT AREAS
################################################################################

# IMPORTANT: This script is provisional! Final data cleaning script is contingent
# upon matching between NHDPlus 2.1 and NHDPlus HR

#Author: Francisco J. Guerrero

# Loading required packages: 

librarian::shelf(tidyverse,
                 utils,
                 quantreg,
                 gginnards,
                 nhdplusTools,
                 leaflet,
                 sp,
                 sf)

# Local Import-Export
raw_data <- "raw"
processed_data <- "processed"

phys_nsi_dat_ref <- read_csv(paste(raw_data,"phys_nsi_dat_reference.csv", sep = '/'),
                        show_col_types = FALSE)

phys_nsi_dat_shp <- sf::st_transform(st_read(paste(raw_data,"shape_files","nis_reference","230623_nis_network_ywrb.shp",sep = "/")),4326)



# Flowlines with no assigned watershed/catchment areas could mostly correspond to 
# disconnected segments

summary(filter(phys_nsi_dat_ref, wshd_area_km2 == 0))

# We find 43 datapoints from first order streams with watershed areas.

phys_nsi_dat_mod1 <- filter(phys_nsi_dat_ref, wshd_area_km2 > 0)

# Looking at datapoints with catchment areas = 0 within the filtered dataset

summary(filter(phys_nsi_dat_mod1, ctch_area_km2 == 0))

# We find 89 data points with catchment areas = 0 distributed across multiple stream
# orders, so need to check their locations before deciding on how to proceed. 

# Mapping zero values for catchment area

leaflet(phys_nsi_dat_shp) %>% 
  addPolylines(weight = 2) %>% 
  addPolylines(data =filter(phys_nsi_dat_shp,AreaSqKM == 0),
               weight =6,
               opacity = 1,
               color = "magenta")

leaflet(phys_nsi_dat_shp) %>%
  addTiles() %>%
  addMarkers(weight = 2) %>%
  addMarkers(data = filter(phys_nsi_dat_shp, AreaSqKM == 0),
             weight = 6,
             opacity = 1,
             color = "magenta")


coordinates <- strsplit(phys_nsi_dat_mod1$geometry, ",")
coordinates <- lapply(coordinates, function(x) {
  x <- as.numeric(x)
  matrix(x, ncol = 2, byrow = TRUE)
})

# Convert the geometry column to LINESTRING
phys_nsi_dat_shp_1 <- st_cast(phys_nsi_dat_shp, "LINESTRING")

# Create a leaflet map and add polylines
leaflet() %>%
  addTiles() %>%
  addPolylines(data = phys_nsi_dat_shp, weight = 2) %>%
  addPolylines(data = filter(phys_nsi_dat_shp, AreaSqKM == 0),
               weight = 6,
               opacity = 1,
               color = "magenta")


leaflet() %>%
  addTiles() %>%
  addMarkers(data = phys_nsi_dat_shp, weight = 2) %>%
  addMarkers(data = filter(phys_nsi_dat_shp, AreaSqKM == 0),
             weight = 6,
             opacity = 1,
             color = "magenta")


# Create a leaflet map and add polylines
leaflet() %>% 
  addTiles() %>% 
  addPolylines(data = coordinates)

library(sp)

# Convert the list of coordinates to a SpatialLinesDataFrame
lines <- lapply(coordinates, function(coords) Line(coords))
lines <- SpatialLinesDataFrame(SpatialLines(lines), data = phys_nsi_dat_mod1)

# Create a leaflet map and add polylines
leaflet() %>% 
  addTiles() %>% 
  addPolylines(data = lines)




son_etal_dat <- read_csv(paste(raw_data,"230406_son_etal_22_results_zen.csv", sep = '/'),
                         show_col_types = FALSE)


ntwk_dat <- phys_dat_ro %>% 
  merge(.,
        son_etal_dat %>% 
          filter(.,time_type=="annual"),
        by = "comid",
        all.x = TRUE) %>% 
  filter(basin!="ipswich")

write.csv(ntwk_dat,paste(raw_data,"230620_guerrero_etal_network_swf.csv", sep = "/"),
          row.names = FALSE)

# Exploring the data via summary

summary(phys_dat_ro)

# We find several variables with zero or missing values.These flow lines do not have a catchment area 
# associated to them because they are not connected to the network (Blodget, comms. pers.). So we will remove thase lines from the 
# dataset. The same applies to flowlines with catchment areas = 0

# We will filter our data for stream and rivers only and check whether this takes care of many
# NAs


phys_dat_mod1 <- phys_dat_ro %>% 
  filter(reach_type!="CanalDitch" & 
           reach_type!="Connector") %>% 
  filter(reach_type!="Pipeline")

summary(phys_dat_mod1)

phys_dat_mod2 <- phys_dat_mod1 %>% 
  # filter(wshd_area_km2!=0 & 
  #          ctch_area_km2!=0) 

summary(phys_dat_mod2) 
  
write.csv(phys_dat_mod2,paste(raw_data,"230620_phys_dat_mod_2.csv", sep = "/"),
          row.names = FALSE)


# Summary

# The dataset "230430_ord_basin_hydrogeom_yrb_wrb.csv" had two modifications:

# 1. Filtering out artificial channels and features (4420 datapoints)

# 2. Removal of catchment/watershed area data with zero values (194 datapoints)



###############################################################################
# Surface area scaling

stream_sa_dat <- phys_dat_mod2 %>% 
  select(basin,
         comid,
         tocomid,
         ctch_area_km2,
         wshd_area_km2,
         mean_ann_flow_m3s,
         reach_type,
         reach_length_km,
         bnkfll_width_m) %>% 
  mutate(stream_area_m2 = reach_length_km*1000*bnkfll_width_m) %>% 
  mutate(wilk_bnkfll_width_m = if_else(wshd_area_km2<5,
                                       2.18*wshd_area_km2^0.191,
                                       if_else(wshd_area_km2<336,
                                               1.41*wshd_area_km2^0.462,
                                               if_else(wshd_area_km2>336,
                                                       7.18*wshd_area_km2^0.183,NA)))) %>% 
  mutate(wilk_stream_area_m2 = reach_length_km*1000*wilk_bnkfll_width_m)


accm_dat <- stream_sa_dat %>% 
  group_by(basin) %>% 
  select(comid,
         tocomid,
         basin,
         wshd_area_km2,
         mean_ann_flow_m3s,
         reach_type,
         stream_area_m2,
         reach_length_km,
         ctch_area_km2,
         wilk_stream_area_m2,
         bnkfll_width_m) %>% 
  mutate(across(stream_area_m2:bnkfll_width_m, ~ calculate_arbolate_sum(data.frame(ID = comid,
                                                                                toID = tocomid,
                                                                                length = .x))) %>% 
           set_names(paste0("accm_", names(select(., stream_area_m2:bnkfll_width_m))))) 


p <- ggplot(data = accm_dat,
            aes(x = wshd_area_km2,
                y = accm_ctch_area_km2,
                color = reach_type))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(reach_type~basin,ncol = 3)
p

p <- ggplot(data = accm_dat,
            aes(x = accm_ctch_area_km2,
                y = accm_wilk_stream_area_m2,
                color = reach_type))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(reach_type~basin,ncol = 3)
p


acm_ntwk_dat <- ntwk_dat %>% 
  filter(is.na(logtotco2g_m2_day)==FALSE) %>% 
  filter(reach_type!="CanalDitch" & 
           reach_type!="Connector") %>% 
  filter(reach_type!="Pipeline") %>% 
  filter(reach_type!="CanalDitch" & 
           reach_type!="Connector") %>% 
  filter(reach_type!="Pipeline") %>% 
  mutate(stream_area_m2 = reach_length_km*1000*bnkfll_width_m) %>% 
  mutate(tot_co2g_day = logtotco2g_m2_day^10 *stream_area_m2) %>% 
  select(comid,
         tocomid,
         basin,
         wshd_area_km2,
         mean_ann_flow_m3s,
         reach_type,
         stream_area_m2,
         tot_co2g_day,
         reach_length_km,
         ctch_area_km2,
         bnkfll_width_m) %>% 
  mutate(across(stream_area_m2:bnkfll_width_m, ~ calculate_arbolate_sum(data.frame(ID = comid,
                                                                                   toID = tocomid,
                                                                                   length = .x))) %>% 
           set_names(paste0("accm_", names(select(., stream_area_m2:bnkfll_width_m))))) 


p <- ggplot(data = acm_ntwk_dat,
            aes(x = wshd_area_km2,
                y = accm_tot_co2g_day/wshd_area_km2,
                color = reach_type))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(~basin,ncol = 2)
p


