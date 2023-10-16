################################################################################
# SCALING WATERSHED FUNCTION: PHYSICAL DATA QA/QC
################################################################################

#Author: Francisco J. Guerrero
gc()
# Loading required packages: 

librarian::shelf(tidyverse,
                 utils,
                 leaflet,
                 sp,
                 sf,
                 nhdplusTools,
                 GGally,
                 htmltools,
                 foreign,
                 data.table)

# Local Import-Export
shapes_data <- "../nsi_ssn_network/data"
local_data <- "./data"
export_data <- "/Users/guerrero-fj/Documents/GitHub/1_scaling_watershed_function/2-swf-analytical.engine/scaling_analysis_willamette_yakima_rcm_23/data"



pnw_rivers_dat <- st_transform(st_read(paste(shapes_data,
                                             "nsi_network_ywrb.shp",sep = "/")),4326)

rcm_23_model_output_dat <- read_csv(paste(local_data,"rcm_23_model_output_data.csv", sep = '/'),
                                    show_col_types = FALSE)


# Let's take a look at the data

summary(rcm_23_model_output_dat)

#Let's also create a dataset that allows for mapping some of these variables:

pnw_rivers_map <- pnw_rivers_dat %>% 
  select(COMID) %>% 
  rename(comid = COMID) %>% 
  merge(.,
        rcm_23_model_output_dat,
        by = "comid", 
        all.x = TRUE)


#Identifying first order streams with watershed areas larger than cathcment areas

pnw_rivers_map <-  pnw_rivers_map %>% 
  mutate(area_match = if_else(stream_order==1 & wshd_area_km2 == ctch_area_km2,
                              "match",if_else(stream_order==1 & wshd_area_km2 > ctch_area_km2,"no_match",
                                              NA)))


# Stream orders rank from 1 - 7, let's take a look at the map to identify reaches
# with the maximum order

leaflet(pnw_rivers_map) %>% 
  addPolylines(weight = 0.5,
               color = "gray",
               opacity = 0.5) %>%
  # addPolylines(data = filter(pnw_rivers_map,stream_order==7),
  #              color = "#EDEF5C",
  #              opacity = 1,
  #              weight = 7) %>%
  # addPolylines(data = filter(pnw_rivers_map,stream_order==6),
  #              color = "#BEE05E",
  #              opacity = 1,
  #              weight = 6) %>%
  # addPolylines(data = filter(pnw_rivers_map,stream_order==5),
  #              color = "#78C86F",
  #              opacity = 1,
  #              weight = 5) %>% 
  # addPolylines(data = filter(pnw_rivers_map,stream_order==4),
  #              color = "#08A47F",
  #              opacity = 1,
  #              weight = 4) %>% 
  # addPolylines(data = filter(pnw_rivers_map,stream_order==3),
  #              color = "#028090",
  #              opacity = 1,
  #              weight = 3) %>% 
  # addPolylines(data = filter(pnw_rivers_map,stream_order==2),
  #              color = "#176B88",
  #              opacity = 1,
  #              weight = 2) %>% 
  # addPolylines(data = filter(pnw_rivers_map,stream_order==1),
  #              color = "#2E4F79",
  #              opacity = 1,
  #              weight = 0.5) %>% 
  addPolylines(data = filter(pnw_rivers_map,area_match=="match"),
               color = "darkorchid",
               opacity = 1,
               weight = 1.5) %>% 
  addPolylines(data = filter(pnw_rivers_map,area_match=="no_match"),
               color = "darkorange",
               opacity = 1,
               weight = 1.5) %>% 
 addProviderTiles("Esri.WorldImagery")



##########
# Variables including zero values

# Calculate the number of zeros in each column
zero_counts <- colSums(nsi_rcm_phys_dat == 0, na.rm = TRUE)

# Create a new dataframe to store the report
zero_report <- data.frame(variable = names(zero_counts), Zeros = zero_counts) %>% 
  filter(Zeros > 0)

# Print the report without row names
print(zero_report, row.names = FALSE)

# We will first remove DUP_COMID and DUP_ArSqKm that are a special feature of 
# the NSI dataset and test for connectivity

# Testing initial network connectivity

test_dat_connectivity <- nsi_rcm_phys_dat %>% 
  filter(DUP_COMID == 0) %>% 
  group_by(basin) %>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
test_dat_connectivity

