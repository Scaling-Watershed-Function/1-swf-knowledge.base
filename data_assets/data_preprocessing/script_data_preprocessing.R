###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima and Willamette River 
# Basins
# DATA PRE-PROCESSING
###############################################################################
# RESPIRATION DATA
###############################################################################

#By : Francisco Guerrero (Modified from Kyongho Son)
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

librarian::shelf(sp,
                 sf,
                 leaflet,
                 utils,
                 zen4R,
                 tidyverse,
                 nhdplusTools)

#Note: Verify that the sf package was successfully installed. Otherwise try install.packages("sf)
#and answer "no" to the following prompt:

#Do you want to install from sources the package which needs compilation? (Yes/no/cancel) 

set.seed(2703)

#########################################################################################################
# Import / Export of assets

# External Import
#TBD

# Local import - Son et al., model input data
model_data <- "model_inputs/model_data" 
model_shapes <- "model_inputs/model_shapes" 

# Local import and export paths to data files for further analysis
raw_data <- "../raw" 
processed_data <- "../processed"

############ loading NHDPlus Data (Shapefiles) ##################################################

# For compatibility with the latest NHDPlus database, the coordinate system for the 
# original shapefiles has to be modified from NAD83 to WGS84. We do so by using the 
# st_transform() function from the package sf

# Shapefiles
# Yakima River Basin
nhd_yrb_stream<-sf::st_transform(st_read(paste(model_shapes,"nhd_CR_stream_sub9.shp",sep = "/")),4326)
# Willamette River Basin
nhd_wrb_stream<-sf::st_transform(st_read(paste(model_shapes,"nhd_CR_stream_sub8.shp",sep = "/")),4326)

# The willamette file contains two extra variables: Name and HUC4. Adding these columns to 
# the yakima file

nhd_yrb_stream <-  nhd_yrb_stream %>% 
  mutate(Name = "Yakima",
         HUC4 = 1703) 

# First, I will combine the shapefiles:
nhd_pnw <- rbind(nhd_wrb_stream,
                 nhd_yrb_stream) %>% 
  rename(comid = COMID,
         id = ID,
         name = Name,
         huc_4 = HUC4) 

# Let's take a look at this data on a map

# Using leaflet to look into maps:

# With the function 'addTiles' you get a base map by default that contain streets names
# and major features. If you want  a different base map, here is hwo to do it:

# A tutorial video: https://www.youtube.com/watch?v=sX9jwUAXShs

# OR follow this instructions:  
# Go to the website: https://leaflet-extras.github.io/leaflet-providers/preview/
# Look through the gallery and pick the base map you'd like to add
# Copy the name that appears below "Provider names for leaflet-providers.js" on the top
# dialogue box
# Call the function 'addProviderTiles' and paste the name chosen between ""
# In the chunk below I chose "Esri.WorldImagery"

leaflet(nhd_pnw) %>% 
  addPolylines(weight = 2) %>%  
  addProviderTiles("Esri.WorldImagery")

# Exploring connectivity across these networks. We need to append the "tocomid" values to 
# use the calculate_arbolate_sum from the nhdplusTools package.

# We will use the blgt23_dat file to extract the "tocomid" values:


blgt23_dat <- read_csv(paste(raw_data,"230620_enhanced_nhdp_2_swf.csv", sep = '/'),
                       show_col_types = FALSE)

nhd_pnw_tocomid <- nhd_pnw %>% 
  merge(.,
        blgt23_dat %>%
          select(comid,tocomid),
        by = "comid",
        all.x = TRUE)

# There are 2918 datapoints in the shape file that are not included in the enhanced
# version of the nhdplus 2.1

# Let's take a look at these data points

leaflet(nhd_pnw_tocomid) %>% 
  addPolylines(weight = 2) %>%  
  # addPolylines(data =filter(nhd_pnw_tocomid,is.na(tocomid)==TRUE),
  #              weight = 2,
  #              opacity = 1,
  #              color = "magenta") %>% 
  addProviderTiles("Esri.WorldImagery")

# A visual inspection of these NA values indicates that most of them correspond to 
# artificial canals and ditches that are outside of the prediction scope for the model. 

# So, we should be able to remove these data and calculate an arbolate sum to verify 
# connectivity


nhd_pnw_clean <- nhd_pnw_tocomid %>% 
  filter(is.na(tocomid)==FALSE) %>% 
  mutate(reach_index = 1,
         basin = if_else(huc_4 == "1703",
                         "yakima",
                         "willamette"))%>% 
  group_by(basin) %>% 
  mutate(accm_reach_index = calculate_arbolate_sum(data.frame(ID = comid,
                                                              toID = tocomid,
                                                              length = reach_index)),
         connectivity = (max(accm_reach_index)/sum(reach_index))*100)
  
# Connectivity indexes are quite low: 25.48% for Willamette and 17.80 for Yakima. 

#Let's check for connectivity in the blgt23_dat

blgt23_dat <-  blgt23_dat %>% 
  mutate(reach_index = 1,
         basin = if_else(huc_4 == "1703",
                         "yakima",
                         if_else(huc_4 == "1709",
                                 "willamette",
                                 "ipswich")))%>% 
  group_by(basin) %>% 
  mutate(accm_reach_index = calculate_arbolate_sum(data.frame(ID = comid,
                                                              toID = tocomid,
                                                              length = reach_index)),
         connectivity = (max(accm_reach_index)/sum(reach_index))*100)


# Now, let's take a look at data gaps in terms of model predictors, as well as
# model predictions

son_rcm_predict <- read_csv(paste(raw_data,"230406_son_etal_22_results_zen.csv",sep = '/'),
                   show_col_types = FALSE)

# The dataset son_rcm_predict contains both values for predicted and predictor variables.
# These data also include annual, spring, and summer time scales. We will use "logtotco2g_m2_day"
# to map gaps

nhd_pnw <- nhd_pnw %>% 
  merge(.,
        son_rcm_predict %>% 
          filter(time_type == "annual") %>% 
          select(comid,
                 logtotco2g_m2_day),
        by = "comid",
        all.x = TRUE)

# Plotting the data using leaflet

leaflet(nhd_pnw) %>% 
  addPolylines(data =filter(nhd_pnw,is.na(logtotco2g_m2_day)==FALSE),
               weight = 2,
               opacity = 1,
               color = "blue") %>%
  # addPolylines(data =filter(nhd_pnw,is.na(logtotco2g_m2_day)),
  #              weight = 2,
  #              opacity = 1,
  #              color = "magenta") %>%   
  addProviderTiles("Esri.WorldImagery")













# National Stream Internet
nis_pnw_stream<-sf::st_transform(st_read(paste(input_data,"nsi_shapes/Flowline_PN17_NSI.shp",sep = "/")),4326)

nis_pnw_stream<-st_read(paste(input_data,"nsi_shapes/Flowline_PN17_NSI.shp",sep = "/"))






nis_ywrb_streams <- nis_pnw_stream %>% 
  mutate(huc_4 = substr(REACHCODE,1,4))%>% 
  filter(huc_4=="1703" | huc_4 == "1709")

leaflet(nis_ywrb_streams) %>% 
  addPolylines(weight = 2)%>%  
  addProviderTiles("Esri.WorldImagery")


# Merging both shapefiles with Blodgett (2023) dataset

bldg_dat0 <- read_csv(paste(raw_data,"230620_enhanced_nhdp_2_swf.csv", sep = '/'),
                     show_col_types = FALSE)

bldg_dat <- bldg_dat0 %>% 
  filter(huc_4!="0107")

# Let's run a test on cumulative function across the bldg_dat

summary(bldg_dat)

bldg_dat <- bldg_dat %>% 
  group_by(huc_4) %>% 
  mutate(test = 1) %>% 
  mutate(accm_test = calculate_arbolate_sum(data.frame(ID = comid,
                                                           toID = tocomid,
                                                           length = test)))

p <- ggplot(data = bldg_dat,
            aes(x = streamorde,
                y = accm_test,
                color = as.factor(streamorde)))+
  geom_point()+
  scale_x_continuous(breaks = c(seq(1,9,1)))+
  scale_y_log10()+
  facet_wrap(~as.factor(huc_4))+
  theme(legend.position = "none")
p


# We observe inconsistent results for reaches order 9 and 1 reach order 5 with
# lower than expected

filter(bldg_dat, streamorde == 5 & accm_test < 10)
# COMID = 24241627

unique(filter(bldg_dat, streamorde == 9)$comid)

order_9_comids <- c("23735693",
                    "23735695",
                    "23735697",
                    "23735699",
                    "23735701",
                    "23735703",
                    "23735705",
                    "23735707",
                    "23735709",
                    "23735711",
                    "23735713",
                    "23735715",
                    "23735717",
                    "23735719",
                    "23735721",
                    "23735723",
                    "24520498")

bldg_dat_cln <- bldg_dat %>% 
  filter(streamorde !=9) %>% 
  filter(comid != 24241627)


bldg_dat_cln <- bldg_dat_cln %>% 
  group_by(huc_4) %>% 
  mutate(connectivity = round((max(accm_test)/sum(test))*100,digits = 1))


p1 <- ggplot(data = bldg_dat_cln,
             aes(x = huc_4,
                 y = connectivity))+
  geom_point()
p1

# Connectivity in Blodget data is pretty high for both watersheds: 97.1 for Yakima
# and 97.5 for Willamette


# Connectivity analysis Son et al, 2022 dataset

sonk_dat <- read_csv(paste(raw_data,"230406_son_etal_22_results_zen.csv", sep = '/'),
                      show_col_types = FALSE) %>% filter(.,time_type == "annual")

summary(sonk_dat)

# adding tocomid to son et al., 2022 data

sonk_merge_dat <- sonk_dat %>% 
  merge(.,
        bldg_dat %>% 
          select(comid,tocomid),
        by = "comid",
        all.x = TRUE)

sonk_merge_dat <- sonk_merge_dat %>% 
  group_by(huc_4.x) %>% 
  mutate(test = 1) %>% 
  mutate(accm_test = calculate_arbolate_sum(data.frame(ID = comid,
                                                       toID = tocomid,
                                                       length = test))) %>% 
  mutate(connectivity = round((max(accm_test)/sum(test))*100,digits = 1))


summary(sonk_merge_dat)

enh_nhd_pnw <- nhd_pnw %>% 
  merge(.,
        bldg_dat_cln,
        by = "comid",
        all.x = TRUE)


enh_nis_pnw <- bldg_dat_cln %>% 
  merge(.,
        nis_ywrb_streams %>% 
          rename(comid = COMID),
        by = "comid",
        all.x = TRUE)

summary(enh_nhd_pnw)

summary(enh_nis_pnw)

# Comparing missing stream order data between the two new shapefiles

leaflet(enh_nhd_pnw) %>% 
  addPolylines(weight = 2)%>% 
  addPolylines(data =filter(enh_nhd_pnw,is.na(streamorde)),
               weight = 5,
               opacity = 1,
               color = "magenta") %>%
  addProviderTiles("Esri.WorldImagery")


leaflet(enh_nis_pnw) %>% 
  addPolylines(weight = 2)%>%  
  addPolylines(data =filter(enh_nis_pnw,is.na(streamorde)),
               weight = 5,
               opacity = 1,
               color = "magenta") %>%
  addProviderTiles("Esri.WorldImagery")

# Estimating network connectivity with a simple test using arbolatesum
# from nhdplusTools package

enh_nhd_pnw_t <-  enh_nhd_pnw %>% 
  mutate(test_col = 1) %>% 
  mutate(accm_test_col = calculate_arbolate_sum(data.frame(ID = comid,
                                                           toID = tocomid,
                                                           length = test_col)))
  

enh_nis_pnw_t <-  enh_nis_pnw %>% 
  # filter(tocomid !=0) %>% 
  mutate(test_col = 1) %>% 
  mutate(accm_test_col = calculate_arbolate_sum(data.frame(ID = comid,
                                                           toID = tocomid,
                                                           length = lengthkm)))

filter(enh_nis_pnw, tocomid == 0)

summary











# csv files with physical hydrological data
nhdp_2_pnw_raw <- read_csv(paste(raw_data,"230410_hydro_info_pnw.csv", sep = "/"),
                           show_col_types = FALSE)

# csv files with land use info
pnw_lnd <- read_csv(paste(raw_data,"230321_pnw_2001_landcover.csv", sep = "/"),
                    show_col_types = FALSE)

# Let's create a master data file with all variables

# We will use the file pnw_lnd as references as it contains all the comids with 
# land use data available.

# First, I will combine the shapefiles:

nhd_pnw <- rbind(nhd_wrb_stream,
                 nhd_yrb_stream) %>% 
  rename(comid = COMID,
         id = ID,
         name = Name,
         huc_4 = HUC4) %>% 
  left_join(.,
            pnw_lnd %>% 
              select(comid,
                     stream_order),
            by = "comid")

# If I do right join above to get coordinates for all the points with land cover
# data, the resulting data set has 536 comids with missing coordinates.

# Let's take a look at this data on a map


# Using leaflet to look into maps:

# With the function 'addTiles' you get a base map by default that contain streets names
# and major features. If you want  a different base map, here is hwo to do it:

# A tutorial video: https://www.youtube.com/watch?v=sX9jwUAXShs

# OR follow this instructions:  
# Go to the website: https://leaflet-extras.github.io/leaflet-providers/preview/
# Look through the gallery and pick the base map you'd like to add
# Copy the name that appears below "Provider names for leaflet-providers.js" on the top
# dialogue box
# Call the function 'addProviderTiles' and paste the name chosen between ""
# In the chunk below I chose "Esri.WorldImagery"


leaflet(nhd_pnw) %>% 
  addPolylines(weight = 2) %>%
  addPolylines(data =filter(nhd_pnw,is.na(stream_order)),
               weight = 2,
               opacity = 1,
               color = "magenta") %>%
  #addProviderTiles("Jawg.Dark")
  addProviderTiles("Esri.WorldImagery")


# National Stream Internet
nis_pnw_stream<-sf::st_transform(st_read(paste(input_data,"nsi_shapes/Flowline_PN17_NSI.shp",sep = "/")),4326)

nis_ywrb_streams <- nis_pnw_stream %>% 
  mutate(huc_4 = substr(REACHCODE,1,4))%>% 
  filter(huc_4=="1703" | huc_4 == "1709")

leaflet(nis_ywrb_streams) %>% 
  addPolylines(weight = 2)

st_write(nis_ywrb_streams,paste(raw_data,"nis_yrb_streams.shp",sep = '/'))

#Merging the two datasets to identify gaps. We will use stream order as an 
# indicator variable

ywrb_stream_dat <- nis_ywrb_streams %>% 
  merge(.,
        nhdp_2_pnw_raw %>% 
          select(ComID,
                 StreamOrde),
        by.x = "COMID",
        by.y = "ComID",
        all.x = TRUE) %>% 
  merge(.,
        pnw_lnd,
        by.x ="COMID",
        by.y = "comid",
        all.x= TRUE)

summary(ywrb_stream_dat)

leaflet(ywrb_stream_dat) %>% 
  addPolylines(weight = 6) %>%
  addPolylines(data =filter(ywrb_stream_dat,wsd_pasture>70),
               weight = 6,
               opacity = 1,
               color = "magenta") %>%
  #addProviderTiles("Jawg.Dark")
  addProviderTiles("Esri.WorldImagery")

#Merging with blodgett data for tocomid fields and arbolatesu calculations

bldg_dat <- read_csv(paste(raw_data,"230620_enhanced_nhdp_2_swf.csv", sep = '/'),
                     show_col_types = FALSE)

pnw_ntwk_dat <- ywrb_stream_dat %>% 
  merge(.,
        bldg_dat,
        by.x = "COMID",
        by.y = "comid",
        all.x = TRUE)
  
leaflet(pnw_ntwk_dat) %>% 
  addPolylines(weight = 6) %>%
  addPolylines(data =filter(pnw_ntwk_dat,FTYPE!="StreamRiver"),
               weight = 6,
               opacity = 1,
               color = "magenta") %>%
  #addProviderTiles("Jawg.Dark")
  addProviderTiles("Esri.WorldImagery")

p <- ggplot(pnw_ntwk_dat,
            aes(x=TotDASqKM,
                y=tot_flowline_length_km,
                color=as.factor(huc_4)))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~as.factor(huc_4),ncol=2)
p

  
################################################################################
# Verify which shapefiles should be the best for matching the network before
# proceeding. DATE: 04/13/2023
################################################################################


# The difference is exactly 2,873. So the network data in pnw_lnd correspond
# to the filtered version of nhdp after removing comids corresponding to irrigation
# channels. There are no additional NAs in this dataset so far, except for locations
# with tot_flowline_length = 0

# Let's check how many of those values we have:

zero_cat_area <- filter(pnw_lnd,inc_flowline_area_km2 == 0)

summary(zero_cat_area)

# There are at least two columns filled with zeroes in the dataset: inc_flowline_area_km2, and 
# cat_sink_area_km2

# Let's examine some potential correlations that could allow to correct these data:

pnw_correlations <- pnw_lnd %>% 
  filter(inc_flowline_area_km2 > 0) %>% 
  select(huc_4,
         flowline_length_km,
         tot_flowline_length_km,
         inc_flowline_area_km2,
         slope) %>% 
  mutate(log_length = log(flowline_length_km,10),
         log_tot_length = log(tot_flowline_length_km,10),
         log_area = log(inc_flowline_area_km2,10),
         log_slope = log(slope,10)) 

pairs(pnw_correlations[,c(6:9)])


# There is a strong correlation between log_area and log_length. Let's look if that
# correlation is different between watersheds

plot_pnw_corr <- pnw_correlations %>% 
  ggplot(aes(log_area,log_length,color = as.factor(huc_4)))+
  # geom_point(alpha = 0.5)+
  geom_smooth(method = "lm")#+
 # facet_wrap(~as.factor(huc_4))
plot_pnw_corr

# Lets first build a prediction model

area_mod <- lm(log_area~log_length+log_slope,data = pnw_correlations)
summary(area_mod)

# The adjusted R-squared is 0.82 with p < 0.0001

# We will start by correcting the catchment area values:

zero_cat_area_corrected <- zero_cat_area %>% 
  mutate(inc_flowline_area_km2 = 10^((1.351645*log(flowline_length_km,10))-(0.024426*log(slope,10))-0.108241),
         cat_sink_area_km2 = inc_flowline_area_km2,
         tot_ups_area_km2 = tot_ups_area_km2 + cat_sink_area_km2,
         cum_div_area_km2 = tot_ups_area_km2)

no_zero_cat_area <- filter(pnw_lnd,inc_flowline_area_km2 > 0) 

summary(no_zero_cat_area)

# The relationship is virtually the same across plots, so we will correct missing 
# values within the zero_tot_flow

pnw_lnd_corrected <- rbind(no_zero_cat_area,zero_cat_area_corrected)




nhd_pnw_corrected0 <- merge(pnw_lnd_corrected,nhd_wrb_stream,all.x=TRUE)

# Checking the network in the map

# Willamette River Basin

nhd_wrb_map <- nhd_wrb_stream %>% 
  mutate(comid = COMID) %>% 
  left_join(.,
            pnw_lnd_corrected %>% 
              filter(huc_4 == 1709) %>% 
              dplyr::select(comid,
                            flowline_length_km,
                            stream_order),
            by = "comid")  

  leaflet(nhd_wrb_map) %>% 
  addPolylines(weight = 3) %>% 
  addPolylines(data = filter(nhd_wrb_map,stream_order == 1),
               color = "magenta",
               opacity = 1,
               weight = 3) %>% 
  addProviderTiles("Esri.WorldImagery")


# Yakima River Basin

nhd_yrb_map <- nhd_yrb_stream %>% 
  mutate(comid = COMID) %>% 
  left_join(.,
            pnw_lnd_corrected %>% 
              filter(huc_4 == 1703) %>% 
              dplyr::select(comid,
                            flowline_length_km,
                            stream_order),
            by = "comid") 

leaflet(nhd_yrb_map) %>% 
  addPolylines(weight = 3) %>% 
  addPolylines(data = filter(nhd_yrb_map,stream_order == 1),
               color = "magenta",
               opacity = 1,
               weight = 3) %>% 
  addProviderTiles("Esri.WorldImagery")  


# Downloading Model outputs Kyongho Son et al., 2022

# Create a temporary directory to store the heavy data from Zenodo
temp_dir <- tempdir()

# Specify the target file name
target_files <- c("nhd_CR_stream_annual_resp_inputs_outputs.rda",
                  "nhd_CR_stream_spring_resp_inputs_outputs.rda",
                  "nhd_CR_stream_summer_resp_inputs_outputs.rda")

# # Generate a unique file name for the temporary file
# temp_files <- file.path(temp_dir, paste0(target_files))


# Download the RDA file from Zenodo to the temporary directory
download_zenodo(path = temp_dir,
                doi = "10.5281/zenodo.6954107",
                files = target_files)

# Load the data from the RDA file
# Read in the files and assign them to variables
annual_dat <- readRDS(file.path(temp_dir, paste0(target_files[1])))
spring_dat <- readRDS(file.path(temp_dir, paste0(target_files[2])))
summer_dat <- readRDS(file.path(temp_dir, paste0(target_files[3])))

# Flagging each data set according to their temporal window
annual_dat$time_type <- "annual"
spring_dat$time_type <- "spring"
summer_dat$time_type <- "summer"

# Unifying column names:
un_colnames <- c("comid","pred_doc","pred_do",
                 "no3_conc_mg_l","logRT_total_hz_s","logq_hz_total_m_s",
                 "logtotco2_o2g_m2_day","logtotco2_ang_m2_day","logtotco2g_m2_day",
                 "time_type" )

colnames(annual_dat) <- un_colnames
colnames(spring_dat) <- un_colnames
colnames(summer_dat) <- un_colnames

resp_annual <- merge(pnw_lnd_corrected,
                     annual_dat,
                     by="comid",
                     all.x = TRUE)

resp_spring <- merge(pnw_lnd_corrected,
                     spring_dat,
                     by = "comid",
                     all.x = TRUE)

resp_summer <- merge(pnw_lnd_corrected,
                     summer_dat,
                     by = "comid",
                     all.x = TRUE)

resp_dat <- rbind(resp_annual,resp_spring,resp_summer)

write.csv(resp_dat,file=paste(raw_data,"230406_son_etal_22_results_zen.csv",sep = '/'),
          row.names = FALSE)



# We have a total of 3391 NAs from model predictions (per time type)

# Let's now take a look at the locations with missing predictions:

# Willamette River Basin

bgc_wrb_map <- nhd_wrb_map %>% 
  mutate(comid = COMID) %>%
  filter(is.na(stream_order)==FALSE) %>% 
  left_join(.,
            resp_dat %>% 
              filter(huc_4 == 1709 & time_type == "annual") %>% 
              # filter(time_type == "annual") %>% 
              dplyr::select(comid,
                            pred_doc,
                            stream_order),
            by = "comid")  

leaflet(bgc_wrb_map) %>% 
  addPolylines(data = filter(bgc_wrb_map,is.na(pred_doc)==FALSE),
               opacity = 1,
               weight = 3) %>% 
  addPolylines(data = filter(bgc_wrb_map,is.na(pred_doc)),
               color = "magenta",
               opacity = 1,
               weight = 3) %>% 
  addProviderTiles("Esri.WorldImagery")


# Yakima River Basin

bgc_yrb_map <- nhd_yrb_map %>% 
  mutate(comid = COMID) %>% 
  filter(is.na(stream_order)==FALSE) %>% 
  left_join(.,
            resp_dat %>% 
              filter(huc_4 == 1703 & time_type == "annual") %>% 
              dplyr::select(comid,
                            pred_doc,
                            stream_order),
            by = "comid") 

leaflet(bgc_yrb_map) %>% 
  addPolylines(data = filter(bgc_yrb_map,is.na(pred_doc)==FALSE),
               opacity = 1,
               weight = 3) %>% 
  addPolylines(data = filter(bgc_yrb_map,is.na(pred_doc)),
               color = "magenta",
               opacity = 1,
               weight = 3) %>% 
  addProviderTiles("Esri.WorldImagery")

# After removing NAs

leaflet(bgc_yrb_map) %>% 
  addPolylines(data = filter(bgc_yrb_map,is.na(pred_doc)==FALSE),
               opacity = 1,
               weight = 3) %>% 
  addProviderTiles("Esri.WorldImagery")

# In both cases, removing the NAs generated by missing predictions, fragments the
# fluvial network

# Let's look at some potential correlations that could help us to fill the gaps

doc_pairs <- pnw_annual_bgc_dat %>% 
  select(pred_doc,
         tot_flowline_length_km,
         tot_ups_area_km2,
         wsd_forest_evg,
         wsd_pasture) %>% 
  mutate(log_doc = log(pred_doc,10),
         log_length = log(tot_flowline_length_km,10),
         log_area = log(tot_ups_area_km2,10),
         log_forest = log((wsd_forest_evg+1),10),
         log_pasture = log((wsd_pasture+1),10))
           
pairs(na.omit(doc_pairs)[,c(6:10)])


rt_pairs <- pnw_annual_bgc_dat %>% 
  select(logRT_total_hz_s,
         tot_flowline_length_km,
         tot_ups_area_km2,
         cat_sink_area_km2,
         stream_order,
         slope) %>% 
  mutate(log_length = log(tot_flowline_length_km,10),
         log_area = log(tot_ups_area_km2,10),
         log_cat_area = log(cat_sink_area_km2,10),
         log_slope = log((slope+1),10))

pairs(na.omit(rt_pairs)[,c(1,5,6:9)])

# Let's try a regression model

rt_mod <- lm(logRT_total_hz_s~log_length+log_area+log_cat_area+log_slope+stream_order,
           data = na.omit(rt_pairs))
summary(rt_mod)


# Let's now merge this data with the land use data:

lnd_dat <- read_csv(file=paste(processed_data,"230324_inf_cont_lnd.csv",sep = "/"),
                    show_col_types = FALSE)

main_hbgc_dat <- merge(resp_dat,lnd_dat,by="comid")


write.csv(main_hbgc_dat,"../1-swf-knowledge.base/assets/data/processed/230406_hbgc_pnw_land.csv",row.names = FALSE)




















# We will now load all the substrate concentration data for the Columbia River Basin to then extract
# values for both the YRB and WRB

stream_annDO<-read_csv(paste(assets_data,"nhd_CR_stream_annual_DO.csv",sep="/"),show_col_types = FALSE)
stream_annno3<-read.csv(paste(assets_data,"nhd_CR_stream_no3.csv",sep="/"),header=T,sep=',',skip=0)
stream_annDOC<-read_csv(paste(assets_data,"nhd_CR_stream_annual_DOC.csv",sep="/"),show_col_types = FALSE)
stream_nexss<-read_csv(paste(assets_data,"nexss_inputs.csv",sep="/"),show_col_types = FALSE)

# We need to rename the COMID in nexss:
stream_nexss <- rename(stream_nexss,
                       COMID = comid_nhd)

## merging the model input data with NHDPLUS stream reach shapefiles
# from : https://www.statology.org/merge-multiple-data-frames-in-r/

df_list <- list(nhd_ywb_stream,
                stream_annDO,
                stream_annDOC,
                stream_annno3,
                stream_nexss)

nhd_ywb_resp <- df_list %>% reduce(full_join, by = "COMID")

nhd_ywb_resp <- rename(nhd_ywb_resp,
                       id = ID,
                       name = Name,
                       stream_do_mg_l = `Stream DO`,
                       stream_doc_mg_l = `Stream DOC`)


# Saving data set as processed data

write_csv(nhd_ywb_resp,file=paste(assets_processed,"230313_wlm_ykm_stream_resp_dat.csv",sep = "/"))


# figures<-"ESS-DIVE/figures"

# jpeg("Stream DOC_YRB_annual_DOC.jpeg", width = 6, height = 6, units = 'in', res = 300) 
# par(cex.main=1.5,cex.axis=1.5) 
# plot(nhd_yrb_stream_resp[,"Stream DOC"],main="", key.pos = 1, key.width = lcm(2), key.length = 1.0,breaks = "fisher",pal=brewer.reds(10),reset=FALSE)
# # plot(st_geometry(nhd_yrb_stream_resp),add=T)
# 
# title("(a) Stream DOC (mg/l)",line=-24, adj = 0.2)
# 
# 
# dev.off()