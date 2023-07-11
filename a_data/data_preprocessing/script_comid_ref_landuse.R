# Land use data set

# COMIDs and land use data

# We'd like to extract the COMIDs from the USGS database directly
# Willamette River Basin is # 170900 and Yakima is 170300

# We start by searching through downloaded datasets for variables of interest, particularly
# a unique source of COMIDs

# The first file on the list is e2nhdplusv2_us.csv which contains regions, COMIDs, 
# Stream Order,FromNode, ToNode, Hydroseq, Total Length (km), Total Area,
# Cumulative Area, Slope, Catchment Area, and HUC8 levels embedded
# in the REACHCODE

# Let's start by loading a basic library through the package librarian.

librarian::shelf(nhdplusTools,
                 nhdR,
                 tidyverse,
                 readr,
                 leaflet,
                 stringr,
                 utils)

## Let's also define our path for local import

inp_dat <- "./nhdpv2_files"

## and a path for local export of the pre-processed raw data

out_dat <- "../1-swf-knowledge.base/assets/data/raw"

# Loading e2nhdplusv2_us_csv

require(readr)
e2nhdp <- read_csv(paste(inp_dat,"e2nhdplusv2_us_csv.zip",sep = '/'), 
                   show_col_types = FALSE)


# We are going to filter this dataset to extract information about both the
# Willamette and Yakima River Basins

e2nhdp$HUC_4 <- as.integer(substr(e2nhdp$REACHCODE,1,4))

e2nhdp_pnw <- filter(e2nhdp,HUC_4==1709|HUC_4==1703)

write.csv(e2nhdp_pnw,paste(out_dat,"230410_hydro_info_pnw.csv", sep = "/"),
          row.names = FALSE)
  
pnw_wsds0 <- select(e2nhdp_pnw,
                   ComID,
                   FL_GNIS_Na,
                   LENGTHKM,
                   REACHCODE,
                   StreamOrde,
                   FromNode,
                   ToNode,
                   Hydroseq,
                   TotLngthKm,
                   IncAreaKm2,
                   CatAreaKm2,
                   TotAreaKM2,
                   CumAreaKm2,
                   SLOPE,
                   HUC_4)
summary(pnw_wsds0)

# Our summary indicates that we have a total of 2873 NA values. We will remove these
# values before continuing with the analysis. However, we need to remove the NAs from
# a specific set of columns, otherwise, there is data lost in the process. 

# We are going to create two "dummy" data sets, that we will merge after clean up. 
# One of which will be or x data set and the other will be cleaned of NAs.

pnw_wsds_i <- select(pnw_wsds0,
                     ComID,
                     FL_GNIS_Na,
                     LENGTHKM,
                     REACHCODE)
pnw_wsds_c <- na.omit(pnw_wsds0[,c(1,5:ncol(pnw_wsds0))])

pnw_wsds <- merge(pnw_wsds_i,pnw_wsds_c,by="ComID")


# Renaming variables to all-lowercase

pnw_wsds <-  rename(all_of(pnw_wsds),
                    comid = ComID,
                    stream_name = FL_GNIS_Na,
                    flowline_length_km = LENGTHKM,
                    reach_code = REACHCODE,
                    stream_order = StreamOrde,
                    from_node = FromNode,
                    to_node = ToNode,
                    hydroseq = Hydroseq,
                    tot_flowline_length_km = TotLngthKm,
                    cat_sink_area_km2 = CatAreaKm2,
                    tot_ups_area_km2 = TotAreaKM2,
                    cum_div_area_km2 = CumAreaKm2,
                    inc_flowline_area_km2 = IncAreaKm2,
                    slope = SLOPE,
                    huc_4 = HUC_4)

# Loading the land use data at the catchment level (i.e. local) and at the watershed
# level

lnd_use_catch <- read_delim(paste(inp_dat,"NLCD01_CAT_CONUS.txt",sep = '/'),
                            delim = ',',show_col_types = FALSE)

lnd_use_wshd <- read_delim(paste(inp_dat,"NLCD01_TOT_CONUS.txt",sep = '/'),
                            delim = ',',show_col_types = FALSE)

# Removing the no-data columns
lnd_use_catch <- select(lnd_use_catch,-CAT_NODATA)
lnd_use_wshd <- select(lnd_use_wshd,-TOT_NODATA)

# Merging with our data set for the Yakima and Willamette 
pnw_w_lndi <- merge(pnw_wsds_i,lnd_use_catch,
                    by.x = "ComID", 
                    by.y = "COMID",
                    all.x = TRUE)

pnw_w_lnd0 <- merge(pnw_wsds,lnd_use_catch, 
                    by.x = "comid", 
                    by.y = "COMID",
                    all.x = TRUE)

pnw_w_lnd <- merge(pnw_w_lnd0,lnd_use_wshd, 
                   by.x = "comid", 
                   by.y = "COMID",
                   all.x = TRUE)

#Renaming columns according to NLCD Land Cover Classification Legend

colnames(pnw_w_lnd) <- gsub("CAT","cat",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("TOT","wsd",colnames(pnw_w_lnd))

colnames(pnw_w_lnd) <- gsub("NLCD01_11","water",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_12","snow",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_21","developed_op",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_22","developed_lw",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_23","developed_md",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_24","developed_hg",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_31","barren",colnames(pnw_w_lnd))

colnames(pnw_w_lnd) <- gsub("NLCD01_41","forest_dcd",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_42","forest_evg",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_43","forest_mxd",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_52","shrub",colnames(pnw_w_lnd))

colnames(pnw_w_lnd) <- gsub("NLCD01_71","grass",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_81","pasture",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_82","crops",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_90","wetland_wood",colnames(pnw_w_lnd))
colnames(pnw_w_lnd) <- gsub("NLCD01_95","wetland_herb",colnames(pnw_w_lnd))

readr::write_csv(pnw_w_lnd,paste(out_dat,"230321_pnw_2001_landcover.csv",sep = '/'))






