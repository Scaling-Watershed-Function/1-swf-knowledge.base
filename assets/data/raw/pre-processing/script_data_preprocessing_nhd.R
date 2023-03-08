###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima and Willamette River 
# Basins
# DATA PRE-PROCESSING
###############################################################################
# RESPIRATION DATA
###############################################################################

#By : Francisco Guerrero (Modified from Kyongho Son)
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

librarian::shelf(sp,sf,raster,rgdal,rasterVis,
                 rgeos,lattice,grid,spatstat,
                 plotKML,fasterize,egg,nhdplusTools,
                 nhdR,colorspace,stars,pals,foreign,
                 tidyverse)
set.seed(2703)

#########################################################################################################
# Import / Export of assets

# Import: Repository path to raw data
raw_data <- "https://raw.githubusercontent.com/Scaling-Watershed-Function/1-swf-knowledge.base/main/assets/data/raw/shapes/"

# Loading NHD data

# Local import
# assets_data <- "../2-swf-analytical.engine/shapes" It does not work


require(sf)
# nhd_yrb_stream <- st_read(assets_data,"nhd_CR_stream_sub8.shp") it does not work

# Apparently files have to be rigth outside in the working directory

nhd_yrb_stream <- st_read("nhd_CR_stream_sub8.shp")

tmp<-st_zm(nhd_yrb_stream)
nhd_yrb_poly<-tmp[,"COMID"]
nhd_yrb_stream<-data.frame(nhd_yrb_poly)
nhd_yrb_stream$COMID<-NULL                          

#################################################################################


############ loading NHD Data----
##model_inputs: model input folder

# Willamette

nhd_CR_stream<-st_read("nhd_CR_stream_sub8.shp")
tmp<-st_zm(nhd_CR_stream)
nhd_CR_poly<-tmp[,"COMID"]

## reading model inputs: substrate concentrations

stream_annDO<-read_csv(paste(model_inputs,"nhd_CR_stream_annual_DO.csv",sep="/"))
stream_annno3<-read.csv(paste(model_inputs,"nhd_CR_stream_no3.csv",sep="/"),header=T,sep=',',skip=0)
stream_annDOC<-read_csv(paste(model_inputs,"nhd_CR_stream_annual_DOC.csv",sep="/"))
stream_nexss<-read_csv(paste(model_inputs,"nexss_inputs.csv",sep="/"))


stream_annDO<-read_csv("nhd_CR_stream_annual_DO.csv",show_col_types = FALSE)
stream_annno3<-read.csv("nhd_CR_stream_no3.csv",header=T,sep=',',skip=0)
stream_annDOC<-read_csv("nhd_CR_stream_annual_DOC.csv",show_col_types = FALSE)
stream_nexss<-read_csv("nexss_inputs.csv",show_col_types = FALSE)

## merging the model input data with NHDPLUS stream reach shapefiles
nhd_CR_stream_resp=merge(nhd_CR_poly,stream_annDO,by="COMID")
nhd_CR_stream_resp=merge(nhd_CR_stream_resp,stream_annDOC,by="COMID")
nhd_CR_stream_resp=merge(nhd_CR_stream_resp,stream_annno3,by="COMID")
nhd_CR_stream_resp=merge(nhd_CR_stream_resp,stream_nexss,by.x="COMID",by.y="comid_nhd")


figures<-"ESS-DIVE/figures"

jpeg("Stream DOC_CRB_annual_DOC.jpeg", width = 6, height = 6, units = 'in', res = 300) 
par(cex.main=1.5,cex.axis=1.5) 
plot(nhd_CR_stream_resp[,"Stream DOC"],main="", key.pos = 1, key.width = lcm(2), key.length = 1.0,breaks = "fisher",pal=brewer.reds(10),reset=FALSE)
# plot(st_geometry(nhd_CR_stream_resp),add=T)

title("(a) Stream DOC (mg/l)",line=-24, adj = 0.2)


dev.off()

# Yakima


nhd_CR_stream<-st_read("nhd_CR_stream_sub9.shp")
tmp<-st_zm(nhd_CR_stream)
nhd_CR_poly<-tmp[,"COMID"]

## reading model inputs: substrate concentrations

## merging the model input data with NHDPLUS stream reach shapefiles
nhd_CR_stream_resp=merge(nhd_CR_poly,stream_annDO,by="COMID")
nhd_CR_stream_resp=merge(nhd_CR_stream_resp,stream_annDOC,by="COMID")
nhd_CR_stream_resp=merge(nhd_CR_stream_resp,stream_annno3,by="COMID")
nhd_CR_stream_resp=merge(nhd_CR_stream_resp,stream_nexss,by.x="COMID",by.y="comid_nhd")



jpeg("Stream DOC_YRB_annual_DOC.jpeg", width = 6, height = 6, units = 'in', res = 300) 
par(cex.main=1.5,cex.axis=1.5) 
plot(nhd_CR_stream_resp[,"Stream DOC"],main="", key.pos = 1, key.width = lcm(2), key.length = 1.0,breaks = "fisher",pal=brewer.reds(10),reset=FALSE)
# plot(st_geometry(nhd_CR_stream_resp),add=T)

title("(a) Stream DOC (mg/l)",line=-24, adj = 0.2)


dev.off()








jpeg(paste(figures,"Stream NO3_CRB_annual.jpeg",sep="/"),width = 6, height = 6, units = 'in', res = 300)
par(cex.main=1.5,cex.axis=1.5)  

plot(nhd_CR_stream_resp[,"no3_conc_mg_l"],main="", key.pos = 1, key.width = lcm(2), key.length = 1.0,breaks = "fisher",pal=brewer.greens(10),reset=FALSE)
plot(st_geometry(CR_basin_shapes),add=T)
title("(b) Stream NO3 (mg/l)",line=-24, adj = 0.2)

dev.off()

jpeg(paste(figures,"Stream DO_CRB_annual.jpeg",sep="/"), width = 6, height = 6, units = 'in', res = 300)
par(cex.main=1.5,cex.axis=1.5)  
plot(nhd_CR_stream_resp[,"pred_annual_DO"],main="", key.pos = 1, key.width = lcm(2), key.length = 1.0,breaks = "fisher",pal=brewer.pubu(10),reset=FALSE)
plot(st_geometry(CR_basin_shapes),add=T)

title("(c) Stream DO(mg/l)",line=-24, adj = 0.2)

dev.off()

jpeg(paste(figures,"Residencetime.jpeg",sep="/"), width = 6, height = 6, units = 'in', res = 300)
par(cex.main=1.5,cex.axis=1.5)  
plot(nhd_CR_stream_resp[,"logRT_total_hz_s"],main="", key.pos = 1, key.width = lcm(2), key.length = 1.0,breaks = "fisher",pal=brewer.oranges(10),reset=FALSE)
plot(st_geometry(CR_basin_shapes),add=T)

title("(d) Residence time, log10(s)",line=-24, adj = 0.2)

dev.off()


jpeg(paste(figures,"Hz_exchangeflux.jpeg",sep="/"), width = 6, height = 6, units = 'in', res = 300)
par(cex.main=1.5,cex.axis=1.5)  
plot(nhd_CR_stream_resp[,"logq_hz_total_m_s"],main="", key.pos = 1, key.width = lcm(2), key.length = 1.0,breaks = "fisher",pal=brewer.ylgnbu(10),reset=FALSE)
plot(st_geometry(CR_basin_shapes),add=T)

title("(e) Exchange flux, log10(m/s)",line=-24, adj = 0.2)

dev.off()