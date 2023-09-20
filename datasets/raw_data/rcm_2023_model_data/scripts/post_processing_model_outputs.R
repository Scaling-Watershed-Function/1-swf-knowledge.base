## reading model outputs and recomputing the model estimates via co2 production or o2 consumption amounts 
library(sp)
library(sf)
library(raster)
library(rgdal)
library(rasterVis)
library(rgeos)
library(lattice)
library(grid)
library(spatstat)
library(plotKML)
library(fasterize)
library(egg)
library(nhdplusTools)
library(nhdR)
library(rgeos)
library(colorspace)
library(stars)
library(pals)
library(foreign)
library(tidyverse)

#### 1) lateral and vertical co2 production (via aerobic/anaerobic respiration) (moles/g) ############
## reading model inputs ### 
df<-read_csv(paste(data_folder,"raw_willamette_yakima_data_rf_scaling_analysis_data.csv",sep="/"))
df$basin_id<-ifelse(df$basin=="yakima",1703,1709)

## model outputs ####
home<-getwd()
model_outputs=paste(home,"data/model_outputs",sep="/")

annual_10vert<-"out_vert.dat" #
annual_10vert<-read.delim(paste(model_outputs,annual_10vert,sep="/"),header=T,sep='',skip=0)

annual_10lat<-"out_lat.dat" #
annual_10lat<-read.delim(paste(model_outputs,annual_10lat,sep="/"),header=T,sep='',skip=0)

# extract 2nd and 3rd year cumulative respiration amounts (moles)
tmp=abs(subset(annual_10vert,annual_10vert$day==730))
tmp2=abs(subset(annual_10vert,annual_10vert$day==1095))

# compute net annual respiration---------
tmp3<-as.data.frame(cbind(tmp$COMID,tmp2$ver_o2_cons_mol-tmp$ver_o2_cons_mol,tmp2$ver_no3_prod_mol-tmp$ver_no3_prod_mol,tmp2$ver_no2_prod_mol-tmp$ver_no2_prod_mol))
colnames(tmp3)=c("COMID","ver_o2_cons_mol","ver_no3_cons_mol","ver_no2_cons_mol")

tmp3$ver_co2_prod_no3_mol<-abs(tmp3$ver_no3_cons_mol*0.5)
tmp3$ver_co2_prod_no2_mol<-abs(tmp3$ver_no2_cons_mol*0.75)
tmp3$ver_co2_prod_o2_mol<-abs(tmp3$ver_o2_cons_mol)
tmp3<-tmp3[,c(1,5,6,7)]

ver_annual_hr<-tmp3
# extract 2nd and 3rd year annual respiration amounts (moles)
tmp=abs(subset(annual_10lat,annual_10lat$day==730))
tmp2=abs(subset(annual_10lat,annual_10lat$day==1095))

tmp3<-as.data.frame(cbind(tmp$COMID,tmp2$lat_o2_cons_mol-tmp$lat_o2_cons_mol,tmp2$lat_no3_prod_mol-tmp$lat_no3_prod_mol,tmp2$lat_no2_prod_mol-tmp$lat_no2_prod_mol))
colnames(tmp3)=c("COMID","lat_o2_cons_mol","lat_no3_cons_mol","lat_no2_cons_mol")

tmp3$lat_co2_prod_no3_mol<-abs(tmp3$lat_no3_cons_mol*0.5)
tmp3$lat_co2_prod_no2_mol<-abs(tmp3$lat_no2_cons_mol*0.75)
tmp3$lat_co2_prod_o2_mol<-abs(tmp3$lat_o2_cons_mol)
tmp3<-tmp3[,c(1,5,6,7)]
lat_annual_hr<-tmp3

#### merge lateral and vertical respiration (co2)  amount (moles)
resp_hr_annual_mole<-merge(ver_annual_hr,lat_annual_hr,by.x="COMID",by.y="COMID")
nhd_stream_annual_resp<-merge(df,resp_hr_annual_mole, by.x = "comid",by.y="COMID")

## anaerobic co2 production (respiration)
nhd_stream_annual_resp$ver_co2_prod_anaer_mol<-nhd_stream_annual_resp$ver_co2_prod_no2_mol+nhd_stream_annual_resp$ver_co2_prod_no3_mol
nhd_stream_annual_resp$lat_co2_prod_anaer_mol<-nhd_stream_annual_resp$lat_co2_prod_no2_mol+nhd_stream_annual_resp$lat_co2_prod_no3_mol
nhd_stream_annual_resp$totco2_prod_anaer_mol<-nhd_stream_annual_resp$ver_co2_prod_anaer_mol+nhd_stream_annual_resp$lat_co2_prod_anaer_mol

# vertical total respiration (mole)
nhd_stream_annual_resp$ver_co2_prod_mol<-nhd_stream_annual_resp$ver_co2_prod_o2_mol+nhd_stream_annual_resp$ver_co2_prod_anaer_mol

# lateral total respiration (mole)
nhd_stream_annual_resp$lat_co2_prod_mol<-nhd_stream_annual_resp$lat_co2_prod_o2_mol+nhd_stream_annual_resp$lat_co2_prod_anaer_mol

# calculate the total aerobic respiration 
nhd_stream_annual_resp$totco2_o2_mol<-nhd_stream_annual_resp$ver_co2_prod_o2_mol+nhd_stream_annual_resp$lat_co2_prod_o2_mol

## converting the mole to g unit
nhd_stream_annual_resp$totco2_o2g<-nhd_stream_annual_resp$totco2_o2_mol*12
nhd_stream_annual_resp$verco2_o2g<-nhd_stream_annual_resp$ver_co2_prod_o2_mol*12
nhd_stream_annual_resp$latco2_o2g<-nhd_stream_annual_resp$lat_co2_prod_o2_mol*12

# calculate the total anaerobic respiration 
nhd_stream_annual_resp$totco2_anaer_mol<-nhd_stream_annual_resp$ver_co2_prod_anaer_mol+nhd_stream_annual_resp$lat_co2_prod_anaer_mol
nhd_stream_annual_resp$totco2_ang<-nhd_stream_annual_resp$totco2_anaer_mol*12

# total co2 production
nhd_stream_annual_resp$totco2_mol<-nhd_stream_annual_resp$totco2_o2_mol+nhd_stream_annual_resp$totco2_anaer_mol
nhd_stream_annual_resp$totco2g<-nhd_stream_annual_resp$totco2_mol*12

## compute mean daily respiration
nhd_stream_annual_resp$totco2g_day<-nhd_stream_annual_resp$totco2g/365
nhd_stream_annual_resp$totco2_o2g_day<-nhd_stream_annual_resp$totco2_o2g/365
nhd_stream_annual_resp$totco2_ang_day<-nhd_stream_annual_resp$totco2_ang/365

## compute mean daily respiration per stream surface area
nhd_stream_annual_resp$totco2g_m2_day=nhd_stream_annual_resp$totco2g_day/(nhd_stream_annual_resp$length_m*nhd_stream_annual_resp$stream_width_m)
nhd_stream_annual_resp$logtotco2g_m2_day=log10(nhd_stream_annual_resp$totco2g_m2_day)

nhd_stream_annual_resp$totco2_o2g_m2_day=nhd_stream_annual_resp$totco2_o2g_day/(nhd_stream_annual_resp$length_m*nhd_stream_annual_resp$stream_width_m)
nhd_stream_annual_resp$logtotco2_o2g_m2_day=log10(nhd_stream_annual_resp$totco2_o2g_m2_day)

nhd_stream_annual_resp$totco2_ang_m2_day=nhd_stream_annual_resp$totco2_ang_day/(nhd_stream_annual_resp$length_m*nhd_stream_annual_resp$stream_width_m)
nhd_stream_annual_resp$logtotco2_ang_m2_day=log10(nhd_stream_annual_resp$totco2_ang_m2_day)


write.csv(nhd_stream_annual_resp,file=paste(model_outputs,"nhd_stream_annual_resp.csv",sep="/"),row.names=FALSE)

#### 2) lateral and vertical O2 consumption (moles/g) ############

df<-read_csv(paste(data_folder,"raw_willamette_yakima_data_rf_scaling_analysis_data.csv",sep="/"))
df$basin_id<-ifelse(df$basin=="yakima",1703,1709)


annual_10vert<-"out_vert.dat" #
annual_10vert<-read.delim(paste(model_outputs,annual_10vert,sep="/"),header=T,sep='',skip=0)

annual_10lat<-"out_lat.dat" #
annual_10lat<-read.delim(paste(model_outputs,annual_10lat,sep="/"),header=T,sep='',skip=0)

# extract 2nd and 3rd year cumulative respiration amounts (moles)
tmp=abs(subset(annual_10vert,annual_10vert$day==730))
tmp2=abs(subset(annual_10vert,annual_10vert$day==1095))


tmp3<-as.data.frame(cbind(tmp$COMID,tmp2$ver_o2_cons_mol-tmp$ver_o2_cons_mol,tmp2$ver_no3_prod_mol-tmp$ver_no3_prod_mol,tmp2$ver_no2_prod_mol-tmp$ver_no2_prod_mol))
colnames(tmp3)=c("COMID","ver_o2_cons_mol","ver_no3_cons_mol","ver_no2_cons_mol")
tmp3<-tmp3[,c(1,2)]
ver_annual_hr<-tmp3

# extract 2nd and 3rd year cumulative respiration amounts (mols)
tmp=subset(annual_10lat,annual_10lat$day==730)
tmp2=subset(annual_10lat,annual_10lat$day==1095)

tmp3<-as.data.frame(cbind(tmp$COMID,tmp2$lat_o2_cons_mol-tmp$lat_o2_cons_mol,tmp2$lat_no3_prod_mol-tmp$lat_no3_prod_mol,tmp2$lat_no2_prod_mol-tmp$lat_no2_prod_mol))
colnames(tmp3)=c("COMID","lat_o2_cons_mol","lat_no3_cons_mol","lat_no2_cons_mol")
tmp3<-tmp3[,c(1,2)]
lat_annual_hr<-tmp3


## vertical and lateral o2 consumption amounts 
resp_hr_annual_mol<-merge(ver_annual_hr,lat_annual_hr,by.x="COMID",by.y="COMID")
nhd_stream_annual_o2_consum<-merge(df,resp_hr_annual_mol, by.x = "comid",by.y="COMID")
## vertical+lateral value
nhd_stream_annual_o2_consum$tot_o2_cons_mol<-nhd_stream_annual_o2_consum$ver_o2_cons_mol+nhd_stream_annual_o2_consum$lat_o2_cons_mol
## mean annual value (/365)
nhd_stream_annual_o2_consum$tot_o2_cons_mol_day<-nhd_stream_annual_o2_consum$tot_o2_cons_mol/365
## mole to g 
nhd_stream_annual_o2_consum$tot_o2_cons_g_m2_day<-nhd_stream_annual_o2_consum$tot_o2_cons_mol_day*32/(nhd_stream_annual_o2_consum$length_m*nhd_stream_annual_o2_consum$stream_width_m)


write.csv(nhd_stream_annual_o2_consum,file=paste(model_outputs,"nhd_stream_annual_o2_consum.csv",sep="/"),row.names=FALSE)

