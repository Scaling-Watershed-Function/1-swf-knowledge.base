# README

## Dataset: NHDPlus v. 2.1: Ancillary hydrological attributes-Yakima and Willamette
River Basins

### Source Citation: Schwarz, G.E., 2019, E2NHDPlusV2_us: Database of Ancillary Hydrologic Attributes 
and Modified Routing for NHDPlus Version 2.1 Flowlines: U.S. Geological Survey data release, 
https://doi.org/10.5066/P986KZEM.


### Documentation:
Brakebill, J.W., Schwarz, G.E., and Wieczorek, M.E., 2020, An enhanced hydrologic stream network based on the NHDPlus medium resolution dataset: U.S. Geological Survey Scientific Investigations Report 2019–5127, 49 p., https://doi.org/10.3133/sir20195127.


This dataset was clipped from the CONUS dataset to obtain values corresponding to 
the Yakima and Willamette River Watershed only. To clip these data, we used a 
reference list of "comid's" after subsetting the the enhanced NHDPlus V.2.1 as the 
reference dataset for COMIDs (Blodgett_23_Network_Attributes). We subsetted the 
enhanced NHDPlus V.2. using the huc_4, which we derived as the first four digits
of the "reachcode" variable. Thus, most our datasets, unless indicated otherwise
correspond the the huc_4 1703 and 1709 for the Yakima and Willamette River Basins
respectively. 
