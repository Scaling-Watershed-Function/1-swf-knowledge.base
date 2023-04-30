# Logs 
 [[Logs]] are used to document general progress in the development of [[PRODUCTS]].  They will consist of a series of [[Quick Note]] documents for individual [[PEOPLE]] to develop ideas that can lead to the creation of new [[Workflows]] or other [[Tools]]
 ----------------------------------------------------------------------
## Quick note
**Date**: 2023-04_29
**People:** [[Francisco J. Guerrero]]
**Products:**[[Scripts]], [[scaling_figures]]
**Platforms:** [[RStudio]], [[Obsidian]]

#### Note


#### Tasks


## Quick note
**Date**: 2023-04_22
**People:** [[Francisco J. Guerrero]]
**Products:**[[Scripts]]
**Platforms:** [[RStudio]], [[Obsidian]]

#### Note
##### Organizing Data Files
So far I have 5 input files of raw data:
* 230314_conus_2001_landcover.csv:
	* This file was originally generated with a local script (script_comid_ref_landuse) stored in 6-swf-preprocessing_local (without filtering for Yakima and Willamette)
	* I call files from the local subfolder ./nhdpv2_files
	* This file was removed
* 230321_pnw_2001_landcover.csv:
	* The updated version of 230321_conus_2001_landcover.csv
	* This file was generated with: script_comid_ref_landuse locally stored in 6-swf-preprocessing_local
	* It calls files from the local subfolder ./nhdpv2_files
	* This file is retained
* 230406_son_etal_22_results_zen.csv
	*  This file is generated with a remote backed up script: script_zenodo_data.R stored in 2-swf-analytical.engine.
	* This script also merges "230324_inf_cont_lnd.csv" with Son et al., data to generate the file "230406_hbgc_pnw_land.csv" in data/processed
* 230410_hydro_info_pnw.csv
	*  This file was originally generated with a local script (script_comid_ref_landuse) stored in 6-swf-preprocessing_local (without filtering for Yakima and Willamette)
	* I call files from the local subfolder ./nhdpv2_files
* 230420_yrb_spatial_corrected.csv
	* This file was originally suplied by Kyongho Son, and it was corrected to show the final comids verified by K. Son and M. Kauffman, which will be published as a geospatial data package
* combined_results_updated_040623.csv
	* lab results from field incubations of aquatic ecosystem respiration. 
	* Provided by Matt Kauffman
##### Principles
* All data are downloaded via RSelenium, so the download process itself is reproducible.
* Tables with specific urls to data files should also be stored in the knowledge base/data, so if the RSelenium script fails, people could still directly download the files and continue with data wrangling.
* Data wrangling scripts are stored in the knowledge base, and any new data set generated from data wrangling is still considered raw data.
* Any scripts analysis performed on the data beyond data wrangling and cleaning should be stored in the analytical engine repository, and the new data produced should be stored in the "processed" folder of the Knowledge Base.
* Any plot created by scripts in the analytical engine should only use minimal formatting. 
* Scripts for draft figures or documents are stored in the production hub as quarto documents. This scripts could be started as a copy of the analytical engine scripts, but with the .qmd extension.  Draft figures are generated from processed data only. 

#### Tasks



## Quick note
**Date**: 2023-04_18
**People:** [[Francisco J. Guerrero]]
**Products:** [[Scripts]]
**Platforms:** [[Java]]


#### Note

Installation of Java for Mac:
In previous iterations I was installing Java 20, this time, when looking for Java for Mac I downloaded from this page and installed Java 1.8.0: 
https://www.java.com/en/download/apple.jsp 
The main difference is that the latest Java is specifically designed for mac engines. 


#### Tasks



## Quick note
**Date**: 2023-04_07
**People:**[[Francisco J. Guerrero]]
**Products: **[[Manuscript]], [[Website]], [[Scripts]]
**Platforms: **[[GitHub]]

#### Note
A new tool for large file storage system: 
https://towardsdatascience.com/version-control-your-large-datasets-using-google-drive-dd42211ab740
https://dvc.org/#use-cases
Installing DVC from GitHub
https://github.com/iterative/dvc
#### Tasks


## Quick note
**Date**: 2023-03_18
**People:** [[Francisco J. Guerrero]]
**Products:** [[2_methods]], [[5_supplements]]
**Platforms:**[[Obsidian]] [[RStudio]]

#### Note
[[Francisco J. Guerrero]]  tested a workflow for connecting documents between the swf_vault
and the production hub. The workflow goes like this:

Raw .md docs are created in obsidian. 
Raw .md docs are opened while working on the production.hub .Rproj
Raw .md could be fully edited and saved (in their corresponding obsidian folder), while working on the production.hub .Rproj
Once edits are completed on the Raw .md, it can be copied to a new .qmd created within the production.hub .Rproj working directory. 
In this way raw.md docs continue to live in obsidian, while .qmd will be in the production hub. 

Additionally, [[Francisco J. Guerrero]] added a README to the 3-sfw-production.hub including a link to preview html files


#### Tasks



## Quick note
**Date**: 2023-03_18
**People:** [[Francisco J. Guerrero]]
**Products:** [[2_methods]], [[5_supplements]]
**Platforms:**[[Obsidian]] [[RStudio]]

#### Note
[[Francisco J. Guerrero]] installed the plug-in "Folder Index" that automatically indexes the notes within a folder and allows for visual representation of the notes immediately.  This plugin saves a lot of time in terms of maintenance of the folder structure and it's representation

The execution of the plugin is still a bit clunky, but it has immense potential. 

In this session, the [[5_supplements]] section will be updated with a table containing the databases with their corresponding description and download links. 

[[Francisco J. Guerrero]] also added [[Zotero]] to [[Tech_platforms]] an still working on re-organizing the folders within the swf_vault 

#### Tasks

## Quick note
**Date**: 2023-03_17
**People:** [[Francisco J. Guerrero]] and [[Kyongho Son]]
**Products:** [[Manuscript]]
**Platforms:**[[Obsidian]], [[Teams]]
#### Note

##### Data Sources
###### NHDPlus
[Attributes for NHDPlus Version 2.1 Catchments and Modified Routing of Upstream Watersheds for the Conterminous United States: Select Basin Characteristics](https://www.sciencebase.gov/catalog/item/57976a0ce4b021cadec97890)
Includes: 
BASIN_CHAR_CAT_CONUS.txt
BASIN_CHAR_TOT_CONUS.txt
BASIN_CHAR_ACC_CONUS.txt
* Contains information on stream slope (percent rise, also referred to as the percent slope), basin slope (percent rise, also referred to as the percent slope), basin area, minimum basin elevation, maximum basin elevation, mean basin elevation, and stream length.

STREAM_DENSITY_CONUS.TXT
* stream density, contains information on stream density (stream kilometers per square kilometer of basin).

SINUOSITY_CONUS.txt
* Contains information on flowline reach sinuosity.

FTYPE_PCT_CONUS.txt
* contains information on each flowline reach type as a percentage to all flowline features per catchment.

Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes for NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream Watersheds for the Conterminous United States (ver. 3.0, January 2021): U.S. Geological Survey data release, https://doi.org/10.5066/F7765D7V.

###### NHDPlus EPA
[NHDPlus Pacific Northwest Data (Vector Processing Unit 17)](https://www.epa.gov/waterdata/nhdplus-pacific-northwest-data-vector-processing-unit-17)
Files to download
NHDPlusV21_PN_17_NHDPlusAttributes
NHDPlusV21_PN_17_NHDPlusCatchment
NHDPlusV21_PN_17_WBDSnapshot


###### NEXSS
The NEXSS model couples NHDPlus-based geomorphologic model with physics-based surrogate models for hyporheic exchange to compute the exchange flux, residence time distribution, and median residence time via a bedform-driven (or vertical) and sinuosity-driven (or lateral) exchange (Data sent by [[Kyongho Son]]). Expecting new data from [[Chucho Gomez Velez]]

###### NLDC
[NLCD 2001 Land Cover (CONUS)](https://www.mrlc.gov/data/nlcd-2001-land-cover-conus)



#### Tasks

## Quick note
**Date**: 2023-03_15
**People:**[[Francisco J. Guerrero]]
**Products:**[[Scripts]], [[Data]]
**Platforms:**[[Obsidian]], [[GitHub]], [[RStudio]]

#### Note
Organizing raw [[Data]] inputs going into the analytical engine.
[[Francisco J. Guerrero]] also wonders how to include notes about other components of the workflow into [[Obsidian]]. Should I have folders for Analytical Engine, Production Hub, Products store and User Interface? It makes sense, since as I make progress through the project, each repository would have predominance. Thus, many notes would be related to them.

[[Data]] inputs into the analytical engine:
Land cover raw data for the estimation of information contributions and marginal entropies indexed by COMID
Stream local respiration rates from [[Kyongho Son]]'s paper (considered raw input for this paper)
Stream physical characteristics including length and area (considered raw input for this paper)

[[Data]] preparation and pre-processing scripts should be within the assets for the Knowledge Base. 
* Scripts should be the closest as possible to their data sources. 

List of variables (to be merged with land use data for entropy):


COMID, 
FromNode,
ToNode,
Hydroseq,
TOT_BASIN_AREA,
CAT_BASIN_AREA,
StreamOrde,
logQ_m3_div_s,
logwbkf_m,
logd_m,
logdbkf_m,
D50_m,

Model Inputs
[[Scripts]]: script_data_preprocessing (analytical engine, should be moved to knowledge base)

nhd_shp. files contain COMID, ID, Name, HUC4, and Geometry for Willamette, and COMID, Name and Geometry for Yakima. The Name column was added before merging the two datasets. 

The remaining variables were obtained from corresponding csv files for DO, DOC, nitrates, and nexss 

(All files obtained from GitLab and need to be Updated link to ESS-DIVE data package, once is published.)

stream_length_m, = 230313_wlm_ykm_stream_resp_dat.csv
logw_m, = 230313_wlm_ykm_stream_resp_dat.csv
pred_annual_DOC, = 230313_wlm_ykm_stream_resp_dat.csv
pred_annual_DO, = 230313_wlm_ykm_stream_resp_dat.csv
no3_conc_mg_l, = 230313_wlm_ykm_stream_resp_dat.csv
logRT_total_hz_s, = 230313_wlm_ykm_stream_resp_dat.csv (as lateral and vertical)
logq_hz_total_m_s, = 230313_wlm_ykm_stream_resp_dat.csv (as lateral and vertical)

Model outputs
(All files downloaded from [zenodo](https://doi.org/10.5281/ zenodo.6954107.) and pre-processed locally for Yakima and Willamette)
[[Scripts]]:
totco2g_m2_day = 230313_wlm_ykm_stream_resp_dat.csv




Analytical engine futher calculations
entropy catchment
entropy watershed
relative entropy catchment
relative entropy watershed
pred_stream_area_m2_fill,
cum_stream_length_m,
cum_stream_area_m2,
cum_totco2g_day,
cum_totco2g_day_Tsurface_m2,
cum_totco2g_day_Tdrain_m2


#### Tasks

- [ ] #todo Make adjustment to the Organization Template-Project Vault to include other components of the workflow in obsidian folders 🔼 🛫 2023-03-17 ⏳ 2023-03-15 📅 2023-03-17


## Quick note
**Date**: 2023-03_17
**People:**
**Products:**
**Platforms:**

#### Note


#### Tasks

## Quick note
**Date**: 2023-03_02
**People:**[[Francisco J. Guerrero]]
**Products:**[[data_prep_scaling]]
**Platforms:**[[Obsidian]], [[GitHub]], [[RStudio]]

#### Note
[[Francisco J. Guerrero]] implemented paths to import data from GitHub.com and to export data into 
local directories following the instructions detailed in the pages:

https://stackoverflow.com/questions/41271176/access-a-file-outside-of-working-directory-without-referencing-full-path 

https://www.tutorialspoint.com/how-to-import-csv-file-data-from-github-in-r

## Quick note
**Date**: 2023-01_26
**People:**[[Francisco J. Guerrero]], [[Kyongho Son]]
**Products:**[[data_prep_scaling]]
**Platforms:**[[Obsidian]], [[GitHub]], [[RStudio]]

#### Note
[[Kyongho Son]] supplied data from the Willamette River Basin. Yet, the data is spread across multiple files, so [[Francisco J. Guerrero]] started to work on harmonizing the dataset. This to avoid having to create multiple scripts to adjust to the different formats. 

#### Tasks
- [x] Complete data harmonization for exploratory analysis in the Willamette River Basin and the Yakama River Basin 🛫 2023-01-26 ⏳ 2023-01-26 📅 2023-01-27 ✅ 2023-01-27

## Quick note
**Date**: 2023-01_23
**People:** [[Francisco J. Guerrero]]
**Products:** [[data_prep_scaling]],[[scaling_figures]]
**Platforms:** [[Obsidian]], [[GitHub]], [[RStudio]]

#### Note
[[Francisco J. Guerrero]] reorganized files between different [[PRODUCTS]]. Particularly separating the [[Scripts]] for data analysis and those corresponding to [[Website]].

Renaming [[Scripts]] convention:  "script_filename.R"
Something similar could be use for data, and other assets: 

"yymmdd_data_filename.csv"
"yymmdd_plot_filename.png"
"yymmdd_docs_filename.pdf"
"yymmdd_figs_filename.png" (For conceptual figures)
"yymmdd_pics_filename.jepg"

I have reorganized content across the folders within the [[Production_hub]], so that data, documents, and figures generated within the hub will belong to the folder assets (different from the assets repository). I think I may have to rename these two folders to avoid confusion. 

I started working on  the [[scaling_figures]] script and made it to the paired plots. There is a strong correlation between watershed area and percentage forest with d50, which could allow us to fill in some of the gaps for variables like hyporheic exchange and residence time. 

#### Tasks
## Quick note
**Date**: 2023-01_20
**People:** [[Francisco J. Guerrero]]
**Products:** [[data_prep_scaling]]
**Platforms:** [[Obsidian]], [[GitHub]], [[RStudio]]

#### Note
For preliminary analysis including [[data_prep_scaling]] and [[scaling_figures]], we will use also some csv files that would be wrangled in [[RStudio]].  These files include:

"220725_yrb_resp_vars_legacy.csv": First data file received from [[Kyongho Son]] that contain local respiration data (valid) and cumulative respiration data (invalid due to a mistake in the calculation). The idea would be to extract the local respiration values and add them to "230117_yrb_respt_vars.csv". 

Another data merge is between "230117_yrb_respt_vars.csv" and "230117_yrb_hbgc_vars.csv" to explore the relationships between hydro-biogeochemical variables (e.g., residence times, hyporheic exchange) and cummulative respiration. 

We will merge the resulting dataset with the data from the landscape heterogeneity analysis that will use "230117_yrb_cmid_land_2011.csv"

Finallly we will merge the resulting dataset from above with with "230110_yrb_spatial_camp.csv" to explore the influence of local scaling with watershed scaling as discussed by [[Wil Wollheim]]

#### Tasks
- [x] Merge data sets for exploratory analysis in the YRB 🛫 2023-01-20 ⏳ 2023-01-20 📅 2023-01-24 ✅ 2023-01-23

