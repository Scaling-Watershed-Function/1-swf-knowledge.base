# Logs 
 [[Logs]] are used to document general progress in the development of [[PRODUCTS]].  They will consist of a series of [[Quick Note]] documents for individual [[PEOPLE]] to develop ideas that can lead to the creation of new [[Workflows]] or other [[TOOLS]]
 ----------------------------------------------------------------------
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

