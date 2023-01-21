# Logs 
 [[Logs]] are used to document general progress in the development of [[PRODUCTS]].  They will consist of a series of [[Quick Note]] documents for individual [[PEOPLE]] to develop ideas that can lead to the creation of new [[Workflows]] or other [[TOOLS]]
## Quick note
**Date**: 2023-01_20
**People:** [[Francisco Guerrero]]
**Products:** [[data_prep_scaling]]
**Platforms:** [[Obsidian]], [[GitHub]], [[RStudio]]

#### Note
For preliminary analysis including [[data_prep_scaling]] and [[scaling_figures]], we will use also some csv files that would be wrangled in [[RStudio]].  These files include:

"220725_yrb_resp_vars_legacy.csv": First data file received from [[Kyongho Son]] that contain local respiration data (valid) and cumulative respiration data (invalid due to a mistake in the calculation). The idea would be to extract the local respiration values and add them to "230117_yrb_respt_vars.csv". 

Another data merge is between "230117_yrb_respt_vars.csv" and "230117_yrb_hbgc_vars.csv" to explore the relationships between hydro-biogeochemical variables (e.g., residence times, hyporheic exchange) and cummulative respiration. 

We will merge the resulting dataset with the data from the landscape heterogeneity analysis that will use "230117_yrb_cmid_land_2011.csv"

Finallly we will merge the resulting dataset from above with with "230110_yrb_spatial_camp.csv" to explore the influence of local scaling with watershed scaling as discussed by [[Wil Wollheim]]

#### Tasks
- [ ] Merge data sets for exploratory analysis in the YRB 🛫 2023-01-20 ⏳ 2023-01-20 📅 2023-01-24
