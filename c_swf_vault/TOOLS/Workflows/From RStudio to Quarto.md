# How to get started with Quarto from where you are

## Check that you are up to date. 
1. Quarto will work better if R, RStudio, and all your packages are up to date. At the moment of writing this note 2022-07-31, the latest versions for R and RStudio are: 4.2.1 and 2022.07.01. Build554 respectively. 
2. You may wonder how to check if you have the latest versions. You can do this: 
	* In RStudio you go to the top menu bar, select "Help" and then "About R Studio". For R, while in RStudio, write "version" in the Console and hit enter. You will find the R version in us at the bottom of the list next to "version.string" 
3. If you don't have the latest versions I suggest you update R first, and then RStudio. But, STOP! You are going to safe the info about all the packages you have already installed. Otherwise you would have to manually update packages in R or RStudio One By One! If you happen to remember what you had installed before. Granted, when running your scripts in RStudio, if a package is not installed, you would get a message asking you to install such package. However, don't you like tidyness and smoothness? I do! This is what I did following these instructions from [stack overflow](https://stackoverflow.com/questions/66450454/how-to-update-all-packages-for-a-new-r-version-quickly-and-easily):
	* Run this in your **previous** R  installation (e.g. you currently have 4.2.0 if you will install 4.2.1)-one line at a time:
		* install.packages("pacman")
		* library(pacman)
		* dput(pacman::p_lib())
	* Copy the output to clipboard
	* Install the **newest** R version (it does not override your previous one, so you can do this first as well)
	* Open your **new** R version and paste the output from the previous step in place of "paste output here"
		* vector_of_packages <- "paste output here", it should look like vector_of_packages <- c("package 1", "package 2"..."package n"), then
		* install.packages(vector_of_packages)
4. Now, download and install the latest version of RStudio.
5. Voila! Happy work!

---
c("askpass", "assertthat", "backports", "base64enc", "bit", "bit64", 
"blob", "boot", "broom", "bslib", "cachem", "callr", "cellranger", 
"class", "cli", "clipr", "cluster", "codetools", "colorBlindness", 
"colorspace", "cowplot", "cpp11", "crayon", "curl", "data.table", 
"DBI", "dbplyr", "digest", "dplyr", "dtplyr", "ellipsis", "evaluate", 
"fansi", "farver", "fastmap", "forcats", "foreign", "fs", "gargle", 
"generics", "GGally", "ggplot2", "glue", "googledrive", "googlesheets4", 
"gridExtra", "gridGraphics", "gtable", "haven", "here", "highr", 
"hms", "htmltools", "httr", "ids", "isoband", "jquerylib", "jsonlite", 
"KernSmooth", "knitr", "labeling", "later", "lattice", "lifecycle", 
"lubridate", "magrittr", "markdown", "MASS", "Matrix", "memoise", 
"mgcv", "mime", "modelr", "munsell", "nlme", "nnet", "openssl", 
"packrat", "pacman", "palmerpenguins", "pillar", "pkgconfig", 
"plyr", "prettyunits", "processx", "progress", "ps", "purrr", 
"quarto", "R6", "rappdirs", "RColorBrewer", "Rcpp", "readr", 
"readxl", "rematch", "rematch2", "remotes", "renv", "reprex", 
"reshape", "rlang", "rmarkdown", "rpart", "rprojroot", "rsconnect", 
"rstudioapi", "rvest", "sass", "scales", "selectr", "spatial", 
"stringi", "stringr", "survival", "sys", "tibble", "tidyr", "tidyselect", 
"tidyverse", "tinytex", "tzdb", "utf8", "uuid", "vctrs", "viridis", 
"viridisLite", "vroom", "withr", "xfun", "xml2", "yaml", "base", 
"compiler", "datasets", "graphics", "grDevices", "grid", "methods", 
"parallel", "splines", "stats", "stats4", "tcltk", "tools", "translations", 
"utils")
------
Publishing bug in windows-part I

Install IIS WWW Publishing service (W3SVc)

The short answer is that you go to the _Programs and Features_ control panel applet. Then click on _Turn Windows Features On and Off_ and enable the assorted IIS-related features you need.

From: https://superuser.com/questions/566044/how-to-install-iis-world-wide-web-publishing-service-w3svc

Also: [# The World Wide Web Publishing Service is disabled or missing_W3SVCDisabledOrNotInstalled](https://docs.microsoft.com/en-us/exchange/the-world-wide-web-publishing-service-is-disabled-or-missing-w3svcdisabledornotinstalled-exchange-2013-help)
