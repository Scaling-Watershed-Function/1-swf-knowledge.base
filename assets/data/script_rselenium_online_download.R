###############################################################################
# Downloading data from websites with RSelenium
###############################################################################

# Online tutorials: 
# Windows:
# 1.Checking for Java:
# Click on the the start icon (window on bottom left)
# On the search bar write "cmd" to open the control comand console
# Once there, type java -version
# If you get the message: 'java is not recognized as an internal
# or external command...' You have to install Java first.
# To do so, follow the instructions in this tutorial:
# 1. Installing Java: https://www.youtube.com/watch?v=IJ-PJbvJBGs
# 2. Intro to RSeleniumhttps://www.youtube.com/watch?v=U1BrIPmhx10
# 3. Downloading documents from the web: https://www.youtube.com/watch?v=BK_JBk_l5uQ&t=94s

# MAC OS:
# Checking for Java:
# Open a terminal and type java --version. If you get a message starting with 
# "The operation could not be completed...". You don't have Java installed. Get
# Java for mac from this [link](https://www.java.com/en/download/apple.jsp) 

# Check that all your r packages are up to date

# This is an important step to be able to run RSelenium without issues. While in R Studio,
# go to the tab "Packages" and click "Update". You can go ahead and update all your packages at
# once (take a bit more time, but is a healthy choice). Otherwise make sure that the 
# following packages are installed and up to date:

# Rcpp
# sys
# openssl
# jsonlite
# curl
# ps
# httr
# yaml
# processx
# netstat

# You can check if you have installed versions of the packages by using `require()`

librarian::shelf(tidyverse,
                 RSelenium,
                 netstat,
                 wdman)

# Run this for the first time so all the drivers and dependencies
selenium()

# You can check the paths where all these drivers were stored by creating a 
# `selenium_object`
selenium_object <- selenium(retcommand = T, check = F)

# Here, there is a critical step you need to do outside of R. The newest
# Chrome drivers include a LICENSE files, that cause issues when trying 
# to open a chrome browser using RSelenium. You need to delete those files.

# First, while still in R, you can locate the path to those files by opening
# the selenium object you just created. 

# This object contains the paths to all the drivers installed, included those 
# for Google Chrome. The path to those files should look something like:

# C:\Users\your_user.name\AppData\Local\binman\binman_chromedriver\win32 (Windows)
#

# You need to navigate to the Application folder and locate the chromedriver files.
# You will find as many folders as google chrome drivers. In that case, open each folder
# and delete the LICENSE files.

# Now, you want 

binman::list_versions("chromedriver")

# You will see the versions available for your computer. To select the version 
# to input in the following code, open chrome and type "chrome//version". Check 
# the first three numbers of the version (e.g. 112), and pick within the available
# version any that matches those three numbers. 


# The following code, should open a browser window that will be controlled from
# here:
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "112.0.5615.28",
                             verbose = FALSE,
                             port = free_port())

# The lines above will open a browser on a separate window, in this case it would
# be chrome, but you can specify other browsers like safari, or firefox. You will 
# see a message right below the search bar like this: "Chrome is being controlled
# by automatic test software". This is because, the original purpose of RSelenium
# is to help developers to privately test their applications online. Yet, the 
# use of this approach for webscrapping and data download, came as a bonus. 

# You can go to any https address using this browser, like YouTube or ESS-DIVE, look
# for the data you need and download it directly into your working directory.

# This is what the rsDriver function is doing:
# Source: https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html#:~:text=The%20rsDriver%20function%20is%20a,it%20runs%20a%20Chrome%20browser.
#"The rsDriver function is a wrapper for the selenium function from the wdman 
# package. It allows the user to manage the binaries used to run a Selenium Server. 
# It returns an environment containing a client and a server. By default, it runs a 
# Chrome browser. Other browsers such as Firefox, PhantomJS, and Internet Explorer 
# can be selected using the browser argument."

# Here is a Client-Server definition from dictionary.com:

# "A computer network in which one centralized, powerful computer (called the server) 
# is a hub to which many less powerful personal computers or workstations (called clients) 
# are connected. The clients run programs and access data that are stored on the server." 

# Downloading data example:

# To start downloading data, we first need to open a client object:

remDr <- rs_driver_object$client
remDr$open()

# Let's navigate to the webpage for Son et al., 2022 data package:

remDr$navigate("https://data.ess-dive.lbl.gov/view/doi:10.15485/1962818")

data_files <- remDr$findElements(using = 'xpath', "//td[@class='download-btn btn-container']/a")

# Create a temporary directory to store the heavy data from ESS-DIVE
temp_dir <- tempdir()

son_dat <- download.file(url = "https://data.ess-dive.lbl.gov/catalog/d1/mn/v2/object/ess-dive-ef92031cc1bc9c5-20230310T201704004",
                         path = temp_dir,
                         destfile = "model_inputs.zip")

crb_dat <- unzip("model_inputs.zip",exdir = temp_dir)

dat <- read_csv(crb_dat[9],
                show_col_types = FALSE)

glimpse(dat)

write.csv(dat,"nexss_inputs.csv")

# once you are done using the server in your session, don't forget to close it:
rs_driver_object$server$stop()







