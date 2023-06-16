###############################################################################
# Downloading data from websites with RSelenium 0: System Prep
###############################################################################

# Author: Francisco J. Guerrero

# Online tutorials: 
# 1. Intro to RSeleniumhttps://www.youtube.com/watch?v=U1BrIPmhx10
# 2. Downloading documents from the web: https://www.youtube.com/watch?v=BK_JBk_l5uQ&t=94s

# REQUIREMENTS

# Java

# Windows:
# 1.Checking for Java:
# Click on the the start icon (window on bottom left)
# On the search bar write "cmd" to open the control command console
# Once there, type java -version
# If you get the message: 'java is not recognized as an internal
# or external command...' You have to install Java first.
# To do so, follow the instructions in this tutorial:
# Installing Java: https://www.youtube.com/watch?v=IJ-PJbvJBGs


# MAC OS:
# Checking for Java:
# Open a terminal and type java --version. If you get a message starting with 
# "The operation could not be completed...". You don't have Java installed. Get
# Java for mac from this [link](https://www.java.com/en/download/apple.jsp) 

# R Packages

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
                 wdman,
                 rvest,
                 data.table)

# SETTING UP SELENIUM

# Run this for the first time so all the drivers and dependencies are installed.
selenium()

# You can check the paths where all these drivers were stored by creating a 
# `selenium_object`
selenium_object <- selenium(retcommand = T, check = F)

# ATENTION! Here, there is a critical step you need to do outside of R. The newest
# Chrome drivers include a LICENSE files, that cause issues when trying 
# to open a chrome browser using RSelenium. You need to delete those files.

# First, while still in R, you can locate the path to those files by opening
# the selenium object you just created. 

# This object contains the paths to all the drivers installed, included those 
# for Google Chrome. The path to those files should look something like:

# C:\Users\your_user.name\AppData\Local\binman\binman_chromedriver\win32 (Windows)
# /Users/your.user.name/Library/Application Support/binman_chromedriver/mac64/ (Mac OS)

# You need to navigate to the Application folder and locate the chromedriver files.
# You will find as many folders as google chrome drivers. In that case, open each folder
# and delete the LICENSE files.

# OPENING A CLIENT-SERVER OBJECT

binman::list_versions("chromedriver")

# You will see the versions available for your computer. 

# To select the version to input in the following code do this: 
# open chrome and type "chrome://version"
# At the time of writing this script I got: 112.0.5615.138
# Compare the first three numbers with those in the versions available in your system (i.e. 112)
# Pick your system version with the highest number after the first three numbers. For instance if you have 
# 112.X.XXX.24 and 112.X.XXX.49, pick the latest. 

# The following code, should open a browser window that will be controlled from
# here:

# Client-Server Object
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "latest",
                             verbose = FALSE,
                             port = free_port())

# Opening a Client Browser (to be used for webscrapping)
remDr <- rs_driver_object$client

# Closing the Client Browser
remDr$close()

# Closing the server
# Stop the Selenium server and close the browser
rs_driver_object$server$stop()

# The lines above will open a browser on a separate window, in this case it would
# be chrome, but you can specify other browsers like safari, or firefox. You will 
# see a message right below the search bar like this: "Chrome is being controlled
# by automatic test software". This is because, the original purpose of RSelenium
# is to help developers to privately test their applications online. Yet, the 
# use of this approach for webscrapping and data download, came as a bonus. 
