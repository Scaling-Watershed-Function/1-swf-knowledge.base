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
# 3. Downloading documents from the web: https://www.youtube.com/watch?v=BK_JBk_l5uQ

# MAC OS:
# Checking for Java:
# Open a terminal and type java --version. If you get a message starting with 
# "The operation could not be completed...". You don't have Java installed. To install 
# Java properly, follow the instructions in this online tutorial (you can't skip the 
# part of the video that is related to MAVEN):
# https://www.youtube.com/watch?v=Mi8YpP9TQSs 

# TROUBLESHOOTING
# Please be careful when creating the .zshrc file (if an error ocurr, that would change
# the configuration of your terminal). If that the case, reset your terminal setting to 
# its original default. Here is how: 
# https://superuser.com/questions/1686083/any-ideas-on-how-to-reset-my-zsh-terminal-to-default
# and start over. 

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


# once you are done using the server in your session, don't forget to close it:
rs_driver_object$server$stop()







