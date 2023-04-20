###############################################################################
# Downloading data from websites with RSelenium
###############################################################################

# Online tutorials: 
# Windows:
# 1.Checking for Java:
# Click on the the start icon (window on bottom left)
# On the search bar write "cmd" to open the control command console
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
                 wdman,
                 rvest,
                 data.table)

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
                             chromever = "112.0.5615.49",
                             verbose = FALSE,
                             port = free_port())
remDr <- rs_driver_object$client
remDr$close()

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

# You can close this browser right away, since we will open another one as a client.


# Here is a Client-Server definition from dictionary.com:

# "A computer network in which one centralized, powerful computer (called the server) 
# is a hub to which many less powerful personal computers or workstations (called clients) 
# are connected. The clients run programs and access data that are stored on the server." 

# Downloading data example:

# To start downloading data, we first need to open a client object:

# Our input variable should be the data package url

target_url <- "https://data.ess-dive.lbl.gov/view/doi:10.15485/1962818"
#target_url <- "https://data.ess-dive.lbl.gov/view/doi:10.5440/1861071" (tested and verified)
#target_url <- "https://data.ess-dive.lbl.gov/view/doi:10.15485/1505624" (tested and verified)


remDr$open()
remDr$navigate(target_url)
Sys.sleep(5) # Wait for the page to load


# Find the expand button and click on it (if exists)

expand_button <- tryCatch(
  remDr$findElement(using = "css selector", value = "#table-container > div > table > tfoot"),
  error = function(e) NULL
)

if (!is.null(expand_button)) {
  expand_button$clickElement()
  Sys.sleep(5)
}

# Extract the table as a data frame
table_html <- remDr$findElement(using = "css selector", 
                                value = "#table-container")$getElementAttribute("outerHTML")[[1]]
table_df <- read_html(table_html) %>% html_table()

files_table <- as.data.frame(table_df[1]) 

colnames(files_table) <- c("position",
                           "name",
                           "info",
                           "file_type",
                           "size",
                           "n_downloads",
                           "action_download")
files_table <- files_table%>% 
  slice(-1) %>% 
  filter(is.na(name)==FALSE)

# Extract the download links for each row
table_rows <- remDr$findElements(using = "css selector", value = "#table-container > div > table > tbody > tr")

download_links <- lapply(1:length(table_rows), function(i) {
  # Construct the selector for the download link for the i-th row
  download_link_selector <- paste0("#table-container > div > table > tbody > tr:nth-child(", i, ") > td.download-btn.btn-container > a")
  # Extract the 'href' attribute of the download link
  download_link_element <- remDr$findElement(using = "css selector", value = download_link_selector)
  download_link_element$getElementAttribute("href")
})

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()

#Printing table with download links
download_list <- as.data.frame(unlist(download_links))
colnames(download_list) <- "links"
download_table <- files_table %>% 
  select(name,file_type,size) %>% 
  cbind(download_list)

print(download_table)


# From the download table the user should be able to pick which file to download

my_data_selection = 9

my_download_url <- download_table$links[my_data_selection]

########################### IN PROGRESS ########################################

# Create a temporary directory to store the heavy data from ESS-DIVE
temp_dir <- tempdir()
temp_file <- tempfile(fileext = ".zip")

temp_dat <- download.file(url = my_download_url,
              destfile = temp_file)

crb_dat <- unzip("temp_dat",exdir = temp_dir)

dat <- read_csv(crb_dat[9], show_col_types = FALSE)

glimpse(dat)

write.csv(dat,"nexss_inputs.csv")

# Delete temporary file
file_name <- "temp_file.zip"

if (file.exists(file_name)) {
  unlink(file_name)
  print("File is deleted..")
} else{
  print("File not exists..")
}
