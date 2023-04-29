###############################################################################
# Downloading data from ESS-DIVE with RSelenium
###############################################################################

# Pre-requisites:
# Make sure you have ran the file "script_system_prep_rselenium.R"

# Loading/installing required libraries
librarian::shelf(tidyverse,
                 RSelenium,
                 netstat,
                 wdman,
                 rvest,
                 data.table)

# Select your target url

# In your internet explorer (chrome, safari, etc.) go to the ESS-DIVE url that
# contains the data you are interested in. 

# For this short example, we are going to download 1 zip file from the data package: 

# Forbes, B. et al., 2023. WHONDRS River Corridor Dissolved Oxygen, Temperature, 
# Sediment Aerobic Respiration, Grain Size, and Water Chemistry from Machine-Learning-Informed 
# Sites across the Contiguous United States. Copy and paste between "" marks your target url in the
# code line below. In our case this would look like:

target_url <- "https://data.ess-dive.lbl.gov/view/doi:10.15485/1923689"


# Decide whether you want to download the full data package or specific files. To specify
# the file(s) you want to download, write the row number below for their download button below.
# For, instance, the link for full download is on the first row, so you would write 

# `my_data_selection <- 1`. 

# In our example we are interested in downloading the files in the folder "CM_SSS_Data_Package.zip".
# The download link for this zip folder is 4, so we write:

my_data_selection <- 4

# It is possible to download multiple files at the same time by using a loop function. But, for right now 
# for simplicity, we will let the users decide the best way to do so, and focus in one download at a time. 

# We will place our files in the downloads folder and extract them into a temporary
# directory to illustrate the options available for storing files. 

# ATTENTION: Depending on your OS, you might have different paths to get to your 
# downloads folder. The code below should work for most OS, but has been only tested
# on Windows_NT and macOS

# Set the path to the downloads folder
downloads_folder <- if (Sys.getenv("OS")=="Windows_NT"){
  file.path("C:/Users", Sys.getenv("USERNAME"), "Downloads") 
} else{file.path(Sys.getenv("HOME"), "Downloads")}

# Opening a Selenium client-server object
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "112.0.5615.49",
                             verbose = FALSE,
                             port = free_port())
remDr <- rs_driver_object$client

# Navitage to your target url and wait for the page to load
remDr$navigate(target_url)
Sys.sleep(5) 

# Many data packages contain lists of files with more than 5 items. In those cases
# those files are not visible to the scrapper function unless we Find the expand button 
# and click on it. The following lines check whether there is and expand button and 
# click on it. 

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

# Extract the download links

if (my_data_selection > 1 {table_rows <- remDr$findElements(using = "css selector", value = "#table-container > div > table > tbody > tr")

download_links <- lapply(1:length(table_rows), function(i) {
  # Construct the selector for the download link for the i-th row
  download_link_selector <- paste0("#table-container > div > table > tbody > tr:nth-child(", i, ") > td.download-btn.btn-container > a")
  # Extract the 'href' attribute of the download link
  download_link_element <- remDr$findElement(using = "css selector", value = download_link_selector)
  download_link_element$getElementAttribute("href")
})}
else{
  download_link_selector <-"#table-container > div > table > thead > tr:nth-child(2) > th.download-container > a"
  download_link_element <- remDr$findElement(using = "css selector", value = download_link_selector)
  download_link_element$getElementAttribute("href")
})








# Stop the Selenium server and close the browser
rs_driver_object$server$stop()





