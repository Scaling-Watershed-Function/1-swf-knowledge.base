###############################################################################
# Downloading data Son et al 2022 Data from ESS-DIVE
###############################################################################

# Pre-requisites:
# Make sure you have ran the file "script_system_prep_rselenium.R"

# Local Import-Export

raw_data <- "../raw"
processed_data <- "../processed"

# Loading/installing required libraries
librarian::shelf(tidyverse,
                 RSelenium,
                 netstat,
                 wdman,
                 rvest,
                 data.table,
                 utils)


# Set the path to the downloads folder
downloads_folder <- if (Sys.getenv("OS")=="Windows_NT"){
  file.path("C:/Users", Sys.getenv("USERNAME"), "Downloads")
} else{file.path(Sys.getenv("HOME"), "Downloads")}


# Opening a Selenium client-server object with specific download preferences
# Set the download preferences (to allow multiple file downloads without pop ups)
chrome_options <- list(
  chromeOptions = list(
    prefs = list(
      "download.default_directory" = "~/Downloads",
      "download.prompt_for_download" = FALSE,
      "download.directory_upgrade" = TRUE,
      "download.overwrite" = TRUE,
      "profile.default_content_settings.popups" = 0,
      "profile.content_settings.exceptions.automatic_downloads.*.setting" = 1,
      "safebrowsing.enabled" = TRUE
    )
  )
)

# Open RSelenium Server
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "latest",
                             verbose = FALSE,
                             port = free_port(),
                             extraCapabilities = chrome_options)


# Open a client browser for webscrapping
remDr <- rs_driver_object$client

################################################################################
# ESS-DIVE MOdel Inputs, Outputs, Scripts - Spatial variation microbial resp.
# Son et al., 2022 (ver. 3.0 Jan-2021)
################################################################################


# To start downloading data, we first need to specify the url we want to navigate to
target_url <- "https://data.ess-dive.lbl.gov/view/doi:10.15485/1962818"

# Go to the web page
remDr$navigate(target_url)

# Wait for the page to load
Sys.sleep(5) 


# Find the expand button and click on it (if exists)
expand_button <- tryCatch(
  remDr$findElement(using = "css selector", value = "#table-container > div > table > tfoot"),
  error = function(e) NULL
)

if (!is.null(expand_button)) {
  expand_button$clickElement()
  Sys.sleep(5)
}

# Explore the page and find css selector
css_selector <- "#table-container > div > table"

# Extract the table as a data frame
table_html <- remDr$findElement(using = "css selector", 
                                value = css_selector)$getElementAttribute("outerHTML")[[1]]
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
  filter(is.na(name)==FALSE) %>% 
  mutate(child = rownames(.))

# Looking at files table, identify the row(s) that contain the files you want to
# download

my_selection <- c(2,3,5)

my_files_table <- files_table[my_selection,]

# Downloading data
# table selector
table_selector <- "#table-container > div > table > tbody > tr"

# Find the rows in the table
table_rows <- remDr$findElements(using = "css selector", value = table_selector)

# Generate new version of table_rows
new_table_rows <- lapply(my_selection, function(i) {
  table_rows[[i]]
})

for (i in 1:length(new_table_rows)){
  
  row <- new_table_rows[[i]]
  
  download_pattern <- my_files_table[i,"file_name"]
  
  download_selector <- paste0("#table-container > div > table > tbody > tr:nth-child(",my_files_table$child[i],") > td.download-btn.btn-container > a")
  
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click()", list(download_button_element))
  
  # Wait for the download to complete
  while (length(list.files(path = downloads_folder, pattern = download_pattern)) == 0) {
    Sys.sleep(1)
  }
  
}

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()

# Reading files from downloads directory

# Get the current time
current_time <- Sys.time()

# Calculate the time threshold for file selection (1 hour ago)
time_threshold <- current_time - 60 * 60 * 2  # 60 seconds * 60 minutes * 2 = 2 hours
  
# Retrieve the zip files downloaded within the last hour
downloaded_files <- list.files(path = downloads_folder, full.names = TRUE, recursive = TRUE)
recent_files <- file.info(downloaded_files)$mtime > time_threshold

# Filter the downloaded files to keep only the recent ones
downloaded_files <- downloaded_files[recent_files]






















"#table-container > div > table > tbody > tr:nth-child(1) > td.download-btn.btn-container > a"

"#table-container > div > table > tbody > tr:nth-child(2) > td.download-btn.btn-container > a"

# Extract the table as a data frame
table_html <- remDr$findElement(using = "css selector", 
                                value = "#table-container")$getElementAttribute("outerHTML")[[1]]
table_df <- read_html(table_html) %>% html_table()

files_table <- as.data.frame(table_df[1]) 



# Extract the download links for each row
table_selector <- "#table-container > div > table > tbody"

table_rows <- remDr$findElements(using = "css selector", value = "#table-container > div > table > tbody > tr")

download_links <- lapply(1:length(table_rows), function(i) {
  # Construct the selector for the download link for the i-th row
  download_link_selector <- paste0("#table-container > div > table > tbody > tr:nth-child(", i, ") > td.download-btn.btn-container > a")
  # Extract the 'href' attribute of the download link
  download_link_element <- remDr$findElement(using = "css selector", value = download_link_selector)
  download_link_element$getElementAttribute("href")
})

# Printing table with download links
download_list <- as.data.frame(unlist(download_links))
colnames(download_list) <- "links"
download_table <- files_table %>% 
  select(name,file_type,size) %>% 
  cbind(download_list)

print(download_table)

# Saving download table for future reference

write.csv(download_table,paste(raw_data,"table_ess_dive_downloads_son_etal_22.csv", sep = '/'),
          row.names = FALSE)

# Downloading data

# Find the rows in the table
table_rows_a <- remDr$findElements(using = "css selector", value = table_selector)



# Decide whether you want to download the full data package or specific files. To specify
# the file(s) you want to download, write the row number below for their download button below.
# For, instance, the link for full download is on the first row, so you would write 

# `my_data_selection <- 1`. 

# In our example we are interested in downloading the files in the folder "model_inputs.zip".
# The download link for this zip folder is 2, so we write:

my_data_selection <- 2

# It is possible to download multiple files at the same time by using a loop function. But, for right now 
# for simplicity, we will let the users decide the best way to do so, and focus in one download at a time. 

# We will place our files in the downloads folder and extract them into a temporary
# directory to illustrate the options available for storing files. 









# Stop the Selenium server and close the browser
rmDriver





"#table-container > div > table > tbody > tr:nth-child(1) > td.download-btn.btn-container > a"

