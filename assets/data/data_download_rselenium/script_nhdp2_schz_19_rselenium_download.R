###############################################################################
# Downloading NHDPlus Version 2.1 Data
###############################################################################

# Pre-requisites:
# Make sure you have ran the file "script_system_prep_rselenium.R"

# Local Import-Export

raw_data <- "raw"
processed_data <- "processed"

# Loading/installing required libraries
librarian::shelf(tidyverse,
                 RSelenium,
                 netstat,
                 wdman,
                 rvest,
                 data.table,
                 utils)

# Opening a Selenium client-server object
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "112.0.5615.49",
                             verbose = FALSE,
                             port = free_port())
remDr <- rs_driver_object$client
remDr$close() #This will close the first browser that is not needed for webscrapping.

# Downloading data example:

# To start downloading data, we first need to specify the url we want to navigate to

target_url <- "https://www.sciencebase.gov/catalog/item/5d16509ee4b0941bde5d8ffe"

# Open a client browser for webscrapping
remDr$open()
remDr$navigate(target_url)
Sys.sleep(5) # Wait for the page to load

# Explore the page and find css selector

css_selector <- "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table"

# Extract the table as a data frame
table_html <- remDr$findElement(using = "css selector", 
                                value = css_selector)$getElementAttribute("outerHTML")[[1]]
table_df <- read_html(table_html) %>% html_table()

files_table <- as.data.frame(table_df[1]) 

colnames(files_table) <- c("dataset",
                           "view",
                           "size",
                           "file_type")
# Downloading data
# table selector
table_selector <- "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr"

# Find the rows in the table
table_rows <- remDr$findElements(using = "css selector", value = table_selector)

# Select the desired row (replace 1 with the desired row number)
row <- table_rows[[2]]

download_selector <- "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(2) > td:nth-child(1) > span.sb-file-get.sb-download-link"

# Find the download button element
download_button_element <- remDr$findElement(using = "css selector", 
                                             value = download_selector)

# Execute the JavaScript event attached to the element
remDr$executeScript("arguments[0].click()", list(download_button_element))

# Wait for the file to download
Sys.sleep(5)

# Set the path to the downloads folder
downloads_folder <- file.path(Sys.getenv("HOME"), "Downloads")

# Find the most recent file in the downloads folder
downloaded_files <- list.files(downloads_folder, full.names = TRUE)
most_recent_file <- downloaded_files[which.max(file.info(downloaded_files)$mtime)]

# Set the path to the temporary directory
temp_dir <- tempdir()

# Get the name of the file
file_name <- basename(most_recent_file)

# Create the full path to the file in the temporary directory
temp_file_path <- file.path(temp_dir, file_name)

# Move the file to the temporary directory
file.rename(most_recent_file, temp_file_path)

# Extract the contents of the zip file to the temporary directory and read them
my_data <- read_csv(unzip(temp_file_path, exdir = temp_dir),
                    show_col_types = FALSE)

my_data$huc_4 <- substr(my_data$REACHCODE, start = 1, stop = 4)

nhd2_pnw <- filter(my_data, huc_4 ==1703 | huc_4==1709)

# write.csv(nhd2_pnw,paste(raw_data,"230423_main_nhdp_2_yrb_wrb.csv", sep = '/'),
#           row.names = FALSE)

# Delete the most recent file from the downloads folder
file.remove(most_recent_file)

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()
  















