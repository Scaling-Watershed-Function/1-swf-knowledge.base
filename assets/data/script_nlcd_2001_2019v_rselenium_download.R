###############################################################################
# Downloading National Land Cover Data v.2019 for the Year 2001
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

# Downloading data:

# To start downloading data, we first need to specify the url we want to navigate to

target_url <- "https://www.sciencebase.gov/catalog/item/5761b67de4b04f417c2d30ae"

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

# CAT CONUS Dataset

# Select the desired row (replace 1 with the desired row number)
row <- table_rows[[2]]

download_selector <- "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(2) > td:nth-child(1) > span"

# Find the download button element
download_button_element <- remDr$findElement(using = "css selector", 
                                             value = download_selector)

# Execute the JavaScript event attached to the element
remDr$executeScript("arguments[0].click()", list(download_button_element))

# Wait for the file to download
Sys.sleep(80)

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

# We need to filter this data using the enhanced NHDPv2

enh_nhd <- read_csv(paste(raw_data,"230423_enhanced_nhdp_2_yrb_wrb.csv", sep = '/'),
                    show_col_types = FALSE)

# Catchment level data
nlcd_cat_yrb_wrb <- enh_nhd %>% 
  select(comid,
         huc_4) %>% 
  merge(.,
        my_data,
        by.x = "comid",
        by.y = "COMID",
        all.x = TRUE)

# Delete the most recent file from the downloads folder
file.remove(most_recent_file)

write.csv(nlcd_cat_yrb_wrb,paste(raw_data,"230424_nlcd_cat_yrb_wrb.csv", sep = '/'),
          row.names = FALSE)

# TOT CONUS Dataset

# Select the desired row (replace 1 with the desired row number)
row <- table_rows[[4]]

download_selector <- "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(4) > td:nth-child(1) > span"

# Find the download button element
download_button_element <- remDr$findElement(using = "css selector", 
                                             value = download_selector)

# Execute the JavaScript event attached to the element
remDr$executeScript("arguments[0].click()", list(download_button_element))

# ATTENTION: There is a pop up that ask for permision to continue downloading multiple files

# Wait for the file to download
Sys.sleep(80)

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

# Watershed level data
nlcd_wsd_yrb_wrb <- enh_nhd %>% 
  select(comid,
         huc_4) %>% 
  merge(.,
        my_data,
        by.x = "comid",
        by.y = "COMID",
        all.x = TRUE)

# Delete the most recent file from the downloads folder
file.remove(most_recent_file)

write.csv(nlcd_wsd_yrb_wrb,paste(raw_data,"230424_nlcd_wsd_yrb_wrb.csv", sep = '/'),
          row.names = FALSE)

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()





############################### T E S T #######################################
# table selector
table_selector <- "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr"

# Find the rows in the table
table_rows <- remDr$findElements(using = "css selector", value = table_selector)

# Select desired row numbers
row_numbers <- c(2, 4)

# Generate new version of table_rows
new_table_rows <- lapply(row_numbers, function(i) {
  table_rows[[i]]
})

# Create an empty list to store the data frames for each file
file_dfs <- list()

# Iterate through the rows of the table
for (i in seq_along(new_table_rows)) {
  
  # Select the row
  row <- new_table_rows[[i]]
  
  # Find the download button element
  download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", 
                              row_numbers[i], ") > td:nth-child(1) > span")
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click()", list(download_button_element))
  
  # Check for pop-up message and click OK if it is present
  tryCatch({
    pop_up_selector <- "div.modal-footer > button.btn.btn-primary"
    pop_up_element <- remDr$findElement(using = "css selector", value = pop_up_selector)
    if (remDr$isVisible(pop_up_element)) {
      remDr$executeScript("arguments[0].click()", list(pop_up_element))
    }
  }, error = function(e) {
    # Do nothing if pop-up message is not present
  })
  
  # Wait for the files to download
  Sys.sleep(200)
  
  # Set the path to the downloads folder
  downloads_folder <- file.path(Sys.getenv("HOME"), "Downloads")
  
  # Find all the downloaded files
  downloaded_files <- list.files(downloads_folder, full.names = TRUE)
  
  # Set the path to the temporary directory
  temp_dir <- tempdir()
  
  # Create a list to store the temporary file paths
  temp_file_paths <- list()
  
  # Loop over the downloaded files and move them to the temporary directory
  for (file_path in downloaded_files) {
    
    # Get the name of the file
    file_name <- basename(file_path)
    
    # Create a unique temporary file name
    temp_file_name <- paste0(file_name, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
    
    # Create the full path to the file in the temporary directory
    temp_file_path <- file.path(temp_dir, temp_file_name)
    
    # Move the file to the temporary directory
    file.rename(file_path, temp_file_path)
    
    # Add the temporary file path to the list
    temp_file_paths <- c(temp_file_paths, temp_file_path)
    
  }
  
  # Loop over the temporary file paths and read the data
  for (temp_file_path in temp_file_paths) {
    
    # Extract the contents of the zip file to the temporary directory and read them
    file_dfs[[i]] <- c(file_dfs[[i]], read_csv(unzip(temp_file_path, exdir = temp_dir),
                                               show_col_types = FALSE))
    
  }
}



