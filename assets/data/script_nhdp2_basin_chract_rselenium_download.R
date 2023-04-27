###############################################################################
# Downloading NHDPlus Version 2.1 Data (Select Basin Characteristics)
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
                 utils, 
                 readr,
                 xml2, 
                 methods)
# Set a target url
target_url <- "https://www.sciencebase.gov/catalog/item/57976a0ce4b021cadec97890"

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
      "profile.content_settings.exceptions.automatic_downloads.*.setting" = 1
    )
  )
)


rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "112.0.5615.49",
                             verbose = FALSE,
                             port = free_port(),
                             extraCapabilities = chrome_options)

# Open a client browser for webscrapping
remDr <- rs_driver_object$client

# Navigate to your target url
remDr$navigate(target_url)

# Wait for the page to load
Sys.sleep(5) 

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

files_table <- files_table %>% 
  mutate(child = rownames(.),
         size_unit = sub("[^[:alpha:]]+", "", size),
         size_MB = if_else(size_unit=="KB",parse_number(size)/1000,parse_number(size)),
         sleep_time = (size_MB/0.75)+5)

my_selection <- c(1,2,4,5,6,11)

my_files_table <- files_table[my_selection,]
  
# Downloading data
# table selector
table_selector <- "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr"

# Find the rows in the table
table_rows <- remDr$findElements(using = "css selector", value = table_selector)

# Generate new version of table_rows
new_table_rows <- lapply(my_selection, function(i) {
  table_rows[[i]]
})

my_row <- 1

row <- new_table_rows[[my_row]]

# Find the download button element
download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", 
                            my_files_table$child[my_row], ") > td:nth-child(1) > span")
download_button_element <- remDr$findElement(using = "css selector", value = download_selector)

# Execute the JavaScript event attached to the element
remDr$executeScript("arguments[0].click()", list(download_button_element))

# Wait for the files to download
sleep_time <- my_files_table[i,]$sleep_time

Sys.sleep(sleep_time)

# Set the path to the downloads folder
downloads_folder <- file.path(Sys.getenv("HOME"), "Downloads")

# Find all the downloaded files
downloaded_files <- list.files(downloads_folder, full.names = TRUE)

# Set the path to the temporary directory
temp_dir <- tempdir()





















# Create an empty list to store the data frames for each file
# file_dfs <- list()

file_dfs <- list()

# Iterate through the rows of the table
for (i in seq_along(new_table_rows)) {
  
  # Select the row
  row <- new_table_rows[[i]]
  
  # Find the download button element
  download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", 
                              my_files_table$child[i], ") > td:nth-child(1) > span")
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click()", list(download_button_element))
  
  # Wait for the files to download
  sleep_time <- my_files_table[i,]$sleep_time
  
  Sys.sleep(sleep_time)
  
  # Set the path to the downloads folder
  downloads_folder <- file.path(Sys.getenv("HOME"), "Downloads")
  
  # Find all the downloaded files
  downloaded_files <- list.files(downloads_folder, full.names = TRUE)
  
  # Set the path to the temporary directory
  temp_dir <- tempdir()
  
  # Create a list to store the temporary file paths
  temp_file_paths <- list()
  
  # Get the name of the file
  file_name <- basename(file_path)
  
  # Extract file extension
  file_ext <- tolower(tools::file_ext(file_name))
  
  # Create a unique temporary file name
  temp_file_name <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"),"_",file_name)
  
  # Create the full path to the file in the temporary directory
  temp_file_path <- file.path(temp_dir, temp_file_name)
  
  # Move the file to the temporary directory
  file.rename(file_path, temp_file_path)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Loop over the downloaded files and move them to the temporary directory
  for (file_path in downloaded_files) {
    
    # Get the name of the file
    file_name <- basename(file_path)
    
    # Extract file extension
    file_ext <- tolower(tools::file_ext(file_name))
    
    # Create a unique temporary file name
    temp_file_name <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"),"_",file_name)
    
    # Create the full path to the file in the temporary directory
    temp_file_path <- file.path(temp_dir, temp_file_name)
    
    # Move the file to the temporary directory
    file.rename(file_path, temp_file_path)
    
    # Add the temporary file path to the list
    temp_file_paths <- c(temp_file_paths, temp_file_path)
    
  }
}











# Metadata files

# Select the table row
my_row <- 2

row <- new_table_rows[[my_row]]

# Find the download button element
download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", 
                            my_files_table$child[my_row], ") > td:nth-child(1) > span")
download_button_element <- remDr$findElement(using = "css selector", value = download_selector)

# Execute the JavaScript event attached to the element
remDr$executeScript("arguments[0].click()", list(download_button_element))

sleep_time <- my_files_table[my_row,]$sleep_time

# Wait for the files to download
Sys.sleep(sleep_time)

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










# Create an empty list to store the data frames for each file
# file_dfs <- list()

file_dfs <- as.list(rep(NULL, length(new_table_rows)))

# Iterate through the rows of the table
for (i in seq_along(new_table_rows)) {
  
  # Select the row
  row <- new_table_rows[[i]]
  
  # Find the download button element
  download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", 
                              my_files_table$child[i], ") > td:nth-child(1) > span")
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click()", list(download_button_element))
  
  # Wait for the files to download
  sleep_time <- my_files_table[i,]$sleep_time

  Sys.sleep(sleep_time)
  
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
    temp_file_name <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"),"_",file_name)
    
    # Create the full path to the file in the temporary directory
    temp_file_path <- file.path(temp_dir, temp_file_name)
    
    # Move the file to the temporary directory
    file.rename(file_path, temp_file_path)
    
    # Add the temporary file path to the list
    temp_file_paths <- c(temp_file_paths, temp_file_path)
    
  }
  
  # Loop over the temporary file paths and extract the data
  for (temp_file_path in temp_file_paths) {

    # Extract the contents of the zip file to the temporary directory and read them
    file_dfs[[i]] <- c(file_dfs[[i]],
                       if (tolower(file_ext) == "zip") {
                         unzip(temp_file_path, exdir = temp_dir)
                       },temp_file_path, exdir = temp_dir)

  }
}

##################################################################################
# Iterate through the rows of the table
for (i in seq_along(new_table_rows)) {
  
  # Select the row
  row <- new_table_rows[[i]]
  
  # Find the download button element
  download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", 
                              my_files_table$child[i], ") > td:nth-child(1) > span")
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click()", list(download_button_element))
  
  # Wait for the file to download
  Sys.sleep(80)
  
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
    file_dfs[[i]] <- c(file_dfs[[i]], unzip(temp_file_path, exdir = temp_dir))
    
  }
  
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
  
}




##################################################################################
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
my_data <- read_csv(temp_file_path, 
                    show_col_types = FALSE)

my_data$huc_4 <- substr(my_data$reachcode, start = 1, stop = 4)

enh_nhd2_pnw <- filter(my_data, huc_4 ==1703 | huc_4==1709)

write.csv(enh_nhd2_pnw,paste(raw_data,"230423_enhanced_nhdp_2_yrb_wrb.csv", sep = '/'),
          row.names = FALSE)

# Delete the most recent file from the downloads folder
file.remove(most_recent_file)

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()
