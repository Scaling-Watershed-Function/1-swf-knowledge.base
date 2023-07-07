###############################################################################
# Downloading National Hydrography Dataset (Flowlines) version 2.1
###############################################################################

# Pre-requisites:
# Make sure you have ran the file "script_system_prep_RSelenium.R"

# Local Import-Export
gc()
raw_data <- "../raw"
processed_data <- "../processed"


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
                 methods,
                 R.utils,
                 sp,
                 sf,
                 leaflet)

# Set path to downloads folder
downloads_folder <- if (Sys.getenv("OS") == "Windows_NT") {
  file.path("C:/Users", Sys.getenv("USERNAME"), "Downloads")
} else {
  file.path(Sys.getenv("HOME"), "Downloads")
}

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

rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "latest",
                             verbose = FALSE,
                             port = free_port(),
                             extraCapabilities = chrome_options)


# Open a client browser for webscrapping
remDr <- rs_driver_object$client

# To start downloading data, we first need to specify the url we want to navigate to

################################################################################
# Willamette River Basin
################################################################################

target_url <- "https://pnnl-gis.maps.arcgis.com/home/item.html?id=3815f01c8e054ac38919b222ce2ba52c"

# Navigate to your target url
remDr$navigate(target_url)

# Finding the download button
download_selector <- "#main-content-area > div.js-item-pane.item-pane.item-pane--active > aside > div > button.text-ellipsis.trailer-half.btn.btn-fill.if-file.if-download.js-action-download.js-open-btn.js-telemetry-primary-action"

download_button_element <- remDr$findElement(using = "css selector", value = download_selector)

# Execute the JavaScript event attached to the element
remDr$executeScript("arguments[0].click()", list(download_button_element))

###############################################################################
# Yakima River Basin
###############################################################################

target_url <- "https://pnnl-gis.maps.arcgis.com/home/item.html?id=65d94941e865408e93523581dc043d55"

# Navigate to your target url
remDr$navigate(target_url)

# Finding the download button
download_selector <- "#main-content-area > div.js-item-pane.item-pane.item-pane--active > aside > div > button.text-ellipsis.trailer-half.btn.btn-fill.if-file.if-download.js-action-download.js-open-btn.js-telemetry-primary-action"

download_button_element <- remDr$findElement(using = "css selector", value = download_selector)

# Execute the JavaScript event attached to the element
remDr$executeScript("arguments[0].click()", list(download_button_element))

################################################################################
# Retrieving the data
################################################################################

retrieve_data <- function(downloads_folder, destination_folder) {
  # Get the current time
  current_time <- Sys.time()
  
  # Calculate the time threshold for file selection (2 hours ago)
  time_threshold <- current_time - 60 * 60 * 2  # 60 seconds * 60 minutes * 2 = 2 hours
  
  # Retrieve the zip files downloaded within the last 2 hours
  downloaded_files <- list.files(path = downloads_folder, full.names = TRUE, pattern = ".zip$", recursive = TRUE)
  recent_files <- file.info(downloaded_files)$mtime > time_threshold
  
  # Filter the downloaded files to keep only the recent ones
  downloaded_files <- downloaded_files[recent_files]
  
  temp_dir <- tempdir()
  
  extracted_files <- vector("list", length(downloaded_files))
  
  for (i in seq_along(downloaded_files)) {
    file_path <- downloaded_files[i]
    
    # Extract the file name without the extension
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Create a new directory with the file name
    new_dir <- file.path(temp_dir, file_name)
    dir.create(new_dir)
    
    # Unzip the file into the new directory
    unzip(file_path, exdir = new_dir)
    
    # Get the extracted file paths
    extracted_files[[i]] <- list.files(path = new_dir, full.names = TRUE)
    
    # Move the files to the destination folder
    for (extracted_file in extracted_files[[i]]) {
      new_file_path <- file.path(destination_folder, basename(extracted_file))
      file.rename(extracted_file, new_file_path)
    }
    
    # Move the downloaded file to the temporary directory
    file.rename(file_path, file.path(temp_dir, basename(file_path)))
    
    # Print the file names within the folder
    cat("Files in", file_name, "folder:\n")
    cat(paste0(extracted_files[[i]], "\n"), sep = "")
    cat("\n")
  }
  
  return(extracted_files)
}

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()

# Set the path for the destination folder
destination_folder <- paste(raw_data, "shape_files", "nhdplus_21", sep = '/')

# Use the function above to retrieve the data and move it to the destination folder:
extracted_data <- retrieve_data(downloads_folder, destination_folder)
