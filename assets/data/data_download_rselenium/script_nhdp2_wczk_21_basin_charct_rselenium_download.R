###############################################################################
# Downloading NHDPlus Version 2.1 Data (Select Basin Characteristics)
###############################################################################
 gc()
# Pre-requisites:
# If running for the first time,
# make sure you have ran the file "script_system_prep_RSelenium.R"

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
                 utils, 
                 readr,
                 xml2, 
                 methods,
                 R.utils)


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


# Open Selenium Server
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "latest",
                             verbose = FALSE,
                             port = free_port(),
                             extraCapabilities = chrome_options)

# Open a client browser for webscrapping
remDr <- rs_driver_object$client

# Set a target url
target_url <- "https://www.sciencebase.gov/catalog/item/57976a0ce4b021cadec97890"

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
  mutate(file_name = sub("\\s.*", "", .$dataset),
         file_extension = sub("^.*\\.", "", file_name),
         child = rownames(.),
         size_unit = sub("[^[:alpha:]]+", "", size),
         size_MB = if_else(size_unit=="KB",parse_number(size)/1000,parse_number(size)))

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

for (i in 1:length(new_table_rows)){
  
  row <- new_table_rows[[i]]
  
  download_pattern <- my_files_table[i,"file_name"]
  
  # download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", 
  #                             my_files_table$child[i], ") > ",td:nth-child(i), "> span")
  
  download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(",my_files_table$child[i],") > td:nth-child(1) > span")
  
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click()", list(download_button_element))
  
  # Wait for the download to complete
  while (length(list.files(path = downloads_folder, pattern = download_pattern)) == 0) {
    Sys.sleep(1)
  }

}

###############################################################################
# Downloading Bankfull Hydraulic Geometry Data
###############################################################################

target_url <- "https://www.sciencebase.gov/catalog/item/5cf02bdae4b0b51330e22b85"

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
  mutate(file_name = sub("\\s.*", "", .$dataset),
         file_extension = sub("^.*\\.", "", file_name),
         child = rownames(.),
         size_unit = sub("[^[:alpha:]]+", "", size),
         size_MB = if_else(size_unit=="KB",parse_number(size)/1000,parse_number(size)))

my_selection <- 2

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

download_pattern <- my_files_table[my_row,"file_name"]


download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", 
                            my_files_table$child[my_row], ") > td:nth-child(1) > span")
download_button_element <- remDr$findElement(using = "css selector", value = download_selector)

# Execute the JavaScript event attached to the element
remDr$executeScript("arguments[0].click()", list(download_button_element))

# Wait for the download to complete
while (length(list.files(path = downloads_folder, pattern = download_pattern)) == 0) {
  Sys.sleep(1)
}

# Reading files from downloads directory

retrieve_and_merge_data <- function(downloads_folder) {
  # Get the current time
  current_time <- Sys.time()
  
  # Calculate the time threshold for file selection (1 hour ago)
  time_threshold <- current_time - 60 * 60 * 2  # 60 seconds * 60 minutes * 2 = 2 hours
  
  # Retrieve the zip files downloaded within the last hour
  downloaded_files <- list.files(path = downloads_folder, full.names = TRUE, pattern = ".zip$", recursive = TRUE)
  recent_files <- file.info(downloaded_files)$mtime > time_threshold
  
  # Filter the downloaded files to keep only the recent ones
  downloaded_files <- downloaded_files[recent_files]
  
  temp_dir <- tempdir()
  
  extract_and_read <- function(file) {
    # Extract the file name without the extension
    file_name <- tools::file_path_sans_ext(basename(file))
    
    # Create a new directory with the file name
    new_dir <- file.path(temp_dir, file_name)
    dir.create(new_dir)
    
    # Unzip the file into the new directory
    unzip(file, exdir = new_dir)
    
    # Get the extracted file path
    extracted_file <- list.files(path = new_dir, full.names = TRUE)
    
    # Determine the file extension
    file_extension <- tools::file_ext(extracted_file)
    
    # Read the contents of the extracted file based on the file extension
    if (tolower(file_extension) %in% c("txt", "csv")) {
      # Read delimited or CSV file
      data <- readr::read_delim(extracted_file, show_col_types = FALSE)
    } else {
      data <- read_csv(extracted_file, show_col_types = FALSE)
    }

    # # Read the contents of the extracted file
    # read_delim(extracted_file, show_col_types = FALSE)
  }
  
  # Extract and read the contents of the recent zip files
  extracted_data <- lapply(downloaded_files, extract_and_read)
  
  # Merge the extracted data into a single dataframe
  bsn_chr_data <- Reduce(function(x, y) merge(x, y, by = "COMID", all.x = TRUE), extracted_data)
  
  return(bsn_chr_data)
}

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()

bsn_chr_dat <- retrieve_and_merge_data("C:/Users/guer310/Downloads")
bsn_chr_dat <- bsn_chr_dat %>% 
  rename(comid = COMID)

# Merging with Blodgett (2023) and Weiczeroek (2021)

hydro_dat_swf <- read_csv(paste(raw_data,"230620_enhanced_nhdp_2_swf.csv",sep = '/'),
                          show_col_types = FALSE)

wczk21_dat <- hydro_dat_swf %>% 
  select(comid) %>% 
  merge(.,bsn_chr_dat,
        by = "comid",
        all.x = TRUE)

write.csv(wczk21_dat,paste(raw_data,"230620_swf_basin_characteristics.csv", sep = '/'),
           row.names = FALSE)
