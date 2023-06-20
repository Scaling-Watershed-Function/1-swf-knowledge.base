###############################################################################
# Data downloads from sciencebase.gov
###############################################################################

#Author: Francisco J. Guerrero

#Date-created: 06-16-2023

# Pre-requisites:
# Make sure you have ran the file "script_system_prep_RSelenium.R"

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
# Downloading data
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

################################################################################
# Attributes for NHDPlus Version 2.1: Select Basin Characteristics
# Weiczeroek et al., 2018 (ver. 3.0 Jan-2021)
################################################################################

# Set a target url
target_url <- "https://www.sciencebase.gov/catalog/item/57976a0ce4b021cadec97890"

# Navigate to your target URL
remDr$navigate(target_url)

# Wait for the page to load
Sys.sleep(5)

# Explore the page and find the CSS selector
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
         size_MB = if_else(size_unit == "KB", parse_number(size) / 1000, parse_number(size)))

my_selection <- c(1, 2, 4, 5, 6, 11)

my_files_table <- files_table[my_selection,]

# table selector
table_selector <- "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr"

# Find the rows in the table
table_rows <- remDr$findElements(using = "css selector", value = table_selector)

# Generate new version of table_rows
new_table_rows <- lapply(my_selection, function(i) {
  table_rows[[i]]
})

for (my_row in 1:length(new_table_rows)) {
  row <- new_table_rows[[my_row]]
  
  download_pattern <- my_files_table[my_row, "file_name"]
  
  download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", my_files_table$child[my_row], ") > td:nth-child(1) > span")
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click(); window.confirm = function(message) { return true; };", list(download_button_element))
  
  # Wait for the download to complete
  while (length(list.files(path = downloads_folder, pattern = download_pattern)) == 0) {
    Sys.sleep(1)
  }
}

################################################################################
# Attributes for NHDPlus Version 2.1: Bankfull Hydraulic Geometry
# Weiczeroek et al., 2018 (ver. 3.0 Jan-2021)
################################################################################

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

# table selector
table_selector <- "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr"

# Find the rows in the table
table_rows <- remDr$findElements(using = "css selector", value = table_selector)

# Generate new version of table_rows
new_table_rows <- lapply(my_selection, function(i) {
  table_rows[[i]]
})

for (my_row in 1:length(new_table_rows)) {
  row <- new_table_rows[[my_row]]
  
  download_pattern <- my_files_table[my_row, "file_name"]
  
  download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", my_files_table$child[my_row], ") > td:nth-child(1) > span")
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click(); window.confirm = function(message) { return true; };", list(download_button_element))
  
  # Wait for the download to complete
  while (length(list.files(path = downloads_folder, pattern = download_pattern)) == 0) {
    Sys.sleep(1)
  }
}

################################################################################
# ENHDPlusV2_us: Ancillary Attributes and Modified Routing for NHDPlus Version 
# 2.1 Flowlines - Schwarz et al., 2018
################################################################################

target_url <- "https://www.sciencebase.gov/catalog/item/5d16509ee4b0941bde5d8ffe"

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

# table selector
table_selector <- "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr"

# Find the rows in the table
table_rows <- remDr$findElements(using = "css selector", value = table_selector)

# Generate new version of table_rows
new_table_rows <- lapply(my_selection, function(i) {
  table_rows[[i]]
})


for (my_row in 1:length(new_table_rows)) {
  row <- new_table_rows[[my_row]]
  
  download_pattern <- my_files_table[my_row, "file_name"]
  
  download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", my_files_table$child[my_row], ") > td:nth-child(1) > span")
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click(); window.confirm = function(message) { return true; };", list(download_button_element))
  
  # Wait for the download to complete
  while (length(list.files(path = downloads_folder, pattern = download_pattern)) == 0) {
    Sys.sleep(1)
  }
}

###############################################################################
# Downloading NHDPlus Version 2.1 Data (Enhanced Network Connectivity)
###############################################################################

target_url <- "https://www.sciencebase.gov/catalog/item/63cb311ed34e06fef14f40a3"

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

# table selector
table_selector <- "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr"

# Find the rows in the table
table_rows <- remDr$findElements(using = "css selector", value = table_selector)

# Generate new version of table_rows
new_table_rows <- lapply(my_selection, function(i) {
  table_rows[[i]]
})

for (my_row in 1:length(new_table_rows)) {
  row <- new_table_rows[[my_row]]
  
  download_pattern <- my_files_table[my_row, "file_name"]
  
  download_selector <- paste0("#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr:nth-child(", my_files_table$child[my_row], ") > td:nth-child(1) > span")
  download_button_element <- remDr$findElement(using = "css selector", value = download_selector)
  
  # Execute the JavaScript event attached to the element
  remDr$executeScript("arguments[0].click(); window.confirm = function(message) { return true; };", list(download_button_element))
  
  # Wait for the download to complete
  while (length(list.files(path = downloads_folder, pattern = download_pattern)) == 0) {
    Sys.sleep(1)
  }
}

################################################################################
# Reading files from downloads directory
################################################################################

retrieve_and_merge_data <- function(downloads_folder) {
  current_time <- Sys.time()
  time_threshold <- current_time - 60 * 60 * 2
  
  downloaded_files <- list.files(path = downloads_folder, full.names = TRUE, pattern = "\\.(zip|csv)$", recursive = TRUE)
  recent_files <- file.info(downloaded_files)$mtime > time_threshold
  downloaded_files <- downloaded_files[recent_files]
  
  temp_dir <- tempdir()
  
  extract_and_read <- function(file) {
    file_name <- tools::file_path_sans_ext(basename(file))
    new_dir <- file.path(temp_dir, file_name)
    dir.create(new_dir)
    
    if (tolower(tools::file_ext(file)) == "zip") {
      unzip(file, exdir = new_dir)
      extracted_file <- list.files(path = new_dir, pattern = "\\.csv$", full.names = TRUE)
      file_extension <- "csv"
    } else {
      extracted_file <- file
      file_extension <- tools::file_ext(extracted_file)
    }
    
    if (tolower(file_extension) %in% c("txt", "csv")) {
      data <- readr::read_delim(extracted_file, show_col_types = FALSE)
    } else {
      data <- read_csv(extracted_file, show_col_types = FALSE)
    }
    
    data
  }
  
  # extracted_data <- lapply(downloaded_files, extract_and_read)
  # 
  # comid_column <- grep("(?i)comid", names(extracted_data[[1]]), ignore.case = TRUE, value = TRUE, perl = TRUE)
  # 
  # if (length(comid_column) > 0) {
  #   comid_column <- comid_column[1]  # Select the first matched column
  #   
  #   for (i in seq_along(extracted_data)) {
  #     col_names <- names(extracted_data[[i]])
  #     matching_columns <- grep(comid_column, col_names, ignore.case = TRUE, value = TRUE, perl = TRUE)
  #     if (length(matching_columns) > 0) {
  #       names(extracted_data[[i]])[matching_columns] <- "COMID"
  #     }
  #   }
  # }
  # 
  # bsn_chr_data <- Reduce(function(x, y) merge(x, y, by = "COMID", all.x = TRUE), extracted_data)
  # 
  # return(bsn_chr_data)
  
  extracted_data <- lapply(downloaded_files, extract_and_read)
  
  comid_column <- grep("(?i)comid", names(extracted_data[[1]]), ignore.case = TRUE, value = TRUE, perl = TRUE)
  
  if (length(comid_column) > 0) {
    comid_column <- comid_column[1]  # Select the first matched column
    
    for (i in seq_along(extracted_data)) {
      col_names <- names(extracted_data[[i]])
      matching_columns <- grep(comid_column, col_names, ignore.case = TRUE, value = TRUE, perl = TRUE)
      if (length(matching_columns) > 0) {
        names(extracted_data[[i]])[matching_columns] <- "COMID"
      }
    }
  }
  
  bsn_chr_data <- Reduce(function(x, y) merge(x, y, by = intersect(names(x), names(y)), all.x = TRUE), extracted_data)
  
  
}


# Stop the Selenium server and close the browser
rs_driver_object$server$stop()

# Creating combined dataset of basin characteristics
bsn_chr_dat <- retrieve_and_merge_data(paste(downloads_folder))










# Reading files from downloads directory

downloaded_files <- list.files(path = downloads_folder, full.names =  TRUE)

temp_dir <- tempdir()

bsn_chr_cat_dat <- unzip(downloaded_files[1],
                         exdir = temp_dir) %>% 
  read_delim(.,show_col_types = FALSE)

bsn_chr_tot_dat <- unzip(downloaded_files[2],
                         exdir = temp_dir) %>% 
  read_delim(.,show_col_types = FALSE)

# bsn_chr_tot_dat <- unzip(downloaded_files[4],
#                          exdir = temp_dir) %>% 
#   read_delim(.,show_col_types = FALSE)

bsn_chr_sin_dat <- unzip(downloaded_files[5],
                         exdir = temp_dir) %>% 
  read_delim(.,show_col_types = FALSE)

bsn_chr_dns_dat <- unzip(downloaded_files[6],
                         exdir = temp_dir) %>% 
  read_delim(.,show_col_types = FALSE)

