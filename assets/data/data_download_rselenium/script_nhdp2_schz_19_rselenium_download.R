###############################################################################
# ENHDPlusV2_us: Ancillary Attributes and Modified Routing for NHDPlus Version 
# 2.1 Flowlines - Schwarz et al., 2018
###############################################################################

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

target_url <- "https://www.sciencebase.gov/catalog/item/5d16509ee4b0941bde5d8ffe"

# Navigate to URL
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

enh_nhd2_pnw <- filter(my_data, huc_4==1703 | huc_4==1709)

enh_nhd2_wil <- filter(my_data, huc_4=="0107")

enh_nhd2_swf <- rbind(enh_nhd2_pnw,enh_nhd2_wil)

# write.csv(nhd2_pnw,paste(raw_data,"230423_main_nhdp_2_yrb_wrb.csv", sep = '/'),
#           row.names = FALSE)

write.csv(enh_nhd2_swf,paste(raw_data,"230620_main_nhdp_2_swf.csv", sep = '/'),
          row.names = FALSE)

# Delete the most recent file from the downloads folder
file.remove(most_recent_file)

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()
  















