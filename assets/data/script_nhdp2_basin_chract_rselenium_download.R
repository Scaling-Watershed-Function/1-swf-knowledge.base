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

my_row <- 6

row <- new_table_rows[[my_row]]

# Find the download button element

# Set the path to the downloads folder
downloads_folder <- file.path(Sys.getenv("HOME"), "Downloads")

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

downloaded_files <- list.files(path = downloads_folder, full.names =  TRUE)

temp_dir <- tempdir()

bsn_chr_acc_dat <- unzip(downloaded_files[1],
                         exdir = temp_dir) %>% 
  read_delim(.,show_col_types = FALSE)

bsn_chr_cat_dat <- unzip(downloaded_files[2],
                         exdir = temp_dir) %>% 
  read_delim(.,show_col_types = FALSE)
  
bsn_chr_tot_dat <- unzip(downloaded_files[3],
                         exdir = temp_dir) %>% 
  read_delim(.,show_col_types = FALSE)

bsn_chr_sin_dat <- unzip(downloaded_files[5],
                         exdir = temp_dir) %>% 
  read_delim(.,show_col_types = FALSE)

bsn_chr_dns_dat <- unzip(downloaded_files[6],
                         exdir = temp_dir) %>% 
  read_delim(.,show_col_types = FALSE)

# Combining all data into a single one:

bsn_chr_data <- bsn_chr_sin_dat %>% 
  merge(.,
        bsn_chr_dns_dat,
        by.x = "COMID",
        by.y = "COMID",
        all.x = TRUE) %>% 
  merge(.,
        bsn_chr_acc_dat,
        by.x = "COMID",
        by.y = "COMID",
        all.x = TRUE) %>% 
  merge(.,
        bsn_chr_cat_dat,
        by.x = "COMID",
        by.y = "COMID",
        all.x = TRUE) %>% 
  merge(.,
        bsn_chr_tot_dat,
        by.x = "COMID",
        by.y = "COMID",
        all.x = TRUE)

# Filtering for YRB and WRB
enh_dat <- read_csv(paste(raw_data,"230423_enhanced_nhdp_2_yrb_wrb.csv", sep = '/'),
         show_col_types = FALSE)

bsn_chr_pnw_data <- enh_dat %>% 
  select(comid) %>% 
  merge(.,
        bsn_chr_data,
        by.x = "comid",
        by.y = "COMID",
        all.x = TRUE)

# Saving the dataset as a raw datafile
write.csv(bsn_chr_pnw_data,paste(raw_data,"230427_pnw_basin_characteristics.csv", sep = '/'),
          row.names = FALSE)

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()


















  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



