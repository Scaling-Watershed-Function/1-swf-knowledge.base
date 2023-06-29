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
target_url <- "https://data.ess-dive.lbl.gov/view/doi:10.15485/1971251"

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

# Extract the download links for each row
table_rows <- remDr$findElements(using = "css selector", value = "#table-container > div > table > tbody > tr")

download_links <- lapply(1:length(table_rows), function(i) {
  # Construct the selector for the download link for the i-th row
  download_link_selector <- paste0("#table-container > div > table > tbody > tr:nth-child(", i, ") > td.download-btn.btn-container > a")
  # Extract the 'href' attribute of the download link
  download_link_element <- remDr$findElement(using = "css selector", value = download_link_selector)
  download_link_element$getElementAttribute("href")
})

# Stop the Selenium server and close the browser
rs_driver_object$server$stop()

#Printing table with download links
download_list <- as.data.frame(unlist(download_links))
colnames(download_list) <- "links"
download_table <- files_table %>% 
  select(name,file_type,size) %>% 
  cbind(download_list)

print(download_table)


# Downloading files

# Getting subset table with links only

links_list <- download_table[,4]



# Saving download table

write.csv(download_table,paste(raw_data,"table_ess_dive_downloads_son_etal_22.csv", sep = '/'),
          row.names = FALSE)


"#table-container > div > table > tbody > tr:nth-child(1) > td.download-btn.btn-container > a"

