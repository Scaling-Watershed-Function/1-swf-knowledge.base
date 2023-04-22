###############################################################################
# Downloading data Son et al 2022 Data from ESS-DIVE
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

target_url <- "https://data.ess-dive.lbl.gov/view/doi:10.15485/1962818"

# Open a client browser for webscrapping
remDr$open()
remDr$navigate(target_url)
Sys.sleep(5) # Wait for the page to load


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

# Saving download table

write.csv(download_table,paste(raw_data,"table_ess_dive_downloads_son_etal_22.csv", sep = '/'),
          row.names = FALSE)

# Downloading files into a temporary directory

download_table <- read_csv(paste(raw_data,"table_ess_dive_downloads_son_etal_22.csv", sep = '/'),
                       show_col_types = FALSE)


# Model inputs
my_data_selection = 9
my_download_url <- download_table$links[my_data_selection]
mod_inp <- tempfile(pattern = 'model_inputs',
                     tmpdir = tempdir(),
                     fileext = '.zip')
download.file(url = my_download_url, 
              mod_inp,
              timeout = max(300, getOption("timeout")))

temp <- tempfile()
download.file(url = my_download_url, 
              temp,
              timeout = max(300, getOption("timeout")))
a <- read.table(unz(temp))


# Model outputs
my_data_selection = 5
my_download_url <- download_table$links[my_data_selection]
mod_out <- tempfile(pattern = 'model_outputs',
                    tmpdir = tempdir(),
                    fileext = '.zip')
download.file(my_download_url, mod_out, quiet = TRUE)



