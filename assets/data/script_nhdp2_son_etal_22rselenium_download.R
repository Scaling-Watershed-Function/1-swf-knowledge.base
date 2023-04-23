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

################################################################################



# Find the expand button and click on it (if exists)
expand_button <- tryCatch(
  remDr$findElement(using = "css selector", 
                    value = css_selector),
  error = function(e) NULL
)

if (!is.null(expand_button)) {
  expand_button$clickElement()
  Sys.sleep(5)
}















# Extract the download links for each row
table_rows <- remDr$findElements(using = "css selector", 
                                 value = css_selector)
table_selector <- "> tbody > tr.sb-original-metadata-row"

download_links <- lapply(1:length(table_rows), function(i) {
  # Construct the selector for the download link for the i-th row
  download_link_selector <- paste(css_selector,table_selector, "td:nth-child(", i, ") > span.sb-file-get.sb-download-link")
  # Extract the 'href' attribute of the download link
  download_link_element <- remDr$findElement(using = "css selector", value = download_link_selector)
  download_link_element$getElementAttribute("data-url")
})


# Using xpath instead of css selector
download_links <- lapply(1:length(table_rows), function(i) {
  # Construct the XPath for the download link for the i-th row
  download_link_xpath <- paste('//*[@id="attached-files-section"]/div[1]/div/div[1]/div[1]/table/tbody/tr[', i, ']/td[1]/span[1]')
  # Extract the 'data-url' attribute of the download link
  download_link_element <- remDr$findElement(using = "xpath", value = download_link_xpath)
  download_link_element$getElementAttribute("data-url")
})

download_links <- lapply(1:length(table_rows), function(i) {
  # Construct the XPath for the download link for the i-th row
  download_link_xpath <- paste0('//*[@id="table-container"]/div/table/tbody/tr[', i + 1, ']/td[2]/a')
  # Extract the 'href' attribute of the download link
  download_link_element <- remDr$findElement(using = "xpath", value = download_link_xpath)
  if (is.null(download_link_element)) {
    return(NA)
  }
  download_link <- download_link_element$getElementAttribute("href")
  if (is.null(download_link)) {
    return(NA)
  }
  download_link
})

#Printing table with download links
download_list <- as.data.frame(unlist(download_links))
colnames(download_list) <- "links"
download_table <- files_table %>% 
  select(name,file_type,size) %>% 
  cbind(download_list)

print(download_table)


#selector = "#attached-files-section > div > div > div.sb-expander-content > div.table-responsive > table > tbody > tr.sb-original-metadata-row > td:nth-child(1) > span.sb-file-get.sb-download-link"
#xpath = "//*[@id="attached-files-section"]/div/div/div[1]/div[1]/table/tbody/tr[1]/td[1]/span[1]"

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


