###############################################################################
# Downloading data from websites with RSelenium
###############################################################################

librarian::shelf(tidyverse,
                 RSelenium,
                 netstat)

# Online tutorials: 
#1.Checking for Java (Windows):
# Click on the the start icon (window on bottom left)
# On the search bar write "cmd" to open the control comand console
# Once there, type java -version
# If you get the message: 'java is not recognized as an internal
# or external command...' You have to install Java first.
# To do so, follow the instructions in this tutorial:
# 1. Installing Java: https://www.youtube.com/watch?v=IJ-PJbvJBGs
# 2. Intro to RSeleniumhttps://www.youtube.com/watch?v=U1BrIPmhx10
# 3. Downloading documents from the web: https://www.youtube.com/watch?v=BK_JBk_l5uQ


rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "112.0.5615.49",
                             verbose = FALSE,
                             port = free_port())


rs_driver_object <- rsDriver(port =free_port(),
                              browser = "chrome",
                              chromever = "latest",
                              iedrver = NULL,
                              verbose = FALSE,
                              check = FALSE)


rs_driver_object2 <- rsDriver(
                      port = free_port(),
                      browser = c("chrome", "firefox", "phantomjs", "internet explorer"),
                      version = "latest",
                      chromever = "latest",
                      geckover = "latest",
                      iedrver = NULL,
                      phantomver = "2.1.1",
                      verbose = FALSE,
                      check = TRUE)





