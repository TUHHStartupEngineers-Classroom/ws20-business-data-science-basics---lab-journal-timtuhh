library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(magrittr)

url_home          <- "https://www.rosebikes.de/fahrräder/rennrad"
# Read in the HTML for the entire webpage
html         <- read_html(url_home)
names <- html_nodes(html, ".catalog-category-bikes__title-text") %>%
    html_text() %>%
    str_replace("\n", "") %>%
    str_remove(pattern = ".\n")
  
#length(names)
#names

prices2 <- html_nodes(html,".catalog-category-bikes__price-title") %>%
  html_text() %>%
  str_replace("\n", "") %>%
  str_remove(pattern = ".\n")


#length(prices2)
#prices2

bike_table <- tibble(names,prices2)
bike_table
