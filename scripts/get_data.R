#### Preamble ####
# Author: Rohan Alexander
# Date: 21 December 2018
# Contact: rohan.alexander@anu.edu.au
# Purpose: Example code for scraping the RBA website.
# Dependencies: You need a CSV with a list of the URLs and filenames.


#### Workspace set-up ####
# install.packages("rvest")
library(rvest)
# install.packages("tidyverse")
library(tidyverse)


#### Load data ####
# Read in the list of the website addresses
data_to_scrape <- read_csv("inputs/addresses.csv")
address_to_visit <- data_to_scrape$address
save_name <- data_to_scrape$save_name


#### Create functions ####
# Create the function that will visit address_to_visit and save to save_name files ####
visit_address_and_save_content <-
  function(name_of_address_to_visit,
           name_of_file_to_save_as) {
    # The function takes two inputs
    read_html(name_of_address_to_visit) %>% # Go to the website and read the html
      html_node("#content") %>% # Find the content part
      html_text() %>% # Extract the text of the content part
      write_lines(name_of_file_to_save_as) # Save as a text file
    print(paste("Done with", name_of_address_to_visit, "at", Sys.time()))  # Helpful so that you know progress when running it on all the records
    Sys.sleep(sample(10:20, 1)) # Space out requests by somewhere between 10 and 20 seconds each so that we don't overwhelm their server
  }

# If there is an error then ignore it and move to the next one
visit_address_and_save_content <-
  safely(visit_address_and_save_content)


#### Scraping ####
# Walk through the addresses and apply the function to each
walk2(address_to_visit,
      save_name,
      ~ visit_address_and_save_content(.x, .y)) 
