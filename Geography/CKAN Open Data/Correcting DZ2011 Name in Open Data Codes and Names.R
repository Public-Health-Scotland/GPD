##########################################################
# Updating DZ2011 Names in Open Data Codes and Names_ONE_OFF
# Calum Purdie
# Original date 07/01/2020
# Latest update author - Calum Purdie
# Latest update date - 07/01/2020
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for updating DZ2011 Names in Open Data Codes and Names
# Approximate run time - <1 second
##########################################################

### 1 - Housekeeping ----

library(magrittr)
library(dplyr)
library(readr)
library(tidylog)
library(janitor)
library(glue)
library(here)
library(ckanr)

# Open Data filepath

od_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                         "Open Data (Non Health Topic)", "Data", 
                         "OD1700008 - Geography Codes")

filename <- "geography_codes_and_labels_DZ2011"

# Set date for open data filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")

### 2 - Recoding ----

# Need to recode Data Zone S01009365
# Name should be Pattiesmuir not Pettiesmuir

# Use the Geography Codes and Names open data file to get the names
# First need to run the httr configuration script

source(here("Geography", "Scottish Postcode Directory", 
            "Set httr configuration for API.R"))

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id <- "395476ab-0720-4740-be07-ff4467141352"

geo_names <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  as_tibble() %>% 
  mutate(DZ2011Name = recode(DZ2011Name, "Pettiesmuir" = "Pattiesmuir"))

# Save the updated file in the Open Data folder

write_csv(geo_names, glue("{od_filepath}/{filename}_{date}.csv"))
