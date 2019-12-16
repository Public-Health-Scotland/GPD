##########################################################
# Updating HSCP Locality Lookup
# Calum Purdie
# Original date 16/12/2019
# Latest update author - Calum Purdie
# Latest update date - 16/12/2019
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for updating HSCP Locality lookups
# Approximate run time - <1 second
##########################################################

### 1 Housekeeping ----

library(magrittr)
library(dplyr)
library(readr)
library(tidylog)
library(glue)
library(ckanr)
library(here)
library(janitor)

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/", 
                      "GPD/1_Geography/HSCPs and Localities/")

data_filepath <- glue("{base_filepath}/Source Data/20191216")

lookup_filepath <- glue("{base_filepath}/Lookup Files/Locality/", 
                        "Update in 2019-12") 

lookup <- "HSCP Localities_DZ11_Lookup_20191216" 


### 2 Read in csv file ----

locality_lookup <- read_csv(glue("{data_filepath}/{lookup}.csv")) %>% 
  clean_names() %>% 
  rename(hscp2019name = hscp2018name, 
         hb2019name = hb2018name)

# Add columns for hb2019name, hscp2019name, ca2019name, datazone2011name and 
# intzone2011name

# Use the Geography Codes and Names open data file to get the names
# First need to run the httr configuration script

source(here("Geography", "Scottish Postcode Directory", 
            "Set httr configuration for API.R"))

# Set url and id

ckan <- src_ckan("https://www.opendata.nhs.scot") 
res_id <- "395476ab-0720-4740-be07-ff4467141352" 

geo_names <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  select(DZ2011, DZ2011Name, CA2011, CA2011Name, HSCP2016, 
         HB2014) %>% 
  rename(data_zone2011 = DZ2011, data_zone2011name = DZ2011Name, 
         ca2019 = CA2011, ca2019name = CA2011Name, 
         hscp2019 = HSCP2016, hb2019 = HB2014) %>%  
  as_tibble()

# Create columns for ca2018 and ca2011

geo_names %<>% 
  mutate(ca2011 = recode(ca2019, 
                         "S12000047" = "S12000015", 
                         "S12000048" = "S12000024", 
                         "S12000049" = "S12000046", 
                         "S12000050" = "S12000044"), 
         ca2018 = recode(ca2019, 
                         "S12000049" = "S12000046", 
                         "S12000050" = "S12000044"))

# Match geo_names onto locality_lookup

locality_lookup %<>% 
  left_join(geo_names) %>% 
  select(data_zone2011, data_zone2011name, hscp_locality, hscp2019name, 
         hscp2019, hscp2018, hscp2016, hb2019name, hb2019, hb2018, hb2014, 
         ca2019name, ca2019, ca2018, ca2011)



### 3 Save files ----

saveRDS(locality_lookup, glue("{lookup_filepath}/{lookup}.rds"))

write_csv(locality_lookup, glue("{lookup_filepath}/{lookup}.csv"))
