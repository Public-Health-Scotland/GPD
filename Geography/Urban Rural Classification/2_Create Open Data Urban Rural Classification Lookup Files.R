### 1 - Information ----

# Codename - Create Open Data Urban Rural Classification Lookup Files
# Original Author - Calum Purdie
# Original Date - 24/10/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("dplyr")
# install.packages("readr")
# install.packages("tidylog")
#
# Description - Code to create Urban Rural Classification lookup files for the 
#               NHSScotland Open Data platform
# Approximate run time - <1 second

od_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                         "Open Data (Non Health Topic)", "Data", "OD1800009 - Urban Rural")
lookups_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", 
                              "GPD", "1_Geography", "Urban Rural Classification", "Lookup Files", 
                              "R Files")


# Read in packages from library

library(dplyr)
library(readr)
library(tidylog)


### 2 - Data Zone Urban Rural Lookup ----

# Read in DataZone2011_urban_rural_2016.rds from the lookups filepath
# Select the relevant columns - drop the UR name columns
# Rename columns to fit open data standards

DZ2011_UR_2016 <- readRDS(file.path(lookups_filepath, "DataZone2011_urban_rural_2016.rds")) %>% 
  select(DataZone2011, UR2_2016, UR3_2016, UR6_2016, UR8_2016) %>% 
  rename(DZ2011 = DataZone2011, 
         UrbanRural2fold2016 = UR2_2016, 
         UrbanRural3fold2016 = UR3_2016, 
         UrbanRural6fold2016 = UR6_2016, 
         UrbanRural8fold2016 = UR8_2016)

# Write csv to open data folder
# Update file name with the date the file is saved

write_csv(DZ2011_UR_2016, file.path(lookups_filepath, "datazone2011_urban_rural_2016_241019.csv"))