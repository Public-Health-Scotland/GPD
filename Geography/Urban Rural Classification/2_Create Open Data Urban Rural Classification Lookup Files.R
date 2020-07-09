##########################################################
# Create Open Data Urban Rural Classification Lookup Files
# Calum Purdie
# Original date 24/10/2019
# Latest update author - Calum Purdie
# Latest update date - 08/07/2020
# Latest update description - formatting code
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code to create Urban Rural Classification lookup files for the 
# Scottish Health and Social Care Open Data platform
# Approximate run time - <1 second
##########################################################


### 1 - Housekeeping ----

# Read in packages

library(dplyr)
library(readr)
library(tidylog)
library(glue)

# Set filepaths

od_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Publications/", 
                    "Open Data (Non Health Topic)/Data/OD1800009 - Urban Rural")

lookups_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/", 
                         "GPD/1_Geography/Urban Rural Classification/", 
                         "Lookup Files/R Files")

# Set date for filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")


### 2 - Data Zone Urban Rural Lookup ----

# Read in DataZone2011_urban_rural_2016.rds from the lookups filepath
# Select the relevant columns - drop the UR name columns
# Rename columns to fit open data standards

DZ2011_UR_2016 <- readRDS(glue("{lookups_filepath}/", 
                               "DataZone2011_urban_rural_2016.rds")) %>% 
  select(DataZone2011, UR2_2016, UR3_2016, UR6_2016, UR8_2016) %>% 
  rename(DataZone = DataZone2011, 
         UrbanRural2fold2016 = UR2_2016, 
         UrbanRural3fold2016 = UR3_2016, 
         UrbanRural6fold2016 = UR6_2016, 
         UrbanRural8fold2016 = UR8_2016)

# Write csv to open data folder
# Update file name with the date the file is saved

write_csv(DZ2011_UR_2016, 
          glue("{od_filepath}/datazone2011_urban_rural_2016_{date}.csv"))
