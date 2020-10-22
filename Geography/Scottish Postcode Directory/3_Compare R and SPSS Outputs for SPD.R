##########################################################
# Compare R and SPSS Outputs for Scottish Postcode Directory
# Calum Purdie
# Original date 07/08/2018
# Data release - Scottish Postcode Directory
# Latest update author - Calum Purdie
# Latest update date - 19/06/2020
# Latest update description - 2020_1 update
# Type of script - Comparison
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for comparing R and SPSS files for Scottish Postcode Directory
# Approximate run time - 2 minutes
##########################################################

### 1 - Housekeeping ----

# Read in packages from library

library(magrittr)
library(dplyr)
library(stringr)
library(haven)
library(sjlabelled)
library(janitor)
library(tidylog)
library(glue)
library(fs)

# Set working directory

SPSS_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                      "1_Geography/Scottish Postcode Directory/Lookup Files")
R_filepath <- glue("{SPSS_filepath}/R files")





### 2 - Reading in files ----

# Read in SPSS file and sort by pc7
# Remove variable labels, formats and widths from SPSS
# Haven reads in SPSS strings as factors
# Convert all factors to characters in the SPSS file

SPD_SPSS <- dir_ls(glue("{SPSS_filepath}/"), regexp = ".sav$") %>%
  read_sav(user_na = F) %>% 
  arrange(pc7) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  clean_names() %>% 
  mutate_if(is.factor, as.character) %>% 
  rename(datazone2001 = data_zone2001, 
         datazone2011 = data_zone2011, 
         intzone2001 = int_zone2001, 
         intzone2011 = int_zone2011)

# Read in R file and sort by pc7

SPD_R <- dir_ls(glue("{R_filepath}/"), regexp = ".rds$") %>%
  readRDS() %>% 
  arrange(pc7) %>% 
  select(-c(hb2019name, hscp2019name, ca2019name, intzone2011name, 
            datazone2011name, ur2_2016_name, ur3_2016_name, ur6_2016_name, 
            ur8_2016_name))



### 3 - Data Manipulation ----

# Calum found an issue with matching Latitude/Longitude columns in the 
# SPSS file with the R file
# R seemed to read in the SPSS file as having extra decimals and very slightly 
# wrong, e.g. 1.28546246499999999999 rather than 1.285452465
# This meant that the R and SPSS files did not show as equal
# Round latitude and longitude columns to 2 decimal places using round_half_up()

SPD_SPSS %<>% 
  mutate(latitude = round_half_up(latitude, 2), 
         longitude = round_half_up(longitude, 2))

# Change some SPSS columns to numeric to match with R
# Set all blank cells and cells with character "NA" as NA in SPSS file

SPD_SPSS %<>% 
  mutate(grid_reference_easting = as.numeric(grid_reference_easting), 
         grid_reference_northing = as.numeric(grid_reference_northing), 
         lgd_1995 = as.numeric(lgd_1995), 
         lgd_1991 = as.numeric(lgd_1991)) %>%  
  mutate_if(is.character, list(~na_if(., ""))) %>% 
  mutate_if(is.character, list(~na_if(., "NA"))) %>% 
  mutate(locality_2001 = str_pad(locality_2001, 6, pad = "0"), 
         settlement_2001 = str_pad(settlement_2001, 3, pad = "0"), 
         locality_1991 = str_pad(locality_1991, 3, pad = "0"), 
         islands_2020 = str_pad(islands_2020, 3, pad = "0"))

# Change all integer columns to numeric in R file
# Round latitude and longitude columns to 2 decimal places using round_half_up()
SPD_R %<>%
  mutate_if(is.integer, as.numeric) %>% 
  mutate(latitude = round_half_up(latitude, 2), 
         longitude = round_half_up(longitude, 2))


### 4 - Compare datasets ----

# Compare datasets

all_equal(SPD_SPSS, SPD_R)
