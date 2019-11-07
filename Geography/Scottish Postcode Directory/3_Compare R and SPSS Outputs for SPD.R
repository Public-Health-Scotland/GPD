### 1 - Information ----

# Codename - Compare R and SPSS Outputs for Scottish Postcode Directory
# Data release - Scotish Postcode Directory
# Original Author - Calum Purdie
# Original Date - 06/09/2018
# Updated - 04/11/2019
# Type - Comparison
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("magrittr")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("haven")
# install.packages("sjlabelled")
# install.packages("janitor")
# install.packages("tidylog")
# install.packages("glue")
# install.packages("fs")
#
# Description - Code for comparing R and SPSS files for 
#               Scottish Postcode Directory
#
# Approximate run time - 5 minutes

# Set working directory

SPSS_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD", "1_Geography", 
                           "Scottish Postcode Directory", "Lookup Files")
R_filepath <- file.path(SPSS_filepath, "R files")

# Read in packages from library

library(magrittr)
library(tidyr)
library(dplyr)
library(stringr)
library(haven)
library(sjlabelled)
library(janitor)
library(tidylog)
library(glue)
library(fs)



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
  mutate_if(is.factor, as.character)

# Read in R file and sort by pc7

SPD_R <- dir_ls(glue("{R_filepath}/"), regexp = ".rds$") %>%
  readRDS() %>% 
  arrange(pc7) %>% 
  select(-c(HB2019Name, HSCP2019Name, CA2019Name, IntZone2011Name, 
            DataZone2011Name, UR2_2016_name, UR3_2016_name, UR6_2016_name, 
            UR8_2016_name))



### 3 - Data Manipulation ----

# Calum found an issue with matching Latitude/Longitude columns in the 
# SPSS file with the R file
# R seemed to read in the SPSS file as having extra decimals and very slightly 
# wrong, e.g. 1.28546246499999999999 rather than 1.285452465
# This meant that the R and SPSS files did not show as equal
# Round Latitude and Longitude columns to 9 decimal places using the round2 
# function (the base R round() function does not work)

# Change some SPSS columns to numeric to match with R
# Set all blank cells as NA in SPSS file

SPD_SPSS %<>% 
  mutate(Latitude = round_half_up(Latitude, 9), 
         Longitude = round_half_up(Longitude, 9))


SPD_SPSS %<>% 
  mutate(Grid_Reference_Easting = as.numeric(Grid_Reference_Easting), 
         Grid_Reference_Northing = as.numeric(Grid_Reference_Northing), 
         LGD_1995 = as.numeric(LGD_1995), 
         LGD_1991 = as.numeric(LGD_1991)) %>% 
  mutate_if(is.character, list(~na_if(., "")))


# Set all blank cells as NA in R file
# Set all integer columns as numeric in R file

SPD_R %<>%
  mutate_if(is.character, list(~na_if(., ""))) %>% 
  mutate_if(is.integer, as.numeric)



### 4 - Compare datasets ----

# Compare datasets

all_equal(SPD_SPSS, SPD_R)

