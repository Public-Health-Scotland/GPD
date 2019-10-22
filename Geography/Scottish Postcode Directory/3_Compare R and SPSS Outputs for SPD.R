### 1 - Information ----

# Codename - Compare R and SPSS Outputs for Scottish Postcode Directory
# Data release - Scotish Postcode Directory
# Original Author - Calum Purdie
# Original Date - 06/09/2018
# Type - Check
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("haven")
# install.packages("sjlabelled")
#
# Description - This document is based on the SPSS syntax for comparing R and SPSS output for 
#               Small Area Population Estimates found within GPD folders. It is designed to allow for the same data 
#               checks to be made. Each section of the SPSS syntax is contained in this file within different
#               subsections.
#
# Approximate run time - 5 minutes

# Set working directory
SPSS_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD", "1_Geography", "Scottish Postcode Directory", "Lookup Files")
R_filepath <- file.path(SPSS_filepath, "R files")

# Read in packages from library
library(tidyr)
library(dplyr)
library(stringr)
library(haven)
library(sjlabelled)
library(janitor)

### 2 - Reading in files ----

# Read in SPSS file and sort by pc7
# Remove variable labels, formats and widths from SPSS

SPD_SPSS <- read_sav(file.path(SPSS_filepath, "Scottish_Postcode_Directory_2019_2.sav"), user_na=F) %>%
  arrange(pc7) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels()

# Read in R file and sort by pc7

SPD_R <- readRDS(file.path(R_filepath, "Scottish_Postcode_Directory_2019_2.rds")) %>% 
  arrange(pc7) %>% 
  select(-c(HB2019Name, HSCP2019Name, CA2019Name, IntZone2011Name, DataZone2011Name, 
            UR2_2016_name, UR3_2016_name, UR6_2016_name, UR8_2016_name))

### 3 - Data Manipulation ----

# Haven reads in SPSS strings as factors
# Convert all factors to characters in the SPSS file

SPD_SPSS <- SPD_SPSS %>% 
  mutate_if(is.factor, as.character)

# Calum found an issue with matching Latitude/Longitude columns in the SPSS file with the R file
# R seemed to read in the SPSS file as having extra decimals and very slightly wrong, e.g. 1.28546246499999999999 rather than 1.285452465
# This meant that the R and SPSS files did not show as equal
# Round Latitude and Longitude columns to 9 decimal places using the round2 function (the base R round() function does not work)

SPD_SPSS[, 17:18] <- round_half_up(SPD_SPSS[, 17:18], 9)

# Change some SPSS columns to numeric to match with R
SPD_SPSS <- SPD_SPSS %>% 
  mutate(Grid_Reference_Easting = as.numeric(Grid_Reference_Easting), 
         Grid_Reference_Northing = as.numeric(Grid_Reference_Northing), 
         LGD_1995 = as.numeric(LGD_1995), 
         LGD_1991 = as.numeric(LGD_1991))

# Set all blank cells as NA in SPSS file
cols <- sapply(SPD_SPSS, class) != 'Date'
SPD_SPSS[cols] <- lapply(SPD_SPSS[cols],  function(x) replace(x, which(x==''), NA))

# Set all blank cells as NA in R file
cols <- sapply(SPD_R, class) != 'Date'
SPD_R[cols] <- lapply(SPD_R[cols],  function(x) replace(x, which(x==''), NA))

# Set all integer columns as numeric in R file
cols <- sapply(SPD_R, class) == 'integer'
SPD_R[cols] <- lapply(SPD_R[cols],  as.numeric)

### 4 - Compare datasets ----

# Compare datasets
all_equal(SPD_SPSS, SPD_R)

