### 1 - Information ----

# Codename - Compare R and SPSS Outputs for Postcode Deprivation Files
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

# Read in packages from library
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(haven)
library(sjlabelled)
library(janitor)

# Set working directory
SPSS_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD", "3_Deprivation", "Postcode Deprivation", "Lookup Files")
R_filepath <- file.path(SPSS_filepath, "R Files")


### 2 - postcode_simd ----

### 2.1 - Reading in files ----

# Read in SPSS file and sort by pc7
# Remove variable labels, formats and widths from SPSS
# Haven reads in SPSS strings as factors
# Convert all factors to characters in the SPSS file

pc_SIMD_SPSS <- read_sav(file.path(SPSS_filepath, "postcode_2019_2_simd2016.sav"), user_na=F) %>%
  arrange(pc7) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character)

# Read in R file and sort by pc7
# Remove geography name columns

pc_SIMD_R <- readRDS(file.path(R_filepath, "postcode_2019_2_simd2016.rds")) %>% 
  arrange(pc7) %>% 
  select(-c(DataZone2011Name, IntZone2011Name, HB2019Name, HSCP2019Name, CA2019Name))

### 2.2 - Compare datasets ----

# Compare datasets
all_equal(pc_SIMD_SPSS, pc_SIMD_R)



### 3 - postcode_all_simd_carstairs ----

### 3.1 - Reading in files ----

# Read in SPSS file and sort by pc7
# Remove variable labels, formats and widths from SPSS
# Change blank cells in the Data Zone columns to be NA
# Haven reads in SPSS strings as factors
# Convert all factors to characters in the SPSS file
# R reads in some of the decimals in the SPSS file incorrectly which make it look like R and SPSS don't match
# Occasionally there is a tiny difference between two values, e.g. a difference of n^-15
# This is because R uses floating point precision: https://floating-point-gui.de/basic/
# Round all values to 2 decimal places using round_half_up

all_SIMD_SPSS <- read_sav(file.path(SPSS_filepath, "postcode_2019_2_all_simd_carstairs.sav")) %>%
  arrange(pc7) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate(DataZone2001_simd2004 = na_if(DataZone2001_simd2004, ""), 
         DataZone2001_simd2006 = na_if(DataZone2001_simd2006, ""),
         DataZone2001_simd2009v2 = na_if(DataZone2001_simd2009v2, ""),
         DataZone2001_simd2012 = na_if(DataZone2001_simd2012, ""),
         DataZone2011_simd2016 = na_if(DataZone2011_simd2016, "")) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.numeric, round_half_up, 2)

# Read in R file and sort by pc7
# Remove DataZone2011Name column as this is not in the SPSS file
# Round all values to 2 decimal places using round_half_up

all_SIMD_R <- readRDS(file.path(R_filepath, "postcode_2019_2_all_simd_carstairs.rds")) %>% 
  arrange(pc7) %>% 
  select(-DataZone2011Name) %>% 
  mutate_if(is.numeric, round_half_up, 2)

### 3.2 - Compare datasets ----

all_equal(all_SIMD_SPSS, all_SIMD_R)

