##########################################################
# Compare R and SPSS Outputs for Postcode SIMD Files
# Calum Purdie
# Original date 06/09/2018
# Latest update author - Calum Purdie
# Latest update date - 02/12/2019
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for comparing R and SPSS files for postcode simd data
# Approximate run time - 5 minutes
##########################################################

### 1 - Information ----

# Read in libraries

library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(haven)
library(sjlabelled)
library(janitor)
library(glue)

# Set filepaths

spss_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD", "3_Deprivation", 
                           "Postcode Deprivation", "Lookup Files")
r_filepath <- file.path(spss_filepath, "R Files")

# Set files to use
pc_simd <- "postcode_2019_2_simd2016"
pc_all <- "postcode_2019_2_all_simd_carstairs"

### 2 - postcode_simd ----

# Read in SPSS file and sort by pc7
# Remove variable labels, formats and widths from SPSS
# Haven reads in SPSS strings as factors
# Convert all factors to characters in the SPSS file

pc_simd_spss <- read_sav(glue("{spss_filepath}/{pc_simd}.sav"), 
                         user_na=F) %>%
  arrange(pc7) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character)

# Read in R file and sort by pc7
# Remove geography name columns

pc_simd_r <- readRDS(glue("{r_filepath}/{pc_simd}.rds")) %>% 
  arrange(pc7) %>% 
  select(-c(DataZone2011Name, IntZone2011Name, HB2019Name, HSCP2019Name, 
            CA2019Name))

# Compare datasets
all_equal(pc_simd_spss, pc_simd_r)



### 3 - postcode_all_simd_carstairs ----

# Read in SPSS file and sort by pc7
# Remove variable labels, formats and widths from SPSS
# Change blank cells in the Data Zone columns to be NA
# Haven reads in SPSS strings as factors
# Convert all factors to characters in the SPSS file
# R reads in some of the decimals in the SPSS file incorrectly which make it 
# look like R and SPSS don't match
# Occasionally there is a tiny difference between two values, 
# e.g. a difference of n^-15
# This is because R uses floating point precision: 
# https://floating-point-gui.de/basic/
# Round all values to 2 decimal places using round_half_up

pc_all_spss <- read_sav(glue("{spss_filepath}/{pc_all}.sav")) %>%
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

pc_all_r <- readRDS(glue("{r_filepath}/{pc_all}.rds")) %>% 
  arrange(pc7) %>% 
  select(-DataZone2011Name) %>% 
  mutate_if(is.numeric, round_half_up, 2)

all_equal(pc_all_spss, pc_all_r)
