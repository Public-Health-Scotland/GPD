##########################################################
# Compare R and SPSS Outputs for Postcode SIMD Files
# Calum Purdie
# Original date 06/09/2018
# Latest update author - Calum Purdie
# Latest update date - 20/05/2020
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for comparing R and SPSS files for postcode simd data
# Approximate run time - 1 minutes
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

spss_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                      "3_Deprivation/Postcode Deprivation/Lookup Files")
r_filepath <- glue("{spss_filepath}/R Files")

# Set version

version <- "2020_2"

# Set files to use

pc_simd <- glue("postcode_{version}_simd2020v2")
pc_all <- glue("postcode_{version}_all_simd_carstairs")



### 2 - postcode_simd ----

# Read in SPSS file and sort by pc7
# Remove variable labels, formats and widths from SPSS
# Haven reads in SPSS strings as factors
# Convert all factors to characters in the SPSS file

pc_simd_spss <- read_sav(glue("{spss_filepath}/{pc_simd}.sav"), 
                         user_na=F) %>%
  clean_names() %>% 
  rename(datazone2011 = data_zone2011, intzone2011 = int_zone2011) %>% 
  arrange(pc7) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(vars(dplyr::contains("_quintile")), as.integer) %>% 
  mutate_at(vars(dplyr::contains("_decile")), as.integer)

# Read in R file and sort by pc7
# Remove geography name columns

pc_simd_r <- readRDS(glue("{r_filepath}/{pc_simd}.rds")) %>% 
  arrange(pc7) %>% 
  select(-c(datazone2011name, intzone2011name, ca2019name, hscp2019name, 
            hb2019name))

# Compare datasets
all_equal(pc_simd_spss, pc_simd_r)

rm(pc_simd_spss, pc_simd_r)



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
  clean_names() %>% 
  rename(datazone2001_simd2004 = data_zone2001_simd2004, 
         datazone2001_simd2006 = data_zone2001_simd2006,
         datazone2001_simd2009v2 = data_zone2001_simd2009v2,
         datazone2001_simd2012 = data_zone2001_simd2012,
         datazone2011_simd2016 = data_zone2011_simd2016, 
         datazone2011_simd2020v2 = data_zone2011_simd2020v2) %>% 
  arrange(pc7) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %<>%
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, list(~na_if(., ""))) %>% 
  mutate_if(is.numeric, round_half_up, 2)

# Read in R file and sort by pc7
# Remove dz2011name column as this is not in the SPSS file
# Round all values to 2 decimal places using round_half_up
# This changes integers to numeric, change them back to integers

pc_all_r <- readRDS(glue("{r_filepath}/{pc_all}.rds")) %>% 
  arrange(pc7) %>% 
  select(-datazone2011name) %>% 
  mutate_if(is.numeric, round_half_up, 2)

all_equal(pc_all_spss, pc_all_r)
