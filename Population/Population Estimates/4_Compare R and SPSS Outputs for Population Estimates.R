### 1 - Information ----

# Codename - Compare R and SPSS Outputs for Population Estimates
# Data release - 2018 mid-year population estimates
# Original Author - Calum Purdie
# Original Date - 25/04/2019
# Updated - 05/11/2019
# Type - Check
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("haven")
# install.packages("sjlabelled")
# install.packages("tidylog")
# install.packafes("glue")
#
# Description - Comparing R and SPSS population estimate files
#
# Approximate run time - 5 minutes

# Read in packages from library

library(tidyr)
library(dplyr)
library(stringr)
library(haven)
library(sjlabelled)
library(tidylog)
library(glue)

# Set filepaths

SPSS_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD", "2_Population", 
                           "Population Estimates", "Lookup Files")
R_filepath <- file.path(SPSS_filepath, "R Files")



### 2 - Compare Council Area Files ----

### 2.1 - Single year files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

CA2019_pop_est_1981_2018_SPSS <- read_sav(glue("{SPSS_filepath}/", 
                                               "CA2019_pop_est_1981_2018.sav"), 
                                          user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character)

# Read in R file
# Remove CA2019Name and SexName columns

CA2019_pop_est_1981_2018_R <- readRDS(glue("{R_filepath}/", 
                                           "CA2019_pop_est_1981_2018.rds")) %>% 
  select(-c(CA2019Name, SexName))


# Compare files

all_equal(CA2019_pop_est_1981_2018_R, CA2019_pop_est_1981_2018_SPSS)



### 2.2 - 5 year age group files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

CA2019_pop_est_5y_1981_2018_SPSS <- read_sav(
  glue("{SPSS_filepath}/CA2019_pop_est_5year_agegroups_1981_2018.sav"), 
       user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character)

# Read in R file
# Remove CA2019Name and SexName columns

CA2019_pop_est_5y_1981_2018_R <- readRDS(
  glue("{R_filepath}/CA2019_pop_est_5year_agegroups_1981_2018.rds")) %>% 
  select(-c(CA2019Name, SexName, AgeGroupName))

# Compare files

all_equal(CA2019_pop_est_5y_1981_2018_SPSS, CA2019_pop_est_5y_1981_2018_R)



### 3 - Compare Health Board Files ----

### 3.1 - Single year files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

HB2019_pop_est_1981_2018_SPSS <- read_sav(glue("{SPSS_filepath}/", 
                                               "HB2019_pop_est_1981_2018.sav"), 
                                               user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character)

# Read in R file
# Remove HB2019Name and SexName columns

HB2019_pop_est_1981_2018_R <- readRDS(glue("{R_filepath}/", 
                                           "HB2019_pop_est_1981_2018.rds")) %>% 
  select(-c(HB2019Name, SexName))

# Compare files

all_equal(HB2019_pop_est_1981_2018_R, HB2019_pop_est_1981_2018_SPSS)



### 3.2 - 5 year age group files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

HB2019_pop_est_5y_1981_2018_SPSS <- read_sav(
  glue("{SPSS_filepath}/HB2019_pop_est_5year_agegroups_1981_2018.sav"), 
       user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character)

# Read in R file
# Remove HB2019Name and SexName columns

HB2019_pop_est_5y_1981_2018_R <- readRDS(
  glue("{R_filepath}/HB2019_pop_est_5year_agegroups_1981_2018.rds")) %>% 
  select(-c(HB2019Name, SexName, AgeGroupName))

# Compare files

all_equal(HB2019_pop_est_5y_1981_2018_R, HB2019_pop_est_5y_1981_2018_SPSS)




### 4 - Compare HSCP Files ----

### 4.1 - Single year files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

HSCP2019_pop_est_1981_2018_SPSS <- read_sav(
  glue("{SPSS_filepath}/HSCP2019_pop_est_1981_2018.sav"), 
  user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character)

# Read in R file
# Remove HSCP2019Name and SexName columns

HSCP2019_pop_est_1981_2018_R <- readRDS(glue("{R_filepath}/", 
                                             "HSCP2019_pop_est_1981_2018.rds")) %>% 
  select(-c(HSCP2019Name, SexName))

# Compare files

all_equal(HSCP2019_pop_est_1981_2018_R, HSCP2019_pop_est_1981_2018_SPSS)



### 4.2 - 5 year age group files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

HSCP2019_pop_est_5y_1981_2018_SPSS <- read_sav(
  glue("{SPSS_filepath}/HSCP2019_pop_est_5year_agegroups_1981_2018.sav"), 
  user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character)

# Read in R file
# Remove HSCP2019Name and SexName columns

HSCP2019_pop_est_5y_1981_2018_R <- readRDS(
  glue("{R_filepath}/HSCP2019_pop_est_5year_agegroups_1981_2018.rds")) %>% 
  select(-c(HSCP2019Name, SexName, AgeGroupName))

# Compare files

all_equal(HSCP2019_pop_est_5y_1981_2018_R, HSCP2019_pop_est_5y_1981_2018_SPSS)
