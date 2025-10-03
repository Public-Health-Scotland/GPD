##########################################################
# Compare R and SPSS Outputs for Population Estimates
# Calum Purdie
# Original date - 25/04/2019
# Data release - Mid-year Council Area Population Estimates
# Latest update author - Calum Purdie
# Latest update date - 01/05/2020
# Latest update description - 2019 estimates
# Type of script - Comparison
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Comparing R and SPSS population estimate files
# Approximate run time - 3 minutes
##########################################################

### 1 - Information ----

library(tidyr)
library(dplyr)
library(stringr)
library(haven)
library(sjlabelled)
library(tidylog)
library(glue)
library(janitor)

# Set filepaths

SPSS_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                      "2_Population/Population Estimates/Lookup Files")
R_filepath <- glue("{SPSS_filepath}/R Files")

# Set years

start <- "1981"
end <- "2019"


### 2 - Compare Council Area Files ----

### 2.1 - Single year files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

CA2019_pop_est_SPSS <- read_sav(glue("{SPSS_filepath}/", 
                                               "CA2019_pop_est_{start}_{end}.sav"), 
                                          user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character) %>% 
  clean_names()

# Read in R file
# Remove name columns

CA2019_pop_est_R <- readRDS(glue("{R_filepath}/", 
                                  "CA2019_pop_est_{start}_{end}.rds")) %>% 
  select(-c(contains("name")))


# Compare files

all_equal(CA2019_pop_est_SPSS, CA2019_pop_est_R)



### 2.2 - 5 year age group files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

CA2019_pop_est_5y_SPSS <- read_sav(
  glue("{SPSS_filepath}/CA2019_pop_est_5year_agegroups_{start}_{end}.sav"), 
  user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character) %>% 
  clean_names()

# Read in R file
# Remove name columns

CA2019_pop_est_5y_R <- readRDS(
  glue("{R_filepath}/CA2019_pop_est_5year_agegroups_{start}_{end}.rds")) %>% 
  select(-c(contains("name")))

# Compare files

all_equal(CA2019_pop_est_5y_SPSS, CA2019_pop_est_5y_R)

rm(CA2019_pop_est_5y_R, CA2019_pop_est_5y_SPSS, 
   CA2019_pop_est_R, CA2019_pop_est_SPSS)



### 3 - Compare Health Board Files ----

### 3.1 - Single year files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

HB2019_pop_est_SPSS <- read_sav(glue("{SPSS_filepath}/", 
                                     "HB2019_pop_est_{start}_{end}.sav"), 
                                          user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character) %>% 
  clean_names()

# Read in R file
# Remove name columns

HB2019_pop_est_R <- readRDS(glue("{R_filepath}/", 
                                 "HB2019_pop_est_{start}_{end}.rds")) %>% 
  select(-c(contains("name")))

# Compare files

all_equal(HB2019_pop_est_SPSS, HB2019_pop_est_R)



### 3.2 - 5 year age group files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

HB2019_pop_est_5y_SPSS <- read_sav(
  glue("{SPSS_filepath}/HB2019_pop_est_5year_agegroups_{start}_{end}.sav"), 
  user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character) %>% 
  clean_names()

# Read in R file
# Remove name columns

HB2019_pop_est_5y_R <- readRDS(
  glue("{R_filepath}/HB2019_pop_est_5year_agegroups_{start}_{end}.rds")) %>% 
  select(-c(contains("name")))

# Compare files

all_equal(HB2019_pop_est_5y_SPSS, HB2019_pop_est_5y_R)

rm(HB2019_pop_est_SPSS, HB2019_pop_est_R, 
   HB2019_pop_est_5y_SPSS, HB2019_pop_est_5y_R)


### 4 - Compare HSCP Files ----

### 4.1 - Single year files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

HSCP2019_pop_est_SPSS <- read_sav(
  glue("{SPSS_filepath}/HSCP2019_pop_est_{start}_{end}.sav"), 
  user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character) %>% 
  clean_names()

# Read in R file
# Remove name columns

HSCP2019_pop_est_R <- readRDS(glue("{R_filepath}/", 
                                   "HSCP2019_pop_est_{start}_{end}.rds")) %>% 
  select(-c(contains("name")))

# Compare files

all_equal(HSCP2019_pop_est_SPSS, HSCP2019_pop_est_R)



### 4.2 - 5 year age group files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

HSCP2019_pop_est_5y_SPSS <- read_sav(
  glue("{SPSS_filepath}/HSCP2019_pop_est_5year_agegroups_{start}_{end}.sav"), 
  user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character) %>% 
  clean_names()

# Read in R file
# Remove name columns

HSCP2019_pop_est_5y_R <- readRDS(
  glue("{R_filepath}/HSCP2019_pop_est_5year_agegroups_{start}_{end}.rds")) %>% 
  select(-c(contains("name")))

# Compare files

all_equal(HSCP2019_pop_est_5y_SPSS, HSCP2019_pop_est_5y_R)
