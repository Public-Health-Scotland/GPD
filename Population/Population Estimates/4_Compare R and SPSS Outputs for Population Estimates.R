### 1 - Information ----

# Codename - Compare R and SPSS Outputs for Population Estimates
# Data release - 2018 mid-year population estimates
# Original Author - Calum Purdie
# Original Date - 25/04/2019
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
setwd("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Population Estimates/Lookup Files/")

# Read in packages from library
library(tidyr)
library(dplyr)
library(stringr)
library(haven)
library(sjlabelled)


### 2 - Compare Council Area Files ----

### 2.1 - Single year files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters
CA2019_pop_est_1981_2018_SPSS <- read_sav("CA2019_pop_est_1981_2018.sav", user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate(CA2019 = as.character(CA2019), 
         CA2018 = as.character(CA2018), 
         CA2011 = as.character(CA2011))

# Read in R file
# Remove CA2019Name and SexName columns
CA2019_pop_est_1981_2018_R <- readRDS("R Files/CA2019_pop_est_1981_2018.rds") %>% 
  select(-c(CA2019Name, SexName))

all_equal(CA2019_pop_est_1981_2018_R, CA2019_pop_est_1981_2018_SPSS)


### 2.2 - 5 year age group files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters
CA2019_pop_est_5year_agegroups_1981_2018_SPSS <- read_sav("CA2019_pop_est_5year_agegroups_1981_2018.sav", user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate(CA2019 = as.character(CA2019), 
         CA2018 = as.character(CA2018), 
         CA2011 = as.character(CA2011))

# Read in R file
# Remove CA2019Name and SexName columns
CA2019_pop_est_5year_agegroups_1981_2018_R <- readRDS("R Files/CA2019_pop_est_5year_agegroups_1981_2018.rds") %>% 
  select(-c(CA2019Name, SexName, AgeGroupName))

all_equal(CA2019_pop_est_5year_agegroups_1981_2018_R, CA2019_pop_est_5year_agegroups_1981_2018_SPSS)


### 3 - Compare Health Board Files ----

### 3.1 - Single year files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters
HB2019_pop_est_1981_2018_SPSS <- read_sav("HB2019_pop_est_1981_2018.sav", user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate(HB2019 = as.character(HB2019), 
         HB2018 = as.character(HB2018), 
         HB2014 = as.character(HB2014))

# Read in R file
# Remove HB2019Name and SexName columns
HB2019_pop_est_1981_2018_R <- readRDS("R Files/HB2019_pop_est_1981_2018.rds") %>% 
  select(-c(HB2019Name, SexName))

all_equal(HB2019_pop_est_1981_2018_R, HB2019_pop_est_1981_2018_SPSS)


### 3.2 - 5 year age group files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters
HB2019_pop_est_5year_agegroups_1981_2018_SPSS <- read_sav("HB2019_pop_est_5year_agegroups_1981_2018.sav", user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate(HB2019 = as.character(HB2019), 
         HB2018 = as.character(HB2018), 
         HB2014 = as.character(HB2014))

# Read in R file
# Remove HB2019Name and SexName columns
HB2019_pop_est_5year_agegroups_1981_2018_R <- readRDS("R Files/HB2019_pop_est_5year_agegroups_1981_2018.rds") %>% 
  select(-c(HB2019Name, SexName, AgeGroupName))

all_equal(HB2019_pop_est_5year_agegroups_1981_2018_R, HB2019_pop_est_5year_agegroups_1981_2018_SPSS)




### 4 - Compare HSCP Files ----

### 4.1 - Single year files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters
HSCP2019_pop_est_1981_2018_SPSS <- read_sav("HSCP2019_pop_est_1981_2018.sav", user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate(HSCP2019 = as.character(HSCP2019), 
         HSCP2018 = as.character(HSCP2018), 
         HSCP2016 = as.character(HSCP2016))

# Read in R file
# Remove HSCP2019Name and SexName columns
HSCP2019_pop_est_1981_2018_R <- readRDS("R Files/HSCP2019_pop_est_1981_2018.rds") %>% 
  select(-c(HSCP2019Name, SexName))

all_equal(HSCP2019_pop_est_1981_2018_R, HSCP2019_pop_est_1981_2018_SPSS)


### 4.2 - 5 year age group files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters
HSCP2019_pop_est_5year_agegroups_1981_2018_SPSS <- read_sav("HSCP2019_pop_est_5year_agegroups_1981_2018.sav", user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate(HSCP2019 = as.character(HSCP2019), 
         HSCP2018 = as.character(HSCP2018), 
         HSCP2016 = as.character(HSCP2016))

# Read in R file
# Remove HSCP2019Name and SexName columns
HSCP2019_pop_est_5year_agegroups_1981_2018_R <- readRDS("R Files/HSCP2019_pop_est_5year_agegroups_1981_2018.rds") %>% 
  select(-c(HSCP2019Name, SexName, AgeGroupName))

all_equal(HSCP2019_pop_est_5year_agegroups_1981_2018_R, HSCP2019_pop_est_5year_agegroups_1981_2018_SPSS)
