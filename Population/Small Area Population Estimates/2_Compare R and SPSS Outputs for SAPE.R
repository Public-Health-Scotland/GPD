### 1 - Information ----

# Codename - Compare R and SPSS Outputs for Small Area Population Estimates
# Data release - Mid-year small area population estimates
# Original Author - Calum Purdie
# Original Date - 31/05/2019
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
library(stringr)
library(haven)
library(sjlabelled)

SPSS_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD", "2_Population", 
                           "Small Area Population Estimates", "Lookup Files")
R_filepath <- file.path(SPSS_filepath, "R Files")

### 2 - Comparison Function ----

compare_DZ <- function(SPSS, R){
  
  SPSS_file <- read_sav(file.path(SPSS_filepath, SPSS), user_na=F) %>%
    zap_formats() %>%
    zap_widths() %>%
    remove_all_labels() %>% 
    mutate_if(is.factor, as.character)
  
  R_file <- readRDS(file.path(R_filepath, R)) %>% 
    select(-c(DataZone2011Name, IntZone2011Name, HB2019Name, HSCP2019Name, CA2019Name))
  
  print(all_equal(R_file, SPSS_file))
  
}

DataZone <- compare_DZ("DataZone2011_pop_est_2011_2018.sav", "DataZone2011_pop_est_2011_2018.rds")
DataZone_5y <- compare_DZ("DataZone2011_pop_est_5year_agegroups_2011_2018.sav", "DataZone2011_pop_est_5year_agegroups_2011_2018.rds")

compare_IZ <- function(SPSS, R){
  
  SPSS_file <- read_sav(file.path(SPSS_filepath, SPSS), user_na=F) %>%
    zap_formats() %>%
    zap_widths() %>%
    remove_all_labels() %>% 
    mutate_if(is.factor, as.character) %>% 
    rename(total_pop = Total_pop)
  
  R_file <- readRDS(file.path(R_filepath, R)) %>% 
    select(-IntZone2011Name)
  
  print(all_equal(R_file, SPSS_file))
  
}

IntZone <- compare_IZ("IntZone2011_pop_est_2011_2018.sav", "IntZone2011_pop_est_2011_2018.rds")
IntZone_5y <- compare_IZ("IntZone2011_pop_est_5year_agegroups_2011_2018.sav", "IntZone2011_pop_est_5year_agegroups_2011_2018.rds")

