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
# install.packages("tidylog")
# install.packages("glue")
# install.packages("janitor")
#
# Description - Comparing R and SPSS output for Small Area Population Estimates
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
library(janitor)

SPSS_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD", "2_Population", 
                           "Small Area Population Estimates", "Lookup Files")
R_filepath <- file.path(SPSS_filepath, "R Files")



### 2 - Data Zones ----

compare_DZ <- function(SPSS, R){
  
  SPSS_file <- read_sav(file.path(SPSS_filepath, SPSS), user_na=F) %>%
    zap_formats() %>%
    zap_widths() %>%
    remove_all_labels() %>% 
    mutate_if(is.factor, as.character) %>% 
    clean_names() %>% 
    rename(datazone2011 = data_zone2011, 
           intzone2011 = int_zone2011)
    
  
  R_file <- readRDS(file.path(R_filepath, R)) %>% 
    select(-c(datazone2011name, intzone2011name, hb2019name, hscp2019name, 
              ca2019name))
  
  print(all_equal(R_file, SPSS_file))
  
}


# Compare Data Zone Single Year Files

DataZone <- compare_DZ("DataZone2011_pop_est_2011_2018.sav", 
                       "DataZone2011_pop_est_2011_2018.rds")

# Compare Data Zone 5 Year Age Group Files

DataZone_5y <- compare_DZ("DataZone2011_pop_est_5year_agegroups_2011_2018.sav", 
                          "DataZone2011_pop_est_5year_agegroups_2011_2018.rds")



### 3 - Int Zones ----

compare_IZ <- function(SPSS, R){
  
  SPSS_file <- read_sav(file.path(SPSS_filepath, SPSS), user_na=F) %>%
    zap_formats() %>%
    zap_widths() %>%
    remove_all_labels() %>% 
    mutate_if(is.factor, as.character) %>% 
    clean_names() %>% 
    rename(intzone2011 = int_zone2011)
  
  R_file <- readRDS(file.path(R_filepath, R)) %>% 
    select(-intzone2011name)
  
  print(all_equal(R_file, SPSS_file))
  
}


# Compare Int Zone Single Year Files

IntZone <- compare_IZ("IntZone2011_pop_est_2011_2018.sav", 
                      "IntZone2011_pop_est_2011_2018.rds")

# Compare Int Zone 5 Year Age Group Files

IntZone_5y <- compare_IZ("IntZone2011_pop_est_5year_agegroups_2011_2018.sav", 
                         "IntZone2011_pop_est_5year_agegroups_2011_2018.rds")

