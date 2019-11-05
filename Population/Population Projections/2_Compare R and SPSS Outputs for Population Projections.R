### 1 - Information ----

# Codename - Compare R and SPSS Outputs for Population Projections
# Data release - 2018 based population projections
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
# install.packages("janitor")
# install.packages("tidylog")
# install.packages("glue")
#
# Description - Code for comparing R and SPSS lookup files for population 
#               projections
#
# Approximate run time - 5 minutes

# Set working directory
SPSS_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD", "2_Population", 
                           "Population Projections", "Lookup Files")
R_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                        "Referencing & Standards", "GPD", "2_Population", 
                        "Population Projections", "Lookup Files", "R Files")

# Read in packages from library

library(tidyr)
library(dplyr)
library(stringr)
library(haven)
library(sjlabelled)
library(janitor)
library(tidylog)
library(glue)


### 2 - Compare Council Area Files ----

### 2.1 - Single year files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Mutate numeric to integers for matching
# Use clean_names so column names match

scot_pop_proj_2018_2043_SPSS <- read_sav(glue("{SPSS_filepath}/", 
                                              "scot_pop_proj_2018_2043.sav"), 
                                         user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.numeric, as.integer) %>% 
  clean_names()

# Read in R file
# Remove SexName column

scot_pop_proj_2018_2043_R <- readRDS(glue("{R_filepath}/", 
                                          "scot_pop_proj_2018_2043.rds")) %>% 
  select(-c(sex_name))

# Compare files

all_equal(scot_pop_proj_2018_2043_R, scot_pop_proj_2018_2043_SPSS)



### 2.2 - 5 year age group files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Mutate numeric to integers for matching
# Use clean_names so column names match

scot_pop_proj_2018_2043_5y_SPSS <- read_sav(
  glue("{SPSS_filepath}/scot_pop_proj_5year_agegroups_2018_2043.sav"), 
  user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.numeric, as.integer) %>% 
  clean_names()

# Read in R file
# Remove SexName and AgeGroupName columns

scot_pop_proj_2018_2043_5y_R <- readRDS(
  glue("{R_filepath}/scot_pop_proj_5year_agegroups_2018_2043.rds")) %>% 
  select(-c(sex_name, age_group_name))

# Compare files

all_equal(scot_pop_proj_2018_2043_R, scot_pop_proj_2018_2043_SPSS)
