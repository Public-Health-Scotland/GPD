### 1 - Information ----

# Codename - Create HSCP Locality Population Estimates
# Data release - Small Area Population Estimates for 2011 Data Zones
# Original Author - Calum Purdie
# Original Date - 23/10/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("readr")
# install.packages("tidylog")
# install.packages("janitor")
#
# Description - This document is for creating HSCP Locality population estimates based on 2011 Data 
#               Zone estimates. This file should be updated yearly for each subsequent release.
#
# Approximate run time - 5 minutes

# Read in packages from library

library(tidyr)
library(dplyr)
library(readxl)
library(readr)
library(tidylog)
library(janitor)

# Set filepaths

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD", 
                           "2_Population", "Small Area Population estimates", "Lookup Files", 
                           "R Files")

lookups_filepath <- file.path("//Isdsf00d03", "cl-out", "lookups", "Unicode")

open_data_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                                "Open Data (Non Health Topic)", "Data", "OD1700007 - Population Estimates")

### 2 - Read in Files ----

# Read in the most recent Data Zone population estimates

DZ2011_pop_est <- readRDS(file.path(base_filepath, "DataZone2011_pop_est_2011_2018.rds"))

# Read in the HSCP Locality file for matching
# Select relevant columns and tidy up names for matching
Localities <- readRDS(file.path(lookups_filepath, "Geography", "HSCP Locality", "HSCP Localities_DZ11_Lookup_20180903.rds")) %>% 
  select(DataZone2011, HSCPLocality) %>% 
  clean_names() %>% 
  rename(datazone2011 = data_zone2011)


### 3 - Create Locality Single Year Estimates ----

# Join the Localities data onto DZ2011_pop_est
# Group by year, hscp_locality and sex and summarise the populations for each age
# This gives the total population for each locality by year and sex

Locality_pop_est <- DZ2011_pop_est %>% 
  left_join(Localities) %>% 
  group_by(year, hscp_locality, sex) %>% 
  summarise_at(vars(age0:total_pop), list(sum)) %>% 
  ungroup()

# Save file as .RDS

saveRDS(Locality_pop_est, file.path(base_filepath, "Locality_pop_est_2011_2018.rds"))



### 4 - Create Locality 5 Year Age Group Estimates ----

Locality_pop_est_5y <- Locality_pop_est %>%
  mutate(ageg04 = rowSums(.[4:8]), 
         ageg59 = rowSums(.[9:13]),
         ageg1014 = rowSums(.[14:18]),
         ageg1519 = rowSums(.[19:23]), 
         ageg2024 = rowSums(.[24:28]),
         ageg2529 = rowSums(.[29:33]), 
         ageg3034 = rowSums(.[34:38]),
         ageg3539 = rowSums(.[39:43]), 
         ageg4044 = rowSums(.[44:48]),
         ageg4549 = rowSums(.[49:53]), 
         ageg5054 = rowSums(.[54:58]),
         ageg5559 = rowSums(.[59:63]), 
         ageg6064 = rowSums(.[64:68]),
         ageg6569 = rowSums(.[69:73]), 
         ageg7074 = rowSums(.[74:78]),
         ageg7579 = rowSums(.[79:83]), 
         ageg8084 = rowSums(.[84:88]),
         ageg8589 = rowSums(.[89:93])) %>% 
  select(-c(age0:age89)) %>% 
  rename(ageg90plus = age90plus) %>% 
  select(year, hscp_locality, sex, ageg04:ageg8589, ageg90plus, total_pop)

# Save file as .RDS

saveRDS(Locality_pop_est_5y, file.path(base_filepath, "Locality_pop_est_5year_agegroups_2011_2018.rds"))



### 5 - Check Final Files ----

# Check that the frequencies for each area is the same.
# Check that the frequencies for each area is equal to the number of years of SAPE data x 2 
# i.e. for 12 years of data, would expect 24
# Here we expect the value to be 14 for 7 years and both gender - UPDATE THIS FOR NEW RELEASE

# Create check function

data_check <- function(input, column){
  
  # Check that the frequencies for each datazone is the same.
  # Check that the frequencies for each datazone is equal to the number of years of SAPE data x 2 
  # i.e. for 12 years of data, would expect 24
  # Here we expect the value to be 14 for 7 years and both gender - UPDATE THIS FOR NEW RELEASE
  
  input %>% count({{column}} != 16) %>% print()
  
  # Check sums add up to total_pop
  # Calculate the sum for all the age columns and check to see if the sums add up to the total_pop
  # Filter for rows where the sums don't add up
  input %>% 
    mutate(sums = ifelse(rowSums(select(., starts_with("age"))) - total_pop != 0, 1, 0)) %>% 
    select(sums) %>% 
    filter(sums != 0) %>% 
    print()
  
  # Check that the frequencies for each year are the same
  
  input %>% count(year) %>% print()
  
  # Check that the frequencies for males and females is equal
  
  input %>% count(sex) %>% print()
  
}

### 5.1 - Check DataZone Single Year Files ----

data_check(Locality_pop_est, "hscp_locality")


### 5.2 - Check DataZone 5 Year Age Group File ----

data_check(Locality_pop_est_5y, "hscp_locality")


### 5.5 - Check Scotland Totals ----

# Run full section of syntax to end
# Check all files have same Scotland total for all years
# Conatct GPD analyst if there are any issues

# Data Zones
# Create single year totals for DataZone

Locality_total <- Locality_pop_est %>%
  group_by(year) %>%
  summarise(Locality_total_pop = sum(total_pop)) %>%
  ungroup()

# Create 5 year age group totals for DataZone

Locality_5y_total <- Locality_pop_est_5y %>%
  group_by(year) %>%
  summarise(Locality_5y_total_pop = sum(total_pop)) %>%
  ungroup()

# Match files together

Locality_total %>%
  full_join(Locality_5y_total)
