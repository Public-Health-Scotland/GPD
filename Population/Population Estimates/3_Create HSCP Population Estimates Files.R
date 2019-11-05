### 1 - Information ----

# Codename - Create HSCP Population Estimates Files
# Data release - Mid-year HSCP Population Estimates
# Original Author - Tina Fu
# Original Date - 05/03/2018
# Updated - 05/11/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("magrittr")
# install.packages("readxl")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("tidylog")
# install.packafes("glue")
#
# Description - Code for creating HSCP population estimates files 
#               based on mid-year estimates released by NRS
#
# Approximate run time - 14 seconds

# Read in packages from library

library(magrittr)
library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(tidylog)
library(glue)

# Set filepaths

filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                      "Referencing & Standards", "GPD", "2_Population", 
                      "Population Estimates")
data_filepath <- file.path(filepath, "Source Data")
output_filepath <- file.path(filepath, "Lookup Files", "R Files")
archive_filepath <- file.path(output_filepath, "Archive")



### 2 - Read in CA2011 lookup file

CA2019_pop_est_1981_2018 <- readRDS(glue("{output_filepath}/", 
                                         "CA2019_pop_est_1981_2018.rds"))


# Create a lookup of CA and HSCP columns from the HSCP Locality lookup
# Select CA2019, CA2018, CA2011, HSCP2019Name, HSCP2019, HSCP2018 and 
# HSCP2016 columns
# This gives you all 32 Council Areas matched to HSCP
# Take distinct rows

CA_HSCP <- read_csv(paste0("//Isdsf00d03/cl-out/lookups/Unicode/", 
                           "Geography/HSCP Locality/", 
                           "HSCP Localities_DZ11_Lookup_20180903.csv")) %>%
  select(CA2019, CA2018, CA2011, HSCP2019Name, HSCP2019, HSCP2018, HSCP2016) %>%
  distinct()


### 3 - Create the HSCP population estimate file ----

# Match the HSCP columns onto the CA estimates file
# Create column for SexName
# Select the relevant columns
# As there are 31 HSCPs and 32 CAs (Stirling and Clackmannanshire join for HSCP) 
# this will produce population separately for both Stirling and Clackmannanshire 
# even though they have the same HSCP code
# Need to recalculate the sums, so group by all variables bar Pop and calculate 
# new Pop totals
# Arrange by Year, HSCP2018, Age and Sex to get the required format

HSCP2019_pop_est_1981_2018 <- CA2019_pop_est_1981_2018 %>%
  full_join(HSCP_Locality) %>%
  select(Year, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, Age, Sex, SexName, 
         Pop) %>%
  group_by(Year, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, Age, Sex, 
           SexName) %>%
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  arrange(Year, HSCP2019, Age, Sex)


# Save file as .RDS

saveRDS(HSCP2019_pop_est_1981_2018, 
        glue("{output_filepath}/HSCP2019_pop_est_1981_2018.rds"))



### 4 - Create the HSCP 5 year age group population estimate file ----

# Create a file for 5 year age group and sex

CA2019_pop_est_5y_1981_2018 <- readRDS(
  glue("{output_filepath}/CA2019_pop_est_5year_agegroups_1981_2018.rds"))

# Match the HSCP columns onto the CA estimates file
# Create column for SexName
# Select the relevant columns
# As there are 31 HSCPs and 32 CAs (Stirling and Clackmannanshire join for HSCP) 
# this will produce population separately for both Stirling and Clackmannanshire 
# even though they have the same HSCP code
# Need to recalculate the sums, so group by all variables bar Pop and calculate 
# new Pop totals
# Arrange by Year, HSCP2018, Age and Sex to get the required format

HSCP2019_pop_est_5year_agegroups_1981_2018 <- CA2019_pop_est_5y_1981_2018 %>%
  full_join(HSCP_Locality) %>%
  select(Year, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, AgeGroup, 
         AgeGroupName, Sex, SexName, Pop) %>%
  group_by(Year, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, AgeGroup, 
           AgeGroupName, Sex, SexName) %>%
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  arrange(Year, HSCP2019, AgeGroup, Sex)

# Save file as .RDS

saveRDS(HSCP2019_pop_est_5year_agegroups_1981_2018, 
        glue("{output_filepath}/HSCP2019_pop_est_5year_agegroups_1981_2018.rds"))



### 5 - Check files ----

### 5.1 - Check function ----

checks <- function(input, age_column){
  
  # Check that all years of the population estimates are there (last update 1982-2017)
  # Check that there are no missing values
  # Check all years have the same % of records
  
  input %>% group_by(Year) %>% count() %>% print(n = Inf)
  
  # Check that all 32 Council Areas are there
  # Check there are no missing values
  # Check all CAs have the same % of records
  
  input %>% group_by(HSCP2019) %>% count() %>% print(n = Inf)
  input %>% group_by(HSCP2018) %>% count() %>% print(n = Inf)
  input %>% group_by(HSCP2016) %>% count() %>% print(n = Inf)
  
  # Check that all 91 ages 0 to 90+ are there
  # Check there are no missing values
  # Check all ages have the same % of records
  
  input %>% group_by({{age_column}}) %>% count() %>% print(n = Inf)
  
  # Check that both males and females are there
  # Check there are no missing values
  # Check both sexes have the same % of records (50/50)
  
  input %>% group_by(Sex) %>% count() %>% print(n = Inf)
  
  # Check that the population values are as expected
  # i.e. no negative values or extremely high values etc
  
  input %>%
    group_by(Pop) %>%
    count() %>%
    arrange(n) %>%
    filter(n <= 0 | n >= 300)
  
  # Select only the new year(s) of data
  
  new_years <- input %>%
    filter(Year > 2011)
  
}



### 5.2 - Check single age file ----

checks(input = HSCP2019_pop_est_1981_2018, age_column = "Age")


### 5.3 - Check 5 year age group file ----

checks(input = HSCP2019_pop_est_5year_agegroups_1981_2018, 
       age_column = "AgeGroup")
