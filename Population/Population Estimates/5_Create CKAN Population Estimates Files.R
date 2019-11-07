### 1 - Information ----

# Codename - Create CKAN Population Estimates Files
# Data release - Mid-year NRS Population Estimates
# Original Author - Calum Purdie
# Original Date - 16/04/2019
# Updated - 06/11/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("magrittr")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("tidylog")
# install.packages("glue")
#
# Description - Code for creating open data files for population estimates
#
# Approximate run time - 10 seconds

# Load packages

library(magrittr)
library(tidyr)
library(dplyr)
library(data.table)
library(tidylog)
library(glue)

# Set filepaths

data_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI",
                           "Referencing & Standards", "GPD", "2_Population", 
                           "Population Estimates", "Lookup Files", "R Files")

od_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                         "Open Data (Non Health Topic", "Data", 
                         "OD1700007 - Population Estimates")

# Set date for filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")



### 2 - Create function to read in population data and restructure ----

geo_pop <- function(filepath, variable){
  
  geo_pop_est_1981_2017 <- readRDS(glue("{data_filepath}/{filepath}")) %>% 
    select(Year, variable, Age, SexName, Pop) %>% 
    rename(Sex = SexName) %>% 
    mutate(Sex = recode(Sex, "M" = "Male", "F" = "Female")) %>% 
    arrange(Year, Sex) %>% 
    spread(Age, Pop) %>% 
    rename_at(4:94, function(x) paste("Age", x, sep=""))
  
  # Group by Year and Sex for all single years of age
  
  Scot_total <- geo_pop_est_1981_2017 %>%
    group_by(Year, Sex) %>%
    summarise_at(vars(Age0:Age90), list(sum)) %>%
    ungroup()
  
  # Add geo_pop_est_1981_2017 and Scot_total
  # Sum across all ages to get totals
  # Sort by Year, variable and Sex
  # Rename Age90 to Age90plus
  # Reorder to set missing data (Scotland CA2011) first
  
  geo_pop_est_1981_2017 %<>%
    full_join(Scot_total) %>%
    mutate(AllAges = rowSums(.[4:94])) %>%
    arrange(Year, Sex) %>%
    rename(Age90plus = Age90) %>% 
    setorder(na.last = F)
  
}



### 3 - Read in Population Estimates ----

# Read in Council Area estimates

CA2011_pop_est <- geo_pop(filepath = "CA2019_pop_est_1981_2018.rds", 
                          variable = "CA2019")

# Read in Health Board estimates

HB2014_pop_est <- geo_pop(filepath = "HB2019_pop_est_1981_2018.rds", 
                                    variable = "HB2019")

# Read in HSCP estimates

HSCP2016_pop_est <- geo_pop(filepath = "HSCP2019_pop_est_1981_2018.rds", 
                                      variable = "HSCP2019")



### 4 - Tidy Council Area Data ----

# Due to minor boundary changes from 02/02/2018 and 01/04/2019 which affected 
# some CA2011 codes, the affected codes need to be updated
# This requires a qualifier field to show the revised codes

# Recode CA2011 to reflect new codes
# Attach Scotland national code
# Create qualifier column for CA2011 and  set it to "r" for revised CA2011 codes
# and set it to "d" for Scotland totals
# Reorder columns

CA2011_pop_est <- CA2011_pop_est %>%
  rename(CA2011 = CA2019) %>% 
  mutate(CA2011 = if_else(is.na(CA2011), "S92000003", CA2011), 
         CA2011QF = case_when(CA2011 == "S92000003" ~ "d", 
                              CA2011 == "S12000047" ~ "r", 
                              CA2011 == "S12000048" ~ "r", 
                              CA2011 == "S12000049" ~ "r", 
                              CA2011 == "S12000050" ~ "r"))  %>%
  select(Year, CA2011, CA2011QF, Sex, AllAges, Age0:Age90plus)

# Write as csv

write_csv(CA2011_pop_est, glue("{od_filepath}/CA2011_pop_est_{date}.csv"))



### 5 - Tidy HB2014 Data ----

# Due to minor boundary changes from 02/02/2018 and 01/04/2019 which affected 
# some CA2011 codes, the affected codes need to be updated
# This requires a qualifier field to show the revised codes

# Recode HB2014 to reflect new codes
# Attach Scotland national code
# Create qualifier column for HB2014 and  set it to "r" for revised HB2014 codes
# and set it to "d" for Scotland totals
# Reorder columns

HB2014_pop_est %<>%
  rename(HB2014 = HB2019) %>% 
  mutate(HB2014 = if_else(is.na(HB2014), "S92000003", HB2014), 
         HB2014QF = case_when(HB2014 == "S92000003" ~ "d", 
                              HB2014 == "S08000029" ~ "r", 
                              HB2014 == "S08000030" ~ "r", 
                              HB2014 == "S08000031" ~ "r", 
                              HB2014 == "S08000032" ~ "r")) %>%
  select(Year, HB2014, HB2014QF, Sex, AllAges, Age0:Age90plus)

# Write as csv

write_csv(HB2014_pop_est, glue("{od_filepath}/HB2014_pop_est_{date}.csv"))



### 6 - Tidy HSCP2016 Data ----

# Due to minor boundary changes from 02/02/2018 and 01/04/2019 which affected 
# some CA2011 codes, the affected codes need to be updated
# This requires a qualifier field to show the revised codes

# Recode HSCP2016 to reflect new codes
# Attach Scotland national code
# Create qualifier column for HSCP2016 and  set it to "r" for revised 
# HSCP2016 codes and set it to "d" for Scotland totals
# Reorder columns

HSCP2016_pop_est %<>%
  rename(HSCP2016 = HSCP2019) %>% 
  mutate(HSCP2016 = if_else(is.na(HSCP2016), "S92000003", HSCP2016), 
         HSCP2016QF = case_when(HSCP2016 == "S92000003" ~ "d", 
                                HSCP2016 == "S37000032" ~ "r", 
                                HSCP2016 == "S37000033" ~ "r", 
                                HSCP2016 == "S37000034" ~ "r", 
                                HSCP2016 == "S37000035" ~ "r")) %>%
  select(Year, HSCP2016, HSCP2016QF, Sex, AllAges, Age0:Age90plus)

# Write as csv

write_csv(HSCP2016_pop_est, glue("{od_filepath}/HSCP2016_pop_est_{date}.csv"))

