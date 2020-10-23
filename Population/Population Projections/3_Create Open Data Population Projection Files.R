##########################################################
# Create Open Data Population Projection Files
# Calum Purdie
# Original date 16/04/2019
# Latest update author - Calum Purdie
# Latest update date - 03/07/2020
# Latest update description - formatting code
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for creating population projection files for the 
# NHS Scotland open data platform
# Approximate run time - 22 seconds
##########################################################

### 1 - Housekeeping ----

library(magrittr)
library(tidyr)
library(dplyr)
library(data.table)
library(glue)
library(readr)
library(tidylog)

# Set filepaths

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/",
                      "2_Population/Population Projections/Lookup Files/",
                      "R Files")
od_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Publications/", 
                    "Open Data (Non Health Topic)/Data/", 
                    "OD1900004 - Population Projections")

# Set date for filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")

start <- "2018"
end <- "2043"



### 2 - Create function to read in population data and restructure ----

pop_proj <- function(filepath, col_start, col_end, variable){
  
  # Read in rds version and rename columns for open data format
  # Arrange by Year, Sex and Age and spread data by Age and Pop
  # Rename Age variables to have Age prefix
  
  geo_pop_proj <- readRDS(filepath) %>%
    select(-sex) %>%
    rename(Sex = sex_name,
           Year = year,
           Age = age,
           Pop = pop) %>%
    arrange(Year, Sex, Age) %>%
    spread(Age, Pop) %>% 
    rename_at(col_start:col_end, function(x) paste("Age", x, sep=""))
  
  # Create totals for male and female combined
  # If Scotland projections group by Year
  # If smaller geography group by Year and geography
  
  if (col_start == 3 & col_end == 93){
    
    all_total <- geo_pop_proj %>% 
      group_by(Year) %>% 
      summarise_at(vars(Age0:Age90), list(sum)) %>%
      ungroup() %>% 
      mutate(Sex = "All")
    
  } else {
  
  all_total <- geo_pop_proj %>% 
    group_by(Year, !!as.name(variable)) %>% 
    summarise_at(vars(Age0:Age90), list(sum)) %>%
    ungroup() %>% 
    mutate(Sex = "All")
  
  }
  
  # Add all_total to geo_pop_est
  
  geo_pop_proj %<>% 
    bind_rows(all_total)

  # Group by Year and Sex for single years of age for calculating Scotland total
  
  scot_total <- geo_pop_proj %>%
    group_by(Year, Sex) %>%
    summarise_at(vars(Age0:Age90), list(sum)) %>%
    ungroup()
  
  # Join geo_pop_proj and scot_total
  # Sum across all ages to get totals
  # Sort by Year and Sex
  # Reorder to set missing data first
  
  geo_pop_proj %<>%
    full_join(scot_total) %>%
    mutate(AllAges = rowSums(.[col_start:col_end]), 
           SexQF = case_when(Sex == "All" ~ "d")) %>%
    arrange(Year, Sex) %>%
    rename(Age90plus = Age90) %>% 
    setorder(na.last = F)
  
}



### 3 - Read in Population Projections ----

# Read in Scotland projections

scotland_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/scot_pop_proj_{start}_{end}.rds"), 
           col_start = 3, col_end = 93)

# Read in CA2019 estimates

CA2019_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/CA2019_pop_proj_{start}_{end}.rds"), 
           col_start = 7, col_end = 97, variable = "ca2019")

# Read in HB2019 estimates

HB2019_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/HB2019_pop_proj_{start}_{end}.rds"), 
           col_start = 7, col_end = 97, variable = "hb2019")

# Read in HSCP2019 estimates

HSCP2019_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/HSCP2019_pop_proj_{start}_{end}.rds"),
           col_start = 7, col_end = 97, variable = "hscp2019")


### 4 - Tidy Scotland data ----

# Create Country column and reorder columns

scotland_pop_proj %<>% 
  mutate(Country = "S92000003") %>% 
  select(Year, Country, Sex, SexQF, AllAges, Age0:Age90plus)

# Write csv to open date projections folder

write_csv(scotland_pop_proj, glue("{od_filepath}/scotland_pop_proj_{date}.csv"), 
          na = "")


### 5 - Tidy CA data ----

# Rename ca2019 to CA
# Attach Scotland national code
# Create qualifier column for CA and set it to "r" for revised CA codes
# and set it to "d" for Scotland totals
# Reorder columns

CA2019_pop_proj %<>%
  rename(CA = ca2019) %>% 
  mutate(CA = if_else(is.na(CA), "S92000003", CA), 
         CAQF = case_when(CA == "S92000003" ~ "d")) %>%
  select(Year, CA, CAQF, Sex, SexQF, AllAges, Age0:Age90plus)

# Write as csv

write_csv(CA2019_pop_proj, glue("{od_filepath}/CA_pop_proj_{date}.csv"), 
          na = "")



### 6 - Tidy HB data ----

# Rename hb2019 to HB
# Attach Scotland national code
# Create qualifier column for CA2011 and  set it to "r" for revised HB2014 codes
# and set it to "d" for Scotland totals
# Reorder columns

HB2019_pop_proj %<>%
  rename(HB = hb2019) %>% 
  mutate(HB = if_else(is.na(HB), "S92000003", HB), 
         HBQF = case_when(HB == "S92000003" ~ "d")) %>%
  select(Year, HB, HBQF, Sex, SexQF, AllAges, Age0:Age90plus)

# Write as csv

write_csv(HB2019_pop_proj, glue("{od_filepath}/HB_pop_proj_{date}.csv"), 
          na = "")



### 7 - Tidy HSCP data ----

# Rename hscp2016 to HSCP
# Attach Scotland national code
# Create qualifier column for HSCP2016 and set it to "r" for revised HSCP2016 
# codes and set it to "d" for Scotland totals
# Reorder columns

HSCP2019_pop_proj %<>%
  rename(HSCP = hscp2019) %>% 
  mutate(HSCP = if_else(is.na(HSCP), "S92000003", HSCP), 
         HSCPQF = case_when(HSCP == "S92000003" ~ "d")) %>% 
  select(Year, HSCP, HSCPQF, Sex, SexQF, AllAges, Age0:Age90plus)

# Write as csv

write_csv(HSCP2019_pop_proj, glue("{od_filepath}/HSCP_pop_proj_{date}.csv"), 
          na = "")
