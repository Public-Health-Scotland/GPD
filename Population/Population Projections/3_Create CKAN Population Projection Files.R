### 1 - Information ----

# Codename - Create CKAN Population Projection Files
# Original Author - Calum Purdie
# Original Date - 16/04/2019
# Updated - 24/10/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("data.table")
#
# Description - Code for creating population projection files for the 
#               NHSScotland open data platform
# Approximate run time - 22 seconds

library(tidyr)
library(dplyr)
library(data.table)
library(glue)
library(readr)

# Set filepaths

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD", 
                           "2_Population", "Population Projections", 
                           "Lookup Files", "R Files")
od_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                         "Open Data (Non Health Topic)", "Data", 
                         "OD1900004 - Population Projections")

# Set date for filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")



### 2 - Create function to read in population data and restructure ----

pop_proj <- function(filepath, a, b){
  
  # Read in rds version and rename columns for open data format
  # Arrange by Year, Sex and Age and spread data by Age and Pop
  
  geo_pop_proj <- readRDS(filepath) %>%
    select(-sex) %>%
    rename(Sex = sex_name,
           Year = year,
           Age = age,
           Pop = pop) %>%
    arrange(Year, Sex, Age) %>%
    spread(Age, Pop)
  
  # Rename Age variables to have Age prefix
  
  colnames(geo_pop_proj)[a:b] <- 
    paste("Age", colnames(geo_pop_proj[,c(a:b)]), sep="")
  
  # Group by Year and Sex for single years of age for calculating Scotland total
  
  Scot_total <- geo_pop_proj %>%
    group_by(Year, Sex) %>%
    summarise_at(vars(Age0:Age90), list(sum)) %>%
    ungroup()
  
  # Join scot_pop_proj_2016_2041 and Scot_total
  # Sum across all ages to get totals
  # Sort by Year and Sex
  
  geo_pop_proj_CKAN <- geo_pop_proj %>%
    full_join(Scot_total) %>%
    mutate(AllAges = rowSums(.[a:b])) %>%
    arrange(Year, Sex)
  
  # Reorder to set missing data (Scotland CA2011) first
  
  geo_pop_proj_CKAN <- setorder(geo_pop_proj_CKAN, na.last = F)

  # Rename Age90 to Age90plus
  
  geo_pop_proj_CKAN <- geo_pop_proj_CKAN %>%
    rename(Age90plus = Age90)
  
}



### 3 - Read in Population Estimates ----

# Read in Scotland projections

scotland_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/scot_pop_proj_2018_2043.rds"), 
           a = 3, b = 93)

# Read in CA2011 estimates

CA2011_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/CA2018_pop_proj_2016_2041.rds"), 
           a = 6, b = 96)

# Read in HB2014 estimates

HB2014_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/HB2018_pop_proj_2016_2041.rds"), 
           a = 6, b = 96)

# Read in HSCP2016 estimates

HSCP2016_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/HSCP2018_pop_proj_2016_2041.rds"),
           a = 6, b = 96)


### 4 - Tidy Scotland data ----

# Create Country column and reorder columns

scotland_pop_proj <- scotland_pop_proj %>% 
  mutate(Country = "S92000003") %>% 
  select(Year, Country, Sex, AllAges, Age0:Age90plus)

# Write csv to open date projections folder

write_csv(scotland_pop_proj, glue("{od_filepath}/scotland_pop_proj_{date}.csv"))



### 5 - Tidy CA2011 data ----

# Due to minor boundary changes from 02/02/2018 and 01/04/2019 which affected 
# some CA2011 codes, the affected codes need to be updated and this requires a 
# qualifier field to show the revised codes

# Recode CA2011 to reflect new codes
# Attach Scotland national code
# Create qualifier column for CA2011 and set it to "r" for revised CA2011 codes
# and set it to "d" for Scotland totals
# Reorder columns

CA2011_pop_proj <- CA2011_pop_proj %>%
  rename(CA2011 = ca2011) %>% 
  mutate(CA2011 = recode(CA2011, 
                         "S12000015" = "S12000047", 
                         "S12000024" = "S12000048", 
                         "S12000046" = "S12000049", 
                         "S12000044" = "S12000050"), 
         CA2011 = if_else(is.na(CA2011), "S92000003", CA2011), 
         CA2011QF = case_when(CA2011 == "S92000003" ~ "d", 
                              CA2011 == "S12000047" ~ "r", 
                              CA2011 == "S12000048" ~ "r", 
                              CA2011 == "S12000049" ~ "r", 
                              CA2011 == "S12000050" ~ "r")) %>%
  select(Year, CA2011, CA2011QF, Sex, AllAges, Age0:Age90plus)

# Write as csv

write_csv(CA2011_pop_proj, glue("{od_filepath}/CA2011_pop_proj_{date}.csv"))



### 6 - Tidy HB2014 data ----

# Due to a minor boundary change from 02/02/2018 and 01/04/2019 which affected 
# some HB2014 codes, the affected codes need to be updated and this requires a 
# qualifier field to show the revised codes

# Recode HB2014 to reflect new codes
# Attach Scotland national code
# Create qualifier column for CA2011 and  set it to "r" for revised HB2014 codes
# and set it to "d" for Scotland totals
# Reorder columns

HB2014_pop_proj <- HB2014_pop_proj %>%
  rename(HB2014 = hb2014) %>% 
  mutate(HB2014 = recode(HB2014, 
                         S08000018 = "S08000029", 
                         S08000027 = "S08000030", 
                         S08000021 = "S08000031", 
                         S08000023 = "S08000032"), 
         HB2014 = if_else(is.na(HB2014), "S92000003", HB2014), 
         HB2014QF = case_when(HB2014 == "S92000003" ~ "d", 
                              HB2014 == "S08000029" ~ "r", 
                              HB2014 == "S08000030" ~ "r", 
                              HB2014 == "S08000031" ~ "r", 
                              HB2014 == "S08000032" ~ "r")) %>%
  select(Year, HB2014, HB2014QF, Sex, AllAges, Age0:Age90plus)

# Write as csv

write_csv(HB2014_pop_proj, glue("{od_filepath}/HB2014_pop_proj_{date}.csv"))


### 7 - Tidy HSCP2016 data ----

# Due to a minor boundary change from 02/02/2018 and 01/04/2019 which affected 
# some HSCP2016 codes, the affected codes need to be updated and this requires a 
# qualifier field to show the revised codes

# Recode HSCP2016 to reflect new codes
# Attach Scotland national code
# Create qualifier column for HSCP2016 and set it to "r" for revised HSCP2016 
# codes and set it to "d" for Scotland totals
# Reorder columns

HSCP2016_pop_proj <- HSCP2016_pop_proj %>%
  rename(HSCP2016 = hscp2016) %>% 
  mutate(HSCP2016 = recode(HSCP2016, 
                           S37000014 = "S37000032", 
                           S37000023 = "S37000033", 
                           S37000015 = "S37000034", 
                           S37000021 = "S37000035"), 
         HSCP2016 = if_else(is.na(HSCP2016), "S92000003", HSCP2016), 
         HSCP2016QF = case_when(HSCP2016 == "S92000003" ~ "d", 
                                HSCP2016 == "S37000032" ~ "r", 
                                HSCP2016 == "S37000033" ~ "r", 
                                HSCP2016 == "S37000034" ~ "r", 
                                HSCP2016 == "S37000035" ~ "r")) %>% 
  select(Year, HSCP2016, HSCP2016QF, Sex, AllAges, Age0:Age90plus)

# Write as csv

write_csv(HSCP2016_pop_proj, glue("{od_filepath}/HSCP2016_pop_proj_{date}.csv"))

