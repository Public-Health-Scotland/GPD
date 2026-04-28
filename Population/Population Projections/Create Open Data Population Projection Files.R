##########################################################
# Create Open Data Population Projection Files
# Calum Purdie
# Original date 16/04/2019
# Latest update author - Calum Purdie
# Latest update date - 03/07/2020
# Latest update description - formatting code
# Type of script - Creation
# Version of R that the script was most recently run on 4.4.2
# Code for creating population projection files for the NHS Scotland open data platform
# Approximate run time - <1 min

# Last used Oct 2025 to update revised projections 
##########################################################

### 1 Housekeeping & set-up ----
rm(list = ls())

# 1.1 Read in packages from library #

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}

pacman::p_load(magrittr,tidyr,dplyr,data.table,glue,readr, tidylog)

base_filepath <- ("/data/geography/Population/Population Projections/")


base_filepath <- glue("/data/geography/Population/Population Projections/Lookup Files/",
                      "R Files")

od_filepath <- glue("/data/geography/Population/Population Projections/Lookup Files/",
                      "CKAN Open Data")
# Set date for filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")

start <- "2022"
end <- "2047"

### 2 Create function to read in population data and restructure ----
source("Pop_proj_OD_functions.R")

### 3 National Projections ----

# Read in Scotland projections
scotland_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/scot_pop_proj_{start}_{end}.rds"), 
           col_start = 3, col_end = 93)

scotland_pop_proj %<>% 
  mutate(Country = "S92000003") %>% 
  select(Year, Country, Sex, SexQF, AllAges, Age0:Age90plus)

# Write csv to open date projections folder

write_csv(scotland_pop_proj, glue("{od_filepath}/scotland_pop_proj_{date}.csv"), 
          na = "")

### 4 CA2019  Projections ----
# Read in CA2019 
CA2019_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/CA2019_pop_proj_{start}_{end}.rds"), 
           col_start = 7, col_end = 97, variable = "ca2019")


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


### 6 HB2019 Projection ----

# Read in HB2019 
HB2019_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/HB2019_pop_proj_{start}_{end}.rds"), 
           col_start = 7, col_end = 97, variable = "hb2019")

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

### 7 HSCP Projections  ----
#
HSCP2019_pop_proj <- 
  pop_proj(filepath = glue("{base_filepath}/HSCP2019_pop_proj_{start}_{end}.rds"),
           col_start = 7, col_end = 97, variable = "hscp2019")

# Rename hscp2016 to HSCP
# Attach Scotland national code
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
