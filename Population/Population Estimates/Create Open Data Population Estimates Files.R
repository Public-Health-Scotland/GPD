##########################################################
# Create Open Data Population Estimates Files
# Calum Purdie
# Original date 16/04/2019
# Latest update author - Gerald Leung
# Latest update date - 26/03/2024
# Latest update description - formatting code
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 4.0.2
# Code for creating open data files for population estimates
# Approximate run time - 10 seconds
##########################################################

### 1 - Housekeeping ----

rm(list = ls())

#### libaries 
if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}

pacman::p_load(magrittr, tidyr,dplyr, data.table, tidylog, glue)

# Set filepaths
base_filepath <- "/data/geography/Population/Population Estimates"

data_filepath <- glue("{base_filepath}/", "Lookup Files/R Files")

od_filepath <- glue("{base_filepath}/", "Open Data")

# Set date for filenames
date <- strftime(Sys.Date(), format = "%d%m%Y")

# Set years
start <- "1981"
end <-"2024"


### 2 - Create function to read in population data and restructure ----

geo_pop <- function(filepath, variable){
  
  geo_pop_est <- readRDS(glue("{data_filepath}/{filepath}_{start}_{end}.rds")) %>%
    select(year, variable, age, sex_name, pop) %>% 
    rename(Year = year, 
           Sex = sex_name, 
           Pop = pop) %>% 
    mutate(Sex = recode(Sex, "M" = "Male", "F" = "Female")) %>% 
    arrange(Year, Sex) %>% 
    spread(age, Pop) %>% 
    rename_at(4:94, function(x) paste("Age", x, sep=""))
  
  # Create totals for male and female combined
  
  all_total <- geo_pop_est %>% 
    group_by(Year, !!as.name(variable)) %>% 
    summarise_at(vars(Age0:Age90), list(sum)) %>%
    ungroup() %>% 
    mutate(Sex = "All")
  
  # Add all_total to geo_pop_est
  
  geo_pop_est %<>% 
    bind_rows(all_total)
  
  # Group by Year and Sex for all single years of age
  
  Scot_total <- geo_pop_est %>%
    group_by(Year, Sex) %>%
    summarise_at(vars(Age0:Age90), list(sum)) %>%
    ungroup()
  
  # Add Scot_total to geo_pop_est
  # Sum across all ages to get totals
  # Sort by Year, variable and Sex
  # Rename Age90 to Age90plus
  # Reorder to set missing data first
  
  geo_pop_est %<>%
    full_join(Scot_total) %>%
    mutate(AllAges = rowSums(.[4:94]), 
           SexQF = case_when(Sex == "All" ~ "d")) %>%
    arrange(Year, Sex) %>%
    rename(Age90plus = Age90) %>% 
    setorder(na.last = F)
  
}


### 3 - Read in Population Estimates ----

# Read in Council Area estimates
CA2019_pop_est <- geo_pop(filepath = "CA2019_pop_est", variable = "ca2019")

# Read in Health Board estimates
HB2019_pop_est <- geo_pop(filepath = "HB2019_pop_est", variable = "hb2019")

# Read in HSCP estimates
HSCP2019_pop_est <- geo_pop(filepath = "HSCP2019_pop_est", variable = "hscp2019")

### 4 - Tidy Council Area Data ----
# Rename ca2019 as CA
# Attach Scotland national code
# Create qualifier column for CA and  set it to "r" for revised CA codes
# and set it to "d" for Scotland totals
# Reorder columns

CA2019_pop_est %<>%
  rename(CA = ca2019) %>% 
  mutate(CA = if_else(is.na(CA), "S92000003", CA), 
         CAQF = case_when(CA == "S92000003" ~ "d"))  %>%
  select(Year, CA, CAQF, Sex, SexQF, AllAges, Age0:Age90plus)

# Write as csv
fwrite(CA2019_pop_est, glue("{od_filepath}/CA2019_pop_est_{date}.csv"), na = "")

### 5 - Tidy Health Board Data ----
# Rename hb2019 to HB
# Attach Scotland national code
# Create qualifier column for HB and  set it to "r" for revised HB codes
# and set it to "d" for Scotland totals
# Reorder columns

HB2019_pop_est %<>%
  rename(HB = hb2019) %>% 
  mutate(HB = if_else(is.na(HB), "S92000003", HB), 
         HBQF = case_when(HB == "S92000003" ~ "d")) %>%
  select(Year, HB, HBQF, Sex, SexQF, AllAges, Age0:Age90plus)

# Write as csv
fwrite(HB2019_pop_est, glue("{od_filepath}/HB2019_pop_est_{date}.csv"), na = "")


### 6 - Tidy HSCP Data ----
# Rename hscp2019 to HSCP
# Attach Scotland national code
# Create qualifier column for HSCP and  set it to "r" for revised 
# HSCP codes and set it to "d" for Scotland totals
# Reorder columns

HSCP2019_pop_est %<>%
  rename(HSCP = hscp2019) %>% 
  mutate(HSCP = if_else(is.na(HSCP), "S92000003", HSCP), 
         HSCPQF = case_when(HSCP == "S92000003" ~ "d")) %>%
  select(Year, HSCP, HSCPQF, Sex, SexQF, AllAges, Age0:Age90plus)

# Write as csv
fwrite(HSCP2019_pop_est,  glue("{od_filepath}/HSCP2019_pop_est_{date}.csv"), na = "")