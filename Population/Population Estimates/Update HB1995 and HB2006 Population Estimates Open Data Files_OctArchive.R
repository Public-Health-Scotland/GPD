##########################################################
# Name of file: Update HB1995 and HB2006 Population Estimates Open Data Files
# Original author: Calum Purdie
# Original date: 02/04/2020
# Latest update author: Calum Purdie
# Latest update date: 07/07/2020
# Latest update description: Initial version
# Type of script: data preparation
# Written/run on: RStudio desktop
# Version of R that the script was most recently run on: R 3.5.1
# Description of content: Code for updating internal HB1995 population
# estimates file from open data
# Approximate run time: 30 seconds.
##########################################################


### 1 - Housekeeping ----

# Load libraries

library(magrittr)
library(dplyr)
library(tidyr)
library(data.table)
library(haven)
library(sjlabelled)
library(glue)
library(tidylog)
library(janitor)

# Set filepaths

data_filepath <- glue("//stats/cl-out/lookups/Unicode/Populations/Estimates")

od_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Publications/", 
                    "Open Data (Non Health Topic)/Data/", 
                    "OD1700007 - Population Estimates")

# Set date and file to use

date <- strftime(Sys.Date(), format = "%d%m%Y")



### 2 - Read in HB1995 File ----

# Read in hb_pop_est_1974_2006 estimates
# Remove SPSS attributes and labels

hb1995_pop_est <- read_sav(glue("{data_filepath}/hb_pop_est_1974_2006.sav")) %>% 
  remove_all_labels() %>% 
  zap_formats() %>% 
  zap_widths() %>% 
  mutate_if(is.factor, as.character)

# Create HB column with HB1995 codes
# Select variables and rename
# Recode sex to strings and spread data to wide format

hb1995_pop_est %<>% 
  mutate(HB = case_when(HBRES == "H" ~ 1,
                        HBRES == "N" ~ 2,
                        HBRES == "T" ~ 3,
                        HBRES == "F" ~ 4,
                        HBRES == "S" ~ 5,
                        HBRES == "B" ~ 6,
                        HBRES == "V" ~ 7,
                        HBRES == "C" ~ 8,
                        HBRES == "G" ~ 9,
                        HBRES == "L" ~ 10,
                        HBRES == "A" ~ 11,
                        HBRES == "Y" ~ 12,
                        HBRES == "R" ~ 13,
                        HBRES == "Z" ~ 14,
                        HBRES == "W" ~ 15)) %>%
  select(YEAR, HB, AGE, SEX, POP) %>%
  rename(Year = YEAR, 
         Age = AGE, 
         Sex = SEX, 
         Pop = POP) %>% 
  mutate(Sex = recode(Sex, "1" = "Male", "2" = "Female")) %>% 
  arrange(Year, Sex) %>% 
  spread(Age, Pop) %>% 
  rename_at(4:94, function(x) paste("Age", x, sep=""))

# Create totals for male and female combined

all_total <- hb1995_pop_est %>% 
  group_by(Year, HB) %>% 
  summarise_at(vars(Age0:Age90), list(sum)) %>%
  ungroup() %>% 
  mutate(Sex = "All")

# Add all_total to hb1995_pop_est

hb1995_pop_est %<>% 
  bind_rows(all_total)

# Group by Year and Sex for all single years of age

Scot_total <- hb1995_pop_est %>%
  group_by(Year, Sex) %>%
  summarise_at(vars(Age0:Age90), list(sum)) %>%
  ungroup()

# Add Scot_total to geo_pop_est
# Sum across all ages to get totals
# Sort by Year, variable and Sex

hb1995_pop_est %<>%
  full_join(Scot_total) %>%
  mutate(SexQF = case_when(Sex == "All" ~ "d")) %>%
  arrange(Year, Sex)

# Split hb1995_pop_est into two datasets, 1974-1980 and 1981-2006
# For years 1974-1980 we only have single year data up to age 85 
# For years 1981-2006 we have single year data up to age 90

# Filter to get specific years
# Rename Age columns
# Reorder to set missing data first
# Add Scotland qualifier for totals

hb1995_pop_est_74_80 <- hb1995_pop_est %>% 
  filter(Year >= 1974 & Year <= 1980) %>% 
  rename(Age85plus = Age85) %>%
  setorder(na.last = F) %>% 
  mutate(HB = if_else(is.na(HB), "S92000003", as.character(HB)), 
         HBQF = case_when(HB == "S92000003" ~ "d"), 
         AllAges = rowSums(.[4:89])) %>% 
  select(Year, HB, HBQF, Sex, SexQF, AllAges, Age0:Age85plus)

hb1995_pop_est_81_06 <- hb1995_pop_est %>% 
  filter(Year >= 1981 & Year <= 2006) %>% 
  rename(Age90plus = Age90) %>%
  setorder(na.last = F) %>% 
  mutate(HB = if_else(is.na(HB), "S92000003", as.character(HB)), 
         HBQF = case_when(HB == "S92000003" ~ "d"), 
         AllAges = rowSums(.[4:94])) %>% 
  select(Year, HB, HBQF, Sex, SexQF, AllAges, Age0:Age90plus)

# Save in open data folder

fwrite(hb1995_pop_est_74_80, 
       glue("{od_filepath}/HB1995_pop_est_1974_1980_{date}.csv"))

fwrite(hb1995_pop_est_81_06, 
       glue("{od_filepath}/HB1995_pop_est_1981_2006_{date}.csv"))



### 3 - Read in HB2006 File ----

# Read in HB2006_pop_est_1981_2013.rds estimates
# Remove SPSS attributes and labels

hb2006_pop_est <- readRDS(glue("{data_filepath}/HB2006_pop_est_1981_2013.rds"))

# Select variables and rename
# Recode sex to strings and spread data to wide format

hb2006_pop_est %<>% 
  select(Year, HB2006, Age, Sex, Pop) %>%
  rename(HB = HB2006) %>% 
  mutate(Sex = recode(Sex, "1" = "Male", "2" = "Female")) %>% 
  arrange(Year, Sex) %>% 
  spread(Age, Pop) %>% 
  rename_at(4:94, function(x) paste("Age", x, sep=""))

# Create totals for male and female combined

all_total <- hb2006_pop_est %>% 
  group_by(Year, HB) %>% 
  summarise_at(vars(Age0:Age90), list(sum)) %>%
  ungroup() %>% 
  mutate(Sex = "All")

# Add all_total to hb2006_pop_est

hb2006_pop_est %<>% 
  bind_rows(all_total)

# Group by Year and Sex for all single years of age

Scot_total <- hb2006_pop_est %>%
  group_by(Year, Sex) %>%
  summarise_at(vars(Age0:Age90), list(sum)) %>%
  ungroup()

# Add Scot_total to geo_pop_est
# Sum across all ages to get totals
# Sort by Year, variable and Sex
# Rename Age90 to Age90plus
# Reorder to set missing data first

hb2006_pop_est %<>%
  full_join(Scot_total) %>%
  arrange(Year, Sex) %>%
  setorder(na.last = F) %>% 
  mutate(AllAges = rowSums(.[4:94]), 
         HB = if_else(is.na(HB), "S92000003", HB), 
         HBQF = case_when(HB == "S92000003" ~ "d"), 
         SexQF = case_when(Sex == "All" ~ "d")) %>%
  rename(Age90plus = Age90) %>% 
  select(Year, HB, HBQF, Sex, SexQF, AllAges, Age0:Age90plus)

# Save in open data folder

fwrite(hb2006_pop_est, glue("{od_filepath}/HB2006_pop_est_{date}.csv"))
