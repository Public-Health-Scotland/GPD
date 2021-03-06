##########################################################
# Create Open Data SAPE Files
# Calum Purdie
# Original date 05/06/2020
# Latest update author - Calum Purdie
# Latest update date - 01/09/2020
# Latest update description 
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Creating open data files for Small Area Population Estimates files for
# yearly NRS release
# Approximate run time
##########################################################


### 1 - Housekeeping ----

# Read in packages from library

library(magrittr)
library(dplyr)
library(data.table)
library(tidylog)
library(janitor)
library(glue)

# Set filepaths

lookup_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                        "/2_Population/Small Area Population estimates/", 
                        "Lookup Files/R Files")
od_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Publications/", 
                    "Open Data (Non Health Topic)/Data/", 
                    "OD1700007 - Population Estimates")

# Set date for open data filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")

# Set new year

year <- 2019

# Set datasets to use

new_dz_estimates <- glue("DataZone2011_pop_est_2011_{year}")
new_dz_estimates_5y <- glue("DataZone2011_pop_est_5year_agegroups_2011_{year}")
new_iz_estimates <- glue("IntZone2011_pop_est_2011_{year}")
new_iz_estimates_5y <- glue("IntZone2011_pop_est_5year_agegroups_2011_{year}")
dz_estimates_2001_2010 <- "DataZone2011_pop_est_2001_2010"
iz_estimates_2001_2010 <- "IntZone2011_pop_est_2001_2010"



### 2 - Create Open Data 2011 DZ Population Estimates Files ----

### 2.1 - 2011 DataZone - Single Year of Age ----

# Read in current data zone estimates and select relevant columns

dz2011_pop_est <- readRDS(glue("{lookup_filepath}/{new_dz_estimates}.rds")) %>% 
  select(-c(datazone2011name, intzone2011:simd2016_crime_rank))

# Need to add in the rebased 2001 - 2010 dz estimates for the open data file
# It was agreed to keep all estimates in one open data file for consistency

dz2011_pop_est_2001_2010 <- readRDS(glue("{lookup_filepath}/", 
                                         "{dz_estimates_2001_2010}.rds")) %>% 
  select(-c(datazone2011name))

# Add dz2011_pop_est_2001_2010 and dz2011_pop_est to create open data file
# Capitalise Age for each age variable

dz2011_pop_est_od <- dz2011_pop_est_2001_2010 %>% 
  bind_rows(dz2011_pop_est) %>% 
  rename(Year = year, DataZone = datazone2011, Sex = sex, AllAges = total_pop) %>% 
  set_colnames(gsub("age", "Age", names(.)))


### 2.2 - Create Totals and Add to Single Year File ----

# Create totals for male and female combined

all_total <- dz2011_pop_est_od %>% 
  group_by(Year, DataZone) %>% 
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>% 
  mutate(Sex = "All")

# Add all_total to dz2011_pop_est_od

dz2011_pop_est_od %<>% 
  bind_rows(all_total)

# Group by Year and Sex for all single year of age

scot_total <- dz2011_pop_est_od %>%
  group_by(Year, Sex) %>%
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>%
  arrange(Year, desc(Sex))

# Combine DZ2011_pop_est and scot_total together
# Recode Sex variable. Sort by Year and Sex
# Scotland total is at the end for each year so move this to start using 
# setorder()

dz2011_pop_est_od %<>%
  full_join(scot_total) %>%
  mutate(Sex = recode(Sex, 'M' = 'Male', 'F' = 'Female')) %>% 
  arrange(Year, Sex) %>%
  setorder(na.last = F)


### 2.3 - Tidy dz2011_pop_est_od ----

# Attach Scotland national code
# Create qualifier column for DataZone and set it to "d" for Scotland totals

dz2011_pop_est_od %<>%
  mutate(DataZone = if_else(is.na(DataZone), "S92000003", DataZone), 
         DataZoneQF = case_when(DataZone == "S92000003" ~ "d"), 
         SexQF =  case_when(Sex == "All" ~ "d")) %>%
  select(Year, DataZone, DataZoneQF, Sex, SexQF, AllAges, Age0:Age90plus)

# Save as csv

fwrite(dz2011_pop_est_od, glue("{od_filepath}/DZ2011-pop-est_{date}.csv"), 
       na = "")



### 3 - Create Open Data 2011 IZ Population Estimates Files ----

### 3.1 - 2011 IntZone - Single Year of Age ----

# Read in current intermediate zone estimates and select relevant columns

iz2011_pop_est <- readRDS(glue("{lookup_filepath}/{new_iz_estimates}.rds")) %>% 
  select(-intzone2011name)

# Need to add in the rebased 2001 - 2010 iz estimates for the open data file
# It was agreed to keep all estimates in one open data file for consistency

iz2011_pop_est_2001_2010 <- readRDS(glue("{lookup_filepath}/", 
                                         "{iz_estimates_2001_2010}.rds")) %>% 
  select(-intzone2011name)

# Add iz2011_pop_est_2001_2010 and iz2011_pop_est to create open data file
# Capitalise Age for each age variable

iz2011_pop_est_OD <- iz2011_pop_est_2001_2010 %>% 
  bind_rows(iz2011_pop_est) %>% 
  rename(Year = year, IntZone = intzone2011, Sex = sex, AllAges = total_pop) %>% 
  set_colnames(gsub("age", "Age", names(.)))



### 3.2 - Create Totals and Add to Single Year File ----

# Create totals for male and female combined

all_total <- iz2011_pop_est_OD %>% 
  group_by(Year, IntZone) %>% 
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>% 
  mutate(Sex = "All")

# Add all_total to iz2011_pop_est_OD

iz2011_pop_est_OD %<>% 
  bind_rows(all_total)

# Group by Year and Sex for all single year of age

scot_total <- iz2011_pop_est_OD %>%
  group_by(Year, Sex) %>%
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>%
  arrange(Year, desc(Sex))

# Combine IZ2011_pop_est_OD and scot_total together
# Recode Sex variable. Sort by Year and Sex
# Scotland total is at the end for each year so move this to start using 
# setorder()

iz2011_pop_est_OD %<>%
  full_join(scot_total) %>%
  mutate(Sex = recode(Sex, 'M' = 'Male', 'F' = 'Female')) %>% 
  arrange(Year, Sex) %>%
  setorder(na.last = F)


### 3.3 - Tidy iz2011_pop_est ----

# Attach Scotland national code
# Create qualifier column for IntZone and set it to "d" for Scotland totals

iz2011_pop_est_OD %<>%
  mutate(IntZone = if_else(is.na(IntZone), "S92000003", IntZone), 
         IntZoneQF = case_when(IntZone == "S92000003" ~ "d"), 
         SexQF = case_when(Sex == "All" ~ "d")) %>%
  select(Year, IntZone, IntZoneQF, Sex, SexQF, AllAges, Age0:Age90plus)

# Save as csv

fwrite(iz2011_pop_est_OD, glue("{od_filepath}/IZ2011-pop-est_{date}.csv"), 
       na = "")
