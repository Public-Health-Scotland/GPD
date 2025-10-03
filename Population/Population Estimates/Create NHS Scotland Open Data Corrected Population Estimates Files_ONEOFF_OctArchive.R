### 1 - Information ----

# Codename - Create NHS Scotland Open Data Corrected Population Estimates Files
# Data release - Corrected Population Estimates for 2002 - 2010
# Original Author - Calum Purdie
# Original Date - 01/07/2019
# Type - Preparation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
#
# Description - Create NHS Scotland
# Approximate run time - 10 seconds

# Read in packages from library
library(tidyr)
library(dplyr)
library(data.table)
library(readr)

# Set working directory to R Code folder
base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI")
data_filepath <- file.path(base_filepath, "Referencing & Standards", "GPD", "2_Population", 
                           "Population Estimates", "Lookup Files", "R Files")
open_data_filepath <- file.path(base_filepath, "Publications", "Open Data (Non Health Topic)", "Data", 
                                "OD1700007 - Population Estimates")
HB2006_filepath <- file.path(base_filepath, "Referencing & Standards", "GPD", "1_Geography", 
                             "Scottish Postcode Directory", "Lookup Files", "R Files")



### 2 - Create function to read in population data and restructure ----

geo_pop <- function(data, variable){
  
  geo_pop_est_1981_2018 <- readRDS(data) %>% 
    select(Year, variable, Age, SexName, Pop) %>% 
    rename(Sex = SexName) %>% 
    mutate(Sex = recode(Sex, "M" = "Male", "F" = "Female")) %>% 
    arrange(Year, Sex) %>% 
    spread(Age, Pop)
  
  # Rename Age variables
  colnames(geo_pop_est_1981_2018)[4:94] <- paste("Age", colnames(geo_pop_est_1981_2018[,c(4:94)]), sep="")
  
  # Group by Year and Sex for all single years of age
  Scot_total <- geo_pop_est_1981_2018 %>%
    group_by(Year, Sex) %>%
    summarise_at(vars(Age0:Age90), list(sum)) %>%
    ungroup()
  
  # Add CA2011_pop_est_1982_2018 and Scot_total
  # Sum across all ages to get totals
  # Sort by Year, CA2011 and Sex
  geo_pop_est_1981_2018_CKAN <- geo_pop_est_1981_2018 %>%
    full_join(Scot_total) %>%
    mutate(AllAges = rowSums(.[4:94])) %>%
    arrange(Year, Sex)
  
  # Recode Sex to character and rename Age90 to Age90plus
  geo_pop_est_1981_2018_CKAN <- geo_pop_est_1981_2018_CKAN %>%
    rename(Age90plus = Age90)
  
  # Reorder to set missing data (Scotland CA2011) first
  geo_pop_est_1981_2018_CKAN <- setorder(geo_pop_est_1981_2018_CKAN, na.last = F)
  
}

### 3 - Read in Population Estimates ----

# Read in CA2011 estimates
CA2011_pop_est_1981_2018 <- geo_pop(data = file.path(data_filepath, "CA2019_pop_est_1981_2018.rds"), variable = "CA2019")

# Read in HB2014 estimates
HB2014_pop_est_1981_2018 <- geo_pop(data = file.path(data_filepath, "HB2019_pop_est_1981_2018.rds"), variable = "HB2019")

# Read in HSCP2016 estimates
HSCP2016_pop_est_1981_2018 <- geo_pop(data = file.path(data_filepath, "HSCP2019_pop_est_1981_2018.rds"), variable = "HSCP2019")



### 4 - Tidy CA2011 Data ----

# Due to minor boundary changes from 02/02/2018 and 01/04/2019 which affected some CA2011 codes, the affected codes
# need to be updated and this requires a qualifier field to show the revised codes

# Recode CA2011 to reflect new codes
CA2011_pop_est_1981_2018 <- CA2011_pop_est_1981_2018 %>%
  rename(CA2011 = CA2019)

# Attach Scotland national code
CA2011_pop_est_1981_2018$CA2011[is.na(CA2011_pop_est_1981_2018$CA2011)] <- 'S92000003'

# Create qualifier column for CA2011 and  set it to "r" for revised CA2011 codes
# and set it to "d" for Scotland totals
CA2011_pop_est_1981_2018 <- CA2011_pop_est_1981_2018 %>%
  mutate(CA2011QF = case_when(CA2011 == "S92000003" ~ "d", 
                              CA2011 == "S12000047" ~ "r", 
                              CA2011 == "S12000048" ~ "r", 
                              CA2011 == "S12000049" ~ "r", 
                              CA2011 == "S12000050" ~ "r"))

# Reorder columns
CA2011_pop_est_1981_2018 <- CA2011_pop_est_1981_2018 %>%
  select(Year, CA2011, CA2011QF, Sex, AllAges, Age0:Age90plus)

# Write as csv
# Change date to date the file is saved
write_csv(CA2011_pop_est_1981_2018, file.path(open_data_filepath, "CA2011_pop_est_01072019.csv"), na = "")

### 5 - Tidy HB2014 Data ----

# Due to a minor boundary change from 02/02/2018 and 01/04/2019 which affected some HB2014 codes, the affected codes
# need to be updated and this requires a qualifier field to show the revised codes

# Recode HB2014 to reflect new codes
HB2014_pop_est_1981_2018 <- HB2014_pop_est_1981_2018 %>%
  rename(HB2014 = HB2019)

# Attach Scotland national code
HB2014_pop_est_1981_2018$HB2014[is.na(HB2014_pop_est_1981_2018$HB2014)] <- 'S92000003'

# Create qualifier column for CA2011 and  set it to "r" for revised HB2014 codes
# and set it to "d" for Scotland totals
HB2014_pop_est_1981_2018 <- HB2014_pop_est_1981_2018 %>%
  mutate(HB2014QF = case_when(HB2014 == "S92000003" ~ "d", 
                              HB2014 == "S08000029" ~ "r", 
                              HB2014 == "S08000030" ~ "r", 
                              HB2014 == "S08000031" ~ "r", 
                              HB2014 == "S08000032" ~ "r"))

# Reorder columns
HB2014_pop_est_1981_2018 <- HB2014_pop_est_1981_2018 %>%
  select(Year, HB2014, HB2014QF, Sex, AllAges, Age0:Age90plus)

# Write as csv
# Change date to date the file is saved
write_csv(HB2014_pop_est_1981_2018, file.path(open_data_filepath, "HB2014_pop_est_01072019.csv"), na = "")



### 6 - Tidy HSCP2016 Data ----

# Due to a minor boundary change from 02/02/2018 and 01/04/2019 which affected some HSCP2016 codes, the affected codes
# need to be updated and this requires a qualifier field to show the revised codes

# Recode CA2011 to reflect new codes
HSCP2016_pop_est_1981_2018 <- HSCP2016_pop_est_1981_2018 %>%
  rename(HSCP2016 = HSCP2019)

# Attach Scotland national code
HSCP2016_pop_est_1981_2018$HSCP2016[is.na(HSCP2016_pop_est_1981_2018$HSCP2016)] <- 'S92000003'

# Create qualifier column for CA2011 and  set it to "r" for revised CA2011 codes
# and set it to "d" for Scotland totals
HSCP2016_pop_est_1981_2018 <- HSCP2016_pop_est_1981_2018 %>%
  mutate(HSCP2016QF = case_when(HSCP2016 == "S92000003" ~ "d", 
                                HSCP2016 == "S37000032" ~ "r", 
                                HSCP2016 == "S37000033" ~ "r", 
                                HSCP2016 == "S37000034" ~ "r", 
                                HSCP2016 == "S37000035" ~ "r"))

# Reorder columns
HSCP2016_pop_est_1981_2018 <- HSCP2016_pop_est_1981_2018 %>%
  select(Year, HSCP2016, HSCP2016QF, Sex, AllAges, Age0:Age90plus)

# Write as csv
# Change date to date the file is saved
write_csv(HSCP2016_pop_est_1981_2018, file.path(open_data_filepath, "HSCP2016_pop_est_01072019.csv"), na = "")



### 7 - Update HB2006 Files ----

# Read in HB2006 estimates
HB2006_pop_est <- geo_pop(file.path(data_filepath, "HB2006_pop_est_1981_2013.rds"), "HB2006")

# Attach Scotland national code
HB2006_pop_est$HB2006[is.na(HB2006_pop_est$HB2006)] <- 'S92000003'

# Reorder columns
HB2006_pop_est <- HB2006_pop_est %>% 
  rename(HB = HB2006) %>% 
  mutate(HBQF = case_when(HB == "S92000003" ~ "d")) %>%
  select(Year, HB, HBQF, Sex, AllAges, Age0:Age90plus)

# Write as csv
# Change date to date the file is saved
write_csv(HB2006_pop_est, file.path(open_data_filepath, "HB2006_pop_est_02042020.csv"), na = "")
