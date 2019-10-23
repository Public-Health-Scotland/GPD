### 1 - Information ----

# Codename - Create CKAN Population Projection Files
# Original Author - Calum Purdie
# Original Date - 16/04/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
#
# Description - This document is for creating population projection files for use on the NHS Scotland Open Data platform. 
#               It is designed to allow for the same data to be inputted and to provide the same output files.
# Approximate run time - 22 seconds

library(tidyr)
library(dplyr)
library(data.table)

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD", 
                           "2_Population", "Population Projections", "Lookup Files", "R Files")
od_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                         "Open Data (Non Health Topic)", "Data", "OD1900004 - Population Projections")

### 2 - Create function to read in population data and restructure ----

pop_proj <- function(filepath, a, b){
  
  geo_pop_proj_2016_2041 <- readRDS(filepath) %>% 
    select(-sex) %>% 
    rename(Sex = sex_name, 
           Year = year, 
           Age = age, 
           Pop = pop) %>% 
    arrange(Year, Sex, Age) %>% 
    spread(Age, Pop)
  
  # Rename Age variables
  colnames(geo_pop_proj_2016_2041)[a:b] <- paste("Age", colnames(geo_pop_proj_2016_2041[,c(a:b)]), sep="")
  
  # Group by Year and Sex for all single years of age for calculating Scotland total
  Scot_total <- geo_pop_proj_2016_2041 %>%
    group_by(Year, Sex) %>%
    summarise_at(vars(Age0:Age90), list(sum)) %>%
    ungroup()
  
  # Add scot_pop_proj_2016_2041 and Scot_total
  # Sum across all ages to get totals
  # Sort by Year and Sex
  geo_pop_proj_2016_2041_CKAN <- geo_pop_proj_2016_2041 %>%
    full_join(Scot_total) %>%
    mutate(AllAges = rowSums(.[a:b])) %>%
    arrange(Year, Sex)
  
  # Reorder to set missing data (Scotland CA2011) first
  geo_pop_proj_2016_2041_CKAN <- setorder(geo_pop_proj_2016_2041_CKAN, na.last = F)

  # Recode Sex to character and rename Age90 to Age90plus
  geo_pop_proj_2016_2041_CKAN <- geo_pop_proj_2016_2041_CKAN %>%
    rename(Age90plus = Age90)
  
}

### 3 - Read in Population Estimates ----

# Read in Scotland projections
Scot_pop_proj_2018_2043 <- pop_proj(filepath = file.path(base_filepath, "scot_pop_proj_2018_2043.rds"), a = 3, b = 93)

# Read in CA2011 estimates
CA2011_pop_proj_2016_2041 <- pop_proj(filepath = file.path(base_filepath, "CA2018_pop_proj_2016_2041.rds"), a = 6, b = 96)

# Read in HB2014 estimates
HB2014_pop_proj_2016_2041 <- pop_proj(filepath = file.path(base_filepath, "HB2018_pop_proj_2016_2041.rds"), a = 6, b = 96)

# Read in HSCP2016 estimates
HSCP2016_pop_proj_2016_2041 <- pop_proj(filepath = file.path(base_filepath, "HSCP2018_pop_proj_2016_2041.rds"), a = 6, b = 96)


### 4 - Tidy Scotland data ----

Scot_pop_proj_2018_2043 <- Scot_pop_proj_2018_2043 %>% 
  mutate(Country = "S92000003") %>% 
  select(Year, Country, Sex, AllAges, Age0:Age90plus)

### Update filename with today's date ###
write_csv(Scot_pop_proj_2018_2043, file.path(od_filepath, "scotland-pop-proj_21102019.csv"), na = "")



### 5 - Tidy CA2011 data ----

# Due to minor boundary changes from 02/02/2018 and 01/04/2019 which affected some CA2011 codes, the affected codes
# need to be updated and this requires a qualifier field to show the revised codes

# Recode CA2011 to reflect new codes
CA2011_pop_proj_2016_2041 <- CA2011_pop_proj_2016_2041 %>%
  mutate(CA2011 = recode(CA2011, 
                         S12000015 = "S12000047", 
                         S12000024 = "S12000048", 
                         S12000046 = "S12000049", 
                         S12000044 = "S12000050"))

# Attach Scotland national code
CA2011_pop_proj_2016_2041$CA2011[is.na(CA2011_pop_proj_2016_2041$CA2011)] <- 'S92000003'

# Create qualifier column for CA2011 and  set it to "r" for revised CA2011 codes
# and set it to "d" for Scotland totals
CA2011_pop_proj_2016_2041 <- CA2011_pop_proj_2016_2041 %>%
  mutate(CA2011QF = case_when(CA2011 == "S92000003" ~ "d", 
                              CA2011 == "S12000047" ~ "r", 
                              CA2011 == "S12000048" ~ "r", 
                              CA2011 == "S12000049" ~ "r", 
                              CA2011 == "S12000050" ~ "r"))

# Reorder columns
CA2011_pop_proj_2016_2041 <- CA2011_pop_proj_2016_2041 %>%
  select(Year, CA2011, CA2011QF, Sex, AllAges, Age0:Age90plus)

# Write as csv
# Change date to date the file is saved
write.csv(CA2011_pop_proj_2016_2041, "//Freddy/DEPT/PHIBCS/PHI/Publications/Open Data (Non Health Topic)/Data/OD1900004 - Population Projections/CA2011-pop-proj_17042019.csv", na = "", row.names = FALSE)


### 6 - Tidy HB2014 data ----

# Due to a minor boundary change from 02/02/2018 and 01/04/2019 which affected some HB2014 codes, the affected codes
# need to be updated and this requires a qualifier field to show the revised codes

# Recode CA2011 to reflect new codes
HB2014_pop_proj_2016_2041 <- HB2014_pop_proj_2016_2041 %>%
  mutate(HB2014 = recode(HB2014, 
                         S08000018 = "S08000029", 
                         S08000027 = "S08000030", 
                         S08000021 = "S08000031", 
                         S08000023 = "S08000032"))

# Attach Scotland national code
HB2014_pop_proj_2016_2041$HB2014[is.na(HB2014_pop_proj_2016_2041$HB2014)] <- 'S92000003'

# Create qualifier column for CA2011 and  set it to "r" for revised HB2014 codes
# and set it to "d" for Scotland totals
HB2014_pop_proj_2016_2041 <- HB2014_pop_proj_2016_2041 %>%
  mutate(HB2014QF = case_when(HB2014 == "S92000003" ~ "d", 
                              HB2014 == "S08000029" ~ "r", 
                              HB2014 == "S08000030" ~ "r", 
                              HB2014 == "S08000031" ~ "r", 
                              HB2014 == "S08000032" ~ "r"))

# Reorder columns
HB2014_pop_proj_2016_2041 <- HB2014_pop_proj_2016_2041 %>%
  select(Year, HB2014, HB2014QF, Sex, AllAges, Age0:Age90plus)

# Write as csv
# Change date to date the file is saved
write.csv(HB2014_pop_proj_2016_2041, "//Freddy/DEPT/PHIBCS/PHI/Publications/Open Data (Non Health Topic)/Data/OD1900004 - Population Projections/HB2014-pop-proj_17042019.csv", na = "", row.names = FALSE)


### 7 - Tidy HSCP2016 data ----

# Due to a minor boundary change from 02/02/2018 and 01/04/2019 which affected some HSCP2016 codes, the affected codes
# need to be updated and this requires a qualifier field to show the revised codes

# Recode CA2011 to reflect new codes
HSCP2016_pop_proj_2016_2041 <- HSCP2016_pop_proj_2016_2041 %>%
  mutate(HSCP2016 = recode(HSCP2016, 
                           S37000014 = "S37000032", 
                           S37000023 = "S37000033", 
                           S37000015 = "S37000034", 
                           S37000021 = "S37000035"))

# Attach Scotland national code
HSCP2016_pop_proj_2016_2041$HSCP2016[is.na(HSCP2016_pop_proj_2016_2041$HSCP2016)] <- 'S92000003'

# Create qualifier column for CA2011 and  set it to "r" for revised CA2011 codes
# and set it to "d" for Scotland totals
HSCP2016_pop_proj_2016_2041 <- HSCP2016_pop_proj_2016_2041 %>%
  mutate(HSCP2016QF = case_when(HSCP2016 == "S92000003" ~ "d", 
                                HSCP2016 == "S37000032" ~ "r", 
                                HSCP2016 == "S37000033" ~ "r", 
                                HSCP2016 == "S37000034" ~ "r", 
                                HSCP2016 == "S37000035" ~ "r"))

# Reorder columns
HSCP2016_pop_proj_2016_2041 <- HSCP2016_pop_proj_2016_2041 %>%
  select(Year, HSCP2016, HSCP2016QF, Sex, AllAges, Age0:Age90plus)

# Write as csv
# Change date to date the file is saved
write.csv(HSCP2016_pop_proj_2016_2041, "//Freddy/DEPT/PHIBCS/PHI/Publications/Open Data (Non Health Topic)/Data/OD1900004 - Population Projections/HSCP2016-pop-proj_17042019.csv", na = "", row.names = FALSE)
