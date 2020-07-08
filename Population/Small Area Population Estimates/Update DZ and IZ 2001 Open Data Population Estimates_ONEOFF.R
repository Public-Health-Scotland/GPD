##########################################################
# Update DZ and IZ 2001 Open Data Population Estimates
# Calum Purdie
# Original date 03/04/2020
# Latest update author - Calum Purdie
# Latest update date - 02/07/2020
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code to update 2001 Data Zone and Int Zone populatione estimates on the 
# Scottish Health and Social Care Open Data portal.
# Approximate run time
##########################################################

### 1 - Housekeeping ----

# Read in packages from library

library(magrittr)
library(dplyr)
library(readr)
library(tidylog)
library(glue)
library(data.table)

# Set filepaths

cl_out <- glue("//Isdsf00d03/cl-out/lookups/Unicode/Populations/Estimates")
open_data <- glue("//Freddy/DEPT/PHIBCS/PHI/Publications/", 
                  "Open Data (Non Health Topic)/Data/", 
                  "OD1700007 - Population Estimates")

# Set date for open data filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")



### 2 Data Zone ----

dz01 <- readRDS(glue("{cl_out}/DataZone2001_pop_est_2001_2014.rds")) %>% 
  select(Year:total_pop)

colnames(dz01) <- gsub("AGE", "Age", colnames(dz01))

# Create totals for male and female combined

all_total <- dz01 %>% 
  group_by(Year, DataZone2001) %>% 
  summarise_at(vars(Age0:total_pop), list(sum)) %>%
  ungroup() %>% 
  mutate(SEX = "All")

# Add all_total to geo_pop_est

dz01 %<>% 
  full_join(all_total)

# Scotland totals
# Group by Year and Sex for all single year of age

Scot_total <- dz01 %>%
  group_by(Year, SEX) %>%
  summarise_at(vars(Age0:total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(Year, desc(SEX))

dz01 %<>%
  full_join(Scot_total) %>%
  rename(DataZone = DataZone2001, 
         Sex = SEX, 
         Age90plus = Age90PLUS, 
         AllAges = total_pop)  %>% 
  arrange(Year, Sex) %>%
  setorder(na.last = F) %>%
  mutate(DataZone = case_when(is.na(DataZone) ~ "S92000003", 
                              TRUE ~ DataZone), 
         DataZoneQF = case_when(DataZone == "S92000003" ~ "d"),
         Sex = recode(Sex, "M" = "Male", "F" = "Female"), 
         SexQF = case_when(Sex == "All" ~ "d")) %>% 
  select(Year, DataZone, DataZoneQF, Sex, SexQF, AllAges, Age0:Age90plus)

fwrite(dz01, glue("{open_data}/DZ2001_pop_est_{date}.csv"))


### 3 Int Zone ----

iz01 <- readRDS(glue("{cl_out}/IntZone2001_pop_est_2001_2014.rds"))

colnames(iz01) <- gsub("AGE", "Age", colnames(iz01))

# Create totals for male and female combined

all_total <- iz01 %>% 
  group_by(Year, IntZone2001) %>% 
  summarise_at(vars(Age0:Total_pop), list(sum)) %>%
  ungroup() %>% 
  mutate(SEX = "All")

# Add all_total to geo_pop_est

iz01 %<>% 
  full_join(all_total)

# Scotland totals

# Group by Year and Sex for all single year of age

Scot_total <- iz01 %>%
  group_by(Year, SEX) %>%
  summarise_at(vars(Age0:Total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(Year, desc(SEX))

iz01 %<>%
  full_join(Scot_total) %>%
  rename(IntZone = IntZone2001, 
         Sex = SEX, 
         Age90plus = Age90PLUS, 
         AllAges = Total_pop) %>%
  arrange(Year, Sex) %>%
  setorder(na.last = F) %>% 
  mutate(IntZone = case_when(is.na(IntZone) ~ "S92000003", 
                             TRUE ~ IntZone), 
         IntZoneQF = case_when(IntZone == "S92000003" ~ "d"),
         Sex = recode(Sex, "M" = "Male", "F" = "Female"), 
         SexQF = case_when(Sex == "All" ~ "d")) %>% 
  select(Year, IntZone, IntZoneQF, Sex, SexQF, AllAges, Age0:Age90plus)

fwrite(iz01, glue("{open_data}/IZ2001_pop_est_{date}.csv"))
