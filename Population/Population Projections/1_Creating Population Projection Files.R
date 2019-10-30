### 1 - Information ----

# Codename - Create Population Projection Files
# Original Author - Calum Purdie
# Original Date - 27/03/2019
# Updated on
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("readr")
# install.packages("janitor")
#
# Description - Code for creating ISD files for NRS population projections for 
# Scotland, Health Board, HSCP and Council Area
# Approximate run time - 30 seconds

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD", "2_Population", 
                           "Population Projections")
lookup_filepath <- file.path(base_filepath, "Lookup Files", "R Files")
data_filepath <- file.path(base_filepath, "Source Data")

# Read in packages from library
library(tidyr)
library(dplyr)
library(readxl)
library(readr)
library(janitor)

### 2 - Create Scotland population projection files ----

### 2.1 - Create Scotland population function to read in data ----

scot_pop <- function(filepath, cells, gender){
  
  scotproj_2018_2043 <- read_xlsx(path = filepath, sheet = "Principal", 
                                  range = cells)
  
  # Rename variables
  names(scotproj_2018_2043)<- c("Age","2018":"2043")
  
  scotproj_2018_2043 %>% 
    gather("Year", "Pop", '2018':'2043') %>% 
    mutate(Sex = gender) %>% 
    filter(Age != "All ages") %>% 
    mutate(Age = as.numeric(Age))
}


### 2.2 - Create male and female Scotland population projections ----

# Create male population projections
scot_pop_proj_2018_2043_m <- 
  scot_pop(filepath = file.path(base_filepath, "Source Data", 
                                "scot_pop_proj_2018_2043.xlsx"), 
           cells = "A137:AA264", gender = 1) %>% 
  clean_names()

# Create female population projections
scot_pop_proj_2018_2043_f <- 
  scot_pop(filepath = file.path(base_filepath, "Source Data", 
                                "scot_pop_proj_2018_2043.xlsx"), 
           cells = "A269:AA396", gender = 2) %>% 
  clean_names()


### 2.3 - Create Scotland population projections by single year of age ----

# Add the male and female files together
# Sort by Year, Age and Sex
# Set all variables as integers
# Create SexName column
# Recode all Age values greater than 90 to 90 for consistency with previous 
# projections
# group_by all columns except Pop and calculate new Pop values for Age = 90
# Ungroup and set AgeGroup as an integer

scot_pop_proj_2018_2043 <- bind_rows(scot_pop_proj_2018_2043_m, 
                                     scot_pop_proj_2018_2043_f) %>% 
  arrange(year, age, sex) %>% 
  mutate(year = as.integer(year), 
         pop = as.integer(pop), 
         sex = as.integer(sex), 
         sex_name = recode(sex, "1" = "Male", "2" = "Female"), 
         age = if_else(age > 90, 90, age)) %>%
  group_by(year, age, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(age = as.integer(age))

saveRDS(scot_pop_proj_2018_2043, 
        file.path(base_filepath, "Lookup Files", "R Files", 
                  "scot_pop_proj_2018_2043.rds"))



### 2.4 - Create file for 5 year age groups ----

# Create AgeGroup column for 5 year age groups
# Group by Year, AgeGroup and Sex - these are the columns you want to keep
# Summarise by pop to get the population for each age group
# Ungroup and set AgeGroup as an integer

scot_pop_proj_2018_2043_5y <- scot_pop_proj_2018_2043 %>% 
  mutate(age_group = case_when(age == 0 ~ 0, 
                              age >= 1 & age <= 4 ~ 1, 
                              age >= 5 & age <= 9 ~ 2, 
                              age >= 10 & age <= 14 ~ 3, 
                              age >= 15 & age <= 19 ~ 4, 
                              age >= 20 & age <= 24 ~ 5, 
                              age >= 25 & age <= 29 ~ 6, 
                              age >= 30 & age <= 34 ~ 7, 
                              age >= 35 & age <= 39 ~ 8, 
                              age >= 40 & age <= 44 ~ 9, 
                              age >= 45 & age <= 49 ~ 10, 
                              age >= 50 & age <= 54 ~ 11, 
                              age >= 55 & age <= 59 ~ 12, 
                              age >= 60 & age <= 64 ~ 13, 
                              age >= 65 & age <= 69 ~ 14, 
                              age >= 70 & age <= 74 ~ 15, 
                              age >= 75 & age <= 79 ~ 16, 
                              age >= 80 & age <= 84 ~ 17, 
                              age >= 85 & age <= 89 ~ 18, 
                              age >= 90 ~ 19), 
         age_group_name = case_when(age_group == 0 ~ "0", 
                                  age_group == 1 ~ "1-4", 
                                  age_group == 2 ~ "5-9", 
                                  age_group == 3 ~ "10-14", 
                                  age_group == 4 ~ "15-19", 
                                  age_group == 5 ~ "20-24", 
                                  age_group == 6 ~ "25-29", 
                                  age_group == 7 ~ "30-34", 
                                  age_group == 8 ~ "35-39", 
                                  age_group == 9 ~ "40-44", 
                                  age_group == 10 ~ "45-49", 
                                  age_group == 11 ~ "50-54", 
                                  age_group == 12 ~ "55-59", 
                                  age_group == 13 ~ "60-64", 
                                  age_group == 14 ~ "65-69", 
                                  age_group == 15 ~ "70-74", 
                                  age_group == 16 ~ "75-79", 
                                  age_group == 17 ~ "80-84", 
                                  age_group == 18 ~ "85-89", 
                                  age_group == 19 ~ "90+")) %>% 
  group_by(year, age_group, age_group_name, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(age_group = as.integer(age_group))

saveRDS(scot_pop_proj_2018_2043_5y, 
        file.path(base_filepath, "Lookup Files", "R Files", 
                  "scot_pop_proj_5year_agegroups_2018_2043.rds"))



### 3 - Create Council Area population projections files ----

### 3.1 - Create lower geography population function to read in the data ----

lower_geo_pop <- function(filepath, cells, gender){
  
  # Read in the sheet names within the excel file
  sheet_names <- excel_sheets(path = filepath)
  
  # Remove unnecessary names - only keep years
  sheet_names <- sheet_names[5:30]

  # Read in the data for each year using sheet_names
  list_all <- lapply(sheet_names, 
                     function(i) read_xlsx(path = filepath, sheet = i, 
                                           range = cells))
  
  # Add a column for Sex to each year's data and create a Year column
  lower_geo_2016_2041 <- mapply(cbind, list_all, "Sex" = gender, 
                                "Year" = seq.int(2016, 2041), SIMPLIFY = F)
  
  # Turn the list into a dataframe
  lower_geo_proj_2016_2041 <- do.call(rbind.data.frame, lower_geo_2016_2041)
  
  lower_geo_proj_2016_2041 <- lower_geo_proj_2016_2041 %>% 
    gather("Age", "Pop", 'All ages':'90') %>% 
    filter(Area != "SCOTLAND" & Age != "All ages")
}


### 3.2 - Create male and female Council Area projections ----

# Update filepaths for new release

# Create the male population projections
CA_proj_2016_2041_m <- lower_geo_pop(file.path(data_filepath, "Archive", 
                                               "2016_2041", 
                                               "ca_pop_proj_2016_2041.xlsx"), 
                                     cells = "A42:CP75", gender = 1) %>% 
  clean_names()

# Create the female population projections
CA_proj_2016_2041_f <- lower_geo_pop(file.path(data_filepath, "Archive", 
                                               "2016_2041", 
                                               "ca_pop_proj_2016_2041.xlsx"), 
                                     cells = "A80:CP113", gender = 2) %>% 
  clean_names()


### 3.3 - Create Council Area population projections by single year of age ----

# Add the male and female files together

CA_pop_proj_2016_2041 <- bind_rows(CA_proj_2016_2041_m, CA_proj_2016_2041_f) %>% 
  rename(ca2011 = code) %>% 
  mutate(ca2018 = recode(ca2011, 'S12000015' = 'S12000047', 
                                 'S12000024' = 'S12000048')) %>% 
  rename(ca2018_name = area) %>% 
  mutate(age = as.integer(age)) %>% 
  mutate(sex_name = recode(sex, "1" = "Male", "2" = "Female")) %>% 
  select(year, ca2018, ca2018_name, ca2011, age, sex, sex_name, pop) %>% 
  arrange(year, ca2018, age, sex) %>% 
  mutate(ca2018_name = gsub("&", "and", ca2018_name))

saveRDS(CA_pop_proj_2016_2041, 
        file.path(lookup_filepath, "CA2018_pop_proj_2016_2041.rds"))


### 3.4 - Create file for 5 year age groups ----

# Create AgeGroup column for 5 year age groups
# Group by Year, AgeGroup and Sex - these are the columns you want to keep
# Summarise by pop to get the population for each age group
# Ungroup and set AgeGroup as an integer
# Reorder columns

CA_pop_proj_2016_2041_5y <- CA_pop_proj_2016_2041 %>% 
  mutate(age_group = case_when(age == 0 ~ 0, 
                              age >= 1 & age <= 4 ~ 1, 
                              age >= 5 & age <= 9 ~ 2, 
                              age >= 10 & age <= 14 ~ 3, 
                              age >= 15 & age <= 19 ~ 4, 
                              age >= 20 & age <= 24 ~ 5, 
                              age >= 25 & age <= 29 ~ 6, 
                              age >= 30 & age <= 34 ~ 7, 
                              age >= 35 & age <= 39 ~ 8, 
                              age >= 40 & age <= 44 ~ 9, 
                              age >= 45 & age <= 49 ~ 10, 
                              age >= 50 & age <= 54 ~ 11, 
                              age >= 55 & age <= 59 ~ 12, 
                              age >= 60 & age <= 64 ~ 13, 
                              age >= 65 & age <= 69 ~ 14, 
                              age >= 70 & age <= 74 ~ 15, 
                              age >= 75 & age <= 79 ~ 16, 
                              age >= 80 & age <= 84 ~ 17, 
                              age >= 85 & age <= 89 ~ 18, 
                              age >= 90 ~ 19)) %>% 
  group_by(year, ca2018, ca2018_name, ca2011, age_group, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(age_group = as.integer(age_group)) %>% 
  select(year, ca2018, ca2018_name, ca2011, age_group, sex, sex_name, pop)

saveRDS(CA_pop_proj_2016_2041_5y, 
        file.path(lookup_filepath, 
                  "CA2018_pop_proj_5year_agegroups_2016_2041.rds"))



### 4 - Create Health Board population projections files ----

### 4.1 - Use lower geography population function to read in data ----

# Create the male population projections
HB_proj_2016_2041_m <- lower_geo_pop(file.path(data_filepath, "Archive", 
                                               "2016_2041", 
                                               "hb_pop_proj_2016_2041.xlsx"), 
                                     cells = "A24:CP39", gender = 1) %>% 
  clean_names()

# Create the female population projections
HB_proj_2016_2041_f <- lower_geo_pop(file.path(data_filepath, "Archive", 
                                               "2016_2041", 
                                               "hb_pop_proj_2016_2041.xlsx"), 
                                     cells = "A44:CP59", gender = 2) %>% 
  clean_names()


### 4.2 - Create Council Area population projections by single year of age ----

# Add the male and female files together

HB_pop_proj_2016_2041 <- bind_rows(HB_proj_2016_2041_m, HB_proj_2016_2041_f) %>% 
  rename(hb2014 = code) %>% 
  mutate(hb2018 = recode(hb2014, 'S08000018' = 'S08000029', 
                                 'S08000027' = 'S08000030')) %>% 
  rename(hb2018_name = area) %>% 
  mutate(age = as.integer(age)) %>% 
  mutate(sex_name = recode(sex, "1" = "Male", "2" = "Female")) %>% 
  select(year, hb2018, hb2018_name, hb2014, age, sex, sex_name, pop) %>% 
  arrange(year, hb2018, age, sex) %>% 
  mutate(hb2018_name = gsub("&", "and", hb2018_name))

saveRDS(HB_pop_proj_2016_2041, file.path(lookup_filepath, 
                                         "HB2018_pop_proj_2016_2041.rds"))


### 4.3 - Create file for 5 year age groups ----

# Create AgeGroup column for 5 year age groups
# Group by Year, AgeGroup and Sex - these are the columns you want to keep
# Summarise by pop to get the population for each age group
# Ungroup and set AgeGroup as an integer
# Reorder columns
HB_pop_proj_2016_2041_5y <- HB_pop_proj_2016_2041 %>% 
  mutate(age_group = case_when(age == 0 ~ 0, 
                               age >= 1 & age <= 4 ~ 1, 
                               age >= 5 & age <= 9 ~ 2, 
                               age >= 10 & age <= 14 ~ 3, 
                               age >= 15 & age <= 19 ~ 4, 
                               age >= 20 & age <= 24 ~ 5, 
                               age >= 25 & age <= 29 ~ 6, 
                               age >= 30 & age <= 34 ~ 7, 
                               age >= 35 & age <= 39 ~ 8, 
                               age >= 40 & age <= 44 ~ 9, 
                               age >= 45 & age <= 49 ~ 10, 
                               age >= 50 & age <= 54 ~ 11, 
                               age >= 55 & age <= 59 ~ 12, 
                               age >= 60 & age <= 64 ~ 13, 
                               age >= 65 & age <= 69 ~ 14, 
                               age >= 70 & age <= 74 ~ 15, 
                               age >= 75 & age <= 79 ~ 16, 
                               age >= 80 & age <= 84 ~ 17, 
                               age >= 85 & age <= 89 ~ 18, 
                               age >= 90 ~ 19)) %>% 
  group_by(year, hb2018, hb2018_name, hb2014, age_group, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(age_group = as.integer(age_group)) %>% 
  select(year, hb2018, hb2018_name, hb2014, age_group, sex, sex_name, pop)

saveRDS(HB_pop_proj_2016_2041_5y, 
        file.path(lookup_filepath, 
                  "HB2018_pop_proj_5year_agegroups_2016_2041.rds"))



### 5 - Create HSCP population projection files ----

### 5.1 - Create HSCP population projections by single year of age ----

# Read in CA_HSCP Lookup file
CA_HSCP_Lookup <- read_csv(paste0("//Isdsf00d03/cl-out/lookups/Unicode/",
                                  "Geography/HSCP Locality/", 
                                  "HSCP Localities_DZ11_Lookup_20180903.csv")) %>% 
  select(CA2018, HSCP2018, HSCP2019Name, HSCP2016) %>% 
  distinct() %>% 
  rename(ca2018 = CA2018, 
         hscp2018 = HSCP2018, 
         hscp2018_name = HSCP2019Name, 
         hscp2016 = HSCP2016)

# Get the Council Area population proejction file
HSCP_pop_proj_2016_2041 <- CA_pop_proj_2016_2041 %>% 
  arrange(ca2018) %>% 
  left_join(CA_HSCP_Lookup) %>% 
  group_by(year, hscp2018, hscp2018_name, hscp2016, age, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

saveRDS(HSCP_pop_proj_2016_2041, file.path(lookup_filepath, 
                                           "HSCP2018_pop_proj_2016_2041.rds"))



### 5.2 - Create file for 5 year age groups ----

# Get the Council Area population proejction file

HSCP_pop_proj_2016_2041_5y <- CA_pop_proj_2016_2041_5y %>% 
  arrange(ca2018) %>% 
  left_join(CA_HSCP_Lookup) %>% 
  group_by(year, hscp2018, hscp2018_name, hscp2016, age_group, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

saveRDS(HSCP_pop_proj_2016_2041_5y, 
        file.path(lookup_filepath, 
                  "HSCP2018_pop_proj_5year_agegroups_2016_2041.rds"))
