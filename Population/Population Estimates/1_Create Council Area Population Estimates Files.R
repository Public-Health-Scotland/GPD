### 1 - Information ----

# Codename - Create Council Area Population Estimates Files
# Data release - Mid-year Council Area Population Estimates
# Original Author - Tina Fu
# Original Date - 01/03/2018
# Updated - 05/11/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("magrittr")
# install.packages("readxl")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("tidylog")
# install.packafes("glue")
#
# Description - Code for creating Council Area population estimates files 
#               based on mid-year estimates released by NRS
#
# Approximate run time - <1 second

# Read in packages from library

library(magrittr)
library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(tidylog)
library(glue)

# Set filepaths

filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                      "Referencing & Standards", "GPD", "2_Population", 
                      "Population Estimates")
data_filepath <- file.path(filepath, "Source Data")
output_filepath <- file.path(filepath, "Lookup Files", "R Files")
archive_filepath <- file.path(output_filepath, "Archive")


### 2 - Import Male and Female Data ----

### 2.1 - Import Data ----

# Male
# Remove unnessary columns
# Transpose the dataset

male <- read_excel(glue("{data_filepath}/Population_Estimates_2018.xlsx"), 
                       sheet = "Table 2", range = "A57:CQ91") %>% 
  select(-c('...4', `All Ages`)) %>% 
  filter(Area1 != "Council areas", Area1 != "Scotland") %>% 
  gather(Age, Pop, `0`:`90+`) %>% 
  mutate(Sex = 1)

# Female
# Remove unnessary columns
# Transpose the dataset

female <- read_excel(glue("{data_filepath}/Population_Estimates_2018.xlsx"), 
                       sheet = "Table 2", range = "A110:CQ144") %>% 
  select(-'...4', -`All Ages`) %>%
  filter(Area1 != "Council areas", Area1 != "Scotland") %>% 
  gather(Age, Pop, `0`:`90+`) %>% 
  mutate(Sex = 2)



### 2.2 - Add Male and Female Files Together ----

# Add male and female together
# Change age of "90+" to "90" and make it as numeric rather than character

mandf <- bind_rows(male, female) %>% 
  mutate(Year = 2018) %>% 
  mutate(Age = case_when(Age == "90+" ~ "90")) %>% 
  mutate(Age = as.numeric(Age))



### 3 - Create File for Latest Year ----

### 3.1 - Attach CA2019 Code and Create Single Year File ----

# Attach CA2019 code and CA2011 code by renaming CA2018
# Rename Area1 to CA2019Name and create a SexName column
# Sort the dataframe and then select the relevant variables

CA2019_pop_est_2018 <- mandf %>% 
  rename(CA2018 = `Area code`) %>% 
  mutate(CA2019 = recode(CA2018, 
                         'S12000046' = 'S12000049', 
                         'S12000044' = 'S12000050')) %>% 
  mutate(CA2011 = recode(CA2018, 
                         'S12000047' = 'S12000015', 
                         'S12000048' = 'S12000024')) %>%
  rename(CA2019Name = Area1) %>%
  mutate(SexName = recode(Sex, '1' = 'M', '2' = 'F')) %>%
  arrange(Year, CA2019, Age, Sex) %>% 
  select(Year, CA2019, CA2019Name, CA2018, CA2011, Age, Sex, SexName, Pop)



### 3.2 - Create 5 Year Age Group File ----

# Create a file for 5 year age groups and sex
# Assign a 5 year age group to each age

CA2019_pop_est_5y_2018 <- CA2019_pop_est_2018 %>% 
  mutate(AgeGroup = case_when(Age == 0 ~ 0, 
                              Age >= 1 & Age <= 4 ~ 1, 
                              Age >= 5 & Age <= 9 ~ 2, 
                              Age >= 10 & Age <= 14 ~ 3, 
                              Age >= 15 & Age <= 19 ~ 4, 
                              Age >= 20 & Age <= 24 ~ 5, 
                              Age >= 25 & Age <= 29 ~ 6, 
                              Age >= 30 & Age <= 34 ~ 7, 
                              Age >= 35 & Age <= 39 ~ 8, 
                              Age >= 40 & Age <= 44 ~ 9, 
                              Age >= 45 & Age <= 49 ~ 10, 
                              Age >= 50 & Age <= 54 ~ 11, 
                              Age >= 55 & Age <= 59 ~ 12, 
                              Age >= 60 & Age <= 64 ~ 13,
                              Age >= 65 & Age <= 69 ~ 14, 
                              Age >= 70 & Age <= 74 ~ 15, 
                              Age >= 75 & Age <= 79 ~ 16, 
                              Age >= 80 & Age <= 84 ~ 17, 
                              Age >= 85 & Age <= 89 ~ 18, 
                              Age >= 90 ~ 19)) %>% 
  mutate(AgeGroupName = case_when(AgeGroup == 0 ~ "0", 
                                  AgeGroup == 1 ~ "1-4", 
                                  AgeGroup == 2 ~ "5-9", 
                                  AgeGroup == 3 ~ "10-14", 
                                  AgeGroup == 4 ~ "15-19", 
                                  AgeGroup == 5 ~ "20-24", 
                                  AgeGroup == 6 ~ "25-29", 
                                  AgeGroup == 7 ~ "30-34", 
                                  AgeGroup == 8 ~ "35-39", 
                                  AgeGroup == 9 ~ "40-44", 
                                  AgeGroup == 10 ~ "45-49", 
                                  AgeGroup == 11 ~ "50-54", 
                                  AgeGroup == 12 ~ "55-59", 
                                  AgeGroup == 13 ~ "60-64", 
                                  AgeGroup == 14 ~ "65-69", 
                                  AgeGroup == 15 ~ "70-74", 
                                  AgeGroup == 16 ~ "75-79", 
                                  AgeGroup == 17 ~ "80-84", 
                                  AgeGroup == 18 ~ "85-89", 
                                  AgeGroup == 19 ~ "90+"))

# Aggregate the dataset into 5 year age group and sex
# Group by Year, CA2018, CA2011, AgeGroup and Sex to get population totals for 
# each level within this
# Ungroup the data and select the relevant variables

CA2019_pop_est_5y_2018 %<>%
  group_by(Year, CA2019, CA2019Name, CA2018, CA2011, AgeGroup, AgeGroupName, 
           Sex, SexName) %>% 
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  select(Year, CA2019, CA2019Name, CA2018, CA2011, AgeGroup, AgeGroupName, 
         Sex, SexName, Pop)



### 4 - Update Historical Files For New Release ----

### 4.1 - Update Single Year File ----

# Read in latest historical release files

CA2018_pop_est_1981_2017 <- readRDS(glue("{archive_filepath}/", 
                                         "CA2018_pop_est_1981_2017.rds"))

# Set year on new file as a character for matching

CA2019_pop_est_2018 %<>% mutate(Year = as.character(Year))

# CA2018_pop_est_1981_2017 does not contain information about CA2019 so need to 
# add this before matching
# THIS IS ONLY REQUIRED FOR 2018 ESTIMATES

CA2018_pop_est_1981_2017 %<>%
  mutate(CA2019Name = CA2018Name, 
         CA2019 = recode(CA2018, 
                         'S12000046' = 'S12000049', 
                         'S12000044' = 'S12000050'))

# Add the latest release to single year file

CA2019_pop_est_1981_2018 <- CA2019_pop_est_2018 %>%
  full_join(CA2018_pop_est_1981_2017) %>% 
  select(-CA2018Name) %>% 
  arrange(Year, CA2019, Age, Sex)

# Save as .RDS file

saveRDS(CA2019_pop_est_1981_2018, 
        glue("{output_filepath}/CA2019_pop_est_1981_2018.rds"))



### 4.2 - Update 5 Year Age Group File ----

# Read in latest historical release files

CA2018_pop_est_5y_1981_2017 <- readRDS(
  glue("{archive_filepath}/CA2018_pop_est_5year_agegroups_1981_2017.rds"))

# Set year as character for matching

CA2019_pop_est_5y_2018 %<>% mutate(Year = as.character(Year))

# CA2018_pop_est_5year_agegroups_1981_2017 does not contain information about 
# CA2019 so need to add this before matching
# THIS IS ONLY REQUIRED FOR 2018 ESTIMATES

CA2018_pop_est_5y_1981_2017 %<>% 
  mutate(CA2019Name = CA2018Name, 
         CA2019 = recode(CA2018, 
                         'S12000046' = 'S12000049', 
                         'S12000044' = 'S12000050'), 
         AgeGroupName = case_when(AgeGroup == 0 ~ "0", 
                                  AgeGroup == 1 ~ "1-4", 
                                  AgeGroup == 2 ~ "5-9", 
                                  AgeGroup == 3 ~ "10-14", 
                                  AgeGroup == 4 ~ "15-19", 
                                  AgeGroup == 5 ~ "20-24", 
                                  AgeGroup == 6 ~ "25-29", 
                                  AgeGroup == 7 ~ "30-34", 
                                  AgeGroup == 8 ~ "35-39", 
                                  AgeGroup == 9 ~ "40-44", 
                                  AgeGroup == 10 ~ "45-49", 
                                  AgeGroup == 11 ~ "50-54", 
                                  AgeGroup == 12 ~ "55-59", 
                                  AgeGroup == 13 ~ "60-64", 
                                  AgeGroup == 14 ~ "65-69", 
                                  AgeGroup == 15 ~ "70-74", 
                                  AgeGroup == 16 ~ "75-79", 
                                  AgeGroup == 17 ~ "80-84", 
                                  AgeGroup == 18 ~ "85-89", 
                                  AgeGroup == 19 ~ "90+"))

# Add the latest release to 5 year age group file

CA2019_pop_est_5year_agegroups_1981_2018 <- CA2019_pop_est_5y_2018 %>%
  full_join(CA2018_pop_est_5y_1981_2017) %>% 
  select(-CA2018Name) %>% 
  arrange(Year, CA2019, AgeGroup, Sex)

# Save as .RDS file

saveRDS(CA2019_pop_est_5year_agegroups_1981_2018, 
        glue("{output_filepath}/CA2019_pop_est_5year_agegroups_1981_2018.rds"))



### 5 - Check files ----

### 5.1 - Check function ----

checks <- function(input, age_column){
  
  # Check that all years of the population estimates are there (last update 1982-2017)
  # Check that there are no missing values
  # Check all years have the same % of records
  
  input %>% group_by(Year) %>% count() %>% print(n = Inf)
  
  # Check that all 32 Council Areas are there
  # Check there are no missing values
  # Check all CAs have the same % of records
  
  input %>% group_by(CA2019) %>% count() %>% print(n = Inf)
  input %>% group_by(CA2018) %>% count() %>% print(n = Inf)
  input %>% group_by(CA2011) %>% count() %>% print(n = Inf)
  
  # Check that all 91 ages 0 to 90+ are there
  # Check there are no missing values
  # Check all ages have the same % of records
  
  input %>% group_by({{age_column}}) %>% count() %>% print(n = Inf)
  
  # Check that both males and females are there
  # Check there are no missing values
  # Check both sexes have the same % of records (50/50)
  
  input %>% group_by(Sex) %>% count() %>% print(n = Inf)
  
  # Check that the population values are as expected
  # i.e. no negative values or extremely high values etc
  
  input %>%
    group_by(Pop) %>%
    count() %>%
    arrange(n) %>%
    filter(n <= 0 | n >= 300)
  
  # Select only the new year(s) of data
  
  new_years <- input %>%
    filter(Year > 2011)
  # 
  # Check Council Area totals against NRS source data
  
  new_years %<>%
    group_by(Year, CA2018) %>%
    summarise(Pop = sum(Pop)) %>%
    ungroup()
  
  View(new_years)
  
  # Check Scotland totals against NRS source data
  
  new_years %>%
    group_by(Year) %>%
    summarise(Pop = sum(Pop)) %>%
    ungroup()
  
}



### 5.2 - Check single age file ----

checks(input = CA2019_pop_est_1981_2018, age_column = "Age")


### 5.3 - Check 5 year age group file ----

checks(input = CA2019_pop_est_5year_agegroups_1981_2018, 
       age_column = "AgeGroup")
