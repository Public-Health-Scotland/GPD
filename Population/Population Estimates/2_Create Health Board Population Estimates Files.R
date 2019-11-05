### 1 - Information ----

# Codename - Create Health Board Population Estimates Files
# Data release - Mid-year Health Board Population Estimates
# Original Author - Tina Fu
# Original Date - 05/03/2018
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
# Description - Code for creating Health Board population estimates files 
#               based on mid-year estimates released by NRS
#
# Approximate run time - 14 seconds

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
# Rename variables
# Transpose the dataset

male <- read_excel(glue("{data_filepath}/Population_Estimates_2018.xlsx"), 
                   sheet = "Table 2", "A93:CQ107") %>% 
  select(-c('...3', `...4`)) %>% 
  set_names(c("HB2018", "HB2019Name","0":"90")) %>% 
  gather(Age, Pop, `0`:`90`) %>% 
  mutate(Sex = 1)

# Female
# Remove unnessary columns
# Transpose the dataset

female <- read_excel(glue("{data_filepath}/Population_Estimates_2018.xlsx"), 
                     sheet = "Table 2", range = "A146:CQ160") %>% 
  select(-c('...3', `...4`)) %>% 
  set_names(c("HB2018", "HB2019Name","0":"90")) %>% 
  gather(Age, Pop, `0`:`90`) %>% 
  mutate(Sex = 2)



### 2.2 - Add Male and Female Files Together ----

# Add male and female together
# Change age to numeric rather than character

mandf <- rbind(male, female) %>% 
  mutate(Year = 2018) %>% 
  mutate(Age = as.numeric(Age))



### 3 - Create File for Latest Year ----

### 3.1 - Attach HB2019 Code and Create Single Year File ----

# Attach HB2019 code and HB2014 code by renaming HB2018
# Create a SexName column
# Sort the dataframe and then select the relevant variables

HB2019_pop_est_2018 <- mandf %>% 
  mutate(HB2019 = recode(HB2018, 'S08000021' = 'S08000031', 'S08000023' = 'S08000032'),
         HB2014 = recode(HB2018, 'S08000029' = 'S08000018', 'S08000030' = 'S08000027')) %>%
  mutate(SexName = recode(Sex, '1' = 'M', '2' = 'F')) %>%
  arrange(Year, HB2019, Age, Sex) %>% 
  select(Year, HB2019, HB2019Name, HB2018, HB2014, Age, Sex, SexName, Pop)



### 3.2 - Create 5 Year Age Group File ----

# Create a file for 5 year age group and sex

HB2019_pop_est_5y_2018 <- HB2019_pop_est_2018 %>%
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
                              Age >= 90 ~ 19), 
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


# Aggregate the dataset into 5 year age group and sex
# Group by Year, HB2018, HB2014, AgeGroup and Sex to get population totals for 
# each level within this
# Ungroup the data and select the relevant variables

HB2019_pop_est_5y_2018 %<>%
  group_by(Year, HB2019, HB2019Name, HB2018, HB2014, AgeGroup, AgeGroupName, 
           Sex, SexName) %>% 
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  arrange(Year, HB2018, AgeGroup, Sex) %>% 
  select(Year, HB2019, HB2019Name, HB2018, HB2014, AgeGroup, AgeGroupName, Sex, 
         SexName, Pop)



### 4 - Update Historical Files For New Release ----

### 4.1 - Update Single Year File ----

# Read in latest historical release files

HB2018_pop_est_1981_2017 <- readRDS(glue("{archive_filepath}/", 
                                         "HB2018_pop_est_1981_2017.rds"))

# Set year on new file as a character for matching

HB2019_pop_est_2018 %<>% mutate(Year = as.character(Year))

# HB2018_pop_est_1981_2017 does not contain information about CA2019 so need to 
# add this before matching
# THIS IS ONLY REQUIRED FOR 2018 ESTIMATES

HB2018_pop_est_1981_2017 %<>%
  mutate(HB2019Name = HB2018Name, 
         HB2019 = recode(HB2018, 
                         'S08000021' = 'S08000031', 
                         'S08000023' = 'S08000032'))

# Add the latest release to single year file

HB2019_pop_est_1981_2018 <- HB2019_pop_est_2018 %>%
  full_join(HB2018_pop_est_1981_2017) %>% 
  select(-HB2018Name) %>% 
  arrange(Year, HB2019, Age, Sex)

# Save as .RDS file

saveRDS(HB2019_pop_est_1981_2018, 
        glue("{output_filepath}/HB2019_pop_est_1981_2018.rds"))



### 4.2 - Update 5 Year Age Group File ----

# Read in latest historical release files (1981-2016)

# Read in latest historical release files

HB2018_pop_est_5y_1981_2017 <- readRDS(
  glue("{archive_filepath}/HB2018_pop_est_5year_agegroups_1981_2017.rds"))

# Set year as character for matching

HB2019_pop_est_5y_2018 %<>% mutate(Year = as.character(Year))


# HB2018_pop_est_1981_2017 does not contain information about CA2019 so need to 
# add this before matching
# THIS IS ONLY REQUIRED FOR 2018 ESTIMATES

HB2018_pop_est_5y_1981_2017 %<>%
  mutate(HB2019Name = HB2018Name, 
         HB2019 = recode(HB2018, 
                         'S08000021' = 'S08000031', 
                         'S08000023' = 'S08000032'), 
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

HB2019_pop_est_5year_agegroups_1981_2018 <- HB2019_pop_est_5y_2018 %>%
  full_join(HB2018_pop_est_5y_1981_2017) %>% 
  select(-HB2018Name) %>% 
  arrange(Year, HB2019, AgeGroup, Sex)

# Save as .RDS file

saveRDS(HB2019_pop_est_5year_agegroups_1981_2018, 
        glue("{output_filepath}/HB2019_pop_est_5year_agegroups_1981_2018.rds"))




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
  
  input %>% group_by(HB2019) %>% count() %>% print(n = Inf)
  input %>% group_by(HB2018) %>% count() %>% print(n = Inf)
  input %>% group_by(HB2014) %>% count() %>% print(n = Inf)
  
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
    filter(n <= 0 | n >= 250)
  
  # Select only the new year(s) of data
  
  new_years <- input %>%
    filter(Year > 2011)
  # 
  # Check Council Area totals against NRS source data
  
  new_years %<>%
    group_by(Year, HB2018) %>%
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

checks(input = HB2019_pop_est_1981_2018, age_column = "Age")


### 5.3 - Check 5 year age group file ----

checks(input = HB2019_pop_est_5year_agegroups_1981_2018, 
       age_column = "AgeGroup")
