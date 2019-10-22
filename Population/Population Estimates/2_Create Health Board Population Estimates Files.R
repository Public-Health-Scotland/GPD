### 1 - Information ----

# Codename - Create Health Board Population Estimates Files_all years
# Data release - Mid-year Health Board Population Estimates
# Original Author - Tina Fu
# Original Date - 05/03/2018
# Type - Preparation
# Written/run on - R Studio Desktop 
# Version - 3.3.2
#
# install.packages("readxl")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readr)
#
# Description - This document is based on the SPSS syntax for Health Board Population Estimates found within
#               GPD folders. It is designed to allow for the same data to be inputted and to provide the 
#               same output files. Each section of the SPSS syntax is contained in this file within different
#               subsections.
#
# Approximate run time - 14 seconds

# Read in packages from library
library(readxl)
library(tidyr)
library(dplyr)
library(readr)

# Set working directory to R Code folder
setwd("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Population Estimates/Lookup Files/R Files/")


### 2 - Import Male and Female Data ----

### 2.1 - Import Data ----
# Male
# Remove unnessary columns
HB_pop_m <- read_excel("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Population Estimates/Source Data/Population_Estimates_2018.xlsx", sheet = "Table 2", range = "A93:CQ107")
HB_pop_tidy_m <- HB_pop_m %>% 
  select(-c('...3', '...4'))

# Rename variables
names(HB_pop_tidy_m)<- c("HB2018", "HB2019Name","0":"90")

# Transpose the dataset
male <- HB_pop_tidy_m %>% 
  gather(Age, Pop, `0`:`90`) %>% 
  mutate(Sex = 1)

# Female
# Remove unnessary columns
HB_pop_f <- read_excel("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Population Estimates/Source Data/Population_Estimates_2018.xlsx", sheet = "Table 2", range = "A146:CQ160")
HB_pop_tidy_f <- HB_pop_f %>% 
  select(-c('...3', '...4'))

# Rename variables
names(HB_pop_tidy_f)<- c("HB2018", "HB2019Name","0":"90")

# Transpose the dataset
female <- HB_pop_tidy_f %>% 
  gather(Age, Pop, `0`:`90`) %>% 
  mutate(Sex = 2)

### 2.2 - Add Male and Female Files Together ----

# Add male and female together
mandf <- rbind(male, female) %>% 
  mutate(Year = 2018)

#Change age to numeric rather than character
mandf <- mandf %>% 
  mutate(Age = as.numeric(Age))

### 3 - Attach HB2018 Code and Create 2017 Population Estimate Files ----

### 3.1 - Attach HB2018 Code and Create 2017 Single Year File ----
# Attach HB2018 code and HB2014 code
# Recode the Area column into the HB2018 GSS codes
# Use the HB2018 column to get HB2014
# Sort the dataframe and then select the relevant variables
HB2019_pop_est_2018 <- mandf %>% 
  mutate(HB2019 = recode(HB2018, 'S08000021' = 'S08000031', 'S08000023' = 'S08000032'),
         HB2014 = recode(HB2018, 'S08000029' = 'S08000018', 'S08000030' = 'S08000027')) %>%
  mutate(SexName = recode(Sex, '1' = 'M', '2' = 'F')) %>%
  arrange(Year, HB2019, Age, Sex) %>% 
  select(Year, HB2019, HB2019Name, HB2018, HB2014, Age, Sex, SexName, Pop)

# HB2018_pop_est_2017 <- mandf %>% 
#   mutate(HB2018 = recode(HB2018_name, 
#                          'Ayrshire and Arran' = 'S08000015', 
#                          'Borders' = 'S08000016', 
#                          'Dumfries and Galloway' = 'S08000017', 
#                          'Fife' = 'S08000029', 
#                          'Forth Valley' = 'S08000019', 
#                          'Grampian' = 'S08000020', 
#                          'Greater Glasgow and Clyde' = 'S08000021', 
#                          'Highland' = 'S08000022', 
#                          'Lanarkshire' = 'S08000023', 
#                          'Lothian' = 'S08000024', 
#                          'Orkney' = 'S08000025', 
#                          'Shetland' = 'S08000026', 
#                          'Tayside' = 'S08000030', 
#                          'Western Isles' = 'S08000028')) %>%
#   mutate(HB2014 = recode(HB2018, 'S08000029' = 'S08000018', 'S08000030' = 'S08000027')) %>%
#   rename(HB2018Name = HB2018_name) %>%
#   mutate(SexName = recode(Sex, '1' = 'M', '2' = 'F')) %>% 
#   arrange(Year, HB2018, Age, Sex) %>% 
#   select(Year, HB2018, HB2018Name, HB2014, Age, Sex, SexName, Pop)


### 3.2 - Create 5 Year Age Group File ----
# Create a file for 5 year age group and sex
HB2019_pop_est_5year_agegroups_2018 <- HB2019_pop_est_2018

# Assign a 5 year age group to each age
HB2019_pop_est_5year_agegroups_2018 <- HB2019_pop_est_5year_agegroups_2018 %>%
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
# Group by Year, HB2018, HB2014, AgeGroup and Sex to get population totals for each level within this
# Ungroup the data and select the relevant variables
HB2019_pop_est_5year_agegroups_2018 <- HB2019_pop_est_5year_agegroups_2018 %>% 
  group_by(Year, HB2019, HB2019Name, HB2018, HB2014, AgeGroup, AgeGroupName, Sex, SexName) %>% 
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  arrange(Year, HB2018, AgeGroup, Sex) %>% 
  select(Year, HB2019, HB2019Name, HB2018, HB2014, AgeGroup, AgeGroupName, Sex, SexName, Pop)


### 4 - Update Historical Files For New Release ----

### 4.1 - Update Single Year File ----
# Read in latest historical release files (1981-2016)
HB2018_pop_est_1981_2017 <- readRDS("Archive/HB2018_pop_est_1981_2017.rds")

# Set year on new file as a character for matching
HB2019_pop_est_2018$Year <- as.character(HB2019_pop_est_2018$Year)

# HB2018_pop_est_1981_2017 does not contain information about CA2019 so need to add this before matching
# THIS IS ONLY REQUIRED FOR 2018 ESTIMATES
HB2018_pop_est_1981_2017 <- HB2018_pop_est_1981_2017 %>% 
  mutate(HB2019Name = HB2018Name, 
         HB2019 = recode(HB2018, 'S08000021' = 'S08000031', 'S08000023' = 'S08000032'))

# Add the latest release to single year file
HB2019_pop_est_1981_2018 <- HB2019_pop_est_2018 %>%
  full_join(HB2018_pop_est_1981_2017) %>% 
  select(-HB2018Name) %>% 
  arrange(Year, HB2019, Age, Sex)

# Save as .RDS file
saveRDS(HB2019_pop_est_1981_2018, paste0("HB2019_pop_est_1981_2018.rds"))

### 4.2 - Update 5 Year Age Group File ----
# Read in latest historical release files (1981-2016)
HB2018_pop_est_5year_agegroups_1981_2017 <- readRDS("Archive/HB2018_pop_est_5year_agegroups_1981_2017.rds")

# Set year as character for matching
HB2019_pop_est_5year_agegroups_2018$Year <- as.character(HB2019_pop_est_5year_agegroups_2018$Year)

# HB2018_pop_est_1981_2017 does not contain information about CA2019 so need to add this before matching
# THIS IS ONLY REQUIRED FOR 2018 ESTIMATES
HB2018_pop_est_5year_agegroups_1981_2017 <- HB2018_pop_est_5year_agegroups_1981_2017 %>% 
  mutate(HB2019Name = HB2018Name, 
         HB2019 = recode(HB2018, 'S08000021' = 'S08000031', 'S08000023' = 'S08000032'), 
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
HB2019_pop_est_5year_agegroups_1981_2018 <- HB2019_pop_est_5year_agegroups_2018 %>%
  full_join(HB2018_pop_est_5year_agegroups_1981_2017) %>% 
  select(-HB2018Name) %>% 
  arrange(Year, HB2019, AgeGroup, Sex)

# Save as .RDS file
saveRDS(HB2019_pop_est_5year_agegroups_1981_2018, paste0("HB2019_pop_est_5year_agegroups_1981_2018.rds"))



### 5 - Check HB2019 files ----

### 5.1 - Check single age file ----

# Check that all years of the population estimates are there (last update 1981-2017)
# Check there are no missing values
# Check all years have the same number of records
as.data.frame(table(HB2019_pop_est_1981_2018$Year))

# Check that all 14 Health Boards are there
# Check there are no missing values
# Check all years have the same number of records
as.data.frame(table(HB2019_pop_est_1981_2018$HB2019))
as.data.frame(table(HB2019_pop_est_1981_2018$HB2018))
as.data.frame(table(HB2019_pop_est_1981_2018$HB2014))

# Check that all 91 ages 0 to 90+ are there
# Check there are no missing values
# Check all ages have the same number of records
as.data.frame(table(HB2019_pop_est_1981_2018$Age))

# Check that both males and females are there
# Check there are no missing values
# Check both sexes have the same % of records
as.data.frame(table(HB2019_pop_est_1981_2018$Sex))

# Check that population values are as expected
# i.e. no negative values or extremely high values etc
pop_test <- as.data.frame(table(HB2019_pop_est_1981_2018$Pop))
View(pop_test)

# Select only the new year(s) of data
HB2019_new <- HB2019_pop_est_1981_2018 %>%
  filter(Year > 2011)

# Check Health Board totals against NRS source data
HB2019_new <- HB2019_new %>% 
  group_by(Year, HB2019) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup()
View(HB2019_new)

# Check Scotland totals against NRS source data
HB2019_new %>% 
  group_by(Year) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup()

### 5.2 - Check 5 year age group file ----

# Check that all years of the population estimates are there (last update 1981-2017)
# Check there are no missing values
# Check all years have the same % of records
as.data.frame(table(HB2019_pop_est_5year_agegroups_1981_2018$Year))

# Check that all 14 Health Boards are there
# Check there are no missing values
# Check all HBs ahve the same % of records
as.data.frame(table(HB2019_pop_est_5year_agegroups_1981_2018$HB2019))
as.data.frame(table(HB2019_pop_est_5year_agegroups_1981_2018$HB2018))
as.data.frame(table(HB2019_pop_est_5year_agegroups_1981_2018$HB2014))

# Check that all 20 age groups are there
# Check there are no missing values
# Check all ages have the same % of records
as.data.frame(table(HB2019_pop_est_5year_agegroups_1981_2018$AgeGroup))

# Check that both males and females are there
# Check there are no missing values
# Check both sexes have the same % of records (50/50)
as.data.frame(table(HB2019_pop_est_5year_agegroups_1981_2018$Sex))

# Check the population values are as expected
# i.e. no negative values or extremely high values etc
pop_test <- as.data.frame(table(HB2019_pop_est_5year_agegroups_1981_2018$Pop))
View(pop_test)

# Select only the new year(s) of data
HB2019_5y_new <- HB2019_pop_est_5year_agegroups_1981_2018 %>% 
  filter(Year > 2011)

# Check Health Board totals against NRS source data
HB2019_5y_new <- HB2019_5y_new %>% 
  group_by(Year, HB2019) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup()
View(HB2019_5y_new)

# Check Scotland totals against NRS source data
HB2019_5y_new %>% 
  group_by(Year) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup()
