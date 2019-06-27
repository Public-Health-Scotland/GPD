### 1 - Information ----

# Codename - Creating Corrected Population Estimates
# Data release - NRS Population Estimates
# Original Author - Calum Purdie
# Original Date - 20/06/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readr")
#
# Description - NRS released revised populatione estimates for 2002 - 2010 due to an error identified for ages
#               81 - 89 and 90+. This code is to download these corrected estimates and update the files in 
#               GPD folders and cl-out
#
# Approximate run time - 1 minute

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(haven)
library(sjlabelled)

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD", "2_Population", 
                           "Population Estimates")
data_filepath <- file.path(base_filepath, "Source Data", "Corrected Estimates 2018")
SPSS_filepath <- file.path(base_filepath, "Lookup Files")
output_filepath <- file.path(SPSS_filepath, "R Files")


### 2 - Council Area ----

### 2.1 - Single Year Age Groups ----

# Use a loop to read in all 38 sheets
# I tried to create a function for this so that it would work for Council Area and Health Board but can't get it working

for (i in as.character(1981:2018)) {
  
  male <- read_excel(file.path(data_filepath, "mid-year-pop-est-18-time-series-1.xlsx"), sheet = i, range = "B40:CP74") %>% 
    select(-`All Ages`) %>% 
    filter(!is.na(Males), Males != "Scotland") %>%
    rename(CA2019Name = Males) %>% 
    mutate(Sex = 1, Year = i)
  
  female <- read_excel(file.path(data_filepath, "mid-year-pop-est-18-time-series-1.xlsx"), sheet = i, range = "B77:CP111") %>% 
    select(-`All Ages`) %>% 
    filter(!is.na(Females), Females != "Scotland") %>% 
    rename(CA2019Name = Females) %>% 
    mutate(Sex = 2, Year = i)
  
  if (i == 1981) {
    combined_data <- bind_rows(male, female)
  } else {
    data_next_year <- bind_rows(male, female)
    combined_data <- bind_rows(combined_data, data_next_year) %>% 
      mutate(SexName = recode(Sex, '1' = 'M', '2' = 'F'))
  }
  
}

# Manipulate date into the correct format

CA2019_pop_est_1981_2018 <- combined_data %>% 
  gather(Age, Pop, "0":"90+") %>% 
  mutate(Age = ifelse(Age == "90+", "90", Age), 
         Age = as.numeric(Age))

# Add in GSS codes
# Take these from the current populatione estimates

CA2019_current_est <- readRDS(file.path(output_filepath, "Archive", "CA2019_pop_est_1981_2018.rds")) %>% 
  select(CA2019Name, CA2019, CA2018, CA2011) %>% 
  distinct()

CA2019_pop_est_1981_2018 <- CA2019_pop_est_1981_2018 %>% 
  left_join(CA2019_current_est) %>% 
  select(Year, CA2019, CA2019Name, CA2018, CA2011, Age, Sex, SexName, Pop) %>% 
  arrange(Year, CA2019, Age, Sex)

saveRDS(CA2019_pop_est_1981_2018, file.path(output_filepath, "CA2019_pop_est_1981_2018.rds"))

### 2.2 - 5 Year Age Groups ----

# Create a file for 5 year age groups and sex
# Assign a 5 year age group to each age

CA2019_pop_est_5year_agegroups_1981_2018 <- CA2019_pop_est_1981_2018 %>%
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
# Group by Year, CA2018, CA2011, AgeGroup and Sex to get population totals for each level within this
# Ungroup the data and select the relevant variables
CA2019_pop_est_5year_agegroups_1981_2018 <- CA2019_pop_est_5year_agegroups_1981_2018 %>% 
  group_by(Year, CA2019, CA2019Name, CA2018, CA2011, AgeGroup, AgeGroupName, Sex, SexName) %>% 
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  select(Year, CA2019, CA2019Name, CA2018, CA2011, AgeGroup, AgeGroupName, Sex, SexName, Pop)

saveRDS(CA2019_pop_est_5year_agegroups_1981_2018, file.path(output_filepath, "CA2019_pop_est_5year_agegroups_1981_2018.rds"))



### 3 - Health Board ----

### 3.1 - Single Year Age Groups ----

# Use a loop to read in all 38 sheets
# I tried to create a function for this so that it would work for Council Area and Health Board but can't get it working

for (i in as.character(1981:2018)) {
  
  male <- read_excel(file.path(data_filepath, "mid-year-pop-est-18-time-series-3.xlsx"), sheet = i, range = "B22:CP38") %>% 
    select(-`All Ages`) %>% 
    filter(!is.na(Males), Males != "Scotland") %>%
    rename(HB2019Name = Males) %>% 
    mutate(Sex = 1, Year = i)
  
  female <- read_excel(file.path(data_filepath, "mid-year-pop-est-18-time-series-3.xlsx"), sheet = i, range = "B41:CP57") %>% 
    select(-`All Ages`) %>% 
    filter(!is.na(Females), Females != "Scotland") %>% 
    rename(HB2019Name = Females) %>% 
    mutate(Sex = 2, Year = i)
  
  if (i == 1981) {
    combined_data <- bind_rows(male, female)
  } else {
    data_next_year <- bind_rows(male, female)
    combined_data <- bind_rows(combined_data, data_next_year) %>% 
      mutate(SexName = recode(Sex, '1' = 'M', '2' = 'F'))
  }
  
}

# Manipulate date into the correct format

HB2019_pop_est_1981_2018 <- combined_data %>% 
  gather(Age, Pop, "0":"90+") %>% 
  mutate(Age = ifelse(Age == "90+", "90", Age), 
         Age = as.numeric(Age))

# Add in GSS codes
# Take these from the current populatione estimates

HB2019_current_est <- readRDS(file.path(output_filepath, "Archive", "HB2019_pop_est_1981_2018.rds")) %>% 
  select(HB2019Name, HB2019, HB2018, HB2014) %>% 
  distinct()

HB2019_pop_est_1981_2018 <- HB2019_pop_est_1981_2018 %>% 
  left_join(HB2019_current_est) %>% 
  select(Year, HB2019, HB2019Name, HB2018, HB2014, Age, Sex, SexName, Pop) %>% 
  arrange(Year, HB2019, Age, Sex)

saveRDS(HB2019_pop_est_1981_2018, file.path(output_filepath, "HB2019_pop_est_1981_2018.rds"))



### 3.2 - 5 Year Age Groups ----

# Create a file for 5 year age groups and sex
# Assign a 5 year age group to each age

HB2019_pop_est_5year_agegroups_1981_2018 <- HB2019_pop_est_1981_2018 %>%
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
# Group by Year, HB2018, HB2011, AgeGroup and Sex to get population totals for each level within this
# Ungroup the data and select the relevant variables
HB2019_pop_est_5year_agegroups_1981_2018 <- HB2019_pop_est_5year_agegroups_1981_2018 %>% 
  group_by(Year, HB2019, HB2019Name, HB2018, HB2014, AgeGroup, AgeGroupName, Sex, SexName) %>% 
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  select(Year, HB2019, HB2019Name, HB2018, HB2014, AgeGroup, AgeGroupName, Sex, SexName, Pop)

saveRDS(HB2019_pop_est_5year_agegroups_1981_2018, file.path(output_filepath, "HB2019_pop_est_5year_agegroups_1981_2018.rds"))



### 4 - HSCP ----

### 4.1 - Single Year Age Groups ----

# Create an HSCP2019Name column
# Read in HSCP Locality lookup and take unique rows to get one row for each HSCP

HSCP_CA <- read_csv("//Isdsf00d03/cl-out/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20180903.csv") %>%
  select(HSCP2019Name, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, HSCP2016) %>%
  distinct() %>%
  arrange(CA2019)

# Match the HSCP columns onto the CA2019 lookup
# Create column for SexName
# Select the relevant columns
# As there are 31 HSCPs and 32 CAs (Stirling and Clackmannanshire join for HSCP) this will produce population
# separately for both Stirling and Clackmannanshire even though they have the same HSCP code
# Need to recalculate the sums, so group by all variables bar Pop and calculate new Pop totals
# Arrange by Year, HSCP2019, Age and Sex to get the required format

HSCP2019_pop_est_1981_2018 <- CA2019_pop_est_1981_2018 %>%
  full_join(HSCP_CA) %>%
  select(Year, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, Age, Sex, SexName, Pop) %>%
  group_by(Year, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, Age, Sex, SexName) %>%
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  arrange(Year, HSCP2019, Age, Sex)

saveRDS(HSCP2019_pop_est_1981_2018, file.path(output_filepath, "HSCP2019_pop_est_1981_2018.rds"))



### 4.2 - Five Year Age Groups ----

# Match the HSCP columns onto the CA2019 lookup
# Create column for SexName
# Select the relevant columns
# As there are 31 HSCPs and 32 CAs (Stirling and Clackmannanshire join for HSCP) this will produce population
# separately for both Stirling and Clackmannanshire even though they have the same HSCP code
# Need to recalculate the sums, so group by all variables bar Pop and calculate new Pop totals
# Arrange by Year, HSCP2019, Age and Sex to get the required format

HSCP2019_pop_est_5year_agegroups_1981_2018 <- CA2019_pop_est_5year_agegroups_1981_2018 %>%
  full_join(HSCP_CA) %>%
  select(Year, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, AgeGroup, AgeGroupName, Sex, SexName, Pop) %>%
  group_by(Year, HSCP2019, HSCP2019Name, HSCP2018,  HSCP2016, AgeGroup, AgeGroupName, Sex, SexName) %>%
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  arrange(Year, HSCP2019, AgeGroup, Sex)

saveRDS(HSCP2019_pop_est_5year_agegroups_1981_2018, file.path(output_filepath, "HSCP2019_pop_est_5year_agegroups_1981_2018.rds"))



### 5 - Comparing Adjustments with NRS ----

# NRS documentation shows the corrected population adjustment rounded to the nearest 10
# https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-year-corrections/correction-to-age-distribution-mid-year%20pop-estimates-2002-2010.pdf

# Compare this files with the adjusted populations created with this code

### 5.1 - Check Council Area Asjustments ----

# Read in old CA2019_pop_est_1981_2018 file
old_CA2019_pop_est_1981_2018 <- readRDS(file.path(output_filepath, "Archive", "CA2019_pop_est_1981_2018.rds"))

old_CA2019_CA <- old_CA2019_pop_est_1981_2018 %>% 
  rename(Pop_2 = Pop) %>% 
  filter(Age >= 90) %>% 
  filter(Year >= 2002 & Year <= 2010) %>% 
  group_by(CA2019Name, Year) %>% 
  summarise(Pop_2 = sum(Pop_2)) %>% 
  arrange(CA2019Name) %>% 
  ungroup()

# Manipulate the new CA2019_pop_est_1981_2018 file

CA_compare <- CA2019_pop_est_1981_2018 %>% 
  filter(Age >= 90) %>% 
  filter(Year >= 2002 & Year <= 2010) %>% 
  group_by(CA2019Name, Year) %>% 
  summarise(Pop = sum(Pop)) %>% 
  arrange(CA2019Name) %>% 
  ungroup()

# Compare these two files and look at the difference in population

compare <- CA_compare %>% 
  left_join(old_CA2019_CA) %>% 
  mutate(diff = Pop - Pop_2) %>% 
  select(CA2019Name, Year, diff) %>% 
  spread(Year, diff)

### 5.2 - Check Scotland level adjustments ----

# Select the Scotland level populations for ages 80+ for 2002-2010 from corrected estimates

total_pop <- CA2019_pop_est_1981_2018 %>% 
  filter(Age >= 80) %>% 
  filter(Year >= 2002 & Year <= 2010) %>% 
  group_by(Age, Year) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup()

# Select the Scotland level populations for ages 80+ for 2002-2010 from old estimates

old_total_pop <- old_CA2019_pop_est_1981_2018 %>% 
  filter(Age >= 80) %>% 
  filter(Year >= 2002 & Year <= 2010) %>% 
  group_by(Age, Year) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup() %>% 
  rename(Pop_2 = Pop)

# Compare these files to determine the difference in population by age and year
# Compare this with NRS documentation

compare_total_pop <- total_pop %>% 
  left_join(old_total_pop) %>% 
  mutate(diff = Pop - Pop_2) %>% 
  select(Age, Year, diff) %>% 
  spread(Age, diff)



### 6 - Compare R and SPSS outputs ----


columns <- c("Year", "geo1", "geo2", "geo3", "Age_check", "Sex", "Pop")

compare_SPSS_R <- function(SPSS_data, R_data, geo1, geo2, geo3, Age_check){
  
  # Read in SPSS file
  # Remove variable labels, formats and widths from SPSS
  # Haven reads in SPSS strings as factors
  # Convert all factors to characters in the SPSS file
  
  SPSS <- read_sav(file.path(SPSS_filepath, SPSS_data), user_na=F) %>%
    zap_formats() %>%
    zap_widths() %>%
    remove_all_labels() %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate(Year = as.character(Year))
  
  # Read in R file and sort by pc7
  
  R <- readRDS(file.path(output_filepath, R_data)) %>% 
    select_(., .dots = columns)
  
  
  # Compare datasets
  all_equal(SPSS, R)
  
}

### 6.1 - Council Area ----

# CA2019_pop_est_1981_2018
compare_SPSS_R("CA2019_pop_est_1981_2018.sav", "CA2019_pop_est_1981_2018.rds", "CA2019", "CA2018", "CA2011", "Age")

# CA2019_pop_est_5year_agegroups_1981_2018
compare_SPSS_R("CA2019_pop_est_5year_agegroups_1981_2018.sav", "CA2019_pop_est_5year_agegroups_1981_2018.rds", "CA2019", "CA2018", "CA2011", "AgeGroup")



### 6.2 - Health Board ----

# HB2019_pop_est_1981_2018
compare_SPSS_R("HB2019_pop_est_1981_2018.sav", "HB2019_pop_est_1981_2018.rds", "HB2019", "HB2018", "HB2014", "Age")

# HB2019_pop_est_1981_2018
compare_SPSS_R("HB2019_pop_est_5year_agegroups_1981_2018.sav", "HB2019_pop_est_5year_agegroups_1981_2018.rds", "HB2019", "HB2018", "HB2014", "AgeGroup")



### 6.3 - HSCP ----

# HSCP2019_pop_est_1981_2018
compare_SPSS_R("HSCP2019_pop_est_1981_2018.sav", "HSCP2019_pop_est_1981_2018.rds", "HSCP2019", "HSCP2018", "HSCP2016", "Age")

# HSCP2019_pop_est_1981_2018
compare_SPSS_R("HSCP2019_pop_est_5year_agegroups_1981_2018.sav", "HSCP2019_pop_est_5year_agegroups_1981_2018.rds", "HSCP2019", "HSCP2018", "HSCP2016", "AgeGroup")

