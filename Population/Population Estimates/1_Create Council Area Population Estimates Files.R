##########################################################
# Create Council Area Population Estimates Files
# Tina Fu
# Original date - 01/03/2018
# Data release - Mid-year Council Area Population Estimates
# Latest update author - Calum Purdie
# Latest update date - 30/04/2020
# Latest update description - 2019 estimates
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for creating council area population estimates files based on mid-year 
# estimates released by NRS
# Approximate run time - 3 minutes
##########################################################

### 1 - Housekeeping ----

library(magrittr)
library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(tidylog)
library(glue)
library(stringr)
library(janitor)

# Set filepaths

filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                 "2_Population/Population Estimates")
data_filepath <- glue("{filepath}/Source Data")
output_filepath <- glue("{filepath}/Lookup Files/R Files")

# Set estimates year

start <- "1981"
prev <- "2018"
new <- "2019"



### 2 - Import Male and Female Data ----

### 2.1 - Import Data ----

# Male
# Remove unnessary columns
# Transpose the dataset

male <- read_excel(glue("{data_filepath}/Population_Estimates_{new}.xlsx"), 
                       sheet = "Table 2", range = "A57:CQ91") %>% 
  select(-c('...4', `All Ages`)) %>%
  filter(`Area code1` != "Council areas", `Area name` != "Scotland") %>% 
  gather(Age, Pop, `0`:`90+`) %>% 
  mutate(Sex = 1)

# Female
# Remove unnessary columns
# Transpose the dataset

female <- read_excel(glue("{data_filepath}/Population_Estimates_{new}.xlsx"),
                       sheet = "Table 2", range = "A110:CQ144") %>% 
  select(-'...4', -`All Ages`) %>%
  filter(`Area code1` != "Council areas", `Area name` != "Scotland") %>% 
  gather(Age, Pop, `0`:`90+`) %>% 
  mutate(Sex = 2)



### 2.2 - Add Male and Female Files Together ----

# Add male and female together
# Change age of "90+" to "90" and make it as numeric rather than character

mandf <- bind_rows(male, female) %>% 
  mutate(year = as.integer(new), 
         Age = case_when(Age == "90+" ~ "90", 
                         TRUE ~ Age), 
         Age = as.integer(Age)) %>% 
  rename(ca2019 = `Area code1`, 
         ca2019name = `Area name`) %>% 
  clean_names()



### 3 - Create File for Latest Year ----

### 3.1 - Create Single Year File ----

# Attach ca2018 code and ca2011 code by recoding ca2019
# Create a sex_name column
# Sort the dataframe and then select the relevant variables

CA2019_pop_est <- mandf  %>% 
  mutate(ca2018 = recode(ca2019, 
                         'S12000049' = 'S12000046', 
                         'S12000050' = 'S12000044')) %>% 
  mutate(ca2011 = recode(ca2018, 
                         'S12000047' = 'S12000015', 
                         'S12000048' = 'S12000024'), 
         sex_name = recode(sex, '1' = 'M', '2' = 'F')) %>%
  arrange(year, ca2019, age, sex) %>% 
  select(year, ca2019, ca2019name, ca2018, ca2011, age, sex, sex_name, pop)



### 3.2 - Create 5 Year age Group File ----

# Create a file for 5 year age groups and sex
# Assign a 5 year age group to each age

CA2019_pop_est_5y <- CA2019_pop_est %>% 
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
  mutate(age_group_name = case_when(age_group == 0 ~ "0", 
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
                                    age_group == 19 ~ "90+"))

# Aggregate the dataset into 5 year age group and sex
# Group data to get population totals for each level within this
# Ungroup the data and select the relevant variables

CA2019_pop_est_5y %<>%
  group_by(year, ca2019, ca2019name, ca2018, ca2011, age_group, age_group_name, 
           sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  select(year, ca2019, ca2019name, ca2018, ca2011, age_group, age_group_name, 
         sex, sex_name, pop)



### 4 - Update Historical Files For New Release ----

### 4.1 - Update Single Year File ----

# Read in latest historical release files

CA2019_pop_est_hist <- readRDS(glue("{output_filepath}/", 
                                    "CA2019_pop_est_{start}_{prev}.rds")) %>% 
  clean_names()

# Add the latest release to single year file

CA2019_pop_est <- CA2019_pop_est_hist %>%
  full_join(CA2019_pop_est) %>% 
  arrange(year, ca2019, age, sex)

# Save as .RDS file

saveRDS(CA2019_pop_est, 
        glue("{output_filepath}/CA2019_pop_est_{start}_{new}.rds"))



### 4.2 - Update 5 Year age Group File ----

# Read in latest historical release files

CA2019_pop_est_5y_hist <- readRDS(
  glue("{output_filepath}/CA2019_pop_est_5year_agegroups_{start}_{prev}.rds")) %>% 
  clean_names()

# Add the latest release to 5 year age group file

CA2019_pop_est_5y <- CA2019_pop_est_5y_hist %>%
  full_join(CA2019_pop_est_5y) %>% 
  arrange(year, ca2019, age_group, sex)

# Save as .RDS file

saveRDS(CA2019_pop_est_5y, 
        glue("{output_filepath}/", 
             "CA2019_pop_est_5year_agegroups_{start}_{new}.rds"))



### 5 - Check files ----

### 5.1 - Check function ----

checks <- function(input){
  
  # Check that all years of the population estimates are there
  # Check that there are no missing values
  # Check all years have the same % of records
  
  input %>% group_by(year) %>% count() %>% print(n = Inf)
  
  # Check that all 32 Council Areas are there
  # Check there are no missing values
  # Check all CAs have the same % of records
  
  input %>% group_by(ca2019) %>% count() %>% print(n = Inf)
  input %>% group_by(ca2018) %>% count() %>% print(n = Inf)
  input %>% group_by(ca2011) %>% count() %>% print(n = Inf)
  input %>% group_by(ca2019name) %>% count() %>% print(n = Inf)
  
  if (str_extract(deparse(substitute(input)), "(..)$") == "5y"){
    
    # Check that all age groups are there
    # Check there are no missing values
    # Check all age groups have the same % of records
    
    input %>% count(age_group) %>% print(n = Inf)
    
  } else {
  
  # Check that all 91 ages 0 to 90+ are there
  # Check there are no missing values
  # Check all ages have the same % of records
  
  input %>% count(age) %>% print(n = Inf)
    
  }
  
  # Check that both males and females are there
  # Check there are no missing values
  # Check both sexes have the same % of records (50/50)
  
  input %>% count(sex) %>% print(n = Inf)
  
  # Check that the population values are as expected
  # i.e. no negative values or extremely high values etc
  
  input %>%
    group_by(pop) %>%
    count() %>%
    arrange(n) %>%
    filter(n <= 0 | n >= 300)
  
  # Select only the new year(s) of data
  
  new_years <- input %>%
    filter(year > 2011)
  
  # Check Council Area totals against NRS source data
  
  new_years %<>%
    group_by(year, ca2019) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()
  
  View(new_years)
  
  # Check Scotland totals against NRS source data
  
  new_years %>%
    group_by(year) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()
  
}



### 5.2 - Check single age file ----

checks(input = CA2019_pop_est)


### 5.3 - Check 5 year age group file ----

checks(input = CA2019_pop_est_5y)
