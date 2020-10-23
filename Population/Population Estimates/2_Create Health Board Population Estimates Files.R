##########################################################
# Create Council Area Population Estimates Files
# Tina Fu
# Original date - 01/03/2018
# Data release - Mid-year Health Board Population Estimates
# Latest update author - Calum Purdie
# Latest update date - 30/04/2020
# Latest update description - 2019 estimates
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for creating health board population estimates files based on mid-year 
# estimates released by NRS
# Approximate run time - 3 minutes
##########################################################

### 1 - Housekeeping ----

# Read in packages from library

library(magrittr)
library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(tidylog)
library(glue)
library(janitor)
library(stringr)

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
# Rename variables
# Transpose the dataset

male <- read_excel(glue("{data_filepath}/Population_Estimates_{new}.xlsx"), 
                   sheet = "Table 2", "A93:CQ107") %>% 
  select(-c('...3', `...4`)) %>% 
  set_names(c("hb2019", "hb2019name","0":"90")) %>% 
  gather(age, pop, `0`:`90`) %>% 
  mutate(sex = 1)

# Female
# Remove unnessary columns
# Transpose the dataset

female <- read_excel(glue("{data_filepath}/Population_Estimates_{new}.xlsx"), 
                     sheet = "Table 2", range = "A146:CQ160") %>% 
  select(-c('...3', `...4`)) %>% 
  set_names(c("hb2019", "hb2019name","0":"90")) %>% 
  gather(age, pop, `0`:`90`) %>% 
  mutate(sex = 2)



### 2.2 - Add Male and Female Files Together ----

# Add male and female together
# Change age to numeric rather than character

mandf <- bind_rows(male, female) %>% 
  mutate(year = as.numeric(new),
         age = as.numeric(age))



### 3 - Create File for Latest Year ----

### 3.1 - Attach hb2019 Code and Create Single Year File ----

# Attach hb2018 code and hb2014 code by renaming hb2019
# Create a sex_name column and add NHS prefix to hb2019name
# Sort the dataframe and then select the relevant variables

HB2019_pop_est <- mandf %>% 
  mutate(hb2018 = recode(hb2019, 
                         'S08000031' = 'S08000021', 
                         'S08000032' = 'S08000023'),
         hb2014 = recode(hb2018, 
                         'S08000029' = 'S08000018', 
                         'S08000030' = 'S08000027'), 
         sex_name = recode(sex, '1' = 'M', '2' = 'F'), 
         hb2019name = paste0("NHS ", hb2019name)) %>%
  arrange(year, hb2019, age, sex) %>% 
  select(year, hb2019, hb2019name, hb2018, hb2014, age, sex, sex_name, pop)



### 3.2 - Create 5 Year Age Group File ----

# Create a file for 5 year age group and sex

HB2019_pop_est_5y <- HB2019_pop_est %>%
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
                                    age_group == 19 ~ "90+"))


# Aggregate the dataset into 5 year age group and sex
# Group data to get population totals for each level within this
# Ungroup the data and select the relevant variables

HB2019_pop_est_5y %<>%
  group_by(year, hb2019, hb2019name, hb2018, hb2014, age_group, age_group_name, 
           sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  arrange(year, hb2019, age_group, sex) %>% 
  select(year, hb2019, hb2019name, hb2018, hb2014, age_group, age_group_name, 
         sex, sex_name, pop)



### 4 - Update Historical Files For New Release ----

### 4.1 - Update Single Year File ----

# Read in latest historical release files

HB2019_pop_est_hist <- readRDS(glue("{output_filepath}/", 
                                    "HB2019_pop_est_{start}_{prev}.rds")) %>% 
  clean_names() %>% 
  mutate(hb2019name = paste0("NHS ", hb2019name))

# Add the latest release to single year file

HB2019_pop_est <- HB2019_pop_est_hist %>%
  full_join(HB2019_pop_est) %>% 
  arrange(year, hb2019, age, sex)

# Save as .RDS file

saveRDS(HB2019_pop_est, 
        glue("{output_filepath}/HB2019_pop_est_{start}_{new}.rds"))



### 4.2 - Update 5 Year Age Group File ----

# Read in latest historical release files

HB2019_pop_est_5y_hist <- readRDS(
  glue("{output_filepath}/", 
       "HB2019_pop_est_5year_agegroups_{start}_{prev}.rds")) %>% 
  clean_names() %>% 
  mutate(hb2019name = paste0("NHS ", hb2019name))

# Add the latest release to 5 year age group file

HB2019_pop_est_5y <- HB2019_pop_est_5y_hist %>%
  full_join(HB2019_pop_est_5y) %>% 
  arrange(year, hb2019, age_group, sex)

# Save as .RDS file

saveRDS(HB2019_pop_est_5y, 
        glue("{output_filepath}/", 
             "HB2019_pop_est_5year_agegroups_{start}_{new}.rds"))




### 5 - Check files ----

### 5.1 - Check function ----

checks <- function(input){
  
  # Check that all years of the population estimates are there 
  # Check that there are no missing values
  # Check all years have the same % of records
  
  input %>% group_by(year) %>% count() %>% print(n = Inf)
  
  # Check that all 14 health boards are there
  # Check there are no missing values
  # Check all HBs have the same % of records
  
  input %>% group_by(hb2019) %>% count() %>% print(n = Inf)
  input %>% group_by(hb2018) %>% count() %>% print(n = Inf)
  input %>% group_by(hb2014) %>% count() %>% print(n = Inf)
  input %>% group_by(hb2019name) %>% count() %>% print(n = Inf)
  
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
  
  input %>% group_by(sex) %>% count() %>% print(n = Inf)
  
  # Check that the population values are as expected
  # i.e. no negative values or extremely high values etc
  
  input %>%
    group_by(pop) %>%
    count() %>%
    arrange(n) %>%
    filter(n <= 0 | n >= 250)
  
  # Select only the new year(s) of data
  
  new_years <- input %>%
    filter(year > 2011)
  # 
  # Check Council Area totals against NRS source data
  
  new_years %<>%
    group_by(year, hb2019) %>%
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

checks(input = HB2019_pop_est)


### 5.3 - Check 5 year age group file ----

checks(input = HB2019_pop_est_5y)
