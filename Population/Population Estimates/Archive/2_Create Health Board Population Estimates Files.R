##########################################################
# Create Health Boards Population Estimates Files
# Originally written by Tina Fu
# Updated by Calum Purdie, Iain MacKinnon, Gerald Leung,Alan Coventry
# Original date - 01/03/2018
# Data release - Mid-year Council Area Population Estimates
# Latest update author - Alan Coventry
# Latest update date - 27/01/2025
# Latest update description - NRS source data format change in mandf. Convert old geography codes into 2019 codes. Geography checks at end.
# Type of script - Creation
# Written/run on Posit Workbench
# Code for creating health board population estimates files based on mid-year estimates AND rebased estimates
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
library(data.table)
#library(haven)

# Set filepaths

# filepath <- glue("//stats/geography", 
#                  "/Population/Population Estimates")
filepath <- glue("/data/geography",
                 "/Population/Population Estimates") # if on Posit
data_filepath <- glue("{filepath}/Source Data")
output_filepath <- glue("{filepath}/Lookup Files/R Files")

# Set estimates year

start <- "1981"
prev <- "2023"
new <- "2024"



### 2 - Import Male and Female Data ----

### 2.1 - Import Data ----
# IM data easier to upload and wrangle as no blank rows between male and female rows
# Remove unnessary columns
# Rename variables
# Transpose the dataset


### 2.1 - Import Data ----

# IM data set easier to upload and wrangle as no blank rows between male and female data sets
## Transpose the dataset

mandf <- read_excel(glue("{data_filepath}/data-mid-year-population-estimates-2024.xlsx"),  #update import file name accordingly
                    sheet = "Table 1", range = "A4:CR4326") %>%  # changed table and cell refs
  filter(
    #`Year` == new,
         `Sex` != "Persons",`Area code` != "S92000003") %>% #IM select HB only 
  select(-"All ages")  %>% # remove prior to pivot
  pivot_longer("0":"90 and over",names_to = "Age",values_to = "pop") %>% 
  rename(hb2019 = `Area code`, hb2019name = `Area name`) %>% # IM replaced gather command
  mutate(Sex = case_when(Sex == 'Males' ~ "1", 
                         Sex == 'Females' ~ "2"),
         year = as.integer(new), 
         Age = case_when(Age == "90 and over" ~ "90",            # 90+ for 2021, 90 and over for 2022. Double check
                         TRUE ~ Age), 
         Age = as.numeric(Age),
         Sex = as.numeric(Sex)) %>% 
  clean_names()

#### 2.2. change the geography codes - change to HB 2019

area_code_changes <- c(
  'S08000001'='S08000015',
  'S08000002'='S08000016',
  'S08000003'='S08000017',
  'S08000005'='S08000019',
  'S08000006'='S08000020',
  'S08000010'='S08000024',
  'S08000011'='S08000025',
  'S08000012'='S08000026',
  'S08000014'='S08000028',
  'S08000004'='S08000029',
  'S08000013'='S08000030',
  'S08000009'='S08000032',
  'S08000007'='S08000031',
  'S08000008'='S08000022',
  'S08000021'='S08000031',
  'S08000023'='S08000032'
)


# Replace values in the ca2019 column, keeping non-matching values intact
mandf$hb2019 <- ifelse(
  mandf$hb2019 %in% names(area_code_changes),  # Check if the value is in the mapping
  area_code_changes[match(mandf$hb2019, names(area_code_changes))],  # Replace if found
  mandf$hb2019  # Retain original value if not found
)


### 3 - Create File for Latest Year ----

### 3.1 - Attach hb2019 Code and Create Single Year File ----

# Attach hb2018 code and hb2014 code by renaming hb2019
# Create a sex_name column and add NHS prefix to hb2019name
# Sort the dataframe and then select the relevant variables

HB2019_pop_est <- mandf %>% 
  filter(area_type == "Health board") %>% 
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
  clean_names()

# Add the latest release to single year file

HB2019_pop_est <- HB2019_pop_est_hist %>%
  full_join(HB2019_pop_est) %>% 
  arrange(year, hb2019, age, sex)

### 4.2 - Update 5 Year Age Group File ----

# Read in latest historical release files

HB2019_pop_est_5y_hist <- readRDS(
  glue("{output_filepath}/", 
       "HB2019_pop_est_5year_agegroups_{start}_{prev}.rds")) %>% 
  clean_names()

# Add the latest release to 5 year age group file

HB2019_pop_est_5y <- HB2019_pop_est_5y_hist %>%
  full_join(HB2019_pop_est_5y) %>% 
  arrange(year, hb2019, age_group, sex)


### 5 - Check files ----

### 5.1 - Check function ----


### 5.2 - Check single age file ----

checks(input = HB2019_pop_est)

# Interpretation
# First console output should have equal no rows per year i.e. 
# each year =  (total No rows /total number of years)
# also check continuous sequence of years starting at 1981
# For 2022, 107016/42 = 2548 (42 years from 1981 - 2022)

# 2nd output hb2019 grouping of table
# should have 14 rows with equal numbers in each
# number in each row should when multiplied by 14 = total number of rows of the file
# 3rd & 4th & 5th does this for the hb2018, hb2014 grouping and hb2019 name. 
# check there are 14 rows and equal split between each
# For 2022, 107016/14 = 7644.

# #6 checks that there are 91 splits, one per age (with equal split within each)
# i.e. divide total no rows by 91
# For 2022, 107016/91 = 1176.

# #7 checks equal number of males and females
# i.e. divide total no rows by 2
# For 2022, 107016/2 = 53508.

# #8 manual check of source data total pop for 2022. 
# Also check that there a sensible increase (or decrease)
# For 2022, should be 5447700. Consistent with the number on the NRS report.

### 5.3 - Check 5 year age group file ----

checks(input = HB2019_pop_est_5y)
# Interpretation
# First console output should have equal no rows per year i.e. 
# each year =  (total No rows /total number of years)
# also check continuous sequence of years starting at 1981
# For 2022, 23520/42 = 560 (42 years from 1981-2022)

# 2nd output hb2019 grouping of table
# should have 14 rows with equal numbers in each
# number in each row should when multiplied by 14 = total row no in table
# 3rd & 4th & 5th does this for the hb2018, hb2014 grouping and hb2019 name. 
# check there are 14 rows and equal split between each
# For 2022, 23520/14 = 1680

# #6 checks that there are 20 age band splits with equal no of rows  within each.
# i.e. divide total no rows by 20
# For 2022, 23520/20 = 1176

# #7 checks equal number of males and females
# i.e. divide total no rows by 2
# For 2022, 23520/2 = 11760

# #8 manual check of source data total pop for 2022. 
# Also check that there a sensible increase (or decrease)
# 2022 should be 5447700, consistent with the number on the NRS report.



### 6.0 Export ####
#### 6.1 -  Single Year #### 
# Save as .RDS file
saveRDS(HB2019_pop_est, 
        glue("{output_filepath}/HB2019_pop_est_{start}_{new}.rds"))


#write_csv FILE
fwrite(HB2019_pop_est, 
       glue("{output_filepath}/HB2019_pop_est_{start}_{new}.csv",
            na=""))
### 6.2 -  5 Year age Group  ####
# Save as .RDS file

saveRDS(HB2019_pop_est_5y, 
        glue("{output_filepath}/", 
             "HB2019_pop_est_5year_agegroups_{start}_{new}.rds"))

#write_csv FILE
fwrite(HB2019_pop_est_5y, 
       glue("{output_filepath}/", 
            "HB2019_pop_est_5year_agegroups_{start}_{new}.csv",
            na=""))

