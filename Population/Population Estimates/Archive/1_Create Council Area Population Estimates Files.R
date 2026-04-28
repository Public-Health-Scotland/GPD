##########################################################
# Create Council Area Population Estimates Files
# Originally written by Tina Fu
# Updated by Calum Purdie, Iain MacKinnon, Gerald Leung,Alan Coventry
# Original date - 01/03/2018
# Data release - Mid-year Council Area Population Estimates
# Latest update author - Alan Coventry
# Latest update date - 27/01/2025
# Latest update description - NRS source data format change in mandf. Geography checks at end
# Type of script - Creation
# Written/run on Posit Workbench
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
library(data.table)

# Set filepaths

# filepath <- glue("//stats/geography", 
#                  "/Population/Population Estimates")
filepath <- glue("/data/geography",
                 "/Population/Population Estimates") # if on Posit
data_filepath <- glue("{filepath}/Source Data")
output_filepath <- glue("{filepath}/Lookup Files/R Files")

# Set estimates year - see SOP. start is 1981. New is new year (output file). Prev is input file (previous year)

start <- "1981"
prev <- "2023" 
new <- "2024"

### 2 - Import Male and Female Data ----

### 2.1 - Import Data ----

# IM data set easier to upload and wrangle as no blank rows between male and female data sets
## Transpose the dataset

mandf <- read_excel(glue("{data_filepath}/data-mid-year-population-estimates-2024.xlsx"),  #update import file name accordingly
                    sheet = "Table 1", range = "A4:CR4326") %>%  # change table and cell refs. Range to include all fields from headesds downward.
  filter(
   #`Year` == new,
    `Sex` != "Persons",
    `Area code` != "S92000003") %>% # select M and F rows only, year of interest, remove scotland totals
  select(-"All ages")  %>%                                         # only need single year of age 
  pivot_longer("0":"90 and over",names_to = "Age",values_to = "pop") %>%    # IM replaced gather command
  mutate(Sex = case_when(Sex == 'Males' ~ "1", 
                         Sex == 'Females' ~ "2"))%>% 
  mutate(year = as.integer(new), 
         Age = case_when(Age == "90 and over" ~ "90",     # 90+ for 2021, 90 and over for 2022. Double check
                         TRUE ~ Age), 
         Age = as.integer(Age),
         Sex = as.integer(Sex)) %>% 
  rename(ca2019 = `Area code`, 
         ca2019name = `Area name`) %>% 
  clean_names()


#### 2.2. change the geography codes - change to 2019

area_code_changes <- c(
  'S12000001'='S12000033',
  'S12000002'='S12000034',
  'S12000003'='S12000041',
  'S12000004'='S12000035',
  'S12000007'='S12000042',
  'S12000009'='S12000045',
  'S12000012'='S12000036',
  'S12000015'='S12000047',
  'S12000016'='S12000049',
  'S12000037'='S12000049',
  'S12000043'='S12000049',
  'S12000046'='S12000049',
  'S12000022'='S12000050',
  'S12000044'='S12000050',
  'S12000024'='S12000048',
  'S12000025'='S12000038'
  
  
)

# Replace values in the ca2019 column, keeping non-matching values intact
mandf$ca2019 <- ifelse(
  mandf$ca2019 %in% names(area_code_changes),  # Check if the value is in the mapping
  area_code_changes[match(mandf$ca2019, names(area_code_changes))],  # Replace if found
  mandf$ca2019  # Retain original value if not found
)


### 3 - Create File for Latest Year ----

### 3.1 - Create Single Year File ----



# Attach ca2018 code and ca2011 code by recoding ca2019
# Create a sex_name column
# Sort the dataframe and then select the relevant variables

CA2019_pop_est <- mandf  %>% 
  filter(area_type=="Council area") %>%  #IM added to ensure CAs only Aug 25
  mutate(ca2018 = recode(ca2019, 
                         'S12000049'='S12000046',
                         'S12000050'='S12000044'
                         
  )) %>% 
  mutate(ca2011 = recode(ca2018, 
                         'S12000047'='S12000015',
                         'S12000048'='S12000024'), 
         sex_name = recode(sex, '1' = 'M', '2' = 'F')) %>% #IM needed to allow full join to historic data
  arrange(year, ca2019, age, sex) %>% 
  select(year, ca2019, ca2019name, ca2018, ca2011, age, sex, sex_name, pop)



### 3.2 - Create 5 Year age Group File ----

# Create a file for 5 year age groups and sex
# Assign a 5 year age group to each age
#apart from 0 years it is an assumption of the team that this allows a birth count to be used.


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

### 4.2 - Update 5 Year age Group File ----

# Read in latest historical release files

CA2019_pop_est_5y_hist <- readRDS(
  glue("{output_filepath}/CA2019_pop_est_5year_agegroups_{start}_{prev}.rds")) %>% 
  clean_names()

# Add the latest release to 5 year age group file

CA2019_pop_est_5y <- CA2019_pop_est_5y_hist %>%
  full_join(CA2019_pop_est_5y) %>% 
  arrange(year, ca2019, age_group, sex)


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
  
  # Select last 10 years of data to check trend / nothing unexpected / outlandish
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
# 1) inital output table is number of single year records: #
# i.e. 91 (ages) * number of Local authorities (32) * number of gender=5824

# 2) second output is number of single year records (total number of rows) divided 
# by number of ca2019 (32) this will increase year on year as a new years data is added

#3) third output is number of single year records divided by  ca2018 (32) this will increase year on year as a new years data is added
#4) fourth count is by Ca2011 as above
#5) fifth is by ca name. Total records/32 CA names.
#6) single year ages divided by total numebr of records. There should be 91 single ages. So total records/91.
#7) grouped into gender therefore total records divided by 2 as each gender has equal number of records.
#8) check 10 year trend is as expected for whole scotland picture. See if any significant changes or odd numers.
### 5.3 - Check 5 year age group file ----

checks(input = CA2019_pop_est_5y)
#1) inital output table is number of 5 yr age bandings  40 (for M&F) * number of Local authorities (32) =1280
#2) second output is total number of records/ number of ca2019 (32) this will increase year on year as a new years data is added
#3) third output is total number of records/ number of ca2018 (32) this will increase year on year as a new years data is added
#4) fourth count is by Ca2011 as above
#5) fifth is by ca name 
#6) total number of records/total number of each age band (there are 20 bands combining both M & F)
#7) grouped into gender therefore total records divided by 2 as each gender has equal number of records.
#8) check 10 year trend is as expected for whole scotland picture.

### 6.0 Export ####
### 6.1 -  Single Year ###

saveRDS(CA2019_pop_est, 
        glue("{output_filepath}/CA2019_pop_est_{start}_{new}.rds"))

#haven::write_sav(CA2019_pop_est, 
#                glue("{output_filepath}/CA2019_pop_est_{start}_{new}.sav"))

#write_csv FILE
fwrite(CA2019_pop_est, 
       glue("{output_filepath}/CA2019_pop_est_{start}_{new}.csv",
            na=""))


### 6.2 -  5 Year age Group  ####
# Save as .RDS file
saveRDS(CA2019_pop_est_5y, 
        glue("{output_filepath}/", 
             "CA2019_pop_est_5year_agegroups_{start}_{new}.rds"))

# .spss
#haven::write_sav(CA2019_pop_est_5y, 
#               glue("{output_filepath}/", 
#                   "CA2019_pop_est_5year_agegroups_{start}_{new}.sav"))

#write_csv FILE
fwrite(CA2019_pop_est_5y, 
       glue("{output_filepath}/", 
            "CA2019_pop_est_5year_agegroups_{start}_{new}.csv",
            na=""))

# for testing
#x <- read_csv(glue("{output_filepath}/", 
#                 "CA2019_pop_est_5year_agegroups_{start}_{prev}.csv"))



# councils <- CA2019_pop_est %>% count(ca2019, ca2018, ca2011)
