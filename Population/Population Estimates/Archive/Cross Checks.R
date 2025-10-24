##########################################################
# Checks for Council Area, HB and HSCP Population Estimates Files
# This script reproduces checks within the creation scripts
# and is used for cross-checking.
# Originally written by Tina Fu
# Updated by Calum Purdie, Iain MacKinnon
# Original date - 01/03/2018
# Data release - Mid-year Council Area Population Estimates
# Latest update author - Gerald Leung
# Latest update date - 26/03/2024
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 4.0.2
##########################################################

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

filepath <- glue("//stats/geography", 
                 "/Population/Population Estimates")
#filepath <- glue("/data/geography", 
#              "/Population/Population Estimates") # if on Posit
data_filepath <- glue("{filepath}/Source Data")
output_filepath <- glue("{filepath}/Lookup Files/R Files")

# Set estimates year - new is previous year to present as estimates lag behind prev is one year back and 
#start is 20 years back

start <- "1981"
prev <- "2021" 
new <- "2022"

#######################################################################################################

############## CA Files ##################################
# CA single year of age file
check_CA2019_pop_est <- readRDS(glue("{output_filepath}/", 
                                    "CA2019_pop_est_{start}_{new}.rds")) %>% 
  clean_names()

# CA 5 year age grouped file
check_CA2019_pop_est_5y <- readRDS(
  glue("{output_filepath}/CA2019_pop_est_5year_agegroups_{start}_{new}.rds")) %>% 
  clean_names()

# Create check function
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


### Check single year age file ----

checks(input = check_CA2019_pop_est)
#1) inital output table is number of single year records:  91 (ages) * number of Local authorities (32) * number of gender=5824
#2) second output is number of single year records (total number of rows) divided by number of ca2019 (32) this will increase year on year as a new years data is added.
# For 2022, should be 244608/32 = 7644. Similarly for #3-#5 below.
#3) third output is number of single year records divided by  caB2018 (32) this will increase year on year as a new years data is added
#4) fourth count is by Ca2011 as above
#5) fifth is by ca name. Total records/32 CA names.
#6) single year ages divided by total numebr of records. There should be 91 single ages. So total records/91.
# For 2022, should be 244608/91 = 2688.
#7) grouped into gender therefore total records divided by 2 as each gender has equal number of records.
# So 244608/2 = 122304
#8) check 10 year trend is as expected for whole scotland picture. See if any significant changes or odd numers.
#  For 2022, total population should be 5447700. This number is consistent with the NRS report.


### Check 5 year age group file ----

checks(input = check_CA2019_pop_est_5y)
#1) inital output table is number of 5 yr age bandings  40 (for M&F) * number of Local authorities (32) =1280
#2) second output is total number of records/ number of ca2019 (32) this will increase year on year as a new years data is added
# For 2022, 53760/32 = 1680. Similarly for #3-#5 below.
#3) third output is total number of records/ number of ca2018 (32) this will increase year on year as a new years data is added
#4) fourth count is by Ca2011 as above
#5) fifth is by ca name 
#6) total number of records/total number of each age band (there are 20 bands combining both M & F).
# For 2022, 53760/20 2688
#7) grouped into gender therefore total records divided by 2 as each gender has equal number of records.
# So 53760/2 = 26880
#8) check 10 year trend is as expected for whole scotland picture.
# Expect 5447700 for 2022. This number is consistent with the NRS report.

################################################################################################################

########### HB files ###########################

# Single age year
check_HB2019_pop_est <- readRDS(glue("{output_filepath}/", 
                                    "HB2019_pop_est_{start}_{new}.rds")) %>% 
  clean_names()

# 5y groups
check_HB2019_pop_est_5y <- readRDS(
  glue("{output_filepath}/", 
       "HB2019_pop_est_5year_agegroups_{start}_{new}.rds")) %>% 
  clean_names()

# Create check function

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



### - Check single age file ----

checks(input = check_HB2019_pop_est)

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

### - Check 5 year age group file ----

checks(input = check_HB2019_pop_est_5y)
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


######################################################################################################

########## HSCP files #####################
# Single age year
check_HSCP2019_pop_est <- readRDS(glue("{output_filepath}/", 
                                     "HSCP2019_pop_est_{start}_{new}.rds")) %>% 
  clean_names()

# 5y groups
check_HSCP2019_pop_est_5y <- readRDS(
  glue("{output_filepath}/", 
       "HSCP2019_pop_est_5year_agegroups_{start}_{new}.rds")) %>% 
  clean_names()

# Create check function
checks <- function(input){
  
  # Check that all years of the population estimates are there
  # Check that there are no missing values
  # Check all years have the same % of records
  
  input %>% group_by(year) %>% count() %>% print(n = Inf)
  
  # Check that all 31 HSCPs are there
  # Check there are no missing values
  # Check all HSCPs have the same % of records
  
  input %>% group_by(hscp2019) %>% count() %>% print(n = Inf)
  input %>% group_by(hscp2018) %>% count() %>% print(n = Inf)
  input %>% group_by(hscp2016) %>% count() %>% print(n = Inf)
  input %>% group_by(hscp2019name) %>% count() %>% print(n = Inf)
  
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
    filter(n <= 0 | n >= 300)
  
  # Select only the new year(s) of data
  
  new_years <- input %>%
    filter(year > 2011)
  
}

### 5.2 - Check single age file ----

checks(input = check_HSCP2019_pop_est)
#1) inital output table is number of single year records  91 * number of HSCP (31) * number of gender = 5642
#2) second output is number of single year records (total rows) divided by number of hscp2019 (31). This will increase year on year as a new years data is added.
# For 2022, 236964/31 = 7644. Similarly for #3-#5 below.
#3) third output is number of single year records divided by  hscp2018 (31). This will increase year on year as a new years data is added.
#4) fourth count is by hscp2016 (31) as above
#5) fifth is by (hscp2019name) name 
#6) single year ages divided by total number of records (i.e. rows/91).
# For 2022, 236964/31 = 2604.
#7) grouped into gender therefore total records divided by 2 as each gender has equal number of records.
# For 2022, 236964/2 = 118482.

### 5.3 - Check 5 year age group file ----

checks(input = check_HSCP2019_pop_est_5y)

#1) inital output table is total number of rows/total number of years
# For 2022, 52080/42 = 1240 (42 years from 1981-2022)
#2) second output is total number of rows divided by number of hscp2019 (31). This will increase year on year as a new years data is added.
# For 2022, 52080/31 = 1680. Similarly for #3-#5 below.
#3) third output is total number of rows divided by  hscp2018 (31). This will increase year on year as a new years data is added.
#4) fourth count is by hscp2016 (31) as above
#5) fifth is by (hscp2019name) name 
#6) Total number of rows divided by total number of age groups (20) (i.e. rows/20).
# For 2022, 52080/20 = 2604.
#7) grouped into gender therefore total records divided by 2 as each gender has equal number of records.
# For 2022, 52080/2 = 26040.




