##########################################################
# Create HSCP Population Estimates Files
# Tina Fu
# Original date - 05/03/2018
# Data release - Mid-year HSCP Population Estimates
# Latest update author - Calum Purdie
# Latest update date - 01/05/2020
# Latest update description - 2019 estimates
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for creating HSCP population estimates files based on mid-year 
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
library(ckanr)

# Set filepaths

filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                 "2_Population/Population Estimates")
data_filepath <- glue("{filepath}/Source Data")
output_filepath <- glue("{filepath}/Lookup Files/R Files")

# Set estimates year

start <- "1981"
prev <- "2018"
new <- "2019"

# Open Data resources

ckan <- src_ckan("https://www.opendata.nhs.scot")
ca2011_id <- "967937c4-8d67-4f39-974f-fd58c4acfda5"
ca2018_id <- "294478b7-3d67-44f6-a462-721d8d2c44dd"
ca2019_id <- "2dab0c9d-09be-4266-97f8-4f83e78db85f"



### 2 - Read in Council Area Estimates ----

CA2019_pop_est <- readRDS(glue("{output_filepath}/", 
                               "CA2019_pop_est_{start}_{new}.rds"))

CA2019_pop_est_5y <- readRDS(
  glue("{output_filepath}/CA2019_pop_est_5year_agegroups_{start}_{new}.rds"))

# Get HSCP columns from open data resources

hscp2016_lookup <- dplyr::tbl(src = ckan$con, from = ca2011_id) %>% 
  select(CA, HSCP) %>% 
  rename(ca2011 = CA, hscp2016 = HSCP) %>%  
  as_tibble()

hscp2018_lookup <- dplyr::tbl(src = ckan$con, from = ca2018_id) %>% 
  select(CA, HSCP) %>% 
  rename(ca2018 = CA, hscp2018 = HSCP) %>%  
  as_tibble()

hscp2019_lookup <- dplyr::tbl(src = ckan$con, from = ca2019_id) %>% 
  select(CA, CAName, HSCP, HSCPName) %>% 
  rename(ca2019 = CA, ca2019name = CAName, hscp2019 = HSCP, 
         hscp2019name = HSCPName) %>%  
  as_tibble()




### 3 - Create the HSCP population estimate file ----

# Match the HSCP columns onto the CA estimates file
# Select the relevant columns
# As there are 31 HSCPs and 32 CAs (Stirling and Clackmannanshire join for HSCP) 
# this will produce population separately for both Stirling and Clackmannanshire 
# even though they have the same HSCP code
# Need to recalculate the sums, so group by all variables bar Pop and calculate 
# new pop totals
# Arrange by year, hscp2019, age and sex to get the required format

HSCP2019_pop_est <- CA2019_pop_est %>%
  left_join(hscp2016_lookup) %>%
  left_join(hscp2018_lookup) %>%
  left_join(hscp2019_lookup) %>%
  select(year, hscp2019, hscp2019name, hscp2018, hscp2016, age, sex, sex_name, 
         pop) %>%
  group_by(year, hscp2019, hscp2019name, hscp2018, hscp2016, age, sex, 
           sex_name) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  arrange(year, hscp2019, age, sex)


# Save file as .RDS

saveRDS(HSCP2019_pop_est, 
        glue("{output_filepath}/HSCP2019_pop_est_{start}_{new}.rds"))



### 4 - Create the HSCP 5 year age group population estimate file ----

# Match the HSCP columns onto the CA estimates file
# Create column for SexName
# Select the relevant columns
# As there are 31 HSCPs and 32 CAs (Stirling and Clackmannanshire join for HSCP) 
# this will produce population separately for both Stirling and Clackmannanshire 
# even though they have the same HSCP code
# Need to recalculate the sums, so group by all variables bar Pop and calculate 
# new pop totals
# Arrange by year, hscp2019, age_group and sex to get the required format

HSCP2019_pop_est_5y <- CA2019_pop_est_5y %>%
  left_join(hscp2016_lookup) %>%
  left_join(hscp2018_lookup) %>%
  left_join(hscp2019_lookup) %>%
  select(year, hscp2019, hscp2019name, hscp2018, hscp2016, age_group, 
         age_group_name, sex, sex_name, pop) %>%
  group_by(year, hscp2019, hscp2019name, hscp2018, hscp2016, age_group, 
           age_group_name, sex, sex_name) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  arrange(year, hscp2019, age_group, sex)

# Save file as .RDS

saveRDS(HSCP2019_pop_est_5y, 
        glue("{output_filepath}/", 
             "HSCP2019_pop_est_5year_agegroups_{start}_{new}.rds"))



### 5 - Check files ----

### 5.1 - Check function ----

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

checks(input = HSCP2019_pop_est)


### 5.3 - Check 5 year age group file ----

checks(input = HSCP2019_pop_est_5y)
