##########################################################
# Datamart Tests
# Calum Purdie
# Original date 30/03/2020
# Latest update author - Calum Purdie
# Latest update date - 13/05/2020
# Latest update description - formatting code
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for testing new population projections uploaded to the datamart
# Run these checks in R and Business Objects and check results match
# Approximate run time - 10 seconds
##########################################################

### 1 Housekeeping ----


rm(list = ls())

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}

pacman::p_load(phsmethods,
               magrittr,tidyr,readxl, readr, janitor,tidylog, glue, stringr, dplyr,data.table)


# Set years to use


# set projection start & end dates 
start <- "2022" #update
end <- "2047" #update


test_year <- "2030" # update
geography <- "SCOTLAND" # update
extract_date <- "20250205" #update

# Set filepath

# Getting main script location for working directory
path_main_script_location = dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(path_main_script_location)

datamart_filepath <- ("/data/geography/Population/Population Datamart/")

uat_filepath <- glue("{datamart_filepath}", "/Lookup Files/UAT_test_files/")



# compare to source data

source_filepath <- ("//data/geography/Population/Population Projections/Lookup Files/R Files/")

scot_filepath <- glue("{source_filepath}/scot_pop_proj_{start}_{end}.rds")
hb_filepath <- glue("{source_filepath}/HB2019_pop_proj_{start}_{end}.rds")
hscp_filepath <- glue("{source_filepath}/HSCP2019_pop_proj_{start}_{end}.rds")
ca_filepath <- glue("{source_filepath}/CA2019_pop_proj_{start}_{end}.rds")



### Scotland projections source data ----

scot_source <- readRDS(scot_filepath)

scot_source_total_pop= scot_source %>% 
  filter(year==test_year) %>% 
  group_by(year) %>% 
  summarise(source_pop= sum(pop)) 

scot_source_sex_pop= scot_source %>% 
  filter(year==test_year) %>% 
  group_by(gender=sex) %>% 
  summarise(source_sex_pop= sum(pop))

scot_source_age_pop= scot_source %>% 
  filter(year==test_year) %>% 
  group_by(age_band=age) %>% 
  summarise(source_age_pop= sum(pop))

#### Scotland uat_test outputs ####

# Check that all years of the population projections are there 
# Check that all totals match

uat_files <- tibble(file_name = list.files(uat_filepath, full.names = TRUE))  %>% 
  print(n = Inf)

# import an example UAT output, update the test_year variable to the selected year 

uat_test_orig <-read_csv(glue({uat_filepath},"POPULATION_",{geography},"_PROJECTIONS_", {test_year},"_",{extract_date},".csv"),
                    skip = 1)

# clean up the imported file

uat_test_clean <- uat_test_orig %>% 
  clean_names() %>% 
  filter(population!="Y") %>% 
  select(1:4) %>% 
  mutate(year=test_year) %>% 
  mutate(across(2:5, as.numeric) ) 
  

uat_total_pop <- uat_test_clean %>% 
  group_by(year) %>% 
  summarise(uat_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(scot_source_total_pop, by="year") %>% 
  mutate(check = case_when(uat_pop == source_pop ~"total pop match",
                                     TRUE ~ "mismatch- check data")) %>% 
  print(n = Inf)
rm(uat_total_pop, scot_source_total_pop)

uat_sex_pop <- uat_test_clean %>% 
  group_by(year, gender) %>% 
  summarise(uat_sex_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(scot_source_sex_pop, by="gender") %>% 
  mutate(sex_pop_match =case_when (uat_sex_pop== source_sex_pop ~ "sex pop match",
                                   TRUE~" There is a mismatch- check data"))%>% 
  print(n = Inf)

rm(uat_sex_pop, scot_source_sex_pop)
  

uat_age_pop <- uat_test_clean %>% 
  group_by(year, age_band) %>% 
  summarise(uat_age_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(source_age_pop, by="age_band") %>% 
  mutate(age_pop_match =case_when (uat_age_pop== source_age_pop ~ "Population by age match",
                                   TRUE~" There is a mismatch- check data"))%>% 
  print(n = Inf)

# Read in Health Board projections

hb <- readRDS(hb_filepath)

# Read in Health Board projections

ca <- readRDS(ca_filepath)

# Read in Health Board projections

hscp <- readRDS(hscp_filepath)

# Check that all years of the population projections are there 
# Check that all totals match



# Choose a specific year and check that the totals by health board and sex match
# Use hb2014 as datamart uses old codes

hb %>% 
  filter(year == 2030) %>% 
  group_by(hb2014name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

hb %>% 
  filter(year == 2043) %>% 
  group_by(hb2014name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

# Choose a specific year and check that the totals by age

hb %>%  
  filter(year == 2018) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

hb %>%  
  filter(year == 2035) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)



### 3 HSCP2016 ----

# Read in Health Board projections

hscp <- readRDS(hscp_filepath)

# Check that all years of the population projections are there 
# Check that all totals match

hscp %>% 
  group_by(year) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

# Choose a specific year and check that the totals by hscp and sex match
# Use hscp2016 as datamart uses old codes

hscp %>% 
  filter(year == 2024) %>% 
  group_by(hscp2019name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

hscp %>% 
  filter(year == 2038) %>% 
  group_by(hscp2019name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

# Choose a specific year and check that the totals by age

hscp %>%  
  filter(year == 2020) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

hscp %>%  
  filter(year == 2029) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)



### 4 CA2011 ----

# Read in Health Board projections

ca <- readRDS(ca_filepath)

# Check that all years of the population projections are there 
# Check that all totals match

ca %>% 
  group_by(year) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

# Choose a specific year and check that the totals by health board and sex match
# Use ca2011 as datamart uses old codes

ca %>% 
  filter(year == 2027) %>% 
  group_by(ca2019name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

ca %>% 
  filter(year == 2034) %>% 
  group_by(ca2019name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

# Choose a specific year and check that the totals by age

ca %>%  
  filter(year == 2026) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

ca %>%  
  filter(year == 2041) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)
