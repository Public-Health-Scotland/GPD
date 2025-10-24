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

# 
# test_year <- "2030" # update
# geography <- "SCOTLAND" # update scotland hb? ca? hscp?
# extract_date <- "20250205" #update

# Set filepath

# Getting main script location for working directory
path_main_script_location = dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(path_main_script_location)

datamart_filepath <- ("/data/geography/Population/Population Datamart/")

uat_filepath <- glue("{datamart_filepath}", "Lookup Files/UAT_test_files/")



#### uat_test outputs ####

# Check that all years of the population projections are there 
# Check that all totals match

# uat_files <- tibble(file_name = list.files(uat_filepath, full.names = TRUE))  %>% 
#   print(n = Inf)
# 
# # import an example UAT output, update the test_year variable to the selected year 
# 
# uat_test_orig <-read_csv(glue({uat_filepath},"POPULATION_",{geography},"_PROJECTIONS_", {test_year},"_",{extract_date},".csv"),
#                          skip = 1)

uat_test_orig <-read_csv(glue({uat_filepath},"SCTASK0593443_Population_projections.csv"))

# clean up the imported file

uat_test_clean <- uat_test_orig %>% 
  select(geog= POPULATION_NAME, geog=POPULATION_NAME, year= FINANCIAL_YEAR, population=POPULATION,
         age=AGE_BAND, ca2019=COUNCIL_AREA_CONFIG_CODE_9, gender = GENDER,
         hb2019= HEALTH_BOARD_CODE_9, hscp2019= HSCP_CODE) 



# compare to source data

source_filepath <- ("//data/geography/Population/Population Projections/Lookup Files/R Files/")

scot_filepath <- glue("{source_filepath}/scot_pop_proj_{start}_{end}.rds")

# if (geography == "SCOTLAND") {
#   source_data <- readRDS(scot_filepath)
# } else if (geography == "HB") {
#   source_data <- readRDS(hb_filepath)
# } else if (geography == "CA") {
#   source_data <- readRDS(ca_filepath)
# } else if (geography == "HSCP") {
#   source_data <- readRDS(hscp_filepath)
# } else {
#   source_data <- readRDS(scot_filepath)
# }

# 
# 
# scot_source_data <- readRDS(scot_filepath)
# hb_source_data <- readRDS(hb_filepath)



### CA projections Checks  ----

#source data - data that NSS data are checked against
ca_filepath <- glue("{source_filepath}/CA2019_pop_proj_{start}_{end}.rds")

ca_source_data <- readRDS(ca_filepath) %>% 
  mutate(year= as.numeric(year), 
         gender =case_when(sex=="1"~1, sex=="2"~2, TRUE~0) )

ca_source_total_pop= ca_source_data %>% 
  group_by(ca2019,year) %>% 
  summarise(source_pop= sum(pop)) 

ca_source_sex_pop= ca_source_data  %>% 
  group_by(ca2019, year, gender) %>% 
  summarise(source_sex_pop= sum(pop))

ca_source_age_pop= ca_source_data  %>% 
  group_by(ca2019, year, age) %>% 
  summarise(source_age_pop= sum(pop))

# uat CA data

ca_uat <- uat_test_clean  %>% 
  filter(!is.na(ca2019))

ca_uat_total_pop <- ca_uat %>% 
  group_by(year, ca2019) %>% 
  summarise(uat_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(ca_source_total_pop, by=c("year","ca2019")) %>% 
  mutate(check = case_when(uat_pop == source_pop ~"total match",
                           TRUE ~ "mismatch - check data")) %>% 
  filter(check== "mismatch- check data") %>% 
  print(n = Inf)
  
ca_uat_sex_pop <- ca_uat %>% 
  group_by(ca2019, year, gender) %>% 
  summarise(uat_sex_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(ca_source_sex_pop, by=c("gender", "year","ca2019")) %>% 
  mutate(check =case_when (uat_sex_pop== source_sex_pop ~ "match",
                                   TRUE~"mismatch - check data"))%>% 
  filter(check== "mismatch - check data") %>% 
  print(n = Inf)

ca_uat_age_pop <- ca_uat %>% 
  group_by(ca2019, year, age) %>% 
  summarise(uat_age_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(ca_source_age_pop, by=c("age", "year","ca2019")) %>% 
  mutate(check =case_when (uat_age_pop== source_age_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check == "mismatch - check data") %>% 
  print(n = Inf)

rm(ca_uat_sex_pop, ca_uat_age_pop, ca_uat )
rm(ca_source_total_pop,  ca_source_sex_pop, ca_source_age_pop, ca_source_data, ca_uat_total_pop)
#### end CA checks ####
### HB projections Checks  ----

#source data - data that NSS data are checked against
hb_filepath <- glue("{source_filepath}/HB2019_pop_proj_{start}_{end}.rds")

hb_source_data <- readRDS(hb_filepath) %>% 
  mutate(year= as.numeric(year), 
         gender =case_when(sex=="1"~1, sex=="2"~2, TRUE~0) )

hb_source_total_pop= hb_source_data %>% 
  group_by(hb2019,year) %>% 
  summarise(source_pop= sum(pop)) 

hb_source_sex_pop= hb_source_data  %>% 
  group_by(hb2019, year, gender) %>% 
  summarise(source_sex_pop= sum(pop))

hb_source_age_pop= hb_source_data  %>% 
  group_by(hb2019, year, age) %>% 
  summarise(source_age_pop= sum(pop))

# uat HB data

hb_uat <- uat_test_clean  %>% 
  filter(!is.na(hb2019))

hb_uat_total_pop <- hb_uat %>% 
  group_by(year, hb2019) %>% 
  summarise(uat_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(hb_source_total_pop, by=c("year","hb2019")) %>% 
  mutate(check = case_when(uat_pop == source_pop ~"total match",
                           TRUE ~ "mismatch - check data")) %>% 
  filter(check== "mismatch- check data") %>% 
  print(n = Inf)

hb_uat_sex_pop <- hb_uat %>% 
  group_by(hb2019, year, gender) %>% 
  summarise(uat_sex_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(hb_source_sex_pop, by=c("gender", "year","hb2019")) %>% 
  mutate(check =case_when (uat_sex_pop== source_sex_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check== "mismatch - check data") %>% 
  print(n = Inf)

hb_uat_age_pop <- hb_uat %>% 
  group_by(hb2019, year, age) %>% 
  summarise(uat_age_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(hb_source_age_pop, by=c("age", "year","hb2019")) %>% 
  mutate(check =case_when (uat_age_pop== source_age_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check == "mismatch - check data") %>% 
  print(n = Inf)

rm(hb_uat_sex_pop, hb_uat_age_pop, hb_uat )
rm(hb_source_total_pop,  hb_source_sex_pop, hb_source_age_pop, hb_source_data, hb_uat_total_pop)
#### end HB checks ####


### HSCP projections Checks  ----

#source data - data that NSS data are checked against

hscp_filepath <- glue("{source_filepath}/HSCP2019_pop_proj_{start}_{end}.rds")

hscp_source_data <- readRDS(hscp_filepath) %>% 
  mutate(year= as.numeric(year), 
         gender =case_when(sex=="1"~1, sex=="2"~2, TRUE~0) )

hscp_source_total_pop= hscp_source_data %>% 
  group_by(hscp2019,year) %>% 
  summarise(source_pop= sum(pop)) 

hscp_source_sex_pop= hscp_source_data  %>% 
  group_by(hscp2019, year, gender) %>% 
  summarise(source_sex_pop= sum(pop))

hscp_source_age_pop= hscp_source_data  %>% 
  group_by(hscp2019, year, age) %>% 
  summarise(source_age_pop= sum(pop))

# uat hscp data

hscp_uat <- uat_test_clean  %>% 
  filter(!is.na(hscp2019))

hscp_uat_total_pop <- hscp_uat %>% 
  group_by(year, hscp2019) %>% 
  summarise(uat_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(hscp_source_total_pop, by=c("year","hscp2019")) %>% 
  mutate(check = case_when(uat_pop == source_pop ~"total match",
                           TRUE ~ "mismatch - check data")) %>% 
  filter(check== "mismatch- check data") %>% 
  print(n = Inf)

hscp_uat_sex_pop <- hscp_uat %>% 
  group_by(hscp2019, year, gender) %>% 
  summarise(uat_sex_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(hscp_source_sex_pop, by=c("gender", "year","hscp2019")) %>% 
  mutate(check =case_when (uat_sex_pop== source_sex_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check== "mismatch - check data") %>% 
  print(n = Inf)

hscp_uat_age_pop <- hscp_uat %>% 
  group_by(hscp2019, year, age) %>% 
  summarise(uat_age_pop= sum(population)) %>% 
  ungroup() %>% 
  left_join(hscp_source_age_pop, by=c("age", "year","hscp2019")) %>% 
  mutate(check =case_when (uat_age_pop== source_age_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check == "mismatch - check data") %>% 
  print(n = Inf)

rm(hscp_uat_sex_pop, hscp_uat_age_pop, hscp_uat )
rm(hscp_source_total_pop,  hscp_source_sex_pop, hscp_source_age_pop, hscp_source_data, hscp_uat_total_pop)
### Scotland projections source data ----

