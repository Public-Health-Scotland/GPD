##########################################################
# Create Council Area Population Estimates Files
# Originally written by Tina Fu
# Updated by Calum Purdie, Iain MacKinnon, Gerald Leung,Alan Coventry
# Original date - 01/03/2018
# Data release - Mid-year Population Estimates
# Latest update author - Iain MacKinnon
# Latest update date - Oct 2025
# Latest update description - NRS source data format. Geography checks at end#
# this script uses NRS time series tables rather than binding single year to 
# existing lookup
# Type of script - revision
# Written/run on Posit Workbench
# Code for creating  area population estimates files based on mid-year 
# estimates released by NRS
##########################################################

###  - Housekeeping  ----

rm(list = ls())

#### libaries 
if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}

pacman::p_load(lubridate, openxlsx,tidyr,readxl, readr, stringr, magrittr,
               glue, janitor, phsmethods,  phsstyles,tidylog, data.table, 
               dplyr, arrow)

#### Set filepaths
# Getting main script location for working directory
path_main_script_location = dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(path_main_script_location)

filepath <- glue("/data/geography",
                 "/Population/Population Estimates") 
data_filepath <- glue("{filepath}/Source Data")
output_filepath <- glue("{filepath}/Lookup Files/R Files")

#### Set estimates year 
start <- "1981" # start is 1981. 
prev <- "2023"  # Prev is input file (previous year)
new <- "2024"   # New is new year (output file).  

#### Geo code Lookups
# used for code joins
gpd_base_path<-"/conf/linkage/output/lookups/Unicode/"
geog_lookup_path <-  glue(gpd_base_path, "Geography/HSCP Locality/")
geog_lookup <- readRDS(glue(geog_lookup_path,"HSCP Localities_DZ11_Lookup_20240513.rds"))

ca_lookup <- geog_lookup %>% 
  select(ca2019, ca2019name, ca2018, ca2011) %>% 
  distinct()

hb_lookup <- geog_lookup %>% 
  select(hb2019, hb2019name, hb2018, hb2014) %>% 
  distinct()

hscp_lookup <- geog_lookup %>% 
  select(ca2019, hscp2019, hscp2019name, hscp2018, 
         hscp2016) %>% 
  distinct()

rm(geog_lookup)


#### Source pop estimate functions
# create age groups and geography checks
source("pop_estimate_functions.R")

### Create CA pop estimates ####

## Import dataset

ca_source_data <- read_excel(glue("{data_filepath}/mid-year-population-estimates-time-series-data-2024.xlsx"),  #update import file name accordingly
                    sheet = "Table 1", range = "A6:CR4362")   # change table and cell refs.

####  Create Single year time series
CA2019_pop_est <- ca_source_data %>% 
  clean_names() %>% 
  filter(sex != "Persons",   area_code != "S92000003") %>% # select M and F rows only, year of interest, remove scotland totals
  select(-all_ages)  %>%       
  rename(x90= x90_and_over) %>%
  pivot_longer(x0:x90, names_to = "age",values_to = "pop")  %>% 
  mutate(sex_name =  case_when (sex == "Females" ~"F",# Create a sex_name column
                              sex =="Males" ~ "M",  
                              TRUE~ "check"),
       sex = as.integer(case_when(sex_name == "M"~"1",
                       sex_name == "F" ~ "2",
                       TRUE~"check")),
       age = as.integer(substr(age, 2, nchar(age))), # remove x from clean name ages 
       pop = as.integer(pop))  %>%  
  rename(ca2019name = area_name) %>% 
  left_join(ca_lookup, by = "ca2019name") %>% 
  arrange(year, ca2019, age, sex) %>% 
  select(year, ca2019, ca2019name, ca2018, ca2011, age, sex, sex_name, pop)

#  Check single age file # refer SOP 
ca_checks(input = CA2019_pop_est)


#### Create 5 CA Year age  dataframe from single year time series -
# Create a file for 5 year age groups and sex
# Assign a 5 year age group to each age
# from 0 years it is an assumption of the team that this allows a birth count to be used.

CA2019_pop_est_5y <- age_group_fun(CA2019_pop_est) %>% 
  group_by(year, ca2019, ca2019name, ca2018, ca2011, age_group, age_group_name, 
           sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  select(year, ca2019, ca2019name, ca2018, ca2011, age_group, age_group_name, 
         sex, sex_name, pop)

# Check 5 year age group file 
ca_checks(input = CA2019_pop_est_5y)

#### Export #
# Single Year #
saveRDS(CA2019_pop_est, 
        glue("{output_filepath}/CA2019_pop_est_{start}_{new}.rds"))

fwrite(CA2019_pop_est, 
       glue("{output_filepath}/CA2019_pop_est_{start}_{new}.csv",
            na=""))

write_parquet(CA2019_pop_est,
              sink = glue("{output_filepath}",
                          "/CA2019_pop_est_{start}_{new}.parquet"),
              compression = "zstd")

# 5 Year age Group
saveRDS(CA2019_pop_est_5y, 
        glue("{output_filepath}/", 
             "CA2019_pop_est_5year_agegroups_{start}_{new}.rds"))

fwrite(CA2019_pop_est_5y, 
       glue("{output_filepath}/", 
            "CA2019_pop_est_5year_agegroups_{start}_{new}.csv",
            na=""))

write_parquet(CA2019_pop_est_5y,
              sink = glue("{output_filepath}",
                          "/CA2019_pop_est_5year_agegroups_{start}_{new}.parquet"),
              compression = "zstd")

### retain CA dataframes until HSCP have been created #

#### Create HSCP estimates ####
# Match the HSCP columns onto the CA estimates file
# Select the relevant columns
# As there are 31 HSCPs and 32 CAs (Stirling and Clackmannanshire join for HSCP) 
# this will produce population separately for both Stirling and Clackmannanshire 
# even though they have the same HSCP code
# Need to recalculate the sums, so group by all variables bar Pop and calculate 
# new pop totals
# Arrange by year, hscp2019, age_group and sex to get the required format

HSCP2019_pop_est <- CA2019_pop_est %>%
  left_join( hscp_lookup, by="ca2019") %>% 
  select(year, hscp2019, hscp2019name, hscp2018, hscp2016, age, sex, sex_name, 
         pop) %>%
  group_by(year, hscp2019, hscp2019name, hscp2018, hscp2016, age, sex, 
           sex_name) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  arrange(year, hscp2019, age, sex)

# Check single age file - See SOP
hscp_checks(input = HSCP2019_pop_est)

# Create the HSCP 5 year age group population estimate file -
HSCP2019_pop_est_5y <- CA2019_pop_est_5y %>%
  left_join( hscp_lookup, by="ca2019") %>% 
  select(year, hscp2019, hscp2019name, hscp2018, hscp2016, age_group, 
         age_group_name, sex, sex_name, pop) %>%
  group_by(year, hscp2019, hscp2019name, hscp2018, hscp2016, age_group, 
           age_group_name, sex, sex_name) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  arrange(year, hscp2019, age_group, sex)

# Check 5 year age group file
hscp_checks(input = HSCP2019_pop_est_5y)

# Export #
saveRDS(HSCP2019_pop_est, 
        glue("{output_filepath}/HSCP2019_pop_est_{start}_{new}.rds"))

fwrite(HSCP2019_pop_est, 
       glue("{output_filepath}/HSCP2019_pop_est_{start}_{new}.csv",
            na=""))

write_parquet(HSCP2019_pop_est,
              sink = glue("{output_filepath}",
                          "/HSCP2019_pop_est_{start}_{new}.parquet"),
              compression = "zstd")

saveRDS(HSCP2019_pop_est_5y, 
        glue("{output_filepath}/", 
             "HSCP2019_pop_est_5year_agegroups_{start}_{new}.rds"))

fwrite(HSCP2019_pop_est_5y, 
       glue("{output_filepath}/HSCP2019_pop_est_5year_agegroups_{start}_{new}.csv",
            na=""))

write_parquet(HSCP2019_pop_est_5y,
              sink = glue("{output_filepath}",
                          "/HSCP2019_pop_est_5year_agegroups_{start}_{new}.parquet"),
              compression = "zstd")



rm(ca_lookup, CA2019_pop_est, CA2019_pop_est_5y, ca_clean_data, ca_source_data)
rm(hscp_lookup, HSCP2019_pop_est, HSCP2019_pop_est_5y)


##### Health Board files  ##

hb_source_data <- read_excel(glue("{data_filepath}/mid-year-population-estimates-time-series-data-2024.xlsx"),  #update import file name accordingly
                             sheet = "Table 2", range = "A6:CR1986")   # change table and cell refs.
# clean and pivot 
HB2019_pop_est <- hb_source_data %>% 
  clean_names() %>% 
  filter(sex != "Persons",   area_code != "S92000003") %>% # select M and F rows only, year of interest, remove scotland totals
  select(-all_ages)  %>%       
  rename(x90= x90_and_over) %>%
  pivot_longer(x0:x90, names_to = "age",values_to = "pop")  %>% 
  mutate(hb2019name= paste("NHS",area_name),
         sex_name =  case_when (sex == "Females" ~"F",
                                sex =="Males" ~ "M",  TRUE~ "check"),
         sex = as.integer(case_when(sex_name == "M"~"1",
                                    sex_name == "F" ~ "2", TRUE~"check")),
         age = as.integer(substr(age, 2, nchar(age))), # remove x from clean name step 
         pop = as.integer(pop) ) %>%  
  left_join(hb_lookup, by = "hb2019name") %>% # add codes
  select(year, hb2019, hb2019name, hb2018, hb2014, age, sex, sex_name, pop) %>% 
  arrange(year, hb2019, age, sex)

# Check single age file - See SOP
hb_checks(input = HB2019_pop_est)

# Check 5 year age group file -
HB2019_pop_est_5y <- age_group_fun(HB2019_pop_est) %>% 
  group_by(year, hb2019, hb2019name, hb2018, hb2014, age_group, age_group_name, 
           sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  arrange(year, hb2019, age_group, sex) %>% 
  select(year, hb2019, hb2019name, hb2018, hb2014, age_group, age_group_name, 
         sex, sex_name, pop)

hb_checks(input = HB2019_pop_est_5y)
#
# Export ##
# Single Year ## 
saveRDS(HB2019_pop_est, 
        glue("{output_filepath}/HB2019_pop_est_{start}_{new}.rds"))

# write_csv FILE
fwrite(HB2019_pop_est, 
       glue("{output_filepath}/HB2019_pop_est_{start}_{new}.csv",
            na=""))
saveRDS(HB2019_pop_est_5y, 
        glue("{output_filepath}/", 
             "HB2019_pop_est_5year_agegroups_{start}_{new}.rds"))

fwrite(HB2019_pop_est_5y, 
       glue("{output_filepath}/", 
            "HB2019_pop_est_5year_agegroups_{start}_{new}.csv",
            na=""))

write_parquet(HB2019_pop_est,
              sink = glue("{output_filepath}",
                          "/HB2019_pop_est_{start}_{new}.parquet"),
              compression = "zstd")

write_parquet(HB2019_pop_est_5y,
              sink = glue("{output_filepath}",
                          "/HB2019_pop_est_5year_agegroups_{start}_{new}.parquet"),
              compression = "zstd")

## End of script