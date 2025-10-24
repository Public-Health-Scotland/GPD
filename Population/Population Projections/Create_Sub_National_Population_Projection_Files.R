##########################################################
# Creating Population Projection Files
# Calum Purdie
# Original date 27/03/2019
# Latest update author - Iain MacKinnon
# Latest update date - October 2025
# Latest update description - 
# Create Sub national version.
# Source the functions from separate script.
# Move the checks to before writing out files
# Type of script - Creation
# Version of R that the script was most recently run on - 4.4.2
# Code for creating  look-up files for NRS population projections for Health Board , HSCP and Council Area
##########################################################

####  Housekeeping & set-up ----
rm(list = ls())

# 1.1 Read in packages from library #

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}

pacman::p_load(haven, #still needed?
               phsmethods,
               magrittr,tidyr,readxl, readr, janitor,tidylog, glue, stringr, dplyr,data.table)

# set projection start & end dates 
start <- "2022" #update
end <- "2047" #update

###  Set filepaths #

# Getting main script location for working directory
path_main_script_location = dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(path_main_script_location)

base_filepath <- ("/data/geography/Population/Population Projections/")

lookup_filepath <- glue("{base_filepath}/Lookup Files/R Files")
data_filepath <- glue({base_filepath},"Source Data/")

# for geog codes
gpd_base_path<-"/conf/linkage/output/lookups/Unicode/"
geog_lookup_path <-  glue(gpd_base_path, "Geography/HSCP Locality/")

# set files to use #

source_data_table <- glue(data_filepath, "2022-based-snpp-data-tables.xlsx")

#  Create Geo code Look ups
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

#   Set functions #
source ("/data/geography/GitHub/GPD-Population/Population Projections/pop_proj_functions.R")

#### Create Council Area population projections by single year of age ----

ca_source_data <- read_excel(source_data_table, sheet = "Table 2",  range = "A6:CR2580")

# Single age bands
# pivot  
# Rename code to ca2019 and join on ca columns from geo_lookup
# Set age as an integer and create sex_name column
# Select relevant columns and arrange

ca_pop_proj <- ca_source_data %>% 
  clean_names() %>% 
  filter(area_name !="Scotland" & sex !="Persons") %>% 
  rename(year = year_to_30_june, ca2019= area_code) %>% 
  mutate(across(everything(), as.character)) %>% # makes pivoting easier
  pivot_longer(cols = 5:96,  names_to = "age",  values_to = "pop" ) %>% 
  filter(age!= "all_ages") %>% 
  mutate(sex_name =  case_when (sex == "Females" ~"Female", sex =="Males" ~ "Male",
                                TRUE~ "check"),
         sex = case_when(sex_name == "Male"~"1", sex_name == "Female" ~ "2", 
                         TRUE~"check"),
         age= case_when(age== "x90_and_over"~ "x90",  TRUE~ age), 
         age = as.integer(substr(age, 2, nchar(age))), # remove x from clean name step 
         pop = as.integer(pop)) %>%  # as integer to allow 5 year aggregations below
  left_join(ca_lookup, by = "ca2019") %>% 
  select(year, ca2019, ca2019name, ca2018, ca2011, age, sex, sex_name, pop) %>% 
  arrange(year, ca2019, age, sex)

## checks - refer SOP
checks(input = ca_pop_proj, geography = "ca", age_column = age)

# write
saveRDS(ca_pop_proj,
        glue("{lookup_filepath}/CA2019_pop_proj_{start}_{end}.rds"))

fwrite(ca_pop_proj, 
       glue("{lookup_filepath}/CA2019_pop_proj_{start}_{end}.csv",
            na=""))

write_parquet(ca_pop_proj,
              sink = glue("{lookup_filepath}",
                          "/CA2019_pop_proj_{start}_{end}.parquet"),
              compression = "zstd")

## Create age_group column for 5 year age groups

# Group data and summarise by pop to get the population for each age group
ca_pop_proj_5y <- age_group_fun(ca_pop_proj) %>% 
  group_by(year, ca2019, ca2019name, ca2018, ca2011, age_group, age_group_name, 
           sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

# Checks- refer SOP
checks(input = ca_pop_proj_5y, geography = "ca", age_column = age_group)# option to update to age_group if wish to check range names

# write
saveRDS(ca_pop_proj_5y, 
        glue("{lookup_filepath}/CA2019_pop_proj_5year_agegroups_{start}_{end}.rds"))

fwrite(ca_pop_proj_5y, 
        glue("{lookup_filepath}/CA2019_pop_proj_5year_agegroups_{start}_{end}.csv"))

write_parquet(ca_pop_proj_5y,
              sink = glue("{lookup_filepath}",
                          "/CA2019_pop_proj_5year_agegroups_{start}_{end}.parquet"),
              compression = "zstd")

#### Create Health Board population projections files ----
# projections by single year of age 

hb_source_data <- read_excel(source_data_table, sheet = "Table 3",  range = "A6:CR1176")

# single age groups
hb_pop_proj <- hb_source_data %>% 
  clean_names() %>% 
  filter(area_name !="Scotland" & sex !="Persons") %>% # 
  rename(year = year_to_30_june, hb2019= area_code) %>%  
  mutate(across(everything(), as.character)) %>% 
  pivot_longer( cols = 5:96,  names_to = "age",  values_to = "pop" ) %>% 
  filter(age!= "all_ages") %>% 
  mutate(sex_name =  case_when (sex == "Females" ~"Female",
                                sex =="Males" ~ "Male",  TRUE~ "check"),
         sex = case_when(sex_name == "Male"~"1",
                         sex_name == "Female" ~ "2", TRUE~"check"),
         age= case_when(age== "x90_and_over"~ "x90", TRUE~ age),
         age = as.integer(substr(age, 2, nchar(age))),
         pop = as.integer(pop)) %>% 
  left_join(hb_lookup, by = "hb2019") %>% # add codes
  select(year, hb2019, hb2019name, hb2018, hb2014, age, sex, sex_name, pop) %>% 
  arrange(year, hb2019, age, sex)

##  HB Checks- refer SOP

checks(input = hb_pop_proj, geography = "hb", age_column = age)



## HB  5 year age groups 
# Create age_group column for 5 year age groups
# Group and summarise by pop to get the population for each age group

hb_pop_proj_5y <- age_group_fun(hb_pop_proj) %>% 
  group_by(year, hb2019, hb2019name, hb2018, hb2014, age_group, age_group_name, 
           sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

#checks- refer SOP
checks(input = hb_pop_proj_5y, geography = "hb", age_column = age_group)

# write
saveRDS(hb_pop_proj, 
        glue("{lookup_filepath}/HB2019_pop_proj_{start}_{end}.rds"))

saveRDS(hb_pop_proj_5y, 
        glue("{lookup_filepath}/HB2019_pop_proj_5year_agegroups_{start}_{end}.rds"))

fwrite(hb_pop_proj_5y, 
       glue("{lookup_filepath}/HB2019_pop_proj_5year_agegroups_{start}_{end}.csv"))

fwrite(hb_pop_proj, 
       glue("{lookup_filepath}/HB2019_pop_proj_{start}_{end}.csv"))

write_parquet(hb_pop_proj,
              sink = glue("{lookup_filepath}",
                          "/HB2019_pop_proj_{start}_{end}.parquet"),
              compression = "zstd")

write_parquet(hb_pop_proj_5y,
              sink = glue("{lookup_filepath}",
                          "/HB2019_pop_proj_5year_agegroups_{start}_{end}.parquet"),
              compression = "zstd")


#### Create HSCP population projection files ----

#  Create HSCP population projections by single year of age
# Take ca_pop_proj and join hscp columns on from geo_lookup
# Group and summarise by pop to get the population for each age group

hscp_pop_proj <- ca_pop_proj %>% 
  left_join(hscp_lookup, by="ca2019") %>% 
  group_by(year, hscp2019, hscp2019name, hscp2018, hscp2016, age, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

# Check HSCP files - refer SOP

checks(input = hscp_pop_proj, geography = "hscp", age_column = age)
# Create file for 5 year age groups 

# Take ca_pop_proj_5y and join hscp columns on from geo_lookup
# Group and summarise by pop to get the population for each age group

hscp_pop_proj_5y <- ca_pop_proj_5y %>% 
  left_join(hscp_lookup, by="ca2019") %>% 
  group_by(year, hscp2019, hscp2019name, hscp2018, hscp2016, age_group, 
           age_group_name, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

# Checks - check SOP
checks(input = hscp_pop_proj_5y, geography = "hscp", age_column = "age_group")

# write 
saveRDS(hscp_pop_proj, 
        glue("{lookup_filepath}/HSCP2019_pop_proj_{start}_{end}.rds"))

saveRDS(hscp_pop_proj_5y, 
        glue("{lookup_filepath}/HSCP2019_pop_proj_5year_agegroups_{start}_{end}.rds"))

fwrite(hscp_pop_proj, 
        glue("{lookup_filepath}/HSCP2019_pop_proj_{start}_{end}.csv"))

fwrite(hscp_pop_proj_5y, 
        glue("{lookup_filepath}/HSCP2019_pop_proj_5year_agegroups_{start}_{end}.csv"))

write_parquet(hscp_pop_proj,
              sink = glue("{lookup_filepath}",
                          "/HSCP2019_pop_proj_{start}_{end}.parquet"),
              compression = "zstd")

write_parquet(hscp_pop_proj_5y,
              sink = glue("{lookup_filepath}",
                          "/HSCP2019_pop_proj_5year_agegroups_{start}_{end}.parquet"),
              compression = "zstd")

#### end script ####
