##########################################################
# Creating Population Projection Files
# Calum Purdie
# Original date 27/03/2019
# Latest update author - Calum Purdie
# Latest update date - 30/03/2020
# Latest update description - formatting code
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for creating ISD files for NRS population projections for Scotland, 
# Health Board, HSCP and Council Area
# Approximate run time - 30 seconds
##########################################################

### 1 - Housekeeping ----

# Read in packages from library

library(magrittr)
library(tidyr)
library(dplyr)
library(readxl)
library(readr)
library(janitor)
library(tidylog)
library(glue)
library(stringr)

# Set filepaths

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                      "2_Population/Population Projections")
lookup_filepath <- glue("{base_filepath}/Lookup Files/R Files")
data_filepath <- glue("{base_filepath}/Source Data")

# Set files to use

scot_pop_data <- glue("{data_filepath}/scot_pop_proj_2018_2043.xlsx")
ca_pop_data <- glue("{data_filepath}/ca_pop_proj_2018_2043.xlsx")
hb_pop_data <- glue("{data_filepath}/hb_pop_proj_2018_2043.xlsx")
geo_lookup_file <- glue("//Isdsf00d03/cl-out/lookups/Unicode/Geography/",
                        "HSCP Locality/HSCP Localities_DZ11_Lookup_20191216.rds")

start <- "2018"
end <- "2043"

# Read in geography names lookup
# This is used for Council Area, Health Board and HSCP Files

geo_lookup <- readRDS(geo_lookup_file) %>%
  select(ca2019, ca2019name, ca2018, ca2011, hscp2019, hscp2019name, hscp2018, 
         hscp2016, hb2019, hb2019name, hb2018, hb2014) %>%
  distinct()



### 2 - Functions ----

# Create Scotland population function to read in data

scot_pop <- function(filepath, cells, gender, start_year, end_year){
  
  scot_proj <- read_xlsx(path = filepath, sheet = "Principal", 
                                  range = cells)
  
  # Rename variables
  names(scot_proj) <- c("Age", start_year:end_year)
  
  scot_proj %>% 
    gather("year", "Pop", start_year:end_year) %>% 
    mutate(sex = gender) %>% 
    filter(Age != "All ages") %>% 
    mutate(Age = as.numeric(Age))
}

# Create lower geography population function to read in the data

lower_geo_pop <- function(filepath, cells, gender, start_year, end_year){
  
  # Read in the sheet names within the excel file
  sheet_names <- excel_sheets(path = filepath)
  
  # Remove unnecessary names - only keep years
  sheet_names <- sheet_names[5:30]
  
  # Read in the data for each year using sheet_names
  list_all <- lapply(sheet_names, 
                     function(i) read_xlsx(path = filepath, sheet = i, 
                                           range = cells))
  
  # Add a column for sex to each year's data and create a year column
  lower_geo <- mapply(cbind, list_all, "sex" = gender, 
                      "Year" = seq.int(start_year, end_year), SIMPLIFY = F)
  
  # Turn the list into a dataframe
  lower_geo_proj <- do.call(rbind.data.frame, lower_geo)
  
  lower_geo_proj %<>% 
    rename("90"= "90+") %>% 
    gather("Age", "Pop", 'All ages':'90') %>% 
    filter(Area != "SCOTLAND" & Age != "All ages")
}


# Age Group Function

age_group_fun <- function(data){
  
  data %<>%
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
  
}

# Check function

checks <- function(input, geography, age_column){
  
  # Check that all years of the population projections are there 
  # Check that there are no missing values
  # Check all years have the same % of records
  
  input %>% group_by(year) %>% count() %>% print(n = Inf)
  
  # Check that all ages 0 to 90+ are there
  # Check there are no missing values
  # Check all ages have the same % of records
  
  input %>% group_by({{age_column}}) %>% count() %>% print(n = Inf)
  
  # Check that both males and females are there
  # Check there are no missing values
  # Check both sexes have the same % of records (50/50)
  
  input %>% group_by(sex) %>% count() %>% print(n = Inf)
  
  # Check Scotland totals against NRS source data
  
  input %>%
    group_by(year) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()
  
  # Check that the population values are as expected
  # i.e. no negative values or extremely high values etc
  
  input %>%
    group_by(pop) %>%
    count() %>%
    arrange(n) %>%
    filter(n <= 0 | n >= 250)
  
  if(geography == "scot"){
    
  } else if (geography == "hb"){
    
    # Check that all 14 Health Boards are there
    # Check there are no missing values
    # Check all HBs have the same % of records
    
    input %>% group_by(hb2019) %>% count() %>% print(n = Inf)
    input %>% group_by(hb2018) %>% count() %>% print(n = Inf)
    input %>% group_by(hb2014) %>% count() %>% print(n = Inf)
    
  } else if (geography == "hscp"){
    
    # Check that all 31 HSCPs are there
    # Check there are no missing values
    # Check all HSCPs have the same % of records
    
    input %>% group_by(hscp2019) %>% count() %>% print(n = Inf)
    input %>% group_by(hscp2018) %>% count() %>% print(n = Inf)
    input %>% group_by(hscp2016) %>% count() %>% print(n = Inf)
    
  } else if (geography == "ca"){
    
    # Check that all 32 Council Areas are there
    # Check there are no missing values
    # Check all CAs have the same % of records
    
    input %>% group_by(ca2019) %>% count() %>% print(n = Inf)
    input %>% group_by(ca2018) %>% count() %>% print(n = Inf)
    input %>% group_by(ca2011) %>% count() %>% print(n = Inf)
    
  } else {
    
    print("Define correct projections data")
    
  }
  
}



### 3 - Create Scotland population projection files ----

### 3.1 - Create male and female Scotland population projections ----

# Create male population projections

scot_proj_m <- scot_pop(filepath = scot_pop_data, 
                        cells = "A137:AA264", gender = 1, 
                        start_year = start, 
                        end_year = end) %>% 
  clean_names()

# Create female population projections

scot_proj_f <- scot_pop(filepath = scot_pop_data, 
                        cells = "A269:AA396", gender = 2, 
                        start_year = start, 
                        end_year = end) %>% 
  clean_names()



### 3.2 - Create Scotland population projections by single year of age ----

# Add the male and female files together
# Sort by year, Age and sex
# Set all variables as integers
# Create sex_name column
# Recode all Age values greater than 90 to 90 for consistency with previous 
# projections
# group_by all columns except Pop and calculate new Pop values for Age = 90
# Ungroup and set age_group as an integer

scot_pop_proj <- bind_rows(scot_proj_m, scot_proj_f) %>% 
  arrange(year, age, sex) %>% 
  mutate(year = as.integer(year), 
         pop = as.integer(pop), 
         sex = as.integer(sex), 
         sex_name = recode(sex, "1" = "Male", "2" = "Female"), 
         age = if_else(age > 90, 90, age)) %>%
  group_by(year, age, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(age = as.integer(age))

saveRDS(scot_pop_proj, 
        glue("{lookup_filepath}/scot_pop_proj_{start}_{end}.rds"))


### 3.3 - Create file for 5 year age groups ----

# Create age_group column for 5 year age groups using age_group_fun
# Group by year, age_group and sex - these are the columns you want to keep
# Summarise by pop to get the population for each age group
# Ungroup and set age_group as an integer

scot_pop_proj_5y <- age_group_fun(scot_pop_proj) %>% 
  group_by(year, age_group, age_group_name, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(age_group = as.integer(age_group))

saveRDS(scot_pop_proj_5y, 
        glue("{lookup_filepath}/scot_pop_proj_5year_agegroups_2018_2043.rds"))

rm(scot_proj_f, scot_proj_m)



### 4 - Create Council Area population projections files ----

### 4.1 - Use lower geography population function to read in data ----

# Create the male population projections

ca_proj_m <- lower_geo_pop(ca_pop_data, 
                           cells = "A44:CP77", gender = 1, 
                           start_year = start, 
                           end_year = end) %>% 
  clean_names()

# Create the female population projections

ca_proj_f <- lower_geo_pop(ca_pop_data, 
                           cells = "A83:CP116", gender = 2, 
                           start_year = start, 
                           end_year = end) %>% 
  clean_names()


### 4.2 - Create Council Area population projections by single year of age ----

# Add the male and female files together
# Rename code to ca2019 and join on ca columns from geo_lookup
# Set age as an integer and create sex_name column
# Select relevant columns and arrange

ca_pop_proj <- bind_rows(ca_proj_m, ca_proj_f) %>% 
  rename(ca2019 = code) %>% 
  left_join(select(geo_lookup, ca2019, ca2019name, ca2018, ca2011)) %>% 
  mutate(age = as.integer(age),
         sex_name = recode(sex, "1" = "Male", "2" = "Female")) %>% 
  select(year, ca2019, ca2019name, ca2018, ca2011, age, sex, sex_name, pop) %>% 
  arrange(year, ca2019, age, sex)

saveRDS(ca_pop_proj, glue("{lookup_filepath}/CA2019_pop_proj_{start}_{end}.rds"))



### 4.3 - Create file for 5 year age groups ----

# Create age_group column for 5 year age groups
# Group data and summarise by pop to get the population for each age group

ca_pop_proj_5y <- age_group_fun(ca_pop_proj) %>% 
  group_by(year, ca2019, ca2019name, ca2018, ca2011, age_group, age_group_name, 
           sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

saveRDS(ca_pop_proj_5y, 
        glue("{lookup_filepath}/CA2019_pop_proj_5year_agegroups_{start}_{end}.rds"))

rm(ca_proj_f, ca_proj_m)



### 5 - Create Health Board population projections files ----

### 5.1 - Use lower geography population function to read in data ----

# Create the male population projections

hb_proj_m <- lower_geo_pop(hb_pop_data, cells = "A26:CP41", gender = 1, 
                           start_year = start, end_year = end) %>% 
  clean_names()

# Create the female population projections

hb_proj_f <- lower_geo_pop(hb_pop_data, cells = "A47:CP62", gender = 2, 
                           start_year = start, end_year = end) %>% 
  clean_names()



### 5.2 - Create Council Area population projections by single year of age ----

# Add the male and female files together
# Rename code to hb2019 and join on hb columns from geo_lookup
# Take distinct rows to remove duplicates
# Set age as an integer and create sex_name column
# Select relevant columns and arrange

hb_pop_proj <- bind_rows(hb_proj_m, hb_proj_f) %>% 
  rename(hb2019 = code) %>% 
  left_join(select(geo_lookup, hb2019, hb2019name, hb2018, hb2014)) %>% 
  distinct() %>% 
  mutate(age = as.integer(age), 
         sex_name = recode(sex, "1" = "Male", "2" = "Female")) %>% 
  select(year, hb2019, hb2019name, hb2018, hb2014, age, sex, sex_name, pop) %>% 
  arrange(year, hb2019, age, sex)

saveRDS(hb_pop_proj, 
        glue("{lookup_filepath}/HB2019_pop_proj_{start}_{end}.rds"))



### 5.3 - Create file for 5 year age groups ----

# Create age_group column for 5 year age groups
# Group and summarise by pop to get the population for each age group

hb_pop_proj_5y <- age_group_fun(hb_pop_proj) %>% 
  group_by(year, hb2019, hb2019name, hb2018, hb2014, age_group, age_group_name, 
           sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

saveRDS(hb_pop_proj_5y, 
        glue("{lookup_filepath}/HB2019_pop_proj_5year_agegroups_{start}_{end}.rds"))

rm(hb_proj_f, hb_proj_m)



### 6 - Create HSCP population projection files ----

### 6.1 - Create HSCP population projections by single year of age ----

# Take ca_pop_proj and join hscp columns on from geo_lookup
# Group and summarise by pop to get the population for each age group

hscp_pop_proj <- ca_pop_proj %>% 
  left_join(select(geo_lookup, ca2019, hscp2019, hscp2019name, hscp2018, 
                   hscp2016)) %>% 
  group_by(year, hscp2019, hscp2019name, hscp2018, hscp2016, age, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

saveRDS(hscp_pop_proj, 
        glue("{lookup_filepath}/HSCP2019_pop_proj_{start}_{end}.rds"))



### 6.2 - Create file for 5 year age groups ----

# Take ca_pop_proj_5y and join hscp columns on from geo_lookup
# Group and summarise by pop to get the population for each age group

hscp_pop_proj_5y <- ca_pop_proj_5y %>% 
  left_join(select(geo_lookup, ca2019, hscp2019, hscp2019name, hscp2018, 
                   hscp2016)) %>% 
  group_by(year, hscp2019, hscp2019name, hscp2018, hscp2016, age_group, 
           age_group_name, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

saveRDS(hscp_pop_proj_5y, 
        glue("{lookup_filepath}/HSCP2019_pop_proj_5year_agegroups_{start}_{end}.rds"))



### 7 - Check files ----

### 7.1 - Check Scotland files ----

checks(input = scot_pop_proj, geography = "scot", age_column = "age")
checks(input = scot_pop_proj_5y, geography = "scot", age_column = "age_group")


### 7.2 - Check Health Board files ----

checks(input = hb_pop_proj, geography = "hb", age_column = "age")
checks(input = hb_pop_proj_5y, geography = "hb", age_column = "age_group")


### 7.3 - Check HSCP files ----

checks(input = hscp_pop_proj, geography = "hscp", age_column = "age")
checks(input = hscp_pop_proj_5y, geography = "hscp", age_column = "age_group")


### 7.4 - Check Council Area files ----

checks(input = ca_pop_proj, geography = "ca", age_column = "age")
checks(input = ca_pop_proj_5y, geography = "ca", age_column = "age_group")
