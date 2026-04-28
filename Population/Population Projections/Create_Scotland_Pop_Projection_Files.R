# Loads NRS Scottish Population Projections, reformats into output files
# Assumes fomatting of NRS data is all in one sheet, years are under one column, 
# ages each have their own column
# Produced on Posit Workbench
# R Script by Iain MacKinnon 
# Last edit Oct /2025

####  Housekeeping & set-up ----

rm(list = ls())

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}

pacman::p_load(phsmethods,
               magrittr,tidyr,readxl, readr, janitor,tidylog, glue, stringr, dplyr,data.table, arrow)

# set projection start & end dates 
start <- "2022" #update
end <- "2047" #update

# Getting main script location for working directory
path_main_script_location = dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(path_main_script_location)


base_filepath <- ("//data/geography/Population/Population Projections")
lookup_filepath <- glue("{base_filepath}/Lookup Files/R Files")
data_filepath <- glue("{base_filepath}/Source Data")

# set scottish population projection data filepath and name. Update as required.
scot_pop_data_path <- glue("{data_filepath}/scot_pop_proj_{start}_{end}.xlsx")

#   Set functions #
source ("pop_proj_functions.R")

#### LOAD DATA ####
# Update sheet and range if required 
scot_pop_data <- read_excel(scot_pop_data_path, sheet = "Table 1", range = "A5:DF83")

####  Single age projections ####
# pivot # Rename 
# Set age as an integer and create sex_name column
# Select relevant columns and arrange

single_year_projections<- scot_pop_data %>% 
  clean_names() %>% 
  filter( sex !="Persons") %>% 
  rename(year = year_ending_june) %>% 
  mutate(across(everything(), as.character)) %>% # makes pivoting easier
  pivot_longer(cols = 3:110,  names_to = "age",  values_to = "pop" ) %>% 
  filter(age!= "all_ages") %>% 
  mutate(sex_name =  case_when(sex == "Females" ~"Female", 
                               sex =="Males" ~ "Male",
                                TRUE~ "check"),
         sex = case_when(sex_name == "Male"~"1", 
                         sex_name == "Female" ~ "2",  TRUE~"check"),
         age= case_when(age == "x105_to_109"~ "x105", 
                        age == "x110_and_over"~ "x110",  
                 TRUE~ age),
         age = as.numeric(substr(age, 2, nchar(age))),  # remove x from clean name step 
         age = case_when(age >= 90 ~ 90, TRUE~ age),
         pop = as.numeric(pop)  ) %>% 
  group_by(year, age, sex, sex_name) %>%
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")  # Sum population and remove grouping

# checks see SOP
checks(input = single_year_projections, geography = "scot", age_column = age)

####  5 Year age projections ####

five_year_projections <- age_group_fun(single_year_projections) %>% # adds in'age group' and 'age_group_name' columns, which groups ages
  group_by(year, age_group,age_group_name, sex, sex_name) %>% # Aggregates five_year_projections population counts by year, age_group, age_group_name, sex, and sex_name
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")      # Sum population and remove grouping

# checks see SOP
checks(input = five_year_projections, geography = "scot", age_column = age_group_name)

#### Export  ####

saveRDS(single_year_projections, 
        glue("{lookup_filepath}/scot_pop_proj_{start}_{end}.rds"))

fwrite(single_year_projections,
       glue("{lookup_filepath}/scot_pop_proj_{start}_{end}.csv"))

write_parquet(single_year_projections,
              sink = glue("{lookup_filepath}",
                          "/scot_pop_proj_{start}_{end}.parquet"),
              compression = "zstd")


saveRDS(five_year_projections, 
        glue("{lookup_filepath}/scot_pop_proj_5year_agegroups_{start}_{end}.rds"))

fwrite(five_year_projections,
       glue("{lookup_filepath}/scot_pop_proj_5year_agegroups_{start}_{end}.csv"))


write_parquet(five_year_projections,
              sink = glue("{lookup_filepath}",
                          "/scot_pop_proj_5year_agegroups_{start}_{end}.parquet"),
              compression = "zstd")

