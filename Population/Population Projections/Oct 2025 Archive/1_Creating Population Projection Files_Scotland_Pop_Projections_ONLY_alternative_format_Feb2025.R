# Loads NRS Scottish Population Projections, reformats into output files
# Assumes fomatting of NRS data is all in one sheet, years are under one column, ageseach have their own column
# Produced on Posit Workbench
# R Script by Alan Coventry, 30/01/2025
# Last edit 30/01/2025
# Version 1.0


#### 1) LOAD  libraries ####
library(glue)
library(readxl)
library(dplyr)
library(tidyverse)
library(janitor)
library(magrittr)
library(data.table)

#### 2) CHOOSE YEARS & LOAD Filepaths ####
start <- "2022"   #update
end   <- "2047"   #update

base_filepath <- ("//data/geography/Population/Population Projections")
lookup_filepath <- glue("{base_filepath}/Lookup Files/R Files")
data_filepath <- glue("{base_filepath}/Source Data")

# set scottish population projection data filepath and name
scot_pop_data_path <- glue("{data_filepath}/scot_pop_proj_{start}_{end}.xlsx")


#### 3) LOAD DATA ####
# Update sheet and range if required - must load in all columns and data
scot_pop_data <- read_excel(scot_pop_data_path, sheet = "Table 1", range = "A5:DF83")


#### 4) CLEAN DATA ####
scot_pop_data <- scot_pop_data[scot_pop_data$Sex != "Persons", ]      #remove persons, keep only male & female
scot_pop_data <- scot_pop_data %>%                                    #remove total population for all ages
  select(-`All ages`)

# Rename 'Sex' to 'sex_name' 
scot_pop_data <- scot_pop_data %>%
  rename(sex_name = Sex)  

# Convert "Females" to "female" and "Males" to "male"
scot_pop_data$sex_name <- recode(scot_pop_data$sex_name, 
                                 "Females" = "Female", 
                                 "Males" = "Male")

#### 5) TRANSPOSE DATA & FURTHER DATA CLEANING ####

#pivot longer

pop_proj_long <- scot_pop_data %>%
  pivot_longer(
    cols = 3:109,      # Selecting columns 3 to 109 - These should be columnns which become a row entres
    names_to = "age",  # Name of the new column that will hold the original column names
    values_to = "pop"  # Name of the new column that will hold the values
  )

# rename year column
names(pop_proj_long)[names(pop_proj_long) == "Year ending June"] <- "year"

#add in sex column which is numeric values 1 and 2 for male female
pop_proj_long <- pop_proj_long %>%
  mutate(sex = ifelse(sex_name == "Female", 2, 
                      ifelse(sex_name == "Male", 1, NA))) %>%
  mutate(sex = as.numeric(sex))
pop_proj_long  <- pop_proj_long  %>%
  select(year, age, sex, sex_name, pop)


# Convert age column to numeric, anything over 90 gets classed as 90
df_long <- pop_proj_long %>%
  mutate(age = ifelse(age %in% c("105 to 109", "110 and over") | as.numeric(age) >= 90, 90, as.numeric(age)))


#### 6) CREATE SINGLE YEAR PROJECTIONS DATA FRAME ####
# Aggregate single_year_projections  population counts by year, age, sex, and sex_name
single_year_projections <- df_long %>%
  group_by(year, age, sex, sex_name) %>%
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")  # Sum population and remove grouping


#### 7) CREATE FIVE YEAR PROJECTIONS DATA FRAME ####

# adds in'age group' and 'age_group_name' columns, which groups ages
five_year_projections <- single_year_projections
five_year_projections <- five_year_projections %<>%
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

# Aggregates five_year_projections population counts by year, age_group, age_group_name, sex, and sex_name
five_year_projections <- five_year_projections %<>%
  group_by(year, age_group,age_group_name, sex, sex_name) %>%
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")      # Sum population and remove grouping

#### 8) WRITE RDS AND CSVS  ###

saveRDS(single_year_projections, 
        glue("{lookup_filepath}/scot_pop_proj_{start}_{end}.rds"))

fwrite(single_year_projections,
       glue("{lookup_filepath}/scot_pop_proj_{start}_{end}.csv"))


saveRDS(five_year_projections, 
        glue("{lookup_filepath}/scot_pop_proj_5year_agegroups_{start}_{end}.rds"))


fwrite(five_year_projections,
       glue("{lookup_filepath}/scot_pop_proj_5year_agegroups_{start}_{end}.csv"))
