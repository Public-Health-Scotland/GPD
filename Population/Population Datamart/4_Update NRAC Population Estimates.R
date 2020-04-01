##########################################################
# Update NRAC Population Estimates
# Calum Purdie
# Original date 09/03/2020
# Latest update author - Calum Purdie
# Latest update date - 10/03/2020
# Latest update description 
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Preparing NRAC Population files for upload to the populations datamart
# Approximate run time
##########################################################

### 1 Housekeeping ----

library(magrittr)
library(tidyr)
library(dplyr)
library(haven)
library(sjlabelled)
library(readr)
library(tidylog)
library(janitor)
library(glue)
library(here)
library(data.table)
library(xfun)

# Set year to use

year <- "201819"

# Set filepaths

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                      "2_Population/Population Datamart")

lookups_filepath <- glue("{base_filepath}/Lookup Files/NRAC/{year}")

data_filepath <- glue("{base_filepath}/Source Data/NRAC/{year}")

template_filepath <- here("Population/Population Datamart/R Templates")

# Set date for filenames

date <- strftime(Sys.Date(), format = "%Y%m%d")

year <- "2018"

# Functions

check_fun <- function(df, variable){
  
  df %>% 
    group_by(!!as.name(variable) ) %>% 
    summarise(Population = sum(Population))
  
}

summary <- function(df){
  
  min(df$Population) %>% print()
  max(df$Population) %>% print()
  any(is.na(df$Population)) %>% print()
  sum(df$Population) %>% print()
  
  df %>% filter(Population == 0) %>% print(n = Inf)
  
}



### 2 Data Zone Populations ----

# Read in source data file
# Remove SPSS formatting such as labels and widths

dz_pop_model <- read_sav(glue("{data_filepath}/NRAC_CHP_model_datazone_", 
                              "weighted_pop.sav")) %>% 
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character)

# Rename variables for upload

dz_pop_model %<>%
  select(-HB2006) %>% 
  rename(Data_Zone = datazone, 
         Council_Area_9 = CA, 
         HSCP_Code = HSCP, 
         NHS_Board_Code_9 = HB2018,
         Age_Band = age, 
         Population = pop)

# The Populations datamart currently only uses the old GSS codes (CA2011, 
# HSCP2016. HB2014), so we need to change codes back to these versions
# This section will not be necessary once the Populations datamart has been 
# updated to include the new GSS codes.

dz_pop_model %<>%
  mutate(Council_Area_9 = recode(Council_Area_9, 
                                 "S12000047" = "S12000015", 
                                 "S12000048" = "S12000024"), 
         HSCP_Code = recode(HSCP_Code, 
                            "S37000032" = "S37000014", 
                            "S37000033" = "S37000023"), 
         NHS_Board_Code_9 = recode(NHS_Board_Code_9, 
                                   "S08000029" = "S08000018", 
                                   "S08000030" = "S08000027"))


# Create population name from caregrp column
# Create Gender column
# Create dummy fields for upload to datamart

dz_pop_model %<>%
  mutate(Population_Name = case_when(caregrp == "Acute" 
                                     ~ "NRAC Acute Datazone Population", 
                                     caregrp == "Community"
                                     ~ "NRAC Community Datazone Population", 
                                     caregrp == "Care of the Elderly"
                                     ~ "NRAC COTE Datazone Population", 
                                     caregrp == "NRAC"
                                     ~ "NRAC Datazone Population", 
                                     caregrp == "HCHS"
                                     ~ "NRAC HCHS Datazone Population", 
                                     caregrp == "Maternity"
                                     ~ "NRAC Maternity Datazone Population", 
                                     caregrp == 
                                       "Mental Health & Learning Difficulties"
                                     ~ "NRAC MHLD Datazone Population", 
                                     caregrp == "Prescribing"
                                     ~ "NRAC Prescribing Datazone Population"), 
         Gender = recode(sex, 
                         "Male" = "1", 
                         "Female" = "2"), 
         Location_Code = NA, 
         Location_Type = NA, 
         Intermediate_Zone = NA, 
         Month = NA, 
         CHP_Code = NA)

# Remove populations and ages that are not required
# Select columns

dz_pop_model %<>%
  filter(!is.na(Population_Name) & Age_Band != "All") %>% 
  arrange(Population_Name, Age_Band, Gender) %>% 
  select(Population_Name, Population, Age_Band, Gender, Location_Code, 
         Location_Type, Data_Zone, Intermediate_Zone, Council_Area_9, 
         NHS_Board_Code_9, Month, CHP_Code, HSCP_Code)



### 3 Check Data Zone File ----

### 3.1 Data_Zone ----

# Check populations by Data_Zone

dz_pop <- check_fun(dz_pop_model, "Data_Zone")

# Run summary statistics to check for any odd data

summary(dz_pop)


### 3.2 Population_Name / Age_Band ----

# Check populations by Population_Name and Age_Band

name_age_pop <- dz_pop_model %>% 
  group_by(Population_Name, Age_Band) %>% 
  summarise(Population = sum(Population))

# Run summary statistics to check for any odd data

summary(name_age_pop)


### 3.3 Age_Band ----

# Check populations by Age_Band

age_pop <- check_fun(dz_pop_model, "Age_Band")

# Run summary statistics to check for any odd data

summary(age_pop)


### 3.4 Gender ----

# Check populations by Gender

gen_pop <- check_fun(dz_pop_model, "Gender")

# Run summary statistics to check for any odd data

summary(gen_pop)



### 4 Add Data Zone Template ----

# Set Population to character for matching

dz_pop_model %<>% 
  mutate(Population = as.character(Population))

# Take number of rows as character for use in below section

rows <- as.character(nrow(dz_pop_model))

# Read in template and set Location_Code as todays date
# Update the number of rows and the year

dz_output <- readRDS(glue("{template_filepath}/Template_NRAC_Datazone.rds")) %>% 
  bind_rows(dz_pop_model) %>% 
  mutate(Location_Code = if_else(Population_Name == "POPULATION", date, 
                                 Location_Code), 
         Location_Type = if_else(Population_Name == "POPULATION", rows, 
                                 Location_Type),
         Age_Band = if_else(Population_Name == "POPULATION", year, 
                            Age_Band))

fwrite(dz_output, glue("{lookups_filepath}/POPULATION_NRAC_DATAZONE_", 
                       "{year}_{date}.csv"), col.names = F)

rm(age_pop, dz_output, dz_pop, dz_pop_model, gen_pop, name_age_pop)



### 5 GP Practice Populations ----

# Read in source data file
# Remove SPSS formatting such as labels and widths

gp_pop_model <- read_sav(glue("{data_filepath}/NRAC_CHP_model_GPprac_", 
                              "weighted_pop.sav")) %>% 
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character)

# Rename variables for upload

gp_pop_model %<>%
  select(-HB2006) %>% 
  rename(Location_Code = gpprac, 
         Council_Area_9 = CA, 
         HSCP_Code = HSCP, 
         NHS_Board_Code_9 = HB2018,
         Age_Band = age, 
         Population = pop)

# The Populations datamart currently only uses the old GSS codes (CA2011, 
# HSCP2016. HB2014), so we need to change codes back to these versions
# This section will not be necessary once the Populations datamart has been 
# updated to include the new GSS codes.

gp_pop_model %<>%
  mutate(Council_Area_9 = recode(Council_Area_9, 
                                 "S12000047" = "S12000015", 
                                 "S12000048" = "S12000024"), 
         HSCP_Code = recode(HSCP_Code, 
                            "S37000032" = "S37000014", 
                            "S37000033" = "S37000023"), 
         NHS_Board_Code_9 = recode(NHS_Board_Code_9, 
                                   "S08000029" = "S08000018", 
                                   "S08000030" = "S08000027"))


# Create population name from caregrp column
# Create Gender column
# Create dummy fields for upload to datamart

gp_pop_model %<>%
  mutate(Population_Name = case_when(caregrp == "Acute" 
                                     ~ "NRAC Acute Practice Population", 
                                     caregrp == "Community"
                                     ~ "NRAC Community Practice Population", 
                                     caregrp == "Care of the Elderly"
                                     ~ "NRAC COTE Practice Population", 
                                     caregrp == "NRAC"
                                     ~ "NRAC Practice Population", 
                                     caregrp == "HCHS"
                                     ~ "NRAC HCHS Practice Population", 
                                     caregrp == "Maternity"
                                     ~ "NRAC Maternity Practice Population", 
                                     caregrp == 
                                       "Mental Health & Learning Difficulties"
                                     ~ "NRAC MHLD Practice Population", 
                                     caregrp == "Prescribing"
                                     ~ "NRAC Prescribing Practice Population"), 
         Gender = recode(sex, 
                         "Male" = "1", 
                         "Female" = "2"), 
         Location_Type = "GPPRA", 
         Data_Zone = NA, 
         Intermediate_Zone = NA, 
         Month = NA, 
         CHP_Code = NA)

# Remove populations and ages that are not required
# Select columns

gp_pop_model %<>%
  filter(!is.na(Population_Name) & Age_Band != "All") %>% 
  arrange(Population_Name, Age_Band, Gender) %>% 
  select(Population_Name, Population, Age_Band, Gender, Location_Code, 
         Location_Type, Data_Zone, Intermediate_Zone, Council_Area_9, 
         NHS_Board_Code_9, Month, CHP_Code, HSCP_Code)



### 6 Check GP Practice File ----

### 6.1 Location_Code ----

# Check populations by Location_Code

lc_pop <- check_fun(gp_pop_model, "Location_Code")

# Run summary statistics to check for any odd data

summary(lc_pop)


### 6.2 Population_Name / Age_Band ----

# Check populations by Population_Name and Age_Band

name_age_pop <- gp_pop_model %>% 
  group_by(Population_Name, Age_Band) %>% 
  summarise(Population = sum(Population))

# Run summary statistics to check for any odd data

summary(name_age_pop)


### 6.3 Age_Band ----

# Check populations by Age_Band

age_pop <- check_fun(gp_pop_model, "Age_Band")

# Run summary statistics to check for any odd data

summary(age_pop)


### 6.4 Gender ----

# Check populations by Gender

gen_pop <- check_fun(gp_pop_model, "Gender")

# Run summary statistics to check for any odd data

summary(gen_pop)



### 7 Add GP Practice Template ----

# Set Population to character for matching

gp_pop_model %<>% 
  mutate(Population = as.character(Population))

# Take number of rows as character for use in below section

rows <- as.character(nrow(gp_pop_model))

# Read in template and set Location_Code as todays date
# Update the number of rows and the year

gp_output <- readRDS(glue("{template_filepath}/Template_NRAC_GP.rds")) %>% 
  bind_rows(gp_pop_model) %>% 
  mutate(Location_Code = if_else(Population_Name == "POPULATION", date, 
                                 Location_Code), 
         Location_Type = if_else(Population_Name == "POPULATION", rows, 
                                 Location_Type),
         Age_Band = if_else(Population_Name == "POPULATION", year, 
                            Age_Band))

fwrite(gp_output, glue("{lookups_filepath}/POPULATION_NRAC_PRACTICE_", 
                       "{year}_{date}.csv"), col.names = F)



### 8 Remove Commas ----

# Due to the format of the datamart files, we need to manually remove several
# commas from the first line of each csv file
# Use this loop below to remove these commas from each file
# As this resaves the csv file, this section can take a long time to run
# As the pattern contains "", '' is used to enclose the full pattern

# Data Zone file

gsub_dir(dir = lookups_filepath, pattern = '2343936,"","","","","","",""',  
         replacement = "2343936", recursive = FALSE, ext = "csv")

# GP Practice file

gsub_dir(dir = lookups_filepath, pattern = '318192,"","","","","","",""',    
         replacement = "318192", recursive = FALSE, ext = "csv")
