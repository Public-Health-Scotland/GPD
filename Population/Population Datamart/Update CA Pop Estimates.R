### 1 - Information ----

# Codename - Update CA Pop Estimates
# Original Author - Calum Purdie
# Original Date - 24/10/2019
# Type - Updating files
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
#
# Description - Code for updating Council Area population estimates in the Populations Datamart.
# Approximate run time - <1 second

library(tidyr)
library(dplyr)
library(tidylog)
library(readr)

# Set filepaths

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD", 
                           "2_Population")
lookups_filepath <- file.path(base_filepath, "Population Estimates", "Lookup Files", "R Files")
templates_filepath <- file.path(base_filepath, "Population Datamart", "Creation of Files", 
                                "Templates", "R Templates")
datamart_filepath <- file.path(base_filepath, "Population Datamart", "Lookup Files", 
                               "Other Geographies")


### 2 - Create Datamart File for Council Area Population Estimates ----

council_area <- function(year, date, pop_name, file, file_name){

# Get most recent population estimates file
# Filter for required year
# Create Population_Name column and rename columns
# Create blank columns to fit datamart structure
# Reorder columns

data <- readRDS(file.path(lookups_filepath, file)) %>% 
  filter(Year == year) %>% 
  mutate(Population_Name = pop_name) %>% 
  rename(Gender = Sex, Age_Band = Age, Population = Pop, Council_Area_9 = CA2011) %>%
  mutate(Location = "", 
         Location_Type = "", 
         Data_Zone = "", 
         Intermediate_Zone = "", 
         NHS_Board_Code_9 = "",
         Month = "", 
         CHP_Code = "", 
         HSCP_Code = "") %>% 
  select(Population_Name, Population, Age_Band, Gender, Location, Location_Type, Data_Zone, 
         Intermediate_Zone, Council_Area_9, NHS_Board_Code_9, Month, CHP_Code, HSCP_Code, Year) %>% 
  mutate_if(is.numeric, as.character)

# Read in correct template
# Add on the population estimates
# Update the date contained within the template to the date the code is run
# Add the year being updated to the Age_Band
# Remove the Year column

Template_CA_estimates <- readRDS(file.path(templates_filepath, "Template_CA_estimates.rds")) %>% 
  bind_rows(CA_pop_est) %>% 
  mutate(Location = if_else(Population_Name == "POPULATION", date, Location), 
         Age_Band = if_else(Population_Name == "POPULATION", year, Age_Band)) %>% 
  select(-Year)

# Save as csv

write_csv(Template_CA_estimates, file.path(datamart_filepath, 
                                           paste0("POPULATION_", file_name, "_", year, "_", date, 
                                                  ".csv")), col_names = F)

}

ca_estimates(year = "2018", date = "20192410", pop_name = "Council Area Population Estimates", 
             file = "CA2019_pop_est_1981_2018.rds", file_name = "CA_ESTIMATES")