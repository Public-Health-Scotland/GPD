### 1 - Information ----

# Codename - Update Datamart Small Area Population Estimates
# Original Author - Calum Purdie
# Original Date - 31/10/2019
# Updated - 
# Type - Updating files
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("tidylog")
# install.packages("readr")
# install.packages("glue")
#
# Description - Code for updating small area population estimates in the 
#               Populations Datamart.
# Approximate run time - <1 second

library(tidyr)
library(dplyr)
library(tidylog)
library(readr)
library(glue)

# Set filepaths

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD", "2_Population")
lookups_filepath <- file.path(base_filepath, "Small Area Population estimates", 
                              "Lookup Files", "R Files")
templates_filepath <- file.path(base_filepath, "Population Datamart", 
                                "Creation of Files", "Templates", "R Templates")
datamart_filepath <- file.path(base_filepath, "Population Datamart", 
                               "Lookup Files", "Other Geographies")

# Set date for filenames

date <- strftime(Sys.Date(), format = "%Y%m%d")


### 2 - Create Function for Outputs ----

datamart_output <- function(start, end, pop_name, file, file_name, template){
  
  for (i in start:end){
    
    # Get most recent population estimates file
    # Filter for required year
    # Create Population_Name column and rename columns
    # Create blank columns to fit datamart structure
    # Reorder columns
    
    data <- readRDS(glue("{lookups_filepath}/{file}")) %>% 
      filter(year == i) %>% 
      gather(Age_Band, Population, "age0":"age90plus") %>% 
      rename(Year = year, 
             Gender = sex, 
             Data_Zone = datazone2011) %>% 
      mutate(Population_Name = pop_name, 
             Location = "", 
             Location_Type = "", 
             Month = "", 
             CHP_Code = "", 
             Intermediate_Zone = "", 
             NHS_Board_Code_9 = "", 
             Council_Area_9 = "",
             HSCP_Code = "") %>% 
      select(Population_Name, Population, Age_Band, Gender, Location, Location_Type, 
             Data_Zone, Intermediate_Zone, Council_Area_9, NHS_Board_Code_9, Month, 
             CHP_Code, HSCP_Code, Year) %>% 
      mutate_if(is.numeric, as.character)
    
    # Read in correct template
    # Add on the population estimates
    # Update the date contained within the template to the date the code is run
    # Add the year being updated to the Age_Band
    # Remove the Year column
    
    output <- readRDS(
      glue("{templates_filepath}/{template}")) %>% 
      bind_rows(data) %>% 
      mutate(Location = if_else(Population_Name == "POPULATION", date, Location), 
             Age_Band = if_else(Population_Name == "POPULATION", as.character(i), Age_Band)) %>% 
      select(-Year)
    
    # Save as csv
    
    write_csv(output, 
              glue("{datamart_filepath}/POPULATION_{file_name}_{i}_{date}.csv"), 
              col_names = F)
    
  }
  
}


### 3 - Data Zone ----

datamart_output(start = "2011", end = "2018", 
                pop_name = "Data Zone 2011 Population Estimates", 
                file = "DataZone2011_pop_est_2011_2018.rds", 
                file_name = "DATAZONE_ESTIMATES", 
                template = "Template_DZ2011_estimates.rds")