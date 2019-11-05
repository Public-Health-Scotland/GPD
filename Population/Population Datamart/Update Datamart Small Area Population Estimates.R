### 1 - Information ----

# Codename - Update Datamart Small Area Population Estimates
# Original Author - Calum Purdie
# Original Date - 31/10/2019
# Updated - 04/11/2019
# Updated - 
# Type - Updating files
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("magrittr")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("tidylog")
# install.packages("readr")
# install.packages("glue")
# install.packages("xfun")
#
# Description - Code for updating small area population estimates in the 
#               Populations Datamart.
# Approximate run time - 30 minutes

library(magrittr)
library(tidyr)
library(dplyr)
library(tidylog)
library(readr)
library(glue)
library(xfun)

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

# Get CHP2012 column for matching

DZ_CHP <- read_csv(paste0("//Isdsf00d03/cl-out/lookups/Unicode/Geography/", 
                          "DataZone2011/DataZone2011.csv")) %>% 
  select(DataZone2011, CHP2012) %>% 
  rename(Data_Zone = DataZone2011, 
         CHP_Code = CHP2012)
  


### 2 - Create Function for Outputs ----

datamart_output <- function(start, end, pop_name, file, file_name, template){
  
  for (i in start:end){
    
    # Get most recent population estimates file
    # Filter for required year
    # Gather data into correct format
    # Remove the age prefix from all age names
    # Create Population_Name column and rename columns
    # Match on CHP2012
    # Create blank columns to fit datamart structure
    # Reorder columns
    
    data <- readRDS(glue("{lookups_filepath}/{file}")) %>% 
      filter(year == i) %>% 
      gather(Age_Band, Population, "age0":"age90plus") %>% 
      mutate(Age_Band = gsub("age", "", Age_Band), 
             Age_Band = recode(Age_Band, "90plus" = "90")) %>% 
      rename(Year = year, 
             Gender = sex, 
             Data_Zone = datazone2011, 
             Intermediate_Zone = intzone2011, 
             NHS_Board_Code_9 = hb2014, 
             Council_Area_9 = ca2011,
             HSCP_Code = hscp2016) %>% 
      left_join(DZ_CHP) %>% 
      mutate(Population_Name = pop_name, 
             Location_Code = "", 
             Location_Type = "", 
             Month = "") %>% 
      select(Population_Name, Population, Age_Band, Gender, Location_Code, 
             Location_Type, Data_Zone, Intermediate_Zone, Council_Area_9, 
             NHS_Board_Code_9, Month, CHP_Code, HSCP_Code, Year) %>% 
      mutate_if(is.numeric, as.character) %>% 
      arrange(Data_Zone)
    
    # Read in correct template
    # Add on the population estimates
    # Update the date contained within the template to the date the code is run
    # Add the year being updated to the Age_Band
    # Remove the Year column
    
    output <- readRDS(
      glue("{templates_filepath}/{template}")) %>% 
      bind_rows(data) %>% 
      mutate(Location_Code = if_else(Population_Name == "POPULATION", date, 
                                     Location_Code), 
             Age_Band = if_else(Population_Name == "POPULATION", as.character(i), 
                                Age_Band)) %>% 
      select(-Year)
    
    # Save as csv
    
    write_csv(output,
              glue("{datamart_filepath}/POPULATION_{file_name}_{i}_{date}.csv"),
              col_names = F)
    
  }
  
  # Due to the format of the datamart files, we need to manually remove several
  # commas from the first line of each csv file
  # Use this loop below to remove these commas from each file
  # As this resaves the csv file, this section can take a long time to run
  
  gsub_dir(dir = datamart_filepath, pattern = "1269632,,,,,,,",
           replacement = "1269632", recursive = FALSE, ext = "csv")
  
}


### 3 - Data Zone ----

datamart_output(start = "2018", end = "2018", 
                pop_name = "Data Zone 2011 Population Estimates", 
                file = "DataZone2011_pop_est_2011_2018.rds", 
                file_name = "DATAZONE2011_ESTIMATES", 
                template = "Template_DZ2011_estimates.rds")
