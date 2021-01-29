##########################################################
# Update Datamart Population Projections
# Calum Purdie
# Original date 24/10/2019
# Latest update author - Calum Purdie
# Latest update date - 27/03/2020
# Latest update description - formatting code
# Type of script - Updating files
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for updating population projections in the Populations Datamart.
# Approximate run time - 30 seconds
##########################################################

### 1 - Housekeeping ----

library(magrittr)
library(tidyr)
library(dplyr)
library(tidylog)
library(readr)
library(glue)
library(xfun)

# Set filepaths

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                      "2_Population")
lookups_filepath <- glue("{base_filepath}/Population Projections/Lookup Files/",
                         "R Files")
templates_filepath <- glue("{base_filepath}/Population Datamart/", 
                           "Creation of Files/Templates/R Templates")
datamart_filepath <- glue("{base_filepath}/Population Datamart/Lookup Files/", 
                          "Other Geographies")

# Set date for filenames

date <- strftime(Sys.Date(), format = "%Y%m%d")


### 2 - Create Function for Outputs ----

datamart_output <- function(start, end, pop_name, file, file_name, template, 
                            geography){
  
  for (i in start:end){
    
    # Get most recent population estimates file
    # Filter for required year
    # Create Population_Name column and rename columns
    # Create blank columns to fit datamart structure
    # Reorder columns
    
    data <- readRDS(glue("{lookups_filepath}/{file}")) %>% 
      filter(year == i) %>% 
      mutate(Population_Name = pop_name) %>% 
      rename(Year = year, 
             Gender = sex, 
             Age_Band = age, 
             Population = pop) %>% 
      mutate(Location = "", 
             Location_Type = "", 
             Month = "", 
             CHP_Code = "")
    
    if(file_name == "SCOTLAND_PROJECTIONS"){
      
      data %<>% 
        mutate(Data_Zone = "", 
               Intermediate_Zone = "", 
               NHS_Board_Code_9 = "",
               HSCP_Code = "", 
               Council_Area_9 = "")
      
    } else if(file_name == "CA_PROJECTIONS"){
      
      data %<>% 
        rename(Council_Area_9 = !!as.name(geography)) %>% 
        mutate(Data_Zone = "", 
               Intermediate_Zone = "", 
               NHS_Board_Code_9 = "",
               HSCP_Code = "")
      
    } else if(file_name == "HSCP_PROJECTIONS"){
      
      data %<>% 
        rename(HSCP_Code = !!as.name(geography)) %>% 
        mutate(Data_Zone = "", 
               Intermediate_Zone = "", 
               NHS_Board_Code_9 = "",
               Council_Area_9 = "")
      
    } else if (file_name == "HBCURRENT_PROJECTIONS"){
      
      data %<>% 
        rename(NHS_Board_Code_9 = !!as.name(geography)) %>% 
        mutate(Data_Zone = "", 
               Intermediate_Zone = "", 
               Council_Area_9 = "",
               HSCP_Code = "")
      
    } else {
      
      print("Define a correct file name")
      
    }
    
    data %<>% 
      select(Population_Name, Population, Age_Band, Gender, Location, 
             Location_Type, Data_Zone, Intermediate_Zone, Council_Area_9, 
             NHS_Board_Code_9, Month, CHP_Code, HSCP_Code, Year) %>% 
      mutate_if(is.numeric, as.character)
    
    # Read in correct template
    # Add on the population PROJECTIONS
    # Update the date contained within the template to the date the code is run
    # Add the year being updated to the Age_Band
    # Remove the Year column
    
    output <- readRDS(
      glue("{templates_filepath}/{template}")) %>% 
      bind_rows(data) %>% 
      mutate(Location = if_else(Population_Name == "POPULATION", date, Location), 
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
  
  if(file_name == "SCOTLAND_PROJECTIONS"){
    
    gsub_dir(dir = datamart_filepath, pattern = "182,,,,,,,", 
             replacement = "182", recursive = FALSE, ext = "csv")
    
  } else if(file_name == "CA_PROJECTIONS"){
    
    gsub_dir(dir = datamart_filepath, pattern = "5824,,,,,,,", 
             replacement = "5824", recursive = FALSE, ext = "csv")
    
  } else if(file_name == "HSCP_PROJECTIONS"){
    
    gsub_dir(dir = datamart_filepath, pattern = "5642,,,,,,,", 
             replacement = "5642", recursive = FALSE, ext = "csv")
    
  } else if (file_name == "HBCURRENT_PROJECTIONS"){
    
    gsub_dir(dir = datamart_filepath, pattern = "2548,,,,,,,", 
             replacement = "2548", recursive = FALSE, ext = "csv")
    
  } else {
    
    print("Define a correct file name")
    
  }
  
}



### 3 - Scotland ----

datamart_output(start = "2018", end = "2043", 
                pop_name = "Scotland Population Projections", 
                file = "scot_pop_proj_2018_2043.rds", 
                file_name = "SCOTLAND_PROJECTIONS", 
                template = "Template_scot_projections.rds")

### 4 - Council Area ----

datamart_output(start = "2018", end = "2043", 
                pop_name = "Council Area Population Projections", 
                file = "CA2019_pop_proj_2018_2043.rds", 
                file_name = "CA_PROJECTIONS", 
                template = "Template_CA_projections.rds", 
                geography = "ca2019")

### 5 - HSCP ----

datamart_output(start = "2018", end = "2043", 
                pop_name = "Health and Social Care Partnership Population Projections", 
                file = "HSCP2019_pop_proj_2018_2043.rds", 
                file_name = "HSCP_PROJECTIONS", 
                template = "Template_HSCP_projections.rds", 
                geography = "hscp2019")

### 6 - Health Board Current ----

datamart_output(start = "2018", end = "2043", 
                pop_name = "NHS Board Current Population Projections", 
                file = "HB2019_pop_proj_2018_2043.rds", 
                file_name = "HBCURRENT_PROJECTIONS", 
                template = "Template_HBcurrent_projections.rds", 
                geography = "hb2019")
