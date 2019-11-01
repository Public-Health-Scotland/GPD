### 1 - Information ----

# Codename - Update Datamart Population Estimates
# Original Author - Calum Purdie
# Original Date - 24/10/2019
# Updated - -01/11/2019
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
# Description - Code for updating population estimates in the 
#               Populations Datamart.
# Approximate run time - <1 second

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
lookups_filepath <- file.path(base_filepath, "Population Estimates", 
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
      filter(Year == i) %>% 
      mutate(Population_Name = pop_name) %>% 
      rename(Gender = Sex, 
             Age_Band = Age, 
             Population = Pop) %>% 
      mutate(Location = "", 
             Location_Type = "", 
             Month = "", 
             CHP_Code = "")
    
    if(file_name == "CA_ESTIMATES"){
      
      data %<>% 
        rename(Council_Area_9 = CA2011) %>% 
        mutate(Data_Zone = "", 
               Intermediate_Zone = "", 
               NHS_Board_Code_9 = "",
               HSCP_Code = "")
      
    } else if(file_name == "HSCP_ESTIMATES"){
      
      data %<>% 
        rename(HSCP_Code = HSCP2016) %>% 
        mutate(Data_Zone = "", 
               Intermediate_Zone = "", 
               NHS_Board_Code_9 = "",
               Council_Area_9 = "")
      
    } else if (file_name == "HBCURRENT_ESTIMATES" | 
               file_name == "HBEVENT_ESTIMATES" | 
               file_name == "HBEVENTv2_ESTIMATES"){
      
      data %<>% 
        rename(NHS_Board_Code_9 = HB2014) %>% 
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
    # Add on the population estimates
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
  
  if(file_name == "CA_ESTIMATES"){
    
    gsub_dir(dir = datamart_filepath, pattern = "5824,,,,,,,", 
             replacement = "5824", recursive = FALSE, ext = "csv")
    
  } else if(file_name == "HSCP_ESTIMATES"){
    
    gsub_dir(dir = datamart_filepath, pattern = "5642,,,,,,,", 
             replacement = "5642", recursive = FALSE, ext = "csv")
    
  } else if (file_name == "HBCURRENT_ESTIMATES" | 
             file_name == "HBEVENT_ESTIMATES" | 
             file_name == "HBEVENTv2_ESTIMATES"){
    
    gsub_dir(dir = datamart_filepath, pattern = "2548,,,,,,,", 
             replacement = "2548", recursive = FALSE, ext = "csv")
    
  } else {
    
    print("Define a correct file name")
    
  }
  
}


### 3 - Council Area ----

datamart_output(start = "1981", end = "2018", 
                pop_name = "Council Area Population Estimates", 
                file = "CA2019_pop_est_1981_2018.rds", 
                file_name = "CA_ESTIMATES", 
                template = "Template_CA_estimates.rds")

### 4 - HSCP ----

datamart_output(start = "1981", end = "2018", 
                pop_name = "Health and Social Care Partnership Population Estimates", 
                file = "HSCP2019_pop_est_1981_2018.rds", 
                file_name = "HSCP_ESTIMATES", 
                template = "Template_HSCP_estimates.rds")

### 5 - Health Board ----

### 5.1 - HBCURRENT ----

datamart_output(start = "1981", end = "2018", 
                pop_name = "NHS Board Current Population Estimates", 
                file = "HB2019_pop_est_1981_2018.rds", 
                file_name = "HBCURRENT_ESTIMATES", 
                template = "Template_HBcurrent_estimates.rds")

### 5.2 - HBEVENT ----

datamart_output(start = "1981", end = "2018", 
                pop_name = "NHS Board At Event Population Estimates", 
                file = "HB2019_pop_est_1981_2018.rds", 
                file_name = "HBEVENT_ESTIMATES", 
                template = "Template_HBevent_estimates.rds")

### 5.3 - HBEVENTv2 ----

datamart_output(start = "1981", end = "2018", 
                pop_name = "NHS Board At Event (excluding Argyll & Clyde) Population Estimates", 
                file = "HB2019_pop_est_1981_2018.rds", 
                file_name = "HBEVENTv2_ESTIMATES", 
                template = "Template_HB2014eventv2_estimates.rds")
