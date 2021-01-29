##########################################################
# Update Datamart Small Area Population Estimates
# Original Author - Calum Purdie
# Original date 31/10/2019
# Latest update author - Calum Purdie
# Latest update date - 04/01/2021
# Latest update description - formatting code
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for updating small area population estimates in the Populations Datamart
# Approximate run time - 30 minutes
##########################################################

### 1 - Housekeeping ----

library(magrittr)
library(tidyr)
library(dplyr)
library(tidylog)
library(readr)
library(glue)
library(xfun)
library(data.table)
library(ckanr)

# Set filepaths

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                      "2_Population")
lookups_filepath <- glue("{base_filepath}/Small Area Population estimates/", 
                         "Lookup Files/R Files")
templates_filepath <- glue("{base_filepath}/Population Datamart/", 
                           "Creation of Files/Templates/R Templates")
datamart_filepath <- glue("{base_filepath}/Population Datamart/Lookup Files/", 
                          "Other Geographies")

# Set date for filenames

date <- strftime(Sys.Date(), format = "%Y%m%d")

# Get CHP2012 column for matching

DZ_CHP <- read_csv(paste0("//Isdsf00d03/cl-out/lookups/Unicode/Geography/", 
                          "DataZone2011/DataZone2011.csv")) %>% 
  select(DataZone2011, CHP2012) %>% 
  rename(Data_Zone = DataZone2011, 
         CHP_Code = CHP2012)

# Use the Geography Codes and Names open data file to get the names
# First need to run the httr configuration script

source(here::here("Geography", "Scottish Postcode Directory", 
                  "Set httr configuration for API.R"))

# Add columns for datazone2011name

# Set url and id

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id <- "395476ab-0720-4740-be07-ff4467141352"

codes <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  select(DataZone, IntZone, CA, HSCP, HB) %>% 
  rename(datazone2011 = DataZone, intzone2011 = IntZone, 
         ca2019 = CA, hscp2019 = HSCP, hb2019 = HB) %>%  
  as_tibble()



### 2 - Create Function for Outputs ----

datamart_output <- function(start, end, pop_name, file, file_name, template, 
                            dz, iz, hb, ca, hscp){
  
  for (i in start:end){
    
    # Get most recent population estimates file
    
    data <- readRDS(glue("{lookups_filepath}/{file}"))
    
    # If file uses 2001-2010 estimates file match on geography codes lookup
    
    if (file == "DataZone2011_pop_est_2001_2010.rds"){
      
      data %<>%
        left_join(codes)
      
    }
      
    # Filter for required year
    # Gather data into correct format
    # Remove the age prefix from all age names
    # Create Population_Name column and rename columns
    # Match on CHP2012
    # Create blank columns to fit datamart structure
    # Reorder columns
    
    data %<>% 
      filter(year == i) %>% 
      gather(Age_Band, Population, "age0":"age90plus") %>% 
      mutate(Age_Band = gsub("age", "", Age_Band), 
             Age_Band = recode(Age_Band, "90plus" = "90"), 
             sex = recode(sex, "M" = "1", "F" = "2")) %>% 
      rename(Year = year, 
             Gender = sex, 
             Data_Zone = !!as.name(dz), 
             Intermediate_Zone = !!as.name(iz), 
             NHS_Board_Code_9 = !!as.name(hb), 
             Council_Area_9 = !!as.name(ca),
             HSCP_Code = !!as.name(hscp)) %>% 
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
    
    # fwrite(output,
    #           glue("{datamart_filepath}/POPULATION_{file_name}_{i}_{date}.csv"),
    #           col.names = F)
    
  }
  
  # # Due to the format of the datamart files, we need to manually remove several
  # # commas from the first line of each csv file
  # # Use this loop below to remove these commas from each file
  # # As this resaves the csv file, this section can take a long time to run
  # 
  # gsub_dir(dir = datamart_filepath, pattern = "1269632,,,,,,,",
  #          replacement = "1269632", recursive = FALSE, ext = "csv")
  
}


### 3 - Data Zone ----

datamart_output(start = "2006", end = "2010", 
                pop_name = "Data Zone 2011 Population Estimates", 
                file = "DataZone2011_pop_est_2001_2010.rds", 
                file_name = "DATAZONE2011_ESTIMATES", 
                template = "Template_DZ2011_estimates.rds", 
                dz = "datazone2011", iz = "intzone2011", hb = "hb2019", 
                ca = "ca2019", hscp = "hscp2019")

datamart_output(start = "2018", end = "2018", 
                pop_name = "Data Zone 2011 Population Estimates", 
                file = "DataZone2011_pop_est_2011_2019.rds", 
                file_name = "DATAZONE2011_ESTIMATES", 
                template = "Template_DZ2011_estimates.rds", 
                dz = "datazone2011", iz = "intzone2011", hb = "hb2019", 
                ca = "ca2019", hscp = "hscp2019")

# datamart_output(start = "2001", end = "2014", 
#                 pop_name = "Data Zone 2001 Population Estimates", 
#                 file = "DataZone2011_pop_est_2001_2014.rds", 
#                 file_name = "DATAZONE2011_ESTIMATES", 
#                 template = "Template_DZ2001_estimates.rds", 
#                 dz = "datazone2001", iz = "intzone2001", hb = "hb2019", 
#                 ca = "ca2019", hscp = "hscp2019")