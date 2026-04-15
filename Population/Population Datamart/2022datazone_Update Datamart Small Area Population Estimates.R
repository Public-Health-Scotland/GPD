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

# Also read section 8 of the SOP for information on edits to functions that are 
# required.

# Last edited 03/04/2023 - Gerald Leung
##########################################################

### 1 - Housekeeping ----

# Set filepaths

rm(list = ls())

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}

pacman::p_load(phsmethods, phsopendata,data.table,here, xfun,
               magrittr,tidyr, readr, janitor,tidylog, glue, dplyr)


# Set filepaths

path_main_script_location = dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(path_main_script_location)
base_filepath      <- glue("//data/geography/Population")
lookups_filepath   <- glue("{base_filepath}/Small Area Population estimates/", 
                           "Lookup Files/R Files")

datamart_filepath  <- glue("{base_filepath}/Population Datamart/Lookup Files/",
                           "Other Geographies")
# output_path <-"/data/geography/Personal/Iain/FV_demographics"
#  datamart_filepath  <- output_path
# # Set date for filenames
# file.create("datamart_filepath")

# keep templates in this folder, so doesn't get uploaded to GitHub
templates_filepath <- "//data/geography/GitHub/GPD-Population/Population Datamart/R Templates"

# Set date for filenames
date <- strftime(Sys.Date(), format = "%Y%m%d")

# Get CHP2012 column for matching
DZ_CHP <- read_csv(paste0("//conf/linkage/output/lookups/Unicode/Geography/DataZone2011/DataZone2011.csv")) %>% 
  select(Data_Zone = DataZone2011, 
         CHP_Code = CHP2012)
spd <- readRDS(paste0("//conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_2.rds"))

codes_2022 <- spd %>% 
  select(datazone2022, intzone2022, ca2019, hscp2019, hb2019, chp_2012) %>% 
  distinct(datazone2022, .keep_all =TRUE)
  
 geo_codes_id <- "395476ab-0720-4740-be07-ff4467141352"
# 

codes_2022 <- spd %>% 
codes <- get_resource(res_id = geo_codes_id) %>%
  select(DataZone, IntZone, CA, HSCP, HB) %>%
  rename(datazone2011 = DataZone, intzone2011 = IntZone,
         ca2019 = CA, hscp2019 = HSCP, hb2019 = HB) %>%
  as_tibble()


data <- readRDS(glue("{lookups_filepath}/", "DataZone2011_pop_est_2011_2024.rds"))

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
    
    
    data <-data  %>% 
      filter(year == 2016) %>%
      pivot_longer(cols = age0:age90plus,
                   names_to = "Age_Band",
                   values_to = "Population") %>% 
      # gather(Age_Band, Population, "age0":"age90plus") %>%
      mutate(Age_Band = gsub("age", "", Age_Band),
             Age_Band = case_when(Age_Band == "90plus" ~ "90", TRUE~ Age_Band),
             sex = recode(sex, "M" = "1", "F" = "2")
             ) %>% 
      rename(Year = year,
             Gender = sex,
             Data_Zone = !!as.name(dz),
             Intermediate_Zone = !!as.name(iz),
             NHS_Board_Code_9 = !!as.name(hb),
             Council_Area_9 = !!as.name(ca),
             HSCP_Code = !!as.name(hscp)) %>%
     # left_join(DZ_CHP) %>%
      mutate(Population_Name = pop_name,
             Location_Code = "",
             Location_Type = "",
             Month = "") %>%
      select(Population_Name, Population, Age_Band, Gender, Location_Code,
             Location_Type, Data_Zone, Intermediate_Zone, Council_Area_9,
             NHS_Board_Code_9, Month, CHP_Code, HSCP_Code, Year = year) %>%
     # mutate_if(is.numeric, as.character) %>%
      mutate(across(where(is.numeric), as.character)) %>% 
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
    # 
    # Save as csv
    
    write_csv(output,
              glue("{datamart_filepath}/POPULATION_{file_name}_{i}_{date}.csv"),
              col_names = F)
    
    }
  
  # Due to the format of the datamart files, we need to manually remove several
  # commas from the first line of each csv file
  # Use this loop below to remove these commas from each file
  # As this resaves the csv file, this section can take a long time to run

  # gsub_dir(dir = datamart_filepath, pattern = "1269632,,,,,,,",
  #          replacement = "1269632", recursive = FALSE, ext = "csv")
  
}


### 3 - Data Zone ----

datamart_output(start = "2016", end = "2016", 
                pop_name = "Data Zone 2022 Population Estimates", 
                file = "DataZone2011_pop_est_2011_2024.rds", 
                file_name = "DATAZONE2011_ESTIMATES", 
                template = "Template_DZ2011_estimates.rds", 
                dz = "datazone2011", iz = "intzone2011", hb = "hb2019", 
                ca = "ca2019", hscp = "hscp2019")



