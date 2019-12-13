##########################################################
# Create SIMD 2016 Population-Weighted Files
# Calum Purdie
# Original date 20/12/2019
# Latest update author - Calum Purdie
# Latest update date - 11/12/2019
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for creating population-weighted SIMD 2016 Files
# Approximate run time - <1 second
##########################################################


### 1 - Housekeeping ----

library(magrittr)
library(dplyr)
library(readr)
library(tidylog)
library(data.table)
library(glue)
library(fs)
library(readxl)
library(janitor)
library(lubridate)

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD", "1_Geography", 
                           "Scottish Postcode Directory")
lookup_filepath <- file.path(base_filepath, "Lookup Files")
spd_filepath <- file.path("Geography", "Scottish Postcode Directory")
CHP_filepath <- file.path(base_filepath, "Source Data", "2019_2", 
                          "Postcode_CHP_Lookups")

version <- "2019_2"



### 2 Read in Data ----

# Use csv version to get correct columns
# rds version has CA2019Name, HB2019Name etc
# Remove pc7 as this column is not required
# Change date format

SPD <- fread(glue("{lookup_filepath}/Scottish_Postcode_Directory_", 
                  "{version}.csv")) %>% 
  select(-pc7)

# Read in correct column names in order from source file

source(glue("{spd_filepath}/Column names for Postcode table.R"))

# Rename columns to required format using renamed_columns

SPD %<>% set_colnames(renamed_columns)



### 3 Reformat Columns to Meet Requirements ----

# Reformat dates to DD/MM/YYYY H:M:S

SPD %<>% 
  mutate(Date_Of_Introduction = format(dmy(Date_Of_Introduction), 
                                       "%d/%m/%Y %H:%M:%S"), 
         Date_Of_Deletion = format(dmy(Date_Of_Deletion), 
                                   "%d/%m/%Y %H:%M:%S"))


# Round Latitude and Longitude to 8 decimal places

SPD %<>% 
  mutate(Latitude = round_half_up(Latitude, 8), 
         Longitude = round_half_up(Longitude, 8))



### 4 Add Old Columns ----

# Mutate in blank columns for old columns no longer in SPD that are still
# required for updating the postcode table

SPD %<>%
  mutate(Electoral_Ward_2011 = NA, 
         Scot_Indx_Mult_Depr_2012_Rank = NA, 
         LAU_2011_Level1 = NA, 
         LAU_2011_Level2 = NA, 
         NUTS_2008_Level2 = NA, 
         NUTS_2008_Level3 = NA, 
         Locality_2012 = NA, 
         Settlement_2012 = NA, 
         Islands_2014 = NA, 
         Travel_To_Work_Area_2007 = NA, 
         Urban_Rural_6Fold_2013_2014 = NA, 
         Urban_Rural_8Fold_2013_2014 = NA)

# Match on data for CHP
# These columns need to be filled in for upload requirements
# Use lookup files provided by NRS for 2019_2 postcodes
# Use !! tidyeval to unquote names
# Running this function can produce warnings
# These are generally around columns that are not used, e.g. dates

CHP_match <- function(lookup, column, new_name){
  
  data <- read_xlsx(glue("{CHP_filepath}/{lookup}.xlsx")) %>% 
    select(Postcode, column) %>% 
    rename(!!new_name := column)
  
  SPD %<>% 
    left_join(data)
  
}

# CHP_2004
SPD <- CHP_match("Postcode_CHP2004", "CommunityHealthPartnership2004Code", 
                 "CHP_2004")

# CHP_2007
SPD <- CHP_match("Postcode_CHP2007", "CommunityHealthPartnership2007Code", 
                 "CHP_2007")

# CHP_Sub_2011
SPD <- CHP_match("Postcode_CHP_SubArea_2011", 
                 "CommunityHealthPartnershipSubArea2011Code", "CHP_Sub_2011")

# CHP_2011
SPD <- CHP_match("Postcode_CHP2011", "CommunityHealthPartnership2011Code", 
                 "CHP_2011")

# CHP_2012
SPD <- CHP_match("Postcode_CHP2012", "CommunityHealthPartnership2012Code", 
                 "CHP_2012")

# Reorder columns

SPD %<>% select(column_order)


### 4 Save file as csv ----

write_csv(SPD, glue("{lookup_filepath}/Scottish_Postcode_Directory_{version}_", 
                    "CDW.csv"), na = "")