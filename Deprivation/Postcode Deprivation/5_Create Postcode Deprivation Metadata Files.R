##########################################################
# Create Postcode Deprivation Metadata Files
# Calum Purdie
# Original date 29/11/2019
# Latest update author - Calum Purdie
# Latest update date - 02/12/2019
# Latest update description 
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code to create metadata for postcode deprivation files on ISD website
# Approximate run time
##########################################################


### 1 Housekeeping ----

library(dplyr)
library(haven)
library(writexl)
library(tidylog)
library(glue)
library(purrr)

# Set filepaths

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD")

lookups_filepath <- file.path(base_filepath, "3_Deprivation", 
                              "Postcode Deprivation", "Lookup Files")

metadata_filepath <- file.path(base_filepath, "4_Communication and Training", 
                               "Website", "5_Metadata of Files", "Excel sheets", 
                               "Deprivation")

# Set files to use

pc_all_file <- "postcode_2019_2_all_simd_carstairs"
pc_simd_file <- "postcode_2019_2_simd2016"

pc_all_metadata <- "All Deprivation_2019_2"
pc_simd_metadata <- "SIMD2016_pc"



### 2 Function ----

metadata <- function(input, output){
  
  # Read in spss data
  
  data <- read_spss(glue("{lookups_filepath}/{input}.sav"))
  
  # Use purrr to get variable labels
  
  data_desc <- map(data, ~ attributes(.)[["label"]]) %>% 
    map_chr(~ if_else(is.null(.), NA_character_, .))
  
  # Create tibble with variable and description columns
  
  data_desc_df <- tibble(variable = names(data), 
                         variable_description = data_desc)
  
  # Save to metadata folder
  
  write_xlsx(data_desc_df, glue("{metadata_filepath}/{output}.xlsx"))
  
}



### 3 Create Metadata Files ----

# posctode_all_simd_carstairs

metadata(input = pc_all_file, output = pc_all_metadata)


# postcode_simd

metadata(input = pc_simd_file, output = pc_simd_metadata)