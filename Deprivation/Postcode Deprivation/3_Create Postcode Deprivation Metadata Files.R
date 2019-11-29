##########################################################
# Create Postcode Deprivation Metadata Files
# Calum Purdie
# Original date 29/11/2019
# Latest update author - Calum Purdie
# Latest update date - 29/11/2019
# Latest update description 
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code to create metadata for postcode deprivation files on ISD website
# Approximate run time
##########################################################


### 1 Housekeeping ----

library(magrittr)
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
                               "Website", "5_Metadata of Files", "Excel sheets")

# Set files to use

pc_all_file <- "postcode_2019_2_all_simd_carstairs"
pc_simd_file <- "postcode_2019_2_simd2016"

pc_all_metadata <- "All Deprivation_2019_2"
pc_simd_metadata <- "SIMD2016_pc"



### 2 posctode_all_simd_carstairs ----

# Read in spss data

pc_all <- read_spss(glue("{lookups_filepath}/{pc_all_file}.sav"))

# Use purrr to get variable labels

pc_all_desc <- map(pc_all, ~ attributes(.)[["label"]]) %>% 
  map_chr(~ if_else(is.null(.), NA_character_, .))

# Create tibble with variable and description columns

pc_all_desc_df <- tibble(variable = names(pc_all), 
                         variable_description = pc_all_desc)

# Save to metadata folder

write_xlsx(pc_all_desc_df, glue("{metadata_filepath}/{pc_all_metadata}.xlsx"))



### 3 postcode_simd ----

# Read in spss data

pc_simd <- read_spss(glue("{lookups_filepath}/{pc_simd_file}.sav"))

# Use purrr to get variable labels

pc_simd_desc <- map(pc_simd, ~ attributes(.)[["label"]]) %>% 
  map_chr(~ if_else(is.null(.), NA_character_, .))

# Create tibble with variable and description columns

pc_simd_desc_df <- tibble(variable = names(pc_simd), 
                          variable_description = pc_simd_desc)

# Save to metadata folder

write_xlsx(pc_simd_desc_df, glue("{metadata_filepath}/{pc_simd_metadata}.xlsx"))


