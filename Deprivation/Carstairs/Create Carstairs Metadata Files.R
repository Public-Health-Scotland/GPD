##########################################################
# Create Carstairs Metadata Files
# Calum Purdie
# Original date 02/12/2019
# Latest update author - Calum Purdie
# Latest update date - 02/12/2019
# Latest update description 
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code to create metadata for carstairs deprivation files on ISD website
# Approximate run time - 4 seconds
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

lookups_filepath <- file.path(base_filepath, "3_Deprivation", "Carstairs", 
                              "Lookup Files")

metadata_filepath <- file.path(base_filepath, "4_Communication and Training", 
                               "Website", "5_Metadata of Files", "Excel sheets", 
                               "Deprivation")

# Set files to use

pcsec81_car81 <- "pcsec1981_carstairs1981"
pcsec91_car81 <- "pcsec1991_carstairs1981"
pcsec91_car91 <- "pcsec1991_carstairs1991"
pcsec01_car01 <- "pcsec2001_carstairs2001"
pcsec01_car01_ca_split <- "pcsec2001_carstairs2001_ca_split"
pcsec11_car11 <- "pcsec2011_carstairs2011"
pcsec11_car11_ca_split <- "pcsec2011_carstairs2011_ca_split"

pcsec81_car81_metadata <- "Carstairs_1981_pcsec1981"
pcsec91_car81_metadata <- "Carstairs_1981_pcsec1991"
pcsec91_car91_metadata <- "Carstairs_1991_pcsec1991"
pcsec01_car01_metadata <- "Carstairs_2001_pcsec2001"
pcsec01_car01_ca_split_metadata <- "Carstairs_2001_pcsec2001_CA_split"
pcsec11_car11_metadata <- "Carstairs_2011_pcsec2011"
pcsec11_car11_ca_split_metadata <- "Carstairs_2011_pcsec2011_CA_split"



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

# pcsec81_carstairs81

metadata(input = pcsec81_car81, 
         output = pcsec81_car81_metadata)

# pcsec91_carstairs81

metadata(input = pcsec91_car81, 
         output = pcsec91_car81_metadata)

# pcsec91_carstairs91

metadata(input = pcsec91_car91, 
         output = pcsec91_car91_metadata)

# pcsec01_carstairs01

metadata(input = pcsec01_car01, 
         output = pcsec01_car01_metadata)

# pcsec01_carstairs01_ca_split

metadata(input = pcsec01_car01_ca_split, 
         output = pcsec01_car01_ca_split_metadata)

# pcsec11_carstairs11

metadata(input = pcsec11_car11, 
         output = pcsec11_car11_metadata)

# pcsec11_carstairs11_ca_split

metadata(input = pcsec11_car11_ca_split, 
         output = pcsec11_car11_ca_split_metadata)
