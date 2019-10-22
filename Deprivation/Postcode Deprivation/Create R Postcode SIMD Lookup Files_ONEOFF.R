### 1 - Information ----

# Codename - Create R Postcode SIMD Lookup Files
# Original Author - Calum Purdie
# Original Date - 09/09/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readr")
#
# Description - Creating .rds versions of all postcode_simd files.
#
# Approximate run time - <1 minute

# Set working directory
base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD", 
                           "3_Deprivation", "Postcode Deprivation", "Lookup Files")
output_filepath <- file.path(base_filepath, "R Files")

# Read in packages from library
library(dplyr)
library(readr)

### 2 - Read in csv files ----

# Read in CA, HSCP and HB name columns from open data
# Rename columns to correct year and set all factors as characters
open_data_names <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/e92d19d4-ced7-40c8-b628-e28e4528fc41/download/geography_codes_and_labels_dz2001_19082019.csv") %>% 
  select(DZ2001, CA2011Name, HSCP2016Name, HB2014Name) %>% 
  rename(DataZone2001 = DZ2001, CA2019Name = CA2011Name, HSCP2019Name = HSCP2016Name, HB2019Name = HB2014Name) %>% 
  mutate_if(is.factor, as.character)

# Use function for first three file
# Match on the geography name columns and select the column order
# Rename PC7 to pc7 to match newer files
# Save file as an rds version

R_data <- function(input_file, output_file){
  
  data <- read_csv(file.path(base_filepath, input_file)) %>% 
    left_join(open_data_names) %>% 
    select(PC7, OA2001, DataZone2001, HB2019, HB2019Name, HB2014, HB2006, CA2019, CA2019Name, 
           CA2018, CA2011, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, everything()) %>% 
    rename(pc7 = PC7)
  
  saveRDS(data, file.path(output_filepath, output_file))
  
}

postcode_2006_2_simd2004 <- R_data("postcode_2006_2_simd2004.csv", "postcode_2006_2_simd2004.rds")

postcode_2009_2_simd2006 <- R_data("postcode_2009_2_simd2006.csv", "postcode_2009_2_simd2006.rds")

postcode_2012_2_simd2009v2 <- R_data("postcode_2012_2_simd2009v2.csv", "postcode_2012_2_simd2009v2.rds")

# simd2012 files doesn't need PC7 to be renamed

postcode_2016_1_simd2012 <- read_csv(file.path(base_filepath, "postcode_2016_1_simd2012.csv")) %>% 
  left_join(open_data_names) %>% 
  select(PC7, OA2001, DataZone2001, HB2019, HB2019Name, HB2014, HB2006, CA2019, CA2019Name, 
         CA2018, CA2011, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, everything())

saveRDS(data, file.path(output_filepath, "postcode_2016_1_simd2012.rds"))
