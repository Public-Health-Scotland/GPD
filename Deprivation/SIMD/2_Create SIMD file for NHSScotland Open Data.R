### 1 - Information ----

# Codename - Create SIMD file for NHSScotland Open Data
# Data release - Scottish Index of Multiple Deprivation
# Original Author - Calum Purdie
# Original Date - 23/09/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("tidylog")
#
# Description - Code to create Data Zone - SIMD lookup files for the NHSScotland
#               open data platform
#
# Approximate run time - <1 seconds


# Read in packages from library

library(tidyr)
library(dplyr)
library(tidylog)

# Update filepaths for new version

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI")
lookup_filepath <- file.path(base_filepath, "Referencing & Standards", "GPD",
                             "3_Deprivation", "SIMD", "Lookup Files", "SIMD 2016", 
                             "R Files")
open_data_filepath <- file.path(base_filepath, "Publications", 
                                "Open Data (Non Health Topic)", "Data", 
                                "OD1700038 - SIMD")


### 2 - Read in lookup file ----

datazone_simd <- readRDS(file.path(lookup_filepath, "DataZone2011_simd2016.rds")) %>% 
  select(DataZone2011:HB2019, HSCP2019, CA2019, simd2016rank:simd2016bt15)


# Rename columns to match open data format
colnames(datazone_simd) <- c("DZ2011", "IZ2011", "HB2014", "HSCP2016", "CA2011", "SIMD2016Rank", 
                "SIMD2016CountryDecile", "SIMD2016CountryQuintile", 
                "SIMD2016HB2014Decile", "SIMD2016HB2014Quintile", 
                "SIMD2015HSCP2016Decile", "SIMD2016HSCP2016Quintile", 
                "SIMD2016CA2011Decile", "SIMD2016CA2011Quintile", 
                "SIMD2016Most15pc", "SIMD2016Least15pc")


### 3 - Add Qualifier columns ----

# Due to minor boundary changes from 02/02/2018 (Keltybridge) and 01/04/2019 
# (Cardowan by Stepps) which affeced some HB, HSCP and CA codes, a qualifier 
# field is required to show which codes have been revised
# Select the columns in the correct order for uploading

datazone_simd <- datazone_simd %>% 
  mutate(HB2014QF = case_when(HB2014 == "S08000029" | 
                              HB2014 == "S08000030" | 
                              HB2014 == "S08000031" | 
                              HB2014 == "S08000032" ~ "r"), 
         HSCP2016QF = case_when(HSCP2016 == "S37000032" | 
                                HSCP2016 == "S37000033" | 
                                HSCP2016 == "S37000034" | 
                                HSCP2016 == "S37000035" ~ "r"), 
         CA2011QF = case_when(CA2011 == "S12000047" | 
                              CA2011 == "S12000048" | 
                              CA2011 == "S12000049" | 
                              CA2011 == "S12000050" ~ "r")) %>% 
  select(DZ2011:CA2011, CA2011QF, HSCP2016, HSCP2016QF, HB2014, HB2014QF, 
         everything())


### 4 - Save file to open data folders ----

# UPDATE FILEPATH WITH CURRENT DATE
write_csv(datazone_simd, file.path(open_data_filepath, "simd2016_23092019.csv"))
