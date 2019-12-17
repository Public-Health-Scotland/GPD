##########################################################
# Updating Deprivation Files
# Calum Purdie
# Original date 15/08/2018
# Latest update author - Calum Purdie
# Latest update date - 02/12/2019
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for updating postcode deprivation files for new postcode directory
# Approximate run time - 8 minutes
##########################################################

### 1 - Housekeeping ----

# Load libraries

library(magrittr)
library(tidyr)
library(dplyr)
library(readr)
library(tidylog)
library(glue)

# Set filepaths

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD")
geo_filepath <- file.path(base_filepath, "1_Geography", 
                          "Scottish Postcode Directory", "Lookup Files", 
                          "R Files")
dep_filepath <- file.path(base_filepath, "3_Deprivation")
simd_filepath <- file.path(dep_filepath, "SIMD", "Lookup Files", "SIMD 2016")
data_filepath <- file.path(dep_filepath, "Postcode Deprivation", "Lookup Files")
carstairs_filepath <- file.path(dep_filepath, "Carstairs", "Lookup Files")
output_filepath <- file.path(dep_filepath, "Postcode Deprivation", 
                             "Lookup Files", "R Files")

spd_version <- "2019_2"
spd_prev_version <- "2019_1.5"

# Set number of decimal places to make sure R and SPSS files match

options(digits = 15)



### 2 - Postcode-SIMD file ----

# Import DataZone2011_simd2016.csv file

dz2011_simd2016 <- read_csv(glue("{simd_filepath}/DataZone2011_simd2016.csv"))

# Open the most recent version of the postcode file

spd <- readRDS(glue("{geo_filepath}/Scottish_Postcode_Directory_", 
                           "{spd_version}.rds"))

# Keep pc7, DataZone2011, IntZone2011 and all CA, HB and HSCP variables
# Sort cases by DataZone2011
# Match on simd information
# Sort by pc7 and rearrange order of variables

postcode_simd2016 <- spd %>% 
  select(pc7, DataZone2011, DataZone2011Name, IntZone2011, IntZone2011Name, 
         CA2019, CA2019Name, CA2018, CA2011, HB2019, HB2019Name, HB2018, HB2014, 
         HSCP2019, HSCP2019Name, HSCP2018, HSCP2016) %>%
  rename(CA2019_new = CA2019, HB2019_new = HB2019, HSCP2019_new = HSCP2019) %>% 
  arrange(DataZone2011) %>% 
  left_join(dz2011_simd2016) %>% 
  arrange(pc7) %>% 
  select(-c(HB2019, HSCP2019, CA2019)) %>% 
  select(pc7, DataZone2011, DataZone2011Name, IntZone2011, IntZone2011Name, 
         HB2019_new, HB2019Name, HB2018, HB2014, HSCP2019_new, HSCP2019Name, 
         HSCP2018, HSCP2016, CA2019_new, CA2019Name, CA2018, 
         CA2011:simd2016_crime_rank) %>% 
  rename(CA2019 = CA2019_new, HB2019 = HB2019_new, HSCP2019 = HSCP2019_new)

# Save as .RDS file - update filename to reflect most recent version of 
# postcode file

saveRDS(postcode_simd2016, 
        glue("{output_filepath}/postcode_{spd_version}_simd2016.rds"))



### 3 - Update Postcode All SIMD and Carstairs file ----

### 3.1 - Get SIMD2016 data from most recent postcode_SIMD2016 file ----

# Match to spd
# Select pc7, OA2001 and OA2011 from SPD_2019_2
# Calum found an issue with OA2001 codes changing so rename this to OA2001_new 
# and use this version for all OA2001 codes
# Rename DataZone2011_simd2016

postcode_all_simd_carstairs <- spd %>%
  select(pc7, OA2001, OA2011) %>%
  rename(OA2001_new = OA2001) %>%
  left_join(postcode_simd2016, by = "pc7") %>%
  rename(DataZone2011_simd2016 = DataZone2011)



### 3.2 - Get older SIMD data from postcode_simd files ----

# Due to an improved boundary location method using the NRS Scottish Address 
# Directory, a very small number of postcodes (around 0.1%) have changed 
# geographies between SPD 2017_1 and now
# Remove HB, CA and HSCP and CHP columns if applicable to ensure we use the most 
# recent version

# Rename OA2001 for each postcode version

# Read in postcode_2016_1_simd2012 file

postcode_2016_1_simd2012 <- read_csv(glue("{data_filepath}/", 
                                          "postcode_2016_1_simd2012.csv")) %>%
  rename(OA2001_2016_1 = OA2001) %>%
  select(-c(HB2019, HB2018, HB2014, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, 
            HSCP2016))

# Read in SPD_2012_2_simd2009v2 file

postcode_2012_2_simd2009v2 <- read_csv(glue("{data_filepath}/", 
                                            "postcode_2012_2_simd2009v2.csv")) %>%
  rename(pc7 = PC7, OA2001_2012_2 = OA2001) %>%
  select(-c(HB2019, HB2018, HB2014, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, 
            HSCP2016, HB2006, CHP2007, CHP2011, CHP2011subarea, CHP2012))

# Read in postcode_2009_2_simd2006 file

postcode_2009_2_simd2006 <- read_csv(glue("{data_filepath}/", 
                                     "postcode_2009_2_simd2006.csv")) %>%
  rename(pc7 = PC7, OA2001_2009_2 = OA2001) %>%
  select(-c(HB2019, HB2018, HB2014, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, 
            HSCP2016, HB2006))

# Read in postcode_2006_2_simd2004 file

postcode_2006_2_simd2004 <- read_csv(glue("{data_filepath}/", 
                                          "postcode_2006_2_simd2004.csv")) %>%
  rename(pc7 = PC7, OA2001_2006_2 = OA2001) %>%
  select(-c(HB2019, HB2018, HB2014, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, 
            HSCP2016, HB2006))



### 3.3 - Match SIMD data to current file ----

# Match to SPD_all_simd_carstairs file
# Rename DataZone2001 for each simd version

postcode_all_simd_carstairs %<>%
  left_join(postcode_2016_1_simd2012, by = "pc7") %>%
  rename(DataZone2001_simd2012 = DataZone2001)%>%
  left_join(postcode_2012_2_simd2009v2, by = "pc7") %>%
  rename(DataZone2001_simd2009v2 = DataZone2001)%>%
  left_join(postcode_2009_2_simd2006, by = "pc7") %>%
  rename(DataZone2001_simd2006 = DataZone2001)%>%
  left_join(postcode_2006_2_simd2004, by = "pc7") %>%
  rename(DataZone2001_simd2004 = DataZone2001)



### - 3.4 Fix OA2001 Codes ----

# Delete the old OA2001 codes and rename OA2001_new to be OA2001.

postcode_all_simd_carstairs %<>%
  select(-c(OA2001_2016_1, OA2001_2012_2, OA2001_2009_2, OA2001_2006_2)) %>%
  rename(OA2001 = OA2001_new) %>%
  arrange(OA2001)



### 3.5 - Tidy Global Environment ----

# Tidy up the files in the global environment to ensure there isn't issues with 
# file size

rm(dz2011_simd2016, postcode_2006_2_simd2004, postcode_2009_2_simd2006, 
   postcode_2012_2_simd2009v2, postcode_2016_1_simd2012, postcode_simd2016, spd)
gc()



### 4 - Match on Carstairs Data ----

# Read in OA2001_Lookup file

OA2001_Lookup <- read_csv(glue("{carstairs_filepath}/OA2001_Lookup.csv"))

# Read in OA2001_carstairs file

OA2001_carstairs <- read_csv(glue("{carstairs_filepath}/oa2001_carstairs.csv"))

# Read in OA2011_carstairs2011 file

OA2011_carstairs2011 <- read_csv(glue("{carstairs_filepath}/", 
                                      "OA2011_carstairs2011.csv"))

# Sort by OA2001 and rename it to OutputArea2001Code
# Match OA2001_Lookup, OA2001_carstairs and OA2011_carstairs2011 to 
# postcode_all_simd_carstairs

postcode_all_simd_carstairs %<>%
  arrange(OA2001) %>%
  rename(OutputArea2001Code = OA2001) %>%
  left_join(OA2001_Lookup, by = "OutputArea2001Code") %>%
  left_join(OA2001_carstairs, by = "OA2001") %>%
  left_join(OA2011_carstairs2011)

# Sort by pc7
# Delete OA2001 and rename OutputArea2001Code as OA2001
# Reorder variables

postcode_all_simd_carstairs %<>%
  arrange(pc7) %>%
  select(-'OA2001') %>%
  rename(OA2001 = OutputArea2001Code) %>%
  select(pc7, DataZone2011_simd2016, DataZone2011Name, 
         simd2016rank:simd2016_crime_rank, DataZone2001_simd2012, 
         simd2012score:DataZone2001_simd2004, 
         simd2004score:simd2004_access_rank, OA2011, 
         pcsec2011:carstairs2011_hb2014_decile, OA2001, pcsec2001, 
         carstairs2001score:carstairs2001_hb_decile, 
         OA1991:carstairs1981_sc_decile)

# Save file as .RDS
saveRDS(postcode_all_simd_carstairs, 
        glue("{output_filepath}/postcode_{spd_version}_all_simd_carstairs.rds"))

write_csv(postcode_all_simd_carstairs, 
          glue("{output_filepath}/",
               "postcode_{spd_version}_all_simd_carstairs.csv"), na = "")



### 5 - Checks - Check Old File Against New ----

# Number of postcodes on current file missing SIMD Data
# From 2017-1 all postcodes should now have SIMD Data

postcode_all_simd_carstairs %>% count(simd2016_sc_quintile)

# Number of postcodes on previous file missing SIMD Data
# None missing a SIMD quantile
# Update file name as required

postcode_all_simd_carstairs_prev <- readRDS(glue("{output_filepath}/Archive/", 
                                                 "postcode_{spd_prev_version}", 
                                                 "_all_simd_carstairs.rds"))

postcode_all_simd_carstairs_prev %>% count(simd2016_sc_quintile)
