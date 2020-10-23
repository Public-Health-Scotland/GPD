##########################################################
# Updating Deprivation Files
# Calum Purdie
# Original date 15/08/2018
# Latest update author - Calum Purdie
# Latest update date - 10/08/2020
# Latest update description - 2020_2 SPD update
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
library(janitor)
library(data.table)

# Set filepaths

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD")
geo_filepath <- glue("{base_filepath}/1_Geography/Scottish Postcode Directory/", 
                     "Lookup Files/R Files")
dep_filepath <- glue("{base_filepath}/3_Deprivation")
simd_filepath <- glue("{dep_filepath}/SIMD/Lookup Files/SIMD 2020")
data_filepath <- glue("{dep_filepath}/Postcode Deprivation/Lookup Files")
carstairs_filepath <- glue("{dep_filepath}/Carstairs/Lookup Files")
output_filepath <- glue("{dep_filepath}/Postcode Deprivation/Lookup Files/", 
                        "R Files")

spd_version <- "2020_2"
spd_prev_version <- "2020_1"

# Set number of decimal places to make sure R and SPSS files match

options(digits = 15)



### 2 - Postcode-SIMD file ----

# Import DataZone2011_simd2010.rds file

dz2011_simd2020v2 <- readRDS(glue("{simd_filepath}/", 
                                  "DataZone2011_simd2020v2.rds")) %>% 
  select(datazone2011, simd2020v2_rank:simd2020v2_crime_rank)

# Open the most recent version of the postcode file

spd <- readRDS(glue("{geo_filepath}/Scottish_Postcode_Directory_", 
                    "{spd_version}.rds"))

# Keep pc7, DataZone2011, IntZone2011 and all CA, hb and hscp variables
# Sort cases by DataZone2011
# Match on simd information
# Sort by pc7 and rearrange order of variables

postcode_simd2020v2 <- spd %>% 
  select(pc8, pc7, datazone2011, datazone2011name, intzone2011, intzone2011name, 
         hb2019, hb2019name, hb2018, hb2014, hscp2019, hscp2019name, hscp2018, 
         hscp2016, ca2019, ca2019name, ca2018, ca2011) %>%
  arrange(datazone2011) %>% 
  left_join(dz2011_simd2020v2) %>% 
  arrange(pc7)

# Set all blank cells as NA in R file

postcode_simd2020v2 %<>%
  mutate_if(is.character, list(~na_if(., "")))

# Save as .RDS file - update filename to reflect most recent version of 
# postcode file

saveRDS(postcode_simd2020v2, 
        glue("{output_filepath}/postcode_{spd_version}_simd2020v2.rds"))



### 3 - Update Postcode All SIMD and Carstairs file ----

### 3.1 - Get SIMD2020 data from most recent postcode_simd2020v2 file ----

# Match to spd
# Select pc7, oa2001 and oa2011 from SPD_2019_2
# Calum found an issue with oa2001 codes changing so rename this to oa2001_new 
# and use this version for all oa2001 codes
# Rename DataZone2011_simd2020

postcode_all_simd_carstairs <- spd %>%
  select(pc8, pc7, oa2001, oa2011) %>%
  rename(oa2001_new = oa2001) %>%
  left_join(postcode_simd2020v2) %>%
  rename(datazone2011_simd2020v2 = datazone2011)



### 3.2 - Get older SIMD data from postcode_simd files ----

# Due to an improved boundary location method using the NRS Scottish Address 
# Directory, a very small number of postcodes (around 0.1%) have changed 
# geographies between SPD 2017_1 and now
# Remove hb, ca and hscp and chp columns if applicable to ensure we use the most 
# recent version

# Rename oa2001 for each postcode version

# Read in postcode_2019_2_simd2016 file

postcode_2019_2_simd2016 <- readRDS(glue("{data_filepath}/R Files/", 
                                         "postcode_2019_2_simd2016.rds")) %>%
  clean_names() %>% 
  rename(datazone2011 = data_zone2011, simd2016_rank = simd2016rank) %>% 
  select(-c(data_zone2011name, int_zone2011, int_zone2011name, hb2019, 
            hb2019name, hb2018, hb2014, ca2019, ca2019name, ca2018, ca2011, 
            hscp2019, hscp2019name, hscp2018, hscp2016))

# Read in postcode_2016_1_simd2012 file

postcode_2016_1_simd2012 <- fread(glue("{data_filepath}/", 
                                       "postcode_2016_1_simd2012.csv")) %>%
  clean_names() %>% 
  rename(oa2001_2016_1 = oa2001, datazone2001 = data_zone2001, 
         simd2012_score = simd2012score, simd2012_rank = simd2012rank) %>%
  select(-c(hb2019, hb2018, hb2014, ca2019, ca2018, ca2011, hscp2019, hscp2018, 
            hscp2016, hb2006, chp2007, chp2011, chp2011subarea, chp2012))

# Read in SPD_2012_2_simd2009v2 file

postcode_2012_2_simd2009v2 <- fread(glue("{data_filepath}/", 
                                         "postcode_2012_2_simd2009v2.csv")) %>%
  clean_names() %>% 
  rename(oa2001_2012_2 = oa2001, datazone2001 = data_zone2001, 
         simd2009v2_score = simd2009v2score, simd2009v2_rank = simd2009v2rank) %>%
  select(-c(hb2019, hb2018, hb2014, ca2019, ca2018, ca2011, hscp2019, hscp2018, 
            hscp2016, hb2006, chp2007, chp2011, chp2011subarea, chp2012))

# Read in postcode_2009_2_simd2006 file

postcode_2009_2_simd2006 <- fread(glue("{data_filepath}/", 
                                       "postcode_2009_2_simd2006.csv")) %>%
  clean_names() %>% 
  rename(oa2001_2009_2 = oa2001, datazone2001 = data_zone2001, 
         simd2006_score = simd2006score, simd2006_rank = simd2006rank) %>%
  select(-c(hb2019, hb2018, hb2014, ca2019, ca2018, ca2011, hscp2019, hscp2018, 
            hscp2016, hb2006))

# Read in postcode_2006_2_simd2004 file

postcode_2006_2_simd2004 <- fread(glue("{data_filepath}/", 
                                       "postcode_2006_2_simd2004.csv")) %>%
  clean_names() %>% 
  rename(oa2001_2006_2 = oa2001, datazone2001 = data_zone2001, 
         simd2004_score = simd2004score, simd2004_rank = simd2004rank) %>%
  select(-c(hb2019, hb2018, hb2014, ca2019, ca2018, ca2011, hscp2019, hscp2018, 
            hscp2016, hb2006))



### 3.3 - Match SIMD data to current file ----

# Match to SPD_all_simd_carstairs file
# Rename DataZone2001 for each simd version

postcode_all_simd_carstairs %<>%
  left_join(postcode_2019_2_simd2016) %>%
  rename(datazone2011_simd2016 = datazone2011) %>%
  left_join(postcode_2016_1_simd2012) %>%
  rename(datazone2001_simd2012 = datazone2001) %>%
  left_join(postcode_2012_2_simd2009v2) %>%
  rename(datazone2001_simd2009v2 = datazone2001) %>%
  left_join(postcode_2009_2_simd2006) %>%
  rename(datazone2001_simd2006 = datazone2001) %>%
  left_join(postcode_2006_2_simd2004) %>%
  rename(datazone2001_simd2004 = datazone2001)



### - 3.4 Fix oa2001 Codes ----

# Delete the old oa2001 codes and rename oa2001_new to be oa2001.

postcode_all_simd_carstairs %<>%
  select(-c(oa2001_2016_1, oa2001_2012_2, oa2001_2009_2, oa2001_2006_2)) %>%
  rename(oa2001 = oa2001_new) %>%
  arrange(oa2001)



### 3.5 - Tidy Global Environment ----

# Tidy up the files in the global environment to ensure there isn't issues with 
# file size

rm(dz2011_simd2020v2, postcode_2006_2_simd2004, postcode_2009_2_simd2006, 
   postcode_2012_2_simd2009v2, postcode_2016_1_simd2012, 
   postcode_2019_2_simd2016, postcode_simd2020v2, spd)
gc()



### 4 - Match on Carstairs Data ----

# Read in oa2001_Lookup file

oa2001_lookup <- read_csv(glue("{carstairs_filepath}/oa2001_Lookup.csv")) %>% 
  clean_names() %>% 
  rename(oa2001code = output_area2001code)

# Read in oa2001_carstairs file

oa2001_carstairs <- read_csv(glue("{carstairs_filepath}/", 
                                  "oa2001_carstairs.csv")) %>% 
  clean_names()

# Read in oa2011_carstairs2011 file

oa2011_carstairs2011 <- read_csv(glue("{carstairs_filepath}/", 
                                      "OA2011_carstairs2011.csv")) %>% 
  clean_names()

# Sort by oa2001 and rename it to OutputArea2001Code
# Match oa2001_Lookup, oa2001_carstairs and oa2011_carstairs2011 to 
# postcode_all_simd_carstairs

postcode_all_simd_carstairs %<>%
  arrange(oa2001) %>%
  rename(oa2001code = oa2001) %>%
  left_join(oa2001_lookup) %>%
  left_join(oa2001_carstairs) %>%
  left_join(oa2011_carstairs2011)

# Sort by pc7
# Delete oa2001 and rename OutputArea2001Code as oa2001
# Reorder variables

postcode_all_simd_carstairs %<>%
  arrange(pc7) %>%
  select(-oa2001) %>%
  rename(oa2001 = oa2001code) %>%
  select(pc8, pc7, datazone2011_simd2020v2, datazone2011name, 
         simd2020v2_rank:simd2020v2_crime_rank, datazone2011_simd2016, 
         simd2016_rank:simd2016_crime_rank, datazone2001_simd2012, 
         simd2012_score:datazone2001_simd2004, 
         simd2004_score:simd2004_access_rank, oa2011, 
         pcsec2011:carstairs2011_hb2014_decile, oa2001, pcsec2001, 
         carstairs2001score:carstairs2001_hb_decile, 
         oa1991:carstairs1981_sc_decile)

# Format columns to be same data type

postcode_all_simd_carstairs %<>%
  mutate_at(vars(dplyr::contains("_quintile")), as.integer) %>% 
  mutate_at(vars(dplyr::contains("_decile")), as.integer)

# Set all blank cells as NA in R file

postcode_all_simd_carstairs %<>%
  mutate_if(is.character, list(~na_if(., "")))

# Save file as .RDS

saveRDS(postcode_all_simd_carstairs, 
        glue("{output_filepath}/postcode_{spd_version}_all_simd_carstairs.rds"))

# write_csv(postcode_all_simd_carstairs, 
#           glue("{data_filepath}/",
#                "postcode_{spd_version}_all_simd_carstairs.csv"), na = "")


### 5 - Checks - Check Old File Against New ----

# Number of postcodes on current file missing SIMD Data
# From 2017-1 all postcodes should now have SIMD Data

postcode_all_simd_carstairs %>% count(simd2020v2_sc_quintile)

# Number of postcodes on previous file missing SIMD Data
# None missing a SIMD quantile
# Update file name as required

postcode_all_simd_carstairs_prev <- readRDS(glue("{output_filepath}/Archive/", 
                                                 "postcode_{spd_prev_version}", 
                                                 "_all_simd_carstairs.rds"))

postcode_all_simd_carstairs_prev %>% count(simd2020v2_sc_quintile)
