### 1 - Information ----

# Codename - Update Deprivation Files
# Data release - Scottish Postcode Directory
# Original Author - Calum Purdie
# Original Date - 15/08/2018
# Type - Update
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readr")
#
# Description - This document is based on the SPSS syntax for updating deprivation files found 
#               within GPD folders. It is designed to allow for the same data to be inputted and  
#               to provide the same output files.
#
# Approximate run time - 8 minutes

# Read in packages from library
library(tidyr)
library(dplyr)
library(readr)
library(tidylog)

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD")
geo_filepath <- file.path(base_filepath, "1_Geography", "Scottish Postcode Directory", "Lookup Files", "R Files")
dep_filepath <- file.path(base_filepath, "3_Deprivation")
simd_filepath <- file.path(dep_filepath, "SIMD", "Lookup Files", "SIMD 2016")
data_filepath <- file.path(dep_filepath, "Postcode Deprivation", "Lookup Files")
carstairs_filepath <- file.path(dep_filepath, "Carstairs", "Lookup Files")
output_filepath <- file.path(dep_filepath, "Postcode Deprivation", "Lookup Files", "R Files")

# To make sure R and SPSS files match
options(digits = 15)

### 2 - Postcode-SIMD file ----

# Import DataZone2011_simd2016.csv file

DZ2011_SIMD2016 <- read_csv(file.path(simd_filepath, "DataZone2011_simd2016.csv"))

# Open the most recent version of the postcode file - UPDATE FILE NAME AS REQUIRED

SPD_2019_2 <- readRDS(file.path(geo_filepath, "Scottish_Postcode_Directory_2019_2.rds"))

# Keep pc7, DataZone2011, IntZone2011 and all CA, HB and HSCP variables
# Sort cases by DataZone2011
# Match on SIMD 2016
# Sort by pc7 and rearrange order of variables

postcode_2019_2_simd2016 <- SPD_2019_2 %>% 
  select(pc7, DataZone2011, DataZone2011Name, IntZone2011, IntZone2011Name, CA2019, CA2019Name, CA2018, CA2011, 
         HB2019, HB2019Name, HB2018, HB2014, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016) %>%
  rename(CA2019_new = CA2019, HB2019_new = HB2019, HSCP2019_new = HSCP2019) %>% 
  arrange(DataZone2011) %>% 
  left_join(DZ2011_SIMD2016) %>% 
  arrange(pc7) %>% 
  select(-c(HB2019, HSCP2019, CA2019)) %>% 
  select(pc7, DataZone2011, DataZone2011Name, IntZone2011, IntZone2011Name, HB2019_new, HB2019Name, HB2018, HB2014, 
         HSCP2019_new, HSCP2019Name, HSCP2018, HSCP2016, CA2019_new, CA2019Name, CA2018, CA2011:simd2016_crime_rank) %>% 
  rename(CA2019 = CA2019_new, HB2019 = HB2019_new, HSCP2019 = HSCP2019_new)

# Save as .RDS file - update filename to reflect most recent version of postcode file
saveRDS(postcode_2019_2_simd2016, file.path(output_filepath, "postcode_2019_2_simd2016.rds"))



### 3 - Update Postcode All SIMD and Carstairs file ----

### 3.1 - Match on 2016 SIMD from the most recent SIMD 2016 postcode lookup file ----

# Match to SPD_2019_2
# Select pc7, OA2001 and OA2011 from SPD_2019_2
# Calum found an issue with OA2001 codes changing so rename this to OA2001_new and use this version for all OA2001 codes
# Rename DataZone2011_simd2016

postcode_2019_2_all_simd_carstairs <- SPD_2019_2 %>%
  select(pc7, OA2001, OA2011) %>%
  rename(OA2001_new = OA2001) %>%
  left_join(postcode_2019_2_simd2016, by = "pc7") %>%
  rename(DataZone2011_simd2016 = DataZone2011)

### 3.2 - Match on 2012 SIMD from the most recent SIMD 2012 postcode lookup file ----

# Read in postcode_2016_1_simd2012 file

# Rename OA2001 to OA2001_2016_1
# Due to an improved boundary location method using the NRS Scottish Address Directory, a very
# small number of postcodes (around 0.1%) have changed geographies between SPD 2017_1 and now
# Remove HB, CA and HSCP columns to ensure we use the most recent version

postcode_2016_1_simd2012 <- read_csv(file.path(data_filepath, "postcode_2016_1_simd2012.csv")) %>%
  rename(OA2001_2016_1 = OA2001) %>%
  select(-c(HB2019, HB2018, HB2014, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, HSCP2016))

# Match to SPD_2019_2_all_simd_carstairs file
# Rename DataZone2001 to DataZone2001_simd2012
postcode_2019_2_all_simd_carstairs <- postcode_2019_2_all_simd_carstairs %>%
  left_join(postcode_2016_1_simd2012, by = "pc7") %>%
  rename(DataZone2001_simd2012 = DataZone2001)


### 3.3 - Match on 2009 SIMD from the most recent SIMD 2009 postcode lookup file ----

# Read in SPD_2012_2_simd2009v2 file

# pc7 called PC7 in 2009v2 file
# Rename OA2001 to OA2001_2012_2
# Remove HB2019, HB2018, HB2014, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, HSCP2016, HB2006, CHP2007, CHP2011, CHP2011subarea, CHP2012

postcode_2012_2_simd2009v2 <- read_csv(file.path(data_filepath, "postcode_2012_2_simd2009v2.csv")) %>%
  rename(pc7 = PC7, OA2001_2012_2 = OA2001) %>%
  select(-c(HB2019, HB2018, HB2014, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, HSCP2016, HB2006, CHP2007, CHP2011, CHP2011subarea, CHP2012))

# Match to SPD_2019_2_all_simd_carstairs file
# Rename DataZone2001 to DataZone2001_simd2009v2

postcode_2019_2_all_simd_carstairs <- postcode_2019_2_all_simd_carstairs %>%
  left_join(postcode_2012_2_simd2009v2, by = "pc7") %>%
  rename(DataZone2001_simd2009v2 = DataZone2001)


### 3.4 - Match on SIMD 2006 from the most recent SIMD 2006 postcode lookup file ----

# Read in SPD_2009_2_simd2006 file

# pc7 called PC7 in 2009v2 file
# Rename OA2001 to OA2001_2012_2
# Remove HB2019, HB2018, HB2014, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, HSCP2016, HB2006

postcode_2009_2_simd2006 <- read_csv(file.path(data_filepath, "postcode_2009_2_simd2006.csv")) %>%
  rename(pc7 = PC7, OA2001_2009_2 = OA2001) %>%
  select(-c(HB2019, HB2018, HB2014, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, HSCP2016, HB2006))

# Match to SPD_2019_2_all_simd_carstairs file
# Rename DataZone2001 to DataZone2001_simd2009v2

postcode_2019_2_all_simd_carstairs <- postcode_2019_2_all_simd_carstairs %>%
  left_join(postcode_2009_2_simd2006, by = "pc7") %>%
  rename(DataZone2001_simd2006 = DataZone2001)


### 3.5 - Match on SIMD 2004 from the most recent SIMD 2004 postcode lookup file ----

# Read in SPD_2006_2_simd2004 file

# pc7 called PC7 in 2009v2 file
# Rename OA2001 to OA2001_2012_2
# Remove HB2019, HB2018, HB2014, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, HSCP2016, HB2006
postcode_2006_2_simd2004 <- read_csv(file.path(data_filepath, "postcode_2006_2_simd2004.csv")) %>%
  rename(pc7 = PC7, OA2001_2006_2 = OA2001) %>%
  select(-c(HB2019, HB2018, HB2014, CA2019, CA2018, CA2011, HSCP2019, HSCP2018, HSCP2016, HB2006))

# Match to SPD_2019_2_all_simd_carstairs file
# Rename DataZone2001 to DataZone2001_simd2009v2
postcode_2019_2_all_simd_carstairs <- postcode_2019_2_all_simd_carstairs %>%
  left_join(postcode_2006_2_simd2004, by = "pc7") %>%
  rename(DataZone2001_simd2004 = DataZone2001)


### - 3.6 Fix OA2001 Codes ----

# Delete the old OA2001 codes and rename OA2001_new to be OA2001.
postcode_2019_2_all_simd_carstairs <- postcode_2019_2_all_simd_carstairs %>%
  select(-c(OA2001_2016_1, OA2001_2012_2, OA2001_2009_2, OA2001_2006_2)) %>%
  rename(OA2001 = OA2001_new) %>%
  arrange(OA2001)


### 3.7 - Tidy Global Environment ----

# Tidy up the files in the global environment to ensure there isn't issues with file size

rm(DZ2011_SIMD2016, postcode_2006_2_simd2004, postcode_2009_2_simd2006, postcode_2012_2_simd2009v2, postcode_2016_1_simd2012, 
   postcode_2019_2_simd2016, SPD_2019_2)
gc()


### 4 - Match on Carstairs ----

# Read in OA2001_Lookup file
OA2001_Lookup <- read_csv(file.path(carstairs_filepath, "OA2001_Lookup.csv"))

# Sort by OA2001 and rename it to OutputArea2001Code
# Match to SPD_2019_2_all_simd_carstairs by postcode_2019_2_all_simd_carstairs
postcode_2019_2_all_simd_carstairs <- postcode_2019_2_all_simd_carstairs %>%
  arrange(OA2001) %>%
  rename(OutputArea2001Code = OA2001) %>%
  left_join(OA2001_Lookup, by = "OutputArea2001Code")

# Read in OA2001_carstairs file
OA2001_carstairs <- read_csv(file.path(carstairs_filepath, "oa2001_carstairs.csv"))

# Sort by OA2001 and match onto postcode_2019_2_all_simd_carstairs
postcode_2019_2_all_simd_carstairs <- postcode_2019_2_all_simd_carstairs %>%
  arrange(OA2001) %>%
  left_join(OA2001_carstairs, by = "OA2001")

# Read in OA2011_carstairs2011 file
OA2011_carstairs2011 <- read_csv(file.path(carstairs_filepath, "OA2011_carstairs2011.csv"))

# Sort by OA2011 and match onto postcode_2019_2_all_simd_carstairs
postcode_2019_2_all_simd_carstairs <- postcode_2019_2_all_simd_carstairs %>%
  arrange(OA2011) %>%
  left_join(OA2011_carstairs2011)

# Sort by pc7
# Delete OA2001 and rename OutputArea2001Code as OA2001
# Reorder variables
postcode_2019_2_all_simd_carstairs <- postcode_2019_2_all_simd_carstairs %>%
  arrange(pc7) %>%
  select(-'OA2001') %>%
  rename(OA2001 = OutputArea2001Code) %>%
  select(pc7, DataZone2011_simd2016, DataZone2011Name, simd2016rank:simd2016_crime_rank, DataZone2001_simd2012, 
         simd2012score:DataZone2001_simd2004, simd2004score:simd2004_access_rank, OA2011, 
         pcsec2011:carstairs2011_hb2014_decile, OA2001, pcsec2001, 
         carstairs2001score:carstairs2001_hb_decile, OA1991:carstairs1981_sc_decile)

# Save file as .RDS
saveRDS(postcode_2019_2_all_simd_carstairs, file.path(output_filepath, "postcode_2019_2_all_simd_carstairs.rds"))

write_csv(postcode_2019_2_all_simd_carstairs, file.path(output_filepath, "postcode_2019_1.5_all_simd_carstairs.csv"), na = "")

### 5 - Checks - Check Old File Against New ----

# Number of postcodes on current file missing SIMD Data
# From 2017-1 all postcodes should now have SIMD Data
postcode_2019_2_all_simd_carstairs %>% count(simd2016_sc_quintile)

# Number of postcodes on previous file missing SIMD Data
# None missing a SIMD quantile
# Update file name as required
postcode_2019_1.5_all_simd_carstairs <- readRDS(file.path(output_filepath, "postcode_2019_1.5_all_simd_carstairs.rds"))
postcode_2019_1.5_all_simd_carstairs %>% count(simd2016_sc_quintile)
