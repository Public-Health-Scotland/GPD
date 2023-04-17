##########################################################
# Create SIMD file for NHSScotland Open Data
# Calum Purdie
# Original date 23/09/2019
# Latest update author - Calum Purdie
# Latest update date - 02/06/2020
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code to create Data Zone - SIMD lookup files for the NHSScotland open data 
# platform
# Approximate run time - <1 second
##########################################################

##########################################################
### 1 - Housekeeping ----
##########################################################

##############################################
### 1.1 - Load Packages ----
##############################################

library(magrittr)
library(tidyr)
library(dplyr)
library(tidylog)
library(glue)
library(readr)

##############################################
### 1.2 -  Set filepaths ---- 
##############################################

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI")

lookup_filepath <- glue("{base_filepath}/Referencing & Standards/GPD/",
                        "3_Deprivation/SIMD/Lookup Files/SIMD 2020")

open_data_filepath <- glue("{base_filepath}/Publications/", 
                           "Open Data (Non Health Topic)/Data/", 
                           "OD1700038 - SIMD")
##############################################
### 1.3 - Set date and file to use
##############################################

date <- strftime(Sys.Date(), format = "%d%m%Y")

file <- "DataZone2011_simd2020v2"

##########################################################
### 2 - Read in lookup file ----
##########################################################

datazone_simd <- readRDS(glue("{lookup_filepath}/{file}.rds")) %>% 
  select(datazone2011, intzone2011, hb2019, hscp2019, ca2019, 
         simd2020v2_rank:simd2020v2bt15)

############################
# Rename columns to match open data format
############################
colnames(datazone_simd) <- c("DataZone", "IntZone", "HB", "HSCP", "CA", 
                             "SIMD2020V2Rank", "SIMD2020V2CountryDecile", 
                             "SIMD2020V2CountryQuintile", "SIMD2020V2HBDecile", 
                             "SIMD2020V2HBQuintile", "SIMD2020V2HSCPDecile", 
                             "SIMD2020V2HSCPQuintile", "SIMD2020V2CADecile", 
                             "SIMD2020V2CAQuintile", "SIMD2020V2Most15pc", 
                             "SIMD2020V2Least15pc")

##########################################################
### 3 - Add Qualifier columns ----
##########################################################

############################
# Due to minor boundary changes from 02/02/2018 (Keltybridge) and 01/04/2019 
# (Cardowan by Stepps) which affeced some HB, HSCP and CA codes, a qualifier 
# field is required to show which codes have been revised
# Select the columns in the correct order for uploading
############################

# datazone_simd %<>% 
#   mutate(HB2014QF = case_when(HB2014 == "S08000029" | 
#                               HB2014 == "S08000030" | 
#                               HB2014 == "S08000031" | 
#                               HB2014 == "S08000032" ~ "r"), 
#          HSCP2016QF = case_when(HSCP2016 == "S37000032" | 
#                                 HSCP2016 == "S37000033" | 
#                                 HSCP2016 == "S37000034" | 
#                                 HSCP2016 == "S37000035" ~ "r"), 
#          CA2011QF = case_when(CA2011 == "S12000047" | 
#                               CA2011 == "S12000048" | 
#                               CA2011 == "S12000049" | 
#                               CA2011 == "S12000050" ~ "r")) %>% 
#   select(DZ2011:IZ2011, CA2011, CA2011QF, HSCP2016, HSCP2016QF, HB2014, HB2014QF, 
#          everything())

##########################################################
### 4 - Save file to open data folders ----
##########################################################

write_csv(datazone_simd, glue("{open_data_filepath}/simd2020V2_{date}.csv"), 
          na = "")

##########################################################
##########################################################
################### End of script ---- ###################
##########################################################
##########################################################