##########################################################
# Name of file: Create SIMD2004-2012 Files for CKAN
# Original author(s): Tina Fu
# Original date: 12/09/2019
# Latest update author: Calum Purdie
# Latest update date: 23/06/20120
# Latest update description: Update SIMD2009 to SIMD2009V"
# Type of script: data preparation
# Written/run on: RStudio desktop
# Version of R that the script was most recently run on: R 3.5.2
# Description of content: To create SIMD2004-2012 files for CKAN.
# It is an one-off script and doesn't need to be run once the files are uploaded.
# Approximate run time: 30 seconds.
##########################################################


### 1 - Housekeeping ----

# Load libraries
library(dplyr)
library(httr)
library(here)
library(ckanr)
library(readr)
library(tidylog)

# Set filepath
simd_files <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                        "Referencing & Standards", "GPD", "3_Deprivation", 
                        "SIMD", "Lookup Files")

open_data_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                                "Publications", "Open Data (Non Health Topic)", 
                                "Data", "OD1700038 - SIMD")

# Set open data resource data

source(here("Geography", "Scottish Postcode Directory", 
            "Set httr configuration for API.R"))

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id <- "e92d19d4-ced7-40c8-b628-e28e4528fc41"

### 2 - Get DZ2001 lookup from CKAN ----

dz2001_lookup <- dplyr::tbl(src = ckan$con, from = res_id) %>%  
  as_tibble() %>% 
  select(DataZone, IntZone, CA, HSCP, HB)



### 3 - Create SIMD2012 file ----

simd2012 <- readRDS(file.path(simd_files, "SIMD 2012", 
                              "DataZone2001_simd2012.rds")) %>% 
  select(DataZone2001, simd2012rank:simd2012_hscp_quintile, 
         simd2012tp15, simd2012bt15) %>% 
  rename(DZ2001 = DataZone2001,
         SIMD2012Rank = simd2012rank, 
         SIMD2012CountryQuintile = simd2012_sc_quintile, 
         SIMD2012CountryDecile = simd2012_sc_decile, 
         SIMD2012HB2006Quintile = simd2012_hb2006_quintile,
         SIMD2012HB2006Decile = simd2012_hb2006_decile,
         SIMD2012HB2014Quintile = simd2012_hb2014_quintile,
         SIMD2012HB2014Decile = simd2012_hb2014_decile,
         SIMD2012CA2011Quintile = simd2012_ca_quintile,
         SIMD2012CA2011Decile = simd2012_ca_decile,
         SIMD2012HSCP2016Quintile = simd2012_hscp_quintile,
         SIMD2012HSCP2016Decile = simd2012_hscp_decile,
         SIMD2012Most15pc = simd2012tp15,
         SIMD2012Least15pc = simd2012bt15) %>% 
  left_join(dz2001_lookup, by = "DZ2001") %>% 
  select(DZ2001, IZ2001:HSCP2016QF, HB2006:HB2014QF, 
         SIMD2012Rank:SIMD2012Least15pc)

write_csv(simd2012, file.path(open_data_filepath, "simd2012_01042019.csv"))

### 4 - Create SIMD2009 file----
simd2009 <- readRDS(file.path(simd_files, "SIMD 2009", 
                              "DataZone2001_simd2009v2.rds")) %>% 
  select(DataZone2001, simd2009v2rank:simd2009v2_hscp_decile, 
         simd2009v2tp15, simd2009v2bt15) %>% 
  rename(DataZone = DataZone2001,
         SIMD2009V2Rank = simd2009v2rank, 
         SIMD2009V2CountryQuintile = simd2009v2_sc_quintile, 
         SIMD2009V2CountryDecile = simd2009v2_sc_decile, 
         SIMD2009V2HBQuintile = simd2009v2_hb2014_quintile,
         SIMD2009V2HBDecile = simd2009v2_hb2014_decile,
         SIMD2009V2CAQuintile = simd2009v2_ca_quintile,
         SIMD2009V2CADecile = simd2009v2_ca_decile,
         SIMD2009V2HSCPQuintile = simd2009v2_hscp_quintile,
         SIMD2009V2HSCPDecile = simd2009v2_hscp_decile,
         SIMD2009V2Most15pc = simd2009v2tp15,
         SIMD2009V2Least15pc = simd2009v2bt15) %>% 
  left_join(dz2001_lookup) %>% 
  select(DataZone, IntZone, CA, HSCP, HB, 
         SIMD2009V2Rank, SIMD2009V2CountryDecile, SIMD2009V2CountryQuintile, 
         SIMD2009V2HBDecile, SIMD2009V2HBQuintile, SIMD2009V2HSCPDecile, 
         SIMD2009V2HSCPQuintile, SIMD2009V2CADecile, SIMD2009V2CAQuintile, 
         SIMD2009V2Most15pc, SIMD2009V2Least15pc)

write_csv(simd2009, file.path(open_data_filepath, "simd2009V2_23062019.csv"))

### 5 - Create SIMD2006 file----
simd2006 <- readRDS(file.path(simd_files, "SIMD 2006", 
                              "DataZone2001_simd2006.rds")) %>% 
  select(DataZone2001, simd2006rank:simd2006bt15) %>% 
  rename(DZ2001 = DataZone2001,
         SIMD2006Rank = simd2006rank, 
         SIMD2006CountryQuintile = simd2006_sc_quintile, 
         SIMD2006CountryDecile = simd2006_sc_decile, 
         SIMD2006HB2006Quintile = simd2006_hb2006_quintile,
         SIMD2006HB2006Decile = simd2006_hb2006_decile,
         SIMD2006HB2014Quintile = simd2006_hb2014_quintile,
         SIMD2006HB2014Decile = simd2006_hb2014_decile,
         SIMD2006CA2011Quintile = simd2006_ca_quintile,
         SIMD2006CA2011Decile = simd2006_ca_decile,
         SIMD2006HSCP2016Quintile = simd2006_hscp_quintile,
         SIMD2006HSCP2016Decile = simd2006_hscp_decile,
         SIMD2006Most15pc = simd2006tp15,
         SIMD2006Least15pc = simd2006bt15) %>% 
  left_join(dz2001_lookup, by = "DZ2001") %>% 
  select(DZ2001, IZ2001:HSCP2016QF, HB2006:HB2014QF, 
         SIMD2006Rank:SIMD2006Least15pc)

write_csv(simd2006, file.path(open_data_filepath, "simd2006_01042019.csv"))

### 6 - Create SIMD2004 file----
simd2004 <- readRDS(file.path(simd_files, "SIMD 2004", 
                              "DataZone2001_simd2004.rds")) %>% 
  select(DataZone2001, simd2004rank:simd2004bt15) %>% 
  rename(DZ2001 = DataZone2001,
         SIMD2004Rank = simd2004rank, 
         SIMD2004CountryQuintile = simd2004_sc_quintile, 
         SIMD2004CountryDecile = simd2004_sc_decile, 
         SIMD2004HB1995Quintile = simd2004_hb1995_quintile,
         SIMD2004HB1995Decile = simd2004_hb1995_decile,
         SIMD2004HB2006Quintile = simd2004_hb2006_quintile,
         SIMD2004HB2006Decile = simd2004_hb2006_decile,
         SIMD2004HB2014Quintile = simd2004_hb2014_quintile,
         SIMD2004HB2014Decile = simd2004_hb2014_decile,
         SIMD2004CA2011Quintile = simd2004_ca_quintile,
         SIMD2004CA2011Decile = simd2004_ca_decile,
         SIMD2004HSCP2016Quintile = simd2004_hscp_quintile,
         SIMD2004HSCP2016Decile = simd2004_hscp_decile,
         SIMD2004Most15pc = simd2004tp15,
         SIMD2004Least15pc = simd2004bt15) %>% 
  left_join(dz2001_lookup, by = "DZ2001") %>% 
  select(DZ2001, IZ2001:HB2014QF, 
         SIMD2004Rank:SIMD2004Least15pc)

write_csv(simd2004, file.path(open_data_filepath, "simd2004_01042019.csv"))

### End of script----


