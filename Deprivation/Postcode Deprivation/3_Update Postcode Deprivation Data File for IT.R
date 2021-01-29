##########################################################
# Update Postcode Deprivation Data File for IT
# Calum Purdie
# Original date 18/06/2020
# Latest update author - Calum Purdie
# Latest update date - 29/01/2021
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for updating postcode deprivation data for IT to upload to APXU/APXP
##########################################################

### 1 Housekeeping ----

# Load libraries

library(magrittr)
library(tidyr)
library(dplyr)
library(tidylog)
library(glue)
library(janitor)
library(readxl)
library(data.table)
library(writexl)

# Set filepaths

base_path <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                  "3_Deprivation")
lookups <- glue("{base_path}/Postcode Deprivation/Lookup Files/R Files")
carstairs <- glue("{base_path}/Carstairs/Lookup Files")
simd <- glue("{base_path}/SIMD/Lookup Files/SIMD 2020")
output <- glue("{base_path}/Postcode Deprivation/Lookup Files/Files for IT")
spd <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/1_Geography/",
            "Scottish Postcode Directory/Lookup Files")
dz <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/1_Geography/", 
           "Data Zone/Lookup Files")

# SPD Version

pc_version <- "2019_2"



### 2 Read in data ----

# Read in the postcode_all_simd_carstairs file for pc_version

dz2011_lookup <- readRDS(glue("{dz}/datazone2011_lookup.rds"))
dz2001_lookup <- readRDS(glue("{dz}/datazone2001_lookup.rds"))

pc_data <- readRDS(glue("{lookups}/Archive/", 
                        "postcode_{pc_version}_all_simd_carstairs.rds")) %>% 
  select(-c(simd2020_rank:simd2020_crime_rank)) %>% 
  rename(POSTCODE = pc7, 
         OA2001 = oa2001, 
         OA2011 = oa2011) %>% 
  mutate_if(is.character, list(~na_if(., "")))



### 3 SIMD2020v2 ----

# Match on DataZone2011_simd2020v2 data
# This step is only required due to using old (2019_2) postcode file

dz2011_simd2020v2 <- readRDS(glue("{simd}/DataZone2011_simd2020v2.rds")) %>% 
  select_at(vars(-contains("name"))) %>% 
  select(datazone2011, hb2019, hb2018, hb2014, ca2019, ca2018, ca2011, hscp2019, 
         hscp2018, hscp2016, simd2020v2_sc_quintile, simd2020v2_sc_decile, 
         simd2020v2_hb2019_quintile, simd2020v2_hb2019_decile, 
         simd2020v2_ca2019_quintile, simd2020v2_ca2019_decile, 
         simd2020v2_hscp2019_quintile, simd2020v2_hscp2019_decile, 
         simd2020v2tp15, simd2020v2bt15) %>% 
  rename(SIMD2020V2_DATAZONE = datazone2011, 
         SIMD2020V2_HB2019 = hb2019, 
         SIMD2020V2_HB2018 = hb2018, 
         SIMD2020V2_HB2014 = hb2014, 
         SIMD2020V2_CA2019 = ca2019, 
         SIMD2020V2_CA2018 = ca2018, 
         SIMD2020V2_CA = ca2011, 
         SIMD2020V2_HSCP2019 = hscp2019, 
         SIMD2020V2_HSCP2018 = hscp2018, 
         SIMD2020V2_HSCP = hscp2016, 
         SIMD2020V2_SCT_QUINTILE = simd2020v2_sc_quintile, 
         SIMD2020V2_SCT_DECILE = simd2020v2_sc_decile, 
         SIMD2020V2_HB2019_QUINTILE = simd2020v2_hb2019_quintile, 
         SIMD2020V2_HB2019_DECILE = simd2020v2_hb2019_decile, 
         SIMD2020V2_CA2019_QUINTILE = simd2020v2_ca2019_quintile, 
         SIMD2020V2_CA2019_DECILE = simd2020v2_ca2019_decile, 
         SIMD2020V2_HSCP2019_QUINTILE = simd2020v2_hscp2019_quintile,  
         SIMD2020V2_HSCP2019_DECILE = simd2020v2_hscp2019_decile, 
         SIMD2020V2_TOP_15 = simd2020v2tp15, 
         SIMD2020V2_BOT_15 = simd2020v2bt15) %>% 
  mutate_if(is.character, list(~na_if(., "")))

simd2020v2 <- pc_data %>%
  select(POSTCODE, datazone2011_simd2020) %>% 
  rename(SIMD2020V2_DATAZONE = datazone2011_simd2020) %>% 
  left_join(dz2011_simd2020v2)

# Update 8 postcodes that moved from Glasgow to Lanarkshire

changed_postcodes <- c("G33 6GS", "G33 6GT", "G33 6GU", "G33 6GW", "G33 6GX",	
                       "G33 6GY",	"G33 6GZ", "G33 6NS")

simd2020v2 %<>%
  mutate(SIMD2020V2_HB2019 = case_when(POSTCODE %in% changed_postcodes ~ "S08000032", 
                                       TRUE ~ SIMD2020V2_HB2019), 
         SIMD2020V2_HSCP2019 = case_when(POSTCODE %in% changed_postcodes ~ "S37000035", 
                                         TRUE ~ SIMD2020V2_HSCP2019), 
         SIMD2020V2_CA2019 = case_when(POSTCODE %in% changed_postcodes ~ "S12000050", 
                                       TRUE ~ SIMD2020V2_CA2019))



### 4 SIMD2016 ----

# SIMD 2016
# NEED TO MATCH ON GEOGRAPHIES EACH TIME

simd2016 <- pc_data %>% 
  left_join(dz2011_lookup %>% select(datazone2011, hb2014, ca2011, hscp2016), 
            by = c("datazone2011_simd2016" = "datazone2011")) %>% 
  select(POSTCODE, datazone2011_simd2016, hb2014, ca2011, hscp2016, 
         simd2016_sc_quintile, simd2016_sc_decile, simd2016_hb2014_quintile, 
         simd2016_hb2014_decile, simd2016_ca2011_quintile, 
         simd2016_ca2011_decile, simd2016_hscp2016_quintile, 
         simd2016_hscp2016_decile, simd2016tp15, simd2016bt15) %>% 
  rename(SIMD2016_DATAZONE = datazone2011_simd2016, 
         SIMD2016_HB2014 = hb2014, 
         SIMD2016_CA = ca2011, 
         SIMD2016_HSCP = hscp2016, 
         SIMD2016_SCT_QUINTILE = simd2016_sc_quintile, 
         SIMD2016_SCT_DECILE = simd2016_sc_decile, 
         SIMD2016_HB2014_QUINTILE = simd2016_hb2014_quintile, 
         SIMD2016_HB2014_DECILE = simd2016_hb2014_decile, 
         SIMD2016_CA_QUINTILE = simd2016_ca2011_quintile, 
         SIMD2016_CA_DECILE = simd2016_ca2011_decile, 
         SIMD2016_HSCP_QUINTILE = simd2016_hscp2016_quintile, 
         SIMD2016_HSCP_DECILE = simd2016_hscp2016_decile, 
         SIMD2016_TOP_15 = simd2016tp15, 
         SIMD2016_BOT_15 = simd2016bt15) %>% 
  mutate_if(is.character, list(~na_if(., "")))



### 5 SIMD2012 ----

# SIMD 2012
# NEED TO MATCH ON GEOGRAPHIES EACH TIME

simd2012 <- pc_data %>% 
  left_join(dz2001_lookup %>% select(datazone2001, hb2006, hb2014, ca2011, 
                                     hscp2016, chp2007, chp2011, chp2012, 
                                     chp2011subarea), 
            by = c("datazone2001_simd2012" = "datazone2001")) %>% 
  select(POSTCODE, datazone2001_simd2012, hb2006, hb2014, chp2007, chp2011, 
         chp2012, chp2011subarea, simd2012_score, simd2012_sc_quintile, 
         simd2012_sc_decile, simd2012_hb2014_quintile, simd2012_hb2014_decile,
         simd2012_hb2006_quintile, simd2012_hb2006_decile, 
         simd2012_chp2007_quintile, simd2012_chp2007_decile, 
         simd2012_chp2011_quintile, simd2012_chp2011_decile, 
         simd2012_chp2012_quintile, simd2012_chp2012_decile, 
         simd2012_chp2011sub_quintile, simd2012_chp2011sub_decile, 
         simd2012tp15, simd2012bt15, ca2011, simd2012_ca_quintile, 
         simd2012_ca_decile, hscp2016, simd2012_hscp_quintile, 
         simd2012_hscp_decile) %>% 
  rename(SIMD2012_DATAZONE = datazone2001_simd2012, 
         SIMD2012_HB2006 = hb2006, 
         SIMD2012_HB2014 = hb2014, 
         SIMD2012_CHP = chp2007, 
         SIMD2012_CHP2011 = chp2011, 
         SIMD2012_CHP2012 = chp2012, 
         SIMD2012_CHPSUB2011 = chp2011subarea, 
         SIMD2012_SCORE = simd2012_score, 
         SIMD2012_SCT_QUINTILE = simd2012_sc_quintile, 
         SIMD2012_SCT_DECILE = simd2012_sc_decile, 
         SIMD2012_HB2014_QUINTILE = simd2012_hb2014_quintile, 
         SIMD2012_HB2014_DECILE = simd2012_hb2014_decile, 
         SIMD2012_HB2006_QUINTILE = simd2012_hb2006_quintile, 
         SIMD2012_HB2006_DECILE = simd2012_hb2006_decile, 
         SIMD2012_CHP_QUINTILE = simd2012_chp2007_quintile, 
         SIMD2012_CHP_DECILE = simd2012_chp2007_decile, 
         SIMD2012_CHP2011_QUINTILE = simd2012_chp2011_quintile, 
         SIMD2012_CHP2011_DECILE = simd2012_chp2011_decile, 
         SIMD2012_CHP2012_QUINTILE = simd2012_chp2012_quintile, 
         SIMD2012_CHP2012_DECILE = simd2012_chp2012_decile, 
         SIMD2012_CHPSUB2011_QUINTILE = simd2012_chp2011sub_quintile, 
         SIMD2012_CHPSUB2011_DECILE = simd2012_chp2011sub_decile, 
         SIMD2012_TOP_15 = simd2012tp15, 
         SIMD2012_BOT_15 = simd2012bt15, 
         SIMD2012_CA = ca2011, 
         SIMD2012_CA_QUINTILE = simd2012_ca_quintile, 
         SIMD2012_CA_DECILE = simd2012_ca_decile, 
         SIMD2012_HSCP = hscp2016, 
         SIMD2012_HSCP_QUINTILE = simd2012_hscp_quintile, 
         SIMD2012_HSCP_DECILE = simd2012_hscp_decile) %>% 
  mutate_if(is.character, list(~na_if(., "")))


### 6 SIMD2009v2 ----

# SIMD 2009v2
# NEED TO MATCH ON GEOGRAPHIES EACH TIME

simd2009v2 <- pc_data %>% 
  left_join(dz2001_lookup %>% select(datazone2001, hb2006, hb2014, ca2011, 
                                     hscp2016, chp2007, chp2011, chp2012, 
                                     chp2011subarea), 
            by = c("datazone2001_simd2009v2" = "datazone2001")) %>% 
  select(POSTCODE, datazone2001_simd2009v2, hb2006, hb2014, chp2007, chp2011, 
         chp2012, chp2011subarea, simd2009v2_score, simd2009v2_sc_quintile, 
         simd2009v2_sc_decile, simd2009v2_hb2014_quintile, simd2009v2_hb2014_decile,
         simd2009v2_hb2006_quintile, simd2009v2_hb2006_decile, 
         simd2009v2_chp2007_quintile, simd2009v2_chp2007_decile, 
         simd2009v2_chp2011_quintile, simd2009v2_chp2011_decile, 
         simd2009v2_chp2012_quintile, simd2009v2_chp2012_decile, 
         simd2009v2_chp2011sub_quintile, simd2009v2_chp2011sub_decile, 
         simd2009v2tp15, simd2009v2bt15, ca2011, simd2009v2_ca_quintile, 
         simd2009v2_ca_decile, hscp2016, simd2009v2_hscp_quintile, 
         simd2009v2_hscp_decile) %>% 
  rename(SIMD2009V2_DATAZONE = datazone2001_simd2009v2, 
         SIMD2009V2_HB2006 = hb2006, 
         SIMD2009V2_HB2014 = hb2014, 
         SIMD2009V2_CHP = chp2007, 
         SIMD2009V2_CHP2011 = chp2011, 
         SIMD2009V2_CHP2012 = chp2012, 
         SIMD2009V2_CHPSUB2011 = chp2011subarea, 
         SIMD2009V2_SCORE = simd2009v2_score, 
         SIMD2009V2_SCT_QUINTILE = simd2009v2_sc_quintile, 
         SIMD2009V2_SCT_DECILE = simd2009v2_sc_decile, 
         SIMD2009V2_HB2014_QUINTILE = simd2009v2_hb2014_quintile, 
         SIMD2009V2_HB2014_DECILE = simd2009v2_hb2014_decile, 
         SIMD2009V2_HB2006_QUINTILE = simd2009v2_hb2006_quintile, 
         SIMD2009V2_HB2006_DECILE = simd2009v2_hb2006_decile, 
         SIMD2009V2_CHP_QUINTILE = simd2009v2_chp2007_quintile, 
         SIMD2009V2_CHP_DECILE = simd2009v2_chp2007_decile, 
         SIMD2009V2_CHP2011_QUINTILE = simd2009v2_chp2011_quintile, 
         SIMD2009V2_CHP2011_DECILE = simd2009v2_chp2011_decile, 
         SIMD2009V2_CHP2012_QUINTILE = simd2009v2_chp2012_quintile, 
         SIMD2009V2_CHP2012_DECILE = simd2009v2_chp2012_decile, 
         SIMD2009V2_CHPSUB2011_QUINTILE = simd2009v2_chp2011sub_quintile, 
         SIMD2009V2_CHPSUB2011_DECILE = simd2009v2_chp2011sub_decile, 
         SIMD2009V2_TOP_15 = simd2009v2tp15, 
         SIMD2009V2_BOT_15 = simd2009v2bt15, 
         SIMD2009V2_CA = ca2011, 
         SIMD2009V2_CA_QUINTILE = simd2009v2_ca_quintile, 
         SIMD2009V2_CA_DECILE = simd2009v2_ca_decile, 
         SIMD2009V2_HSCP = hscp2016, 
         SIMD2009V2_HSCP_QUINTILE = simd2009v2_hscp_quintile, 
         SIMD2009V2_HSCP_DECILE = simd2009v2_hscp_decile) %>% 
  mutate_if(is.character, list(~na_if(., "")))



### 7 SIMD2006----

# SIMD 2006
# NEED TO MATCH ON GEOGRAPHIES EACH TIME

simd2006 <- pc_data %>% 
  left_join(dz2001_lookup %>% select(datazone2001, hb2006, hb2014, ca2011, 
                                     hscp2016), 
            by = c("datazone2001_simd2006" = "datazone2001")) %>% 
  select(POSTCODE, datazone2001_simd2006, hb2006, hb2014, simd2006_score, 
         simd2006_sc_quintile, simd2006_sc_decile,
         simd2006_hb2014_quintile, simd2006_hb2014_decile,
         simd2006_hb2006_quintile, simd2006_hb2006_decile, 
         simd2006tp15, simd2006bt15, ca2011, simd2006_ca_quintile, 
         simd2006_ca_decile, hscp2016, simd2006_hscp_quintile, 
         simd2006_hscp_decile) %>% 
  rename(SIMD2006_DATAZONE = datazone2001_simd2006, 
         SIMD2006_HB2006 = hb2006, 
         SIMD2006_HB2014 = hb2014, 
         SIMD2006_SCORE = simd2006_score, 
         SIMD2006_SCT_QUINTILE = simd2006_sc_quintile, 
         SIMD2006_SCT_DECILE = simd2006_sc_decile, 
         SIMD2006_HB2014_QUINTILE = simd2006_hb2014_quintile, 
         SIMD2006_HB2014_DECILE = simd2006_hb2014_decile, 
         SIMD2006_HB2006_QUINTILE = simd2006_hb2006_quintile, 
         SIMD2006_HB2006_DECILE = simd2006_hb2006_decile, 
         SIMD2006_TOP_15 = simd2006tp15, 
         SIMD2006_BOT_15 = simd2006bt15, 
         SIMD2006_CA = ca2011, 
         SIMD2006_CA_QUINTILE = simd2006_ca_quintile, 
         SIMD2006_CA_DECILE = simd2006_ca_decile, 
         SIMD2006_HSCP = hscp2016, 
         SIMD2006_HSCP_QUINTILE = simd2006_hscp_quintile, 
         SIMD2006_HSCP_DECILE = simd2006_hscp_decile) %>% 
  mutate_if(is.character, list(~na_if(., "")))

# Reverse Ordering

simd2006 %<>%
  mutate(SIMD2006_SCT_QUINTILE = 6 - SIMD2006_SCT_QUINTILE, 
         SIMD2006_SCT_DECILE = 11 - SIMD2006_SCT_DECILE, 
         SIMD2006_HB2006_QUINTILE = 6 - SIMD2006_HB2006_QUINTILE, 
         SIMD2006_HB2006_DECILE = 11 - SIMD2006_HB2006_DECILE, 
         SIMD2006_HB2014_QUINTILE = 6 - SIMD2006_HB2014_QUINTILE, 
         SIMD2006_HB2014_DECILE = 11 - SIMD2006_HB2014_DECILE, 
         SIMD2006_CA_QUINTILE = 6 - SIMD2006_CA_QUINTILE, 
         SIMD2006_CA_DECILE = 11 - SIMD2006_CA_DECILE, 
         SIMD2006_HSCP_QUINTILE = 6 - SIMD2006_HSCP_QUINTILE, 
         SIMD2006_HSCP_DECILE = 11 - SIMD2006_HSCP_DECILE)



### 8 SIMD 2004 ----

# SIMD 2004
# NEED TO MATCH ON GEOGRAPHIES EACH TIME

simd2004 <- pc_data %>% 
  left_join(dz2001_lookup %>% select(datazone2001, hb1995, hb2006, hb2014, 
                                     ca2011, hscp2016), 
            by = c("datazone2001_simd2004" = "datazone2001")) %>% 
  select(POSTCODE, datazone2001_simd2004, hb1995, hb2006, hb2014, 
         simd2004_score, simd2004_sc_quintile, simd2004_sc_decile,
         simd2004_hb2014_quintile, simd2004_hb2014_decile,
         simd2004_hb2006_quintile, simd2004_hb2006_decile, 
         simd2004_hb1995_quintile, simd2004_hb1995_decile, 
         simd2004tp15, simd2004bt15, ca2011, simd2004_ca_quintile, 
         simd2004_ca_decile, hscp2016, simd2004_hscp_quintile, 
         simd2004_hscp_decile) %>% 
  rename(SIMD2004_DATAZONE = datazone2001_simd2004,
         SIMD2004_HB = hb1995,
         SIMD2004_HB2006 = hb2006,
         SIMD2004_HB2014 = hb2014,
         SIMD2004_SCORE = simd2004_score,
         SIMD2004_SCT_QUINTILE = simd2004_sc_quintile,
         SIMD2004_SCT_DECILE = simd2004_sc_decile,
         SIMD2004_HB2014_QUINTILE = simd2004_hb2014_quintile,
         SIMD2004_HB2014_DECILE = simd2004_hb2014_decile,
         SIMD2004_HB2006_QUINTILE = simd2004_hb2006_quintile,
         SIMD2004_HB2006_DECILE = simd2004_hb2006_decile,
         SIMD2004_HB_QUINTILE = simd2004_hb1995_quintile,
         SIMD2004_HB_DECILE = simd2004_hb1995_decile,
         SIMD2004_TOP_15 = simd2004tp15,
         SIMD2004_BOT_15 = simd2004bt15,
         SIMD2004_CA = ca2011,
         SIMD2004_CA_QUINTILE = simd2004_ca_quintile,
         SIMD2004_CA_DECILE = simd2004_ca_decile,
         SIMD2004_HSCP = hscp2016,
         SIMD2004_HSCP_QUINTILE = simd2004_hscp_quintile,
         SIMD2004_HSCP_DECILE = simd2004_hscp_decile) %>% 
  mutate_if(is.character, list(~na_if(., "")))

# Reverse Ordering

simd2004 %<>%
  mutate(SIMD2004_SCT_QUINTILE = 6 - SIMD2004_SCT_QUINTILE, 
         SIMD2004_SCT_DECILE = 11 - SIMD2004_SCT_DECILE, 
         SIMD2004_HB2014_QUINTILE = 6 - SIMD2004_HB2014_QUINTILE, 
         SIMD2004_HB2014_DECILE = 11 - SIMD2004_HB2014_DECILE, 
         SIMD2004_HB2006_QUINTILE = 6 - SIMD2004_HB2006_QUINTILE, 
         SIMD2004_HB2006_DECILE = 11 - SIMD2004_HB2006_DECILE, 
         SIMD2004_HB_QUINTILE = 6 - SIMD2004_HB_QUINTILE, 
         SIMD2004_HB_DECILE = 11 - SIMD2004_HB_DECILE, 
         SIMD2004_CA_QUINTILE = 6 - SIMD2004_CA_QUINTILE, 
         SIMD2004_CA_DECILE = 11 - SIMD2004_CA_DECILE, 
         SIMD2004_HSCP_QUINTILE = 6 - SIMD2004_HSCP_QUINTILE, 
         SIMD2004_HSCP_DECILE = 11 - SIMD2004_HSCP_DECILE)



### 9 Carstairs ----

# 2001 Carstairs

oa2001_lookup <- fread(glue("{carstairs}/oa2001_lookup.csv")) %>% 
  mutate_if(is.character, list(~na_if(., "")))

oa2001_carstairs <- fread(glue("{carstairs}/oa2001_carstairs.csv")) %>% 
  left_join(oa2001_lookup) %>% 
  select(OutputArea2001Code, HB, pcsec2001, carstairs2001score, 
         carstairs2001_sc_quintile, carstairs2001_sc_decile, 
         carstairs2001_hb_quintile, carstairs2001_hb_decile, OA1991, pcsec1991, 
         carstairs1991score, carstairs1991_sc_quintile, carstairs1991_sc_decile, 
         carstairs1991_sc_7cat, carstairs1981score, carstairs1981_sc_quintile, 
         carstairs1981_sc_decile, carstairs1981_sc_7cat) %>% 
  rename(OA2001 = OutputArea2001Code,
         CARSTAIRS2001_HB = HB,
         PCSEC2001 = pcsec2001,
         CARST_SCORE_2001 = carstairs2001score,
         CARST_SCT_QUINTILE_2001 = carstairs2001_sc_quintile,
         CARST_SCT_DECILE_2001 = carstairs2001_sc_decile,
         CARST_HB_QUINTILE_2001 = carstairs2001_hb_quintile,
         CARST_HB_DECILE_2001 = carstairs2001_hb_decile,
         OA1991 = OA1991,
         PCSEC1991 = pcsec1991,
         CARST_SCORE_1991 = carstairs1991score,
         CARST_SCT_DECILE_1991 = carstairs1991_sc_decile,
         CARST_SCT_QUINTILE_1991 = carstairs1991_sc_quintile,
         CARST_SCT_CATEGORY_1991 = carstairs1991_sc_7cat,
         CARST_SCORE_1981 = carstairs1981score,
         CARST_SCT_QUINTILE_1981 = carstairs1981_sc_quintile,
         CARST_SCT_DECILE_1981 = carstairs1981_sc_decile,
         CARST_SCT_CATEGORY_1981 = carstairs1981_sc_7cat) %>% 
  mutate_if(is.character, list(~na_if(., "")))

# 2011 Carstairs

oa2011_carstairs <- fread(glue("{carstairs}/oa2011_carstairs2011.csv")) %>% 
  select(OA2011, HB2014, pcsec2011, carstairs2011_score, 
         carstairs2011_sc_quintile, carstairs2011_sc_decile, 
         carstairs2011_hb2014_quintile, carstairs2011_hb2014_decile) %>% 
  rename(CARSTAIRS2011_HB2014 = HB2014, 
         PCSEC2011 = pcsec2011, 
         CARST_SCORE_2011 = carstairs2011_score, 
         CARST_SCT_QUINTILE_2011 = carstairs2011_sc_quintile, 
         CARST_SCT_DECILE_2011 = carstairs2011_sc_decile, 
         CARST_HB2014_QUINTILE_2011 = carstairs2011_hb2014_quintile, 
         CARST_HB2014_DECILE_2011 = carstairs2011_hb2014_decile) %>% 
  mutate_if(is.character, list(~na_if(., "")))


# Get postcode level carstairs

pc_oa2001 <- pc_data %>% 
  select(POSTCODE, OA2001) %>% 
  left_join(oa2001_carstairs)

pc_oa2011 <- pc_data %>% 
  select(POSTCODE, OA2011) %>% 
  left_join(oa2011_carstairs)

# Reverse Ordering

pc_oa2001 %<>%
  mutate(CARST_SCT_QUINTILE_2001 = 6 - CARST_SCT_QUINTILE_2001, 
         CARST_SCT_DECILE_2001 = 11 - CARST_SCT_DECILE_2001, 
         CARST_HB_QUINTILE_2001 = 6 - CARST_HB_QUINTILE_2001, 
         CARST_HB_DECILE_2001 = 11 - CARST_HB_DECILE_2001, 
         CARST_SCT_QUINTILE_1991 = 6 - CARST_SCT_QUINTILE_1991, 
         CARST_SCT_DECILE_1991 = 11 - CARST_SCT_DECILE_1991, 
         CARST_SCT_CATEGORY_1991 = 8 - CARST_SCT_CATEGORY_1991, 
         CARST_SCT_QUINTILE_1981 = 6 - CARST_SCT_QUINTILE_1981, 
         CARST_SCT_DECILE_1981 = 11 - CARST_SCT_DECILE_1981, 
         CARST_SCT_CATEGORY_1981 = 8 - CARST_SCT_CATEGORY_1981)



### 10 Join data ----

pc_dep <- simd2012 %>% 
  full_join(simd2009v2) %>% 
  full_join(simd2006) %>% 
  full_join(simd2004) %>% 
  full_join(pc_oa2011) %>% 
  full_join(pc_oa2001) %>% 
  full_join(simd2016) %>% 
  full_join(simd2020v2) %>% 
  select(POSTCODE:SIMD2012_BOT_15, SIMD2009V2_DATAZONE:SIMD2009V2_BOT_15, 
         SIMD2006_DATAZONE:SIMD2006_BOT_15, SIMD2004_DATAZONE:SIMD2004_BOT_15, 
         OA2011:CARST_SCT_CATEGORY_1981, SIMD2004_CA, SIMD2006_CA, 
         SIMD2009V2_CA, SIMD2012_CA, SIMD2004_CA_QUINTILE, SIMD2004_CA_DECILE, 
         SIMD2006_CA_QUINTILE, SIMD2006_CA_DECILE, SIMD2009V2_CA_QUINTILE, 
         SIMD2009V2_CA_DECILE, SIMD2012_CA_QUINTILE, SIMD2012_CA_DECILE, 
         SIMD2004_HSCP, SIMD2006_HSCP, SIMD2009V2_HSCP, SIMD2012_HSCP, 
         SIMD2004_HSCP_QUINTILE, SIMD2004_HSCP_DECILE, SIMD2006_HSCP_QUINTILE, 
         SIMD2006_HSCP_DECILE, SIMD2009V2_HSCP_QUINTILE, SIMD2009V2_HSCP_DECILE, 
         SIMD2012_HSCP_QUINTILE, SIMD2012_HSCP_DECILE, 
         SIMD2016_DATAZONE:SIMD2020V2_BOT_15) %>% 
  mutate_all(funs(ifelse(is.na(.), "#NULL!", .)))



### 11 Checks ----

pc_dep %>% select(SIMD2020V2_DATAZONE) %>% count(SIMD2020V2_DATAZONE == "#NULL!")
pc_dep %>% select(SIMD2016_DATAZONE) %>% count(SIMD2016_DATAZONE == "#NULL!")
pc_dep %>% select(SIMD2012_DATAZONE) %>% count(SIMD2012_DATAZONE == "#NULL!")
pc_dep %>% select(SIMD2009V2_DATAZONE) %>% count(SIMD2009V2_DATAZONE == "#NULL!")
pc_dep %>% select(SIMD2006_DATAZONE) %>% count(SIMD2006_DATAZONE == "#NULL!")
pc_dep %>% select(SIMD2004_DATAZONE) %>% count(SIMD2004_DATAZONE == "#NULL!")

pc_dep %>% count(SIMD2020V2_HB2019, SIMD2020V2_HB2018, SIMD2020V2_HB2014)
pc_dep %>% count(SIMD2020V2_HSCP2019, SIMD2020V2_HSCP2018, SIMD2020V2_HSCP) %>% print(n=Inf)
pc_dep %>% count(SIMD2020V2_CA2019, SIMD2020V2_CA2018, SIMD2020V2_CA) %>% print(n=Inf)

pc_dep %>% count(SIMD2020V2_HB2019, SIMD2020V2_HSCP2019, SIMD2020V2_CA2019) %>% print(n=Inf)
pc_dep %>% count(SIMD2020V2_HB2018, SIMD2020V2_HSCP2018, SIMD2020V2_CA2018) %>% print(n=Inf)
pc_dep %>% count(SIMD2020V2_HB2014, SIMD2020V2_HSCP, SIMD2020V2_CA) %>% print(n=Inf)

quin_cols <- pc_dep %>% 
  select(matches("QUINTILE")) %>% 
  mutate(negative = rowSums(. < 1 & . > 5)) %>% 
  filter(negative != 0)

dec_cols <- pc_dep %>% 
  select(matches("QUINTILE")) %>% 
  mutate(negative = rowSums(. < 1 & . > 10)) %>% 
  filter(negative != 0)

# SIMD 2012

pc_dep %>% filter(SIMD2012_DATAZONE == "#NULL!") %>% 
  count(SIMD2012_HB2014, SIMD2012_HB2014_DECILE, SIMD2012_HB2014_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2012_DATAZONE == "#NULL!") %>% 
  count(SIMD2012_HB2006, SIMD2012_HB2006_DECILE, SIMD2012_HB2006_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2012_DATAZONE == "#NULL!") %>% 
  count(SIMD2012_CA, SIMD2012_CA_DECILE, SIMD2012_CA_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2012_DATAZONE == "#NULL!") %>% 
  count(SIMD2012_HSCP, SIMD2012_HSCP_DECILE, SIMD2012_HSCP_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2012_DATAZONE == "#NULL!") %>% 
  count(SIMD2012_CHP, SIMD2012_CHP_DECILE, SIMD2012_CHP_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2012_DATAZONE == "#NULL!") %>% 
  count(SIMD2012_CHP2012, SIMD2012_CHP2012_DECILE, SIMD2012_CHP2012_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2012_DATAZONE == "#NULL!") %>% 
  count(SIMD2012_CHP2011, SIMD2012_CHP2011_DECILE, SIMD2012_CHP2011_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2012_DATAZONE == "#NULL!") %>% 
  count(SIMD2012_CHPSUB2011, SIMD2012_CHPSUB2011_DECILE, SIMD2012_CHPSUB2011_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2012_DATAZONE == "#NULL!") %>% 
  count(SIMD2012_SCORE, SIMD2012_SCT_DECILE, SIMD2012_SCT_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2012_DATAZONE == "#NULL!") %>% 
  count(SIMD2012_BOT_15, SIMD2012_TOP_15) %>% 
  print(n=Inf)


# SIMD 2009v2

pc_dep %>% filter(SIMD2009V2_DATAZONE == "#NULL!") %>% 
  count(SIMD2009V2_HB2014, SIMD2009V2_HB2014_DECILE, SIMD2009V2_HB2014_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2009V2_DATAZONE == "#NULL!") %>% 
  count(SIMD2009V2_HB2006, SIMD2009V2_HB2006_DECILE, SIMD2009V2_HB2006_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2009V2_DATAZONE == "#NULL!") %>% 
  count(SIMD2009V2_CA, SIMD2009V2_CA_DECILE, SIMD2009V2_CA_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2009V2_DATAZONE == "#NULL!") %>% 
  count(SIMD2009V2_HSCP, SIMD2009V2_HSCP_DECILE, SIMD2009V2_HSCP_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2009V2_DATAZONE == "#NULL!") %>% 
  count(SIMD2009V2_CHP, SIMD2009V2_CHP_DECILE, SIMD2009V2_CHP_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2009V2_DATAZONE == "#NULL!") %>% 
  count(SIMD2009V2_CHP2012, SIMD2009V2_CHP2012_DECILE, SIMD2009V2_CHP2012_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2009V2_DATAZONE == "#NULL!") %>% 
  count(SIMD2009V2_CHP2011, SIMD2009V2_CHP2011_DECILE, SIMD2009V2_CHP2011_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2009V2_DATAZONE == "#NULL!") %>% 
  count(SIMD2009V2_CHPSUB2011, SIMD2009V2_CHPSUB2011_DECILE, SIMD2009V2_CHPSUB2011_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2009V2_DATAZONE == "#NULL!") %>% 
  count(SIMD2009V2_SCORE, SIMD2009V2_SCT_DECILE, SIMD2009V2_SCT_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2009V2_DATAZONE == "#NULL!") %>% 
  count(SIMD2009V2_BOT_15, SIMD2009V2_TOP_15) %>% 
  print(n=Inf)


# SIMD 2006

pc_dep %>% filter(SIMD2006_DATAZONE == "#NULL!") %>% 
  count(SIMD2006_HB2014, SIMD2006_HB2014_DECILE, SIMD2006_HB2014_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2006_DATAZONE == "#NULL!") %>% 
  count(SIMD2006_HB2006, SIMD2006_HB2006_DECILE, SIMD2006_HB2006_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2006_DATAZONE == "#NULL!") %>% 
  count(SIMD2006_CA, SIMD2006_CA_DECILE, SIMD2006_CA_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2006_DATAZONE == "#NULL!") %>% 
  count(SIMD2006_HSCP, SIMD2006_HSCP_DECILE, SIMD2006_HSCP_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2006_DATAZONE == "#NULL!") %>% 
  count(SIMD2006_SCORE, SIMD2006_SCT_DECILE, SIMD2006_SCT_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2006_DATAZONE == "#NULL!") %>% 
  count(SIMD2006_BOT_15, SIMD2006_TOP_15) %>% 
  print(n=Inf)


# SIMD 2004

pc_dep %>% filter(SIMD2004_DATAZONE == "#NULL!") %>% 
  count(SIMD2004_HB2014, SIMD2004_HB2014_DECILE, SIMD2004_HB2014_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2004_DATAZONE == "#NULL!") %>% 
  count(SIMD2004_HB2006, SIMD2004_HB2006_DECILE, SIMD2004_HB2006_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2004_DATAZONE == "#NULL!") %>% 
  count(SIMD2004_CA, SIMD2004_CA_DECILE, SIMD2004_CA_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2004_DATAZONE == "#NULL!") %>% 
  count(SIMD2004_HSCP, SIMD2004_HSCP_DECILE, SIMD2004_HSCP_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2004_DATAZONE == "#NULL!") %>% 
  count(SIMD2004_SCORE, SIMD2004_SCT_DECILE, SIMD2004_SCT_QUINTILE) %>% 
  print(n=Inf)

pc_dep %>% filter(SIMD2004_DATAZONE == "#NULL!") %>% 
  count(SIMD2004_BOT_15, SIMD2004_TOP_15) %>% 
  print(n=Inf)

# Save output

write_xlsx(pc_dep, glue("{output}/PC_DEPRIVATION_DATA_{pc_version}.xlsx"))
