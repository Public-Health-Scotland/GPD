##########################################################
# Update Postcode Deprivation Data File for IT
# Calum Purdie
# Original date 18/06/2020
# Latest update author - Calum Purdie
# Latest update date - 13/07/2020
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 351
# Code for updating postcode deprivation data for IT to upload to APEXT/APEXP
# Approximate run time - 1 minutes
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
library(writexl)
library(ckanr)

# Set filepaths

base_path <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                  "3_Deprivation")
lookups <- glue("{base_path}/Postcode Deprivation/Lookup Files/R Files")
carstairs <- glue("{base_path}/Carstairs/Lookup Files")
simd <- glue("{base_path}/SIMD/Lookup Files/SIMD 2020")
output <- glue("{base_path}/Postcode Deprivation/Lookup Files/Files for IT")
spd <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/1_Geography/",
            "Scottish Postcode Directory/Previous Versions/Archive")
chp <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/1_Geography/", 
            "Scottish Postcode Directory/Source Data/2019_2/", 
            "Postcode_CHP_Lookups")

# SPD Version

pc_version <- "2019_2"

# Use the Geography Codes and Names open data file to get the names
# First need to run the httr configuration script

source(here::here("Geography", "Scottish Postcode Directory", 
                  "Set httr configuration for API.R"))



### 2 Read in data ----

# Read in the postcode_all_simd_carstairs file for pc_version

pc_data <- readRDS(glue("{lookups}/Archive/", 
                        "postcode_{pc_version}_all_simd_carstairs.rds")) %>% 
  select(-c(simd2020_rank:simd2020_crime_rank)) %>% 
  rename(POSTCODE = pc7)

# Read in required geographies from SPD


### DATAZONE 2011 and DATAZONE 2001 LOOKUPS ----

geo_data <- readRDS(glue("{spd}/Scottish_Postcode_Directory_{pc_version}.rds")) %>% 
  # select(CA2019, CA2018, CA2011, HB2019, HB2018, HB2014, HB2006, HB1995, 
  #        HSCP2019, HSCP2018, HSCP2016, DataZone2011, DataZone2001) %>% 
  select(CA2011, HB2014, HB2006, HB1995, HSCP2016, DataZone2001) %>%
  distinct()

# Get data zone lookups from open data

# Get data zone 2001 lookup

# Set url and id

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id <- "e92d19d4-ced7-40c8-b628-e28e4528fc41"

dz2001 <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  select(DataZone, CA, HSCP, HB) %>% 
  as_tibble() %>% 
  left_join(geo_data %>% select(-c(DataZone2011, HB2019, HSCP2019, CA2019)), 
            by = c("DataZone" = "DataZone2001")) %>% 
  distinct()

# Get data zone 2011 lookup

# Set url and id

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id <- "395476ab-0720-4740-be07-ff4467141352"

dz2011 <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  select(DataZone, CA, HSCP, HB) %>% 
  as_tibble()

# Read in CHP data

chp2007 <- read_xlsx(glue("{chp}/Postcode_CHP2007.xlsx")) %>% 
  select(Postcode, CommunityHealthPartnership2007Code) %>% 
  rename(POSTCODE = Postcode, 
         CHP = CommunityHealthPartnership2007Code)

chp2011 <- read_xlsx(glue("{chp}/Postcode_CHP2011.xlsx")) %>% 
  select(Postcode, CommunityHealthPartnership2011Code) %>% 
  rename(POSTCODE = Postcode, 
         CHP2011 = CommunityHealthPartnership2011Code)

chp2011_sub <- read_xlsx(glue("{chp}/Postcode_CHP_SubArea_2011.xlsx")) %>% 
  select(Postcode, CommunityHealthPartnershipSubArea2011Code) %>% 
  rename(POSTCODE = Postcode, 
         CHPSUB2011 = CommunityHealthPartnershipSubArea2011Code)

chp2012 <- read_xlsx(glue("{chp}/Postcode_CHP2012.xlsx")) %>% 
  select(Postcode, CommunityHealthPartnership2012Code) %>% 
  rename(POSTCODE = Postcode, 
         CHP2012 = CommunityHealthPartnership2012Code)



### 3 SIMD2020v2 ----

# Match on DataZone2011_simd2020v2 data
# This step is only required due to using old (2019_2) postcode file

dz2011_simd2020v2 <- readRDS(glue("{simd}/DataZone2011_simd2020v2.rds")) %>% 
  select_at(vars(-contains("name"))) %>% 
  mutate(SIMD2020V2_DATAZONE = datazone2011) %>% 
  rename(SIMD2020V2_SCT_QUINTILE = simd2020v2_sc_quintile, 
         SIMD2020V2_SCT_DECILE = simd2020v2_sc_decile, 
         SIMD2020V2_HB2019_QUINTILE = simd2020v2_hb2019_quintile, 
         SIMD2020V2_HB2019_DECILE = simd2020v2_hb2019_decile, 
         SIMD2020V2_TOP_15 = simd2020v2tp15, 
         SIMD2020V2_BOT_15 = simd2020v2bt15, 
         SIMD2020V2_CA2019_QUINTILE = simd2020v2_ca2019_quintile, 
         SIMD2020V2_CA2019_DECILE = simd2020v2_ca2019_decile, 
         SIMD2020V2_HSCP2019_QUINTILE = simd2020v2_hscp2019_quintile, 
         SIMD2020V2_HSCP2019_DECILE = simd2020v2_hscp2019_decile, 
         SIMD2020V2_HB2019 = hb2019, 
         SIMD2020V2_HB2018 = hb2018,
         SIMD2020V2_HB2014 = hb2014,
         SIMD2020V2_HSCP2019 = hscp2019,
         SIMD2020V2_HSCP2018 = hscp2018,
         SIMD2020V2_HSCP = hscp2016,
         SIMD2020V2_CA2019 = ca2019,
         SIMD2020V2_CA2018 = ca2018,
         SIMD2020V2_CA = ca2011) %>% 
  select(SIMD2020V2_DATAZONE, SIMD2020V2_SCT_QUINTILE, SIMD2020V2_SCT_DECILE, 
         SIMD2020V2_HB2019_QUINTILE, SIMD2020V2_HB2019_DECILE, 
         SIMD2020V2_TOP_15, SIMD2020V2_BOT_15, SIMD2020V2_CA2019_QUINTILE, 
         SIMD2020V2_CA2019_DECILE, SIMD2020V2_HSCP2019_QUINTILE, 
         SIMD2020V2_HSCP2019_DECILE, SIMD2020V2_HB2019, SIMD2020V2_HB2018, 
         SIMD2020V2_HB2014, SIMD2020V2_CA2019, SIMD2020V2_CA2018, SIMD2020V2_CA, 
         SIMD2020V2_HSCP2019, SIMD2020V2_HSCP2018, SIMD2020V2_HSCP, 
         datazone2011)

simd2020v2 <- pc_data %>%
  select(POSTCODE, datazone2011_simd2020) %>% 
  left_join(dz2011_simd2020v2, by = c("datazone2011_simd2020" = "datazone2011")) %>% 
  select(-datazone2011_simd2020)



### 4 SIMD2016 ----

# SIMD 2016
# NEED TO MATCH ON GEOGRAPHIES EACH TIME

simd2016 <- pc_data %>% 
  left_join(dz2011, by = c("datazone2011_simd2016" = "DataZone")) %>% 
  rename(SIMD2016_DATAZONE = datazone2011_simd2016, 
         SIMD2016_SCT_QUINTILE = simd2016_sc_quintile, 
         SIMD2016_SCT_DECILE = simd2016_sc_decile, 
         SIMD2016_HB2014_QUINTILE = simd2016_hb2014_quintile, 
         SIMD2016_HB2014_DECILE = simd2016_hb2014_decile, 
         SIMD2016_TOP_15 = simd2016tp15, 
         SIMD2016_BOT_15 = simd2016bt15, 
         SIMD2016_CA_QUINTILE = simd2016_ca2011_quintile, 
         SIMD2016_CA_DECILE = simd2016_ca2011_decile, 
         SIMD2016_HSCP_QUINTILE = simd2016_hscp2016_quintile, 
         SIMD2016_HSCP_DECILE = simd2016_hscp2016_decile,
         SIMD2016_HB2019 = HB2019, 
         SIMD2016_HB2018 = HB2018, 
         SIMD2016_HB2014 = HB2014, 
         SIMD2016_CA2019 = CA2019, 
         SIMD2016_CA2018 = CA2018, 
         SIMD2016_CA2011 = CA2011, 
         SIMD2016_HSCP2019 = HSCP2019, 
         SIMD2016_HSCP2018 = HSCP2018, 
         SIMD2016_HSCP2016 = HSCP2016) %>% 
  select(pc7, SIMD2016_DATAZONE, SIMD2016_SCT_QUINTILE, SIMD2016_SCT_DECILE, 
         SIMD2016_HB2014_QUINTILE, SIMD2016_HB2014_DECILE, SIMD2016_TOP_15, 
         SIMD2016_BOT_15, SIMD2016_CA_QUINTILE, SIMD2016_CA_DECILE, 
         SIMD2016_HSCP_QUINTILE, SIMD2016_HSCP_DECILE, SIMD2016_HB2019, 
         SIMD2016_HB2018, SIMD2016_HB2014, SIMD2016_CA2019, SIMD2016_CA2018, 
         SIMD2016_CA2011, SIMD2016_HSCP2019, SIMD2016_HSCP2018, 
         SIMD2016_HSCP2016)
