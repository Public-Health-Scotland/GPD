##########################################################################
# Check Postcode table on MGSREF APXU
# Calum Purdie
# Original date 06/01/2020
# Latest update author - Calum Purdie
# Latest update date - 19/01/2021
# Latest update description - Adding code for checking PC_GEOGRAPHY_VW
# Type of script - Checking
# Written/run on RStudio Server
# Version of R that the script was most recently run on - 3.6.1
# Code for checking the postcode table on MGSREF APXU
#########################################################################

### 1 Housekeeping ----

library(magrittr)
library(dplyr)
library(janitor)
library(tidylog)
library(stringr)
library(odbc)
library(glue)

# Set APXU connection

APXU_connection <- odbc::dbConnect(
  drv = odbc::odbc(),
  dsn = "APXU",
  uid = rstudioapi::askForPassword("APXU Username:"),
  pwd = rstudioapi::askForPassword("APXU Password:"))

# Set spd path

spd_path <- glue("/conf/linkage/output/lookups/Unicode/Geography/", 
                 "Scottish Postcode Directory/")


### 2 Checking ----

# Read in POSTCODE table and clean names
# Sort postcodes in order

postcode <- tbl(APXU_connection, "POSTCODE") %>%
  collect() %>% 
  arrange(POSTCODE)


# Check 9 digit geography codes
# Manually check the 9 digit codes start with the expected 3 characters below:

# S00 - OUTPUT_AREA_2001 AND OUTPUT_AREA_2011
# S01 - DATAZONE and DATAZONE_2011
# S02 - INTERMEDIATE_ZONE and INTZONE_2011
# S03 - CHP_2004, CHP_2007, CHP2011 AND CHP_2012
# S05 - REG_OUTCOME_AREA_CPP
# S06 - REG_OUTCOME_AREA_LOC
# S08 - SG_HB_AREA, HEALTH_BOARD_2014, HEALTH_BOARD_2018 AND HEALTH_BOARD_2019
# S09 - ENT_REG_2008
# S11 - STRATEGIC_DEV_PLAN_AREA
# S12 - COUNCIL_AREA_2011, COUNCIL_AREA_2018 AND COUNCIL_AREA_2019
# S13 - ELECTORAL_WARD
# S14 - PARL_CONSTITUENCY
# S16 - SPC_2014
# S17 - SPR_2014
# S19 - LOCALITY
# S20 - SETTLEMENT_2016
# S22 - TTWA_2013 - This shows codes beginning with K
#                   This is for borders areas relating to Carlisle
# S26 - CHP_SUBS_2011
# S30 - LAU_2019_LEVEL1
# S35 - CIVIL_PARISH
# S37 - HSCP_2016, HSCP_2018 AND HSCP_2019


gss_cols <- c("ELECTORAL_WARD", "PARL_CONSTITUENCY", "CIVIL_PARISH", 
              "OUTPUT_AREA_2001", "DATAZONE", "DATAZONE_2011", 
              "INTERMEDIATE_ZONE", "INTZONE_2011", "REG_OUTCOME_AREA_LOC", 
              "REG_OUTCOME_AREA_CPP", "ENT_REG_2008", "SG_HB_AREA", "CHP_2011", 
              "CHP_SUBS_2011", "STRATEGIC_DEV_PLAN_AREA", "CHP_2012", 
              "OUTPUT_AREA_2011", "HEALTH_BOARD_2014", "SPC_2014", "SPR_2014", 
              "HSCP_2016", "CHP_2004", "CHP_2007", "COUNCIL_AREA_2011", 
              "TTWA_2013", "COUNCIL_AREA_2019", "COUNCIL_AREA_2018", 
              "HEALTH_BOARD_2019", "HEALTH_BOARD_2018", "HSCP_2019", 
              "HSCP_2018", "LAU_2019_LEVEL1", "SETTLEMENT_2016")

for (i in gss_cols){
  
  postcode %>% 
    mutate(!!as.name(i) := str_sub(!!as.name(i), 1, 3)) %>% 
    distinct(!!as.name(i)) %>% 
    filter(!is.na(!!as.name(i))) %>% 
    print()
  
}


### 3 Frequencies ----

freq_cols <- c("NUTS_2008_LEVEL2", "NUTS_2008_LEVEL3", 
               "HEALTH_BOARD_AREA", "ISLAND_CODE", "LGD_1991", 
               "LGD_1995", "URBAN_RURAL", "URBRUR_6FOLD", 
               "HEALTH_BOARD_CYPHER", "HEALTH_BOARD_AREA_2006", 
               "COUNCIL_AREA", "GRIDLINK_POS_ACCURACY", 
               "NEVER_DIGITISED", "URBRUR_2FOLD", 
               "URBRUR_3FOLD")

# Check the below codes match the code range on the data dictionary.
# Check that leading zeros are included as expected

for (i in freq_cols){
  
  postcode %>% group_by(!!as.name(i)) %>% count() %>% print(n = Inf)
  
}

# Check LOCALITY, SIMD and OUTPUT_AREA_1991

postcode %>% group_by(LOCALITY) %>% count() %>% View()
postcode %>% group_by(SIMD) %>% count() %>% View()
postcode %>% group_by(OUTPUT_AREA_1991) %>% count() %>% View()



### 4 Grid Checks ----

# Check that the below fields are grid references

postcode %>% group_by(GRID_REF_EASTING) %>% count() %>% View()
postcode %>% group_by(GRID_REF_NORTHING) %>% count() %>% View()
postcode %>% group_by(LATITUDE) %>% count() %>% View()
postcode %>% group_by(LONGITUDE) %>% count() %>% View()



### 5 Counts Check ----

# check that the below fields are counts

postcode %>% group_by(DELIVERY_POINT_COUNT) %>% count() %>% View()
postcode %>% group_by(DELIVERY_POINT_COUNT_NR) %>% count() %>% View()
postcode %>% group_by(HOUSEHOLD_COUNT) %>% count() %>% View()
postcode %>% group_by(HOUSEHOLD_COUNT_1991) %>% count() %>% View()
postcode %>% group_by(RESIDENT_COUNT_1991) %>% count() %>% View()
postcode %>% group_by(CENSUS_HHOLDCOUNT_2001) %>% count() %>% View()
postcode %>% group_by(CENSUS_POPULATION_COUNT_2001) %>% count() %>% View()
postcode %>% group_by(CENSUS_HHOLD_COUNT_2011) %>% count() %>% View()
postcode %>% group_by(CENSUS_POPULATION_COUNT_2011) %>% count() %>% View()



### 6 Postcodes ----

# Check postcode info

postcode %>% group_by(POSTCODE) %>% count() %>% View()
postcode %>% group_by(POSTCODE_7) %>% count() %>% View()
postcode %>% group_by(LINK_SMALL_POSTCODE) %>% count() %>% View()
postcode %>% group_by(PCSULINK) %>% count() %>% View()

pc_cols <- c("SPLIT_POSTCODE", "SPLITSULINK", "POSTCODE_TYPE", 
             "IMPUTED", "SPLIT_IND", "POSTCODE_INDICATOR", 
             "SOURCE_FLAG", "GRIDLINK_GRID_REF_INDICATOR")

for (i in pc_cols){
  
  postcode %>% group_by(!!as.name(i)) %>% count() %>% print(n = Inf)
  
}



### 7 Misc Fields ----

# Check misc fields

misc_cols <- c("ONS_DHA", "USER_ID", "COMMENTS", "CD_YEAR", 
               "CD_QUARTER")

for (i in misc_cols){
  
  postcode %>% group_by(!!as.name(i)) %>% count() %>% print(n = Inf)
  
}



### 8 Dates ----

# Check date fields look ok

postcode %>% group_by(INTRODUCTION_DATE) %>% count() %>% View()
postcode %>% filter(SOURCE_FLAG != "MNUL") %>% group_by(INTRODUCTION_DATE) %>% count() %>% View()
postcode %>% group_by(DELETION_DATE) %>% count() %>% View()
postcode %>% group_by(DATE_OF_INSERTION) %>% count() %>% View()



### 9 Dropped/Nulled Fields ----

# Check dropped, nulled or no longer used fields

postcode %>% group_by(LGR_1995) %>% count() %>% print(n = Inf)

# For LGR_1995, non blank fields refer to dummy postcodes

postcode %>% filter(!is.na(LGR_1995)) %>% group_by(SOURCE_FLAG) %>% count() %>% 
  print(n = Inf)

null_cols <- c("LOCALITY_2010", "SETTLEMENT_2010", "LOCALITY_2006", 
               "SETTLEMENT_2006", "LOCALITY_2008", "SETTLEMENT_2008", 
               "EW_1995", "EW_1999", "SPARLCON", 
               "SPARLREG", "NUTS", "LGR_1991", 
               "EURO_CONSTITUENCY", "RED_DW_1991", "MS_ACCOM_IND", 
               "STUD_ACCOM_IND", "SH_ACCOM_IND", 
               "COMMUNITY_HEALTH_PARTS", "SPC_0511", "SPR_0511", 
               "OLD_INTRODUCTION_DATE", "OLD_DELETION_DATE", 
               "OLD_GRID_REF", "LEC", "TTWA", "LOCALITY_2012", 
               "SETTLEMENT_2012", "WORKPLACE_ZONE_2011", "LAU_2011_LEVEL1", 
               "LAU_2011_LEVEL2", "NUTS_2013_LEVEL2", "NUTS_2013_LEVEL3")

for (i in null_cols){
  
  postcode %>% group_by(!!as.name(i)) %>% count() %>% print(n = Inf)
  
}



### 10 Totals ----

# Check Total number of records in the file - Should increase each time
# UPDATE THIS EACH TIME THE SYNTAX IS RUN
# 2015_2 = 2,555,422
# 2016_1 = 2,555,760
# 2016_2 = 2,555,643 - number has went down due to NRS now sending single record 
#                      file instead of current combined
# 2016_2 = 2,555,828 - checked in March 2017 ahead of moving to production, 
#                      increase is due to increased manual postcodes (DR)
# 2019_2 = 2,633,240


# Check frequencies of postcode source flag

postcode %>% 
  group_by(SOURCE_FLAG) %>% 
  summarise(n = n()) %>% 
  mutate(pc = round_half_up(n / sum(n) * 100, 1))

# 2015_2: DUMM (0.0%) GROL (1.6%) GROS (7.0%) MNUL (0.0%) ONS (91.4%)
# 2016_1: DUMM (0.0%) GROL (1.6%) GROS (7.0%) MNUL (0.0%) ONS (91.4%)
# 2016_2: DUMM (0.0%) GROL (1.6%) GROS (7.0%) MNUL (0.0%) ONS (91.4%)
# 2019_2: DUMM (0.0%) GROL (1.6%) GROS (6.9%) MNUL (0.0%) ONS (91.5%)

# Check frequencies of postcode type for Scottish postcodes
# Should be around 80% SUs 20% LUs

postcode %>% 
  filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  group_by(POSTCODE_TYPE) %>% 
  summarise(n = n()) %>% 
  mutate(pc = n/sum(n)*100)

# 2015_2:  L (18.9%) S (81.1%)
# 2016_1:  L (18.9%) S (81.1%)
# 2016_2:  L (18.4%) S (81.6%)
# 2019_2:  L (18.4%) S (81.6%)


# Compare number of Scotland postcodes with the number on the single record file
# SPD 2019_2 contains 223,286 postcodes

# Scottish postcodes on APEXT

postcode %>% filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% count()



### 11 Geographies ----

# Check Health Board 2006 9 digit codes against the 2 digit codes

postcode %>% filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  group_by(HEALTH_BOARD_AREA_2006, SG_HB_AREA) %>% count()

# Check Health Board 2019 against the Health Board cypher
# Should be 8 postcodes with HB2018 21 and HB2019 32 - otherwise the same

postcode %>% filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  group_by(HEALTH_BOARD_2019, HEALTH_BOARD_2018, HEALTH_BOARD_2014, 
           HEALTH_BOARD_CYPHER) %>% count()

# Ensure those without Health Board are English postcodes (ONS), 
# manual additions (MNUL) or dummy postcodes (DUMM)

postcode %>% filter(is.na(HEALTH_BOARD_2019)) %>% 
  group_by(SOURCE_FLAG) %>% count()

# Check Council Area derived field is populated correctly by comparing with 
# 9 digit code
# Should be 8 postcodes with CA2018 46 and CA2019 50

postcode %>% filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  group_by(COUNCIL_AREA, COUNCIL_AREA_2018, COUNCIL_AREA_2019) %>% count() %>% 
  print(n = Inf)



### 12 ONS Postcodes ----

# Select all ONS postcodes
# These are postcodes from elsewhere in the UK

ons_postcodes <- postcode %>% 
  filter(SOURCE_FLAG == "ONS")

# All ONS postcodes should have S08200001 for their Health Board fields

hb_cols <- c("HEALTH_BOARD_2019", "HEALTH_BOARD_2018", "HEALTH_BOARD_2014", 
             "SG_HB_AREA")

for (i in hb_cols){
  
  ons_postcodes %>% group_by(!!as.name(i)) %>% count() %>% print(n = Inf)
  
}

# All ONS postcodes should have S37999998 for their HSCP fields

hscp_cols <- c("HSCP_2019", "HSCP_2018", "HSCP_2016")

for (i in hscp_cols){
  
  ons_postcodes %>% group_by(!!as.name(i)) %>% count() %>% print(n = Inf)
  
}

# All ONS postcodes should have S03999985 for their CHP_2004 field

ons_postcodes %>% group_by(CHP_2004) %>% count() %>% print(n = Inf)

rm(ons_postcodes, hb_cols, hscp_cols)



### 13 Dummy Postcodes ----

# Select all dummy postcodes

dummy_postcodes <- postcode %>% 
  filter(SOURCE_FLAG == "DUMM") %>% 
  select(POSTCODE, HEALTH_BOARD_CYPHER, HEALTH_BOARD_AREA, SG_HB_AREA, 
         HEALTH_BOARD_AREA_2006, HEALTH_BOARD_2014, HEALTH_BOARD_2018, 
         HEALTH_BOARD_2019, HSCP_2019, HSCP_2018, HSCP_2016, CHP_2004)

lookup <- postcode %>% 
  select(HEALTH_BOARD_2019, HEALTH_BOARD_CYPHER) %>% 
  distinct()

cypher_check <- dummy_postcodes %>% 
  filter(HSCP_2016 != "S37999999" & !is.na(HSCP_2016))


mnul <- postcode %>% 
  filter(SOURCE_FLAG == "MNUL") %>% 
  select(POSTCODE, HEALTH_BOARD_CYPHER, HEALTH_BOARD_AREA, SG_HB_AREA, 
         HEALTH_BOARD_AREA_2006, HEALTH_BOARD_2014, HEALTH_BOARD_2018, 
         HEALTH_BOARD_2019, HSCP_2019, HSCP_2018, HSCP_2016, CHP_2004)


### 14 PC GEOGRAPHY VIEW ----

# Read in SPD for comparison
# Select and format relevant columns and rename

# spd <- readRDS(glue("{spd_path}/Scottish_Postcode_Directory_2020_2.rds")) %>% 
spd <- readRDS(glue("/conf/linkage/output/lookups/Archive/", 
                    "Scottish_Postcode_Directory_2019_2.rds")) %>% 
  # select(pc8, grid_reference_northing, grid_reference_easting, 
  #        date_of_introduction, date_of_deletion, datazone2011, intzone2011, 
  #        oa2011, ca2019, ew2019, lgd_1995, spr2014, spc2014, upc2005, ur8_2016, 
  #        lau_level1_2019, hb2019, hscp2019) %>% 
  # mutate(grid_reference_northing = as.character(grid_reference_northing), 
  #        grid_reference_easting = as.character(grid_reference_easting), 
  #        lgd_1995 = as.character(lgd_1995), 
  #        lgd_1995 = str_pad(lgd_1995, 2, pad = "0"), 
  #        ur8_2016 = as.character(ur8_2016)) %>% 
  select(pc8, Grid_Reference_Northing, Grid_Reference_Easting, 
         Date_Of_Introduction, Date_Of_Deletion, DataZone2011, IntZone2011, 
         OA2011, CA2019, EW2019, LGD_1995, SPR2014, SPC2014, UPC2005, UR8_2016, 
         LAU_Level1_2019, HB2019, HSCP2019) %>% 
  mutate(Grid_Reference_Northing = as.character(Grid_Reference_Northing), 
         Grid_Reference_Easting = as.character(Grid_Reference_Easting), 
         LGD_1995 = as.character(LGD_1995), 
         LGD_1995 = str_pad(LGD_1995, 2, pad = "0"), 
         UR8_2016 = as.character(UR8_2016)) %>% 
  set_colnames(c("POSTCODE", "GRID_REF_NORTHING", "GRID_REF_EASTING", "INTRODUCTION_DATE", 
                 "DELETION_DATE", "DATAZONE", "INTERMEDIATE_ZONE", "OUTPUT_AREA", "COUNCIL_AREA", 
                 "ELECTORAL_WARD", "LGD", "SCOTTISH_PARL_REGION", "SCOTTISH_CONSTITUENCY", 
                 "UK_CONSTITUENCY", "URBAN_RURAL", "LAU_1", "HEALTH_BOARD", 
                 "HSCP"))

# Read in PC_GEOGRAPHY_VW data and filter for postcodes in spd
# Sort data by postcode and for each postcode, select the postcode with the 
# most recent end date and then select columns
# Format dates

pc_geo_vw <- tbl(APXU_connection, "PC_GEOGRAPHY_VW") %>%
  collect() %>% 
  filter(POSTCODE %in% spd$POSTCODE) %>% 
  arrange(POSTCODE) %>% 
  group_by(POSTCODE) %>% 
  top_n(1, END_DATE) %>% 
  select(POSTCODE, GRID_REF_NORTHING, GRID_REF_EASTING, INTRODUCTION_DATE, 
         DELETION_DATE, DATAZONE, INTERMEDIATE_ZONE, OUTPUT_AREA, COUNCIL_AREA, 
         ELECTORAL_WARD, LGD, SCOTTISH_PARL_REGION, SCOTTISH_CONSTITUENCY, 
         UK_CONSTITUENCY, URBAN_RURAL, LAU_1, HEALTH_BOARD, 
         HSCP) %>% 
  mutate(INTRODUCTION_DATE = lubridate::as_date(INTRODUCTION_DATE), 
         DELETION_DATE = lubridate::as_date(DELETION_DATE))

# Check if spd matches pc_geo_vw 

all_equal(spd, pc_geo_vw)
