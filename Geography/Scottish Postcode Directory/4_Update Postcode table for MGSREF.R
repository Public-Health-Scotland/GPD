##########################################################
# Update Postcode table for MGSREF
# Calum Purdie
# Original date 20/12/2019
# Latest update author - Calum Purdie
# Latest update date - 29/01/2021
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for creating postcode table output for MGSREF from SPD
##########################################################



### 1 - Housekeeping ----

library(magrittr)
library(dplyr)
library(readr)
library(tidylog)
library(data.table)
library(fs)
library(readxl)
library(janitor)
library(lubridate)
library(glue)

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                      "1_Geography/Scottish Postcode Directory")
lookup_filepath <- glue("{base_filepath}/Lookup Files")
spd_filepath <- glue("Geography/Scottish Postcode Directory")
CHP_filepath <- glue("{base_filepath}/Source Data/2019_2/Postcode_CHP_Lookups")

version <- "2020_2"



### 2 Read in Data ----

# Use csv version to get correct columns
# rds version has CA2019Name, HB2019Name etc
# Remove pc7 as this column is not required
# Change date format

SPD <- fread(glue("{lookup_filepath}/Scottish_Postcode_Directory_", 
                  "{version}.csv")) %>% 
  select(-pc7) %>% 
  rename(Postcode = pc8, 
         Postcode_Type = PostcodeType, 
         pcsulink = PCSuLink,
         splitsulink = SplitSuLink, 
         Delivery_Point_Count = DPC, 
         Delivery_Point_Count_NR = DPC_NR, 
         Household_Count = HHC, 
         Council_Area_2019 = CA2019, 
         Council_Area_2018 = CA2018, 
         Council_Area_2011 = CA2011, 
         UK_Parl_Const_2005 = UPC2005, 
         Scottish_Parl_Reg_2014 = SPR2014, 
         Scottish_Parl_Const_2014 = SPC2014, 
         Electoral_Ward_2019 = EW2019, 
         Health_Board_Area_2019 = HB2019, 
         Health_Board_Area_2018 = HB2018,
         Health_Board_Area_2014 = HB2014,
         Health_Board_Area_2006 = HB2006,
         Health_Board_Area_1995 = HB1995,
         HSC_Partnership_2019 = HSCP2019, 
         HSC_Partnership_2018 = HSCP2018, 
         HSC_Partnership_2016 = HSCP2016, 
         Output_Area_2011 = OA2011, 
         Output_Area_2001 = OA2001, 
         Output_Area_1991 = OA1991, 
         DataZone_2011 = DataZone2011, 
         DataZone_2001 = DataZone2001, 
         IntZone_2011 = IntZone2011, 
         IntZone_2001 = IntZone2001, 
         Census_Household_Count_2011 = HHC2011, 
         Census_Population_Count_2011 = Pop2011, 
         Census_Household_Count_2001 = HHC2001, 
         Census_Population_Count_2001 = Pop2001, 
         Census_Household_Count_1991 = HHC1991, 
         Census_Population_Count_1991 = Pop1991, 
         Scot_Indx_Mult_Depr_2020v2_Rank = SIMD2020v2_rank, 
         LAU_2019_Level1 = LAU_Level1_2019, 
         NUTS_2018_Level2 = NUTS_Level2_2018, 
         NUTS_2018_Level3 = NUTS_Level3_2018, 
         Enterprise_Region_2008 = Ent_Region_2008, 
         National_Park_2010 = Nat_Park_2010, 
         Registration_District_2007 = Reg_Dist_2007, 
         ROA_Local_2006 = ROA_local_2006, 
         Strategic_Dev_Plan_Area_2013 = SDPA_2013, 
         Travel_To_Work_Area_2011 = TTWA_2011, 
         Urban_Rural_2Fold_2016 = UR2_2016, 
         Urban_Rural_3Fold_2016 = UR3_2016, 
         Urban_Rural_6Fold_2016 = UR6_2016, 
         Urban_Rural_8Fold_2016 = UR8_2016, 
         GridlinkIndicator = Gridlink_Ind)



### 3 Reformat Columns to Meet Requirements ----

# Reformat dates to DD/MM/YYYY H:M:S

SPD %<>% 
  mutate(Date_Of_Introduction = format(dmy(Date_Of_Introduction), 
                                       "%d/%m/%Y %H:%M:%S"), 
         Date_Of_Deletion = format(dmy(Date_Of_Deletion), 
                                   "%d/%m/%Y %H:%M:%S"))


# Round Latitude and Longitude to 8 decimal places

SPD %<>% 
  mutate(Latitude = round_half_up(Latitude, 8), 
         Longitude = round_half_up(Longitude, 8))



### 4 Add Old Columns ----

# Mutate in blank columns for old columns no longer in SPD that are still
# required for updating the postcode table

SPD %<>%
  mutate(Electoral_Ward_2011 = NA, 
         Scot_Indx_Mult_Depr_2012_Rank = NA, 
         LAU_2011_Level1 = NA, 
         LAU_2011_Level2 = NA, 
         NUTS_2008_Level2 = NA, 
         NUTS_2008_Level3 = NA, 
         Locality_2012 = NA, 
         Settlement_2012 = NA, 
         Islands_2014 = NA, 
         Travel_To_Work_Area_2007 = NA, 
         Urban_Rural_6Fold_2013_2014 = NA, 
         Urban_Rural_8Fold_2013_2014 = NA, 
         Scot_Indx_Mult_Depr_2016_Rank = NA, 
         Islands_2016 = NA)

# Match on data for CHP
# These columns need to be filled in for upload requirements
# Use lookup files provided by NRS for 2019_2 postcodes
# Use !! tidyeval to unquote names
# Running this function can produce warnings
# These are generally around columns that are not used, e.g. dates

CHP_match <- function(lookup, column, new_name){
  
  data <- read_xlsx(glue("{CHP_filepath}/{lookup}.xlsx")) %>% 
    select(Postcode, column) %>% 
    rename(!!new_name := column)
  
  SPD %<>% 
    left_join(data)
  
}

# CHP_2004
SPD <- CHP_match("Postcode_CHP2004", "CommunityHealthPartnership2004Code", 
                 "CHP_2004")

# CHP_2007
SPD <- CHP_match("Postcode_CHP2007", "CommunityHealthPartnership2007Code", 
                 "CHP_2007")

# CHP_Sub_2011
SPD <- CHP_match("Postcode_CHP_SubArea_2011", 
                 "CommunityHealthPartnershipSubArea2011Code", "CHP_Sub_2011")

# CHP_2011
SPD <- CHP_match("Postcode_CHP2011", "CommunityHealthPartnership2011Code", 
                 "CHP_2011")

# CHP_2012
SPD <- CHP_match("Postcode_CHP2012", "CommunityHealthPartnership2012Code", 
                 "CHP_2012")

# Reorder columns

SPD %<>% select(Postcode, SplitChar, PCDistrict, PCSector, 
                Date_Of_Introduction, Date_Of_Deletion, Postcode_Type, 
                pcsulink, splitsulink, Imputed, Delivery_Point_Count, 
                Delivery_Point_Count_NR, Household_Count, 
                Grid_Reference_Easting, Grid_Reference_Northing, 
                Latitude, Longitude, Split_Indicator, 
                Council_Area_2011, UK_Parl_Const_2005, 
                Scottish_Parl_Reg_2014, Scottish_Parl_Const_2014, 
                Electoral_Ward_2011, Health_Board_Area_2014, 
                Health_Board_Area_2006, Health_Board_Area_1995, 
                HSC_Partnership_2016, CHP_2012, CHP_2011, CHP_Sub_2011, 
                CHP_2007, CHP_2004, Output_Area_2011, Output_Area_2001, 
                Output_Area_1991, DataZone_2011, DataZone_2001,
                IntZone_2011, IntZone_2001,
                Census_Household_Count_2011, Census_Population_Count_2011, 
                Census_Household_Count_2001, Census_Population_Count_2001, 
                Census_Household_Count_1991, Census_Population_Count_1991, 
                Scot_Indx_Mult_Depr_2012_Rank, LAU_2011_Level1, 
                LAU_2011_Level2, NUTS_2008_Level2, NUTS_2008_Level3, 
                Locality_2012, Locality_2001, Locality_1991, 
                Settlement_2012, Settlement_2001, Civil_Parish_1930, 
                Enterprise_Region_2008, Islands_2014, LGD_1995, 
                LGD_1991, National_Park_2010, Registration_District_2007, 
                ROA_CPP_2006, ROA_Local_2006, 
                Strategic_Dev_Plan_Area_2013, Travel_To_Work_Area_2007, 
                Urban_Rural_6Fold_2013_2014, Urban_Rural_8Fold_2013_2014, 
                GridlinkIndicator, Council_Area_2019, Council_Area_2018, 
                Electoral_Ward_2019, Health_Board_Area_2019, 
                Health_Board_Area_2018, HSC_Partnership_2019, 
                HSC_Partnership_2018, Scot_Indx_Mult_Depr_2016_Rank, 
                LAU_2019_Level1, NUTS_2018_Level2, NUTS_2018_Level3, 
                Locality_2016, Settlement_2016, Islands_2016, 
                Travel_To_Work_Area_2011, Urban_Rural_2Fold_2016, 
                Urban_Rural_3Fold_2016, Urban_Rural_6Fold_2016, 
                Urban_Rural_8Fold_2016, Gridlink_Pos_Accuracy, 
                NeverDigitised, Scot_Indx_Mult_Depr_2020v2_Rank, Islands_2020)


### 4 Save file as csv ----

write_csv(SPD, glue("{lookup_filepath}/Scottish_Postcode_Directory_{version}_", 
                    "CDW.csv"), na = "")