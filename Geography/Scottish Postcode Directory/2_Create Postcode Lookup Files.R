### 1 - Information ----

# Codename - Check NRS SPD
# Data release - Scottish Postcode Directory
# Original Author - Calum Purdie
# Original Date - 07/08/2018
# Type - Preparation
# Written/run on - R Studio Desktop 
# Version - 3.3.2
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readr")
#
# Description - This document is based on the SPSS syntax for creating postcode lookup files
#               found within GPD folders. It is designed to allow for the same data checks and changes to be made
#               and to be consistent with the SPSS syntax
#
# Approximate run time - 77 seconds


# Read in packages from library

library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readxl)
library(tidylog)

# Update filepaths for new version

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD", "1_Geography", "Scottish Postcode Directory")
data_filepath <- file.path(base_filepath, "Source Data", "2019_2")
output_filepath <- file.path(base_filepath, "Lookup Files", "R Files")


# There has previously been issues when comparing Latitude/Longitude between the R version and SPSS version
# Changing the number of digits seems to solve this

options(digits = 15)

### 2 - Rename variables ----

# Import Scottish Postcode Directory
# Rename variables

SPD <- read.csv(file.path(data_filepath, "SingleRecord.csv"), stringsAsFactors = F) %>% 
  rename(pc8 = Postcode, 
         SplitChar = SplitChar, 
         PCDistrict = PostcodeDistrict, 
         PCSector = PostcodeSector, 
         Date_Of_Introduction = DateOfIntroduction, 
         Date_Of_Deletion = DateOfDeletion, 
         PostcodeType = PostcodeType, 
         PCSuLink = LinkedSmallUserPostcode, 
         SplitSuLink = LinkedSmallUserPostcodeSplitChar,
         Imputed = Imputed,
         DPC = DeliveryPointCount,
         DPC_NR  = DeliveryPointCountNonResidential,
         HHC = HouseholdCount,
         Grid_Reference_Easting = GridReferenceEasting,
         Grid_Reference_Northing = GridReferenceNorthing,
         Latitude = Latitude,
         Longitude = Longitude,
         Split_Indicator = SplitIndicator,
         CA2019 = CouncilArea2019Code,
         UPC2005 = UKParliamentaryConstituency2005Code,
         SPR2014 = ScottishParliamentaryRegion2014Code,
         SPC2014 = ScottishParliamentaryConstituency2014Code,
         EW2019 = ElectoralWard2019Code,
         HB2019 = HealthBoardArea2019Code,
         HB2006 = HealthBoardArea2006Code,
         HB1995 = HealthBoardArea1995Code,
         HSCP2019 = IntegrationAuthority2019Code,
         OA2011 = OutputArea2011Code,
         OA2001 = OutputArea2001Code,
         OA1991 = OutputArea1991Code,
         DataZone2011 = DataZone2011Code,
         DataZone2001 = DataZone2001Code,
         IntZone2011 = IntermediateZone2011Code,
         IntZone2001 = IntermediateZone2001Code,
         HHC2011 = CensusHouseholdCount2011,
         Pop2011 = CensusPopulationCount2011,
         HHC2001 = CensusHouseholdCount2001,
         Pop2001 = CensusPopulationCount2001,
         HHC1991 = CensusHouseholdCount1991,
         Pop1991 = CensusPopulationCount1991,
         SIMD2016rank = ScottishIndexOfMultipleDeprivation2016Rank,
         LAU_Level1_2019 = LAU2019Level1Code,
         NUTS_Level2_2018 = NUTS2018Level2Code,
         NUTS_Level3_2018 = NUTS2018Level3Code,
         Locality_2016 = Locality2016Code,
         Locality_2001 = Locality2001Code,
         Locality_1991 = Locality1991Code,
         Settlement_2016 = Settlement2016Code,
         Settlement_2001 = Settlement2001Code,
         Civil_Parish_1930 = CivilParish1930Code,
         Ent_Region_2008 = EnterpriseRegion2008Code,
         Islands_2016 = Islands2016Code,
         LGD_1995 = LocalGovernmentDistrict1995Code,
         LGD_1991 = LocalGovernmentDistrict1991Code,
         Nat_Park_2010 = NationalPark2010Code,
         Reg_Dist_2007 = RegistrationDistrict2007Code,
         ROA_CPP_2006 = ROACommunityPlanningPartnership2006Code,
         ROA_local_2006 = ROALocal2006Code,
         SDPA_2013 = StrategicDevelopmentPlanningArea2013Code,
         TTWA_2011 = TravelToWorkArea2011Code,
         UR6_2016 = UrbanRural6Fold2016Code,
         UR8_2016 = UrbanRural8Fold2016Code,
         Gridlink_Ind = GridlinkIndicator,
         Gridlink_Pos_Accuracy = GridLinkPositionalAccuracy,
         NeverDigitised = NeverDigitised, 
         CA2011 = CouncilArea2011Code,
         HB2014 = HealthBoardArea2014Code,
         HSCP2016 = IntegrationAuthority2016Code,
         CA2018 = CouncilArea2018Code, 
         HB2018 = HealthBoardArea2018Code,
         HSCP2018 = IntegrationAuthority2018Code)



### 3 - Change Variable Formats ----

# Change Date Variables Into Correct Format
SPD <- SPD %>%
  mutate(Date_Of_Introduction = as.Date(Date_Of_Introduction, "%d/%m/%Y")) %>% 
  mutate(Date_Of_Deletion = as.Date(Date_Of_Deletion, "%d/%m/%Y"))

# Add leading zeroes to some columns
# If this is missed the file won't show as a 100% comparison with the SPSS version for quality assurance check
SPD$Locality_2001 <- str_pad(SPD$Locality_2001, 6, pad = "0")
SPD$Settlement_2001 <- str_pad(SPD$Settlement_2001, 3, pad = "0")
SPD$Locality_1991 <- str_pad(SPD$Locality_1991, 3, pad = "0")
SPD$Islands_2016 <- str_pad(SPD$Islands_2016, 3, pad = "0")

# Create 7 character postcode variable
SPD <- SPD %>%
  mutate(pc7 = ifelse(nchar(pc8) == 6, gsub(" ","  ",pc8),
               ifelse(nchar(pc8) == 7, pc8,
               ifelse(nchar(pc8) == 8, gsub(" ","",pc8),NA)))) %>%
  arrange(pc7, desc(Date_Of_Introduction), Date_Of_Deletion)

# Add columns for HB2019Name, HSCP2019Name, CA2019Name, DataZone2011Name and IntZone2011Name

# Use the Geography Codes and Names open data file to get the names
# read_csv producing a timeout error when Calum tried to run it so best using read.csv

geo_names <- read.csv(file = "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/395476ab-0720-4740-be07-ff4467141352/download/geography_codes_and_labels_dz2011_01042019.csv") %>% 
  select(DZ2011, DZ2011Name, IZ2011Name, CA2011Name, HSCP2016Name, HB2014Name) %>% 
  rename(DataZone2011 = DZ2011, DataZone2011Name = DZ2011Name, IntZone2011Name = IZ2011Name, 
         CA2019Name = CA2011Name, HSCP2019Name = HSCP2016Name, HB2019Name = HB2014Name) %>% 
  mutate_if(is.factor, as.character)

# Join the name columns onto the SPD by DataZone2011
# As a result of the 2019 boundary change, there are 8 postcodes moving from 
# Glasgow City to North Lanarkshire
# We need to manually recode the name columns for these postcodes as matching
# on by DataZone2011 uses the old geography names, i.e. Lanarkshire codes but
# Glasgow names

SPD <- SPD %>% 
  left_join(geo_names) %>%
  mutate(HB2019Name = if_else(pc7 == "G33 6GS" | pc7 == "G33 6GT" | 
                              pc7 == "G33 6GU" | pc7 == "G33 6GW" | 
                              pc7 == "G33 6GX" | pc7 == "G33 6GY" |
                              pc7 == "G33 6GZ" | pc7 == "G33 6NS", 
                              "NHS Lanarkshire", HB2019Name), 
         HSCP2019Name = if_else(pc7 == "G33 6GS" | pc7 == "G33 6GT" | 
                                pc7 == "G33 6GU" | pc7 == "G33 6GW" | 
                                pc7 == "G33 6GX" | pc7 == "G33 6GY" |
                                pc7 == "G33 6GZ" | pc7 == "G33 6NS", 
                                "North Lanarkshire", HSCP2019Name), 
         CA2019Name = if_else(pc7 == "G33 6GS" | pc7 == "G33 6GT" | 
                              pc7 == "G33 6GU" | pc7 == "G33 6GW" | 
                              pc7 == "G33 6GX" | pc7 == "G33 6GY" |
                              pc7 == "G33 6GZ" | pc7 == "G33 6NS", 
                              "North Lanarkshire", CA2019Name))

# Join on UR2 and UR3 columns from postcode lookup

UR_lookup <- read_excel(file.path(data_filepath, "ISD_UR16 2 and 3 Fold.xlsx")) %>% 
  rename(pc8 = Postcode, 
         UR2_2016 = UrbanRural2Fold2016Code, 
         UR3_2016 = UrbanRural3Fold2016Code)

# Add in UR name columns

SPD <- SPD %>% 
  left_join(UR_lookup) %>% 
  mutate(UR2_2016_name = case_when(UR2_2016 == 1 ~ "1 Urban Areas", 
                                   UR2_2016 == 2 ~ "2 Rural Areas"),
         UR3_2016_name = case_when(UR3_2016 == 1 ~ "1 Rest of Scotland", 
                                   UR3_2016 == 2 ~ "2 Accessible Rural",
                                   UR3_2016 == 3 ~ "3 Remote Rural"),
         UR6_2016_name = case_when(UR6_2016 == 1 ~ "1 Large Urban Areas", 
                                   UR6_2016 == 2 ~ "2 Other Urban Areas",
                                   UR6_2016 == 3 ~ "3 Accessible Small Towns",
                                   UR6_2016 == 4 ~ "4 Remote Small Towns", 
                                   UR6_2016 == 5 ~ "5 Accessible Rural",
                                   UR6_2016 == 6 ~ "6 Remote Rural"),
         UR8_2016_name = case_when(UR8_2016 == 1 ~ "1 Large Urban Areas", 
                                   UR8_2016 == 2 ~ "2 Other Urban Areas",
                                   UR8_2016 == 3 ~ "3 Accessible Small Towns",
                                   UR8_2016 == 4 ~ "4 Remote Small Towns", 
                                   UR8_2016 == 5 ~ "5 Very Remote Small Towns",
                                   UR8_2016 == 6 ~ "6 Accessible Rural",
                                   UR8_2016 == 7 ~ "7 Remote Rural", 
                                   UR8_2016 == 8 ~ "8 Very Remote Rural"))


### 4 - Check Data ----

# Select distinct rows (i.e. ensure there are no duplicates)
# If total number of rows changes then there were duplicates in the original file - contact NRS if this happens
SPD <- distinct(SPD)

#Sort data by pc7 and make pc7 first column
SPD <- SPD %>%
  arrange(pc7) %>%
  select(pc7, pc8:CA2019, CA2019Name, CA2018, CA2011, UPC2005:HB2019, HB2019Name, HB2018, HB2014,
         HB2006, HB1995, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, OA2011:OA1991, DataZone2011, 
         DataZone2011Name, DataZone2001, IntZone2011, IntZone2011Name, IntZone2001, 
         HHC2011:TTWA_2011, UR2_2016, UR2_2016_name, UR3_2016, UR3_2016_name, UR6_2016, 
         UR6_2016_name, UR8_2016, UR8_2016_name, Gridlink_Ind:NeverDigitised)

### 5 - Save file as .RDS ----
saveRDS(SPD, file.path(output_filepath, "Scottish_Postcode_Directory_2019_2.rds"))
