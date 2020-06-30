##########################################################
# Create Postcode Lookup Files
# Calum Purdie
# Original date 07/08/2018
# Data release - Scottish Postcode Directory
# Latest update author - Calum Purdie
# Latest update date - 19/05/2020
# Latest update description 
# Type of script - Preparation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for creating PHS version of the Scottish Postcode Directory
# Approximate run time - 3 minutes
##########################################################

### 1 - Housekeeping ----

library(magrittr)
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(tidylog)
library(glue)
library(janitor)
library(here)
library(ckanr)

# Set version to use

version <- "2020_1"

# Update filepaths for new version

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                      "1_Geography/Scottish Postcode Directory")
data_filepath <- glue("{base_filepath}/Source Data/{version}")
output_filepath <- glue("{base_filepath}/Lookup Files/R Files")
simd_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                      "3_Deprivation/SIMD/Lookup Files/SIMD 2020")

# There have previously been issues when comparing Latitude/Longitude between 
# the R version and SPSS version
# Changing the number of digits seems to solve this

options(digits = 15)



### 2 - Rename variables ----

# Import Scottish Postcode Directory
# Use read.csv as read_csv and fread produced a couple of errors to do with
# decimals
# Sort cases by Postcode

spd <- read.csv(glue("{data_filepath}/SingleRecord.csv"), 
                     stringsAsFactors = F) %>% 
  arrange(Postcode)

# Rename variables

spd %<>%
  rename(pc8 = Postcode, 
         split_char = SplitChar, 
         pc_district = PostcodeDistrict, 
         pc_sector = PostcodeSector, 
         date_of_introduction = DateOfIntroduction, 
         date_of_deletion = DateOfDeletion, 
         postcode_type = PostcodeType, 
         pc_su_link = LinkedSmallUserPostcode, 
         split_su_link = LinkedSmallUserPostcodeSplitChar,
         imputed = Imputed,
         dpc = DeliveryPointCount,
         dpc_nr  = DeliveryPointCountNonResidential,
         hhc = HouseholdCount,
         grid_reference_easting = GridReferenceEasting,
         grid_reference_northing = GridReferenceNorthing,
         latitude = Latitude,
         longitude = Longitude,
         split_indicator = SplitIndicator,
         ca2019 = CouncilArea2019Code,
         upc2005 = UKParliamentaryConstituency2005Code,
         spr2014 = ScottishParliamentaryRegion2014Code,
         spc2014 = ScottishParliamentaryConstituency2014Code,
         ew2019 = ElectoralWard2019Code,
         hb2019 = HealthBoardArea2019Code,
         hb2006 = HealthBoardArea2006Code,
         hb1995 = HealthBoardArea1995Code,
         hscp2019 = IntegrationAuthority2019Code,
         oa2011 = OutputArea2011Code,
         oa2001 = OutputArea2001Code,
         oa1991 = OutputArea1991Code,
         datazone2011 = DataZone2011Code,
         datazone2001 = DataZone2001Code,
         intzone2011 = IntermediateZone2011Code,
         intzone2001 = IntermediateZone2001Code,
         hhc2011 = CensusHouseholdCount2011,
         pop2011 = CensusPopulationCount2011,
         hhc2001 = CensusHouseholdCount2001,
         pop2001 = CensusPopulationCount2001,
         hhc1991 = CensusHouseholdCount1991,
         pop1991 = CensusPopulationCount1991,
         simd2016_rank = ScottishIndexOfMultipleDeprivation2016Rank,
         lau_level1_2019 = LAU2019Level1Code,
         nuts_level2_2018 = NUTS2018Level2Code,
         nuts_level3_2018 = NUTS2018Level3Code,
         locality_2016 = Locality2016Code,
         locality_2001 = Locality2001Code,
         locality_1991 = Locality1991Code,
         settlement_2016 = Settlement2016Code,
         settlement_2001 = Settlement2001Code,
         civil_parish_1930 = CivilParish1930Code,
         ent_region_2008 = EnterpriseRegion2008Code,
         islands_2016 = Islands2016Code,
         lgd_1995 = LocalGovernmentDistrict1995Code,
         lgd_1991 = LocalGovernmentDistrict1991Code,
         nat_park_2010 = NationalPark2010Code,
         reg_dist_2007 = RegistrationDistrict2007Code,
         roa_cpp_2006 = ROACommunityPlanningPartnership2006Code,
         roa_local_2006 = ROALocal2006Code,
         sdpa_2013 = StrategicDevelopmentPlanningArea2013Code,
         ttwa_2011 = TravelToWorkArea2011Code,
         ur6_2016 = UrbanRural6Fold2016Code,
         ur8_2016 = UrbanRural8Fold2016Code,
         gridlink_ind = GridlinkIndicator,
         gridlink_pos_accuracy = GridLinkPositionalAccuracy,
         never_digitised = NeverDigitised, 
         ca2011 = CouncilArea2011Code,
         hb2014 = HealthBoardArea2014Code,
         hscp2016 = IntegrationAuthority2016Code,
         ca2018 = CouncilArea2018Code, 
         hb2018 = HealthBoardArea2018Code,
         hscp2018 = IntegrationAuthority2018Code)



### 3 - Change Variable Formats ----

# Change date variables into correct format

spd %<>%
  mutate(date_of_introduction = as.Date(date_of_introduction, "%d/%m/%Y"), 
         date_of_deletion = as.Date(date_of_deletion, "%d/%m/%Y"))


# Add leading zeroes to some columns
# If this is missed the file won't show as a 100% comparison with the SPSS 
# version for quality assurance check

spd %<>% 
  mutate(locality_2001 = str_pad(locality_2001, 6, pad = "0"), 
         settlement_2001 = str_pad(settlement_2001, 3, pad = "0"), 
         locality_1991 = str_pad(locality_1991, 3, pad = "0"), 
         islands_2016 = str_pad(islands_2016, 3, pad = "0"))

# Create 7 character postcode variable

spd %<>% 
  mutate(pc7 = case_when(nchar(pc8) == 6 ~ gsub(" ","  ",pc8), 
                         nchar(pc8) == 7 ~ pc8,
                         nchar(pc8) == 8 ~ gsub(" ", "", pc8))) %>%
  arrange(pc7, desc(date_of_introduction), date_of_deletion)

##### 2020_1 ONE OFF #####

# Calum noticed an issue with TD6 9LQ's Intermediate Zone
# This postcode's intermediate zone has changed from S02002301 to S02002296 but 
# all other postcode in it's data zone (S01012292) have stayed within S02002301
# NRS agree this is a mistake and can be manually fixed as a one off for 2020_1

spd %<>%
  mutate(intzone2011 = case_when(pc7 == "TD6 9LQ" ~ "S02002301", 
                                 TRUE ~ intzone2011))

# Join on UR2 and UR3 columns from postcode lookup

UR_lookup <- read_excel(glue("{data_filepath}/ISD_UR16 2 and 3 Fold.xlsx")) %>% 
  rename(pc8 = Postcode, 
         ur2_2016 = UrbanRural2Fold2016Code, 
         ur3_2016 = UrbanRural3Fold2016Code)

# Add in UR name columns

spd %<>% 
  left_join(UR_lookup) %>% 
  mutate(ur2_2016_name = case_when(ur2_2016 == 1 ~ "1 Urban Areas", 
                                   ur2_2016 == 2 ~ "2 Rural Areas"),
         ur3_2016_name = case_when(ur3_2016 == 1 ~ "1 Rest of Scotland", 
                                   ur3_2016 == 2 ~ "2 Accessible Rural",
                                   ur3_2016 == 3 ~ "3 Remote Rural"),
         ur6_2016_name = case_when(ur6_2016 == 1 ~ "1 Large Urban Areas", 
                                   ur6_2016 == 2 ~ "2 Other Urban Areas",
                                   ur6_2016 == 3 ~ "3 Accessible Small Towns",
                                   ur6_2016 == 4 ~ "4 Remote Small Towns", 
                                   ur6_2016 == 5 ~ "5 Accessible Rural",
                                   ur6_2016 == 6 ~ "6 Remote Rural"),
         ur8_2016_name = case_when(ur8_2016 == 1 ~ "1 Large Urban Areas", 
                                   ur8_2016 == 2 ~ "2 Other Urban Areas",
                                   ur8_2016 == 3 ~ "3 Accessible Small Towns",
                                   ur8_2016 == 4 ~ "4 Remote Small Towns", 
                                   ur8_2016 == 5 ~ "5 Very Remote Small Towns",
                                   ur8_2016 == 6 ~ "6 Accessible Rural",
                                   ur8_2016 == 7 ~ "7 Remote Rural", 
                                   ur8_2016 == 8 ~ "8 Very Remote Rural"))

# # Join on SIMD 2020 from postcode lookup
# 
# simd_lookup <- read_excel(glue("{data_filepath}/ISD_SIMD2020.xlsx")) %>% 
#   rename(pc8 = Postcode, 
#          simd2020_rank = ScottishIndexOfMultipleDeprivation2020Rank)

# Join on SIMD 2020 from DataZone2011_simd2020v2 lookup

simd_lookup <- readRDS(glue("{simd_filepath}/DataZone2011_simd2020v2.rds")) %>% 
  select(datazone2011, simd2020v2_rank)

spd %<>% 
  left_join(simd_lookup)

# Set all blank cells as NA in R file

spd %<>%
  mutate_if(is.character, list(~na_if(., "")))



### 4 - Use Open Data API for Column Names ----

# Use the Geography Codes and Names open data file to get the names
# First need to run the httr configuration script

source(here::here("Geography", "Scottish Postcode Directory", 
                  "Set httr configuration for API.R"))

# Add columns for datazone2011name

# Set url and id

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id <- "395476ab-0720-4740-be07-ff4467141352"

dz_names <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  select(DataZone, DataZoneName) %>% 
  rename(datazone2011 = DataZone, datazone2011name = DataZoneName) %>%  
  as_tibble()


# Add column for intzone2011name

# Set url and id

res_id <- "e3e885cc-2530-4b3c-bead-9eda9782264f"

iz_names <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  select(IntZone, IntZoneName) %>% 
  rename(intzone2011 = IntZone, intzone2011name = IntZoneName) %>%  
  as_tibble()


# Add column for ca2019name

# Set url and id

res_id <- "2dab0c9d-09be-4266-97f8-4f83e78db85f"

ca_names <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  select(CA, CAName) %>% 
  rename(ca2019 = CA, ca2019name = CAName) %>%  
  as_tibble()


# Add column for hscp2019name

# Set url and id

res_id <- "ccfeea67-2407-413c-9676-01c03527046a"

hscp_names <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  select(HSCP, HSCPName) %>% 
  rename(hscp2019 = HSCP, hscp2019name = HSCPName) %>%  
  as_tibble()


# Add column for hb2019name

# Set url and id

res_id <- "f177be64-e94c-4ddf-a2ee-ea58d648d55a"

hb_names <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  select(HB, HBName) %>% 
  rename(hb2019 = HB, hb2019name = HBName) %>%  
  as_tibble()


# Join the name columns onto the spd by DataZone2011
# As a result of the 2019 boundary change, there are 8 postcodes moving from 
# Glasgow City to North Lanarkshire
# We need to manually recode the name columns for these postcodes as matching
# on by DataZone2011 uses the old geography names, i.e. Lanarkshire codes but
# Glasgow names

spd %<>%
  left_join(dz_names) %>%
  left_join(iz_names) %>%
  left_join(ca_names) %>%
  left_join(hscp_names) %>%
  left_join(hb_names)



### 5 - Check Data ----

# Check for any duplicates
# Contact NRS if there are any duplicated postcodes

spd %>% count(pc7) %>% filter(n > 1)

# Sort data by pc7 and make pc7 first column

spd %<>%
  arrange(pc7) %>%
  select(pc7, pc8:ca2019, ca2019name, ca2018, ca2011, upc2005:hb2019, 
         hb2019name, hb2018, hb2014, hb2006, hb1995, hscp2019, hscp2019name, 
         hscp2018, hscp2016, oa2011:oa1991, datazone2011, datazone2011name, 
         datazone2001, intzone2011, intzone2011name, intzone2001, 
         hhc2011:pop1991, simd2020v2_rank, lau_level1_2019:ttwa_2011, ur2_2016, 
         ur2_2016_name, ur3_2016, ur3_2016_name, ur6_2016, ur6_2016_name, 
         ur8_2016, ur8_2016_name, gridlink_ind:never_digitised)



### 5 - Save file as .RDS ----

saveRDS(spd, glue("{output_filepath}/Scottish_Postcode_Directory_{version}.rds"))
