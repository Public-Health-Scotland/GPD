start <- Sys.time()

### 1 - Information ----

# Codename - Check NRS SPD
# Data release - Scottish Postcode Directory
# Original Author - Calum Purdie
# Original Date - 06/08/2018
# Updated - 04/11/2019
# Type - Preparation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages(magrittr)
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages(tidylog)
# install.packages(glue)
#
# Description - Code for checking the Scottish Postcode Directory file receieved
#               from National Records of Scotland
#
# Approximate run time - 1 minute


### Read in packages from library

library(magrittr)
library(tidyr)
library(dplyr)
library(tidylog)
library(glue)

## Set filepath

data_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD", "1_Geography", 
                           "Scottish Postcode Directory", "Source Data", 
                           "2019_2")



### 2 - Tidying SPD data ----

# Import Scottish Postcode Directory
# Use read.csv as read_csv produced a couple of errors to do with decimals
# Sort cases by Postcode

SPD_data <- read.csv(glue("{data_filepath}/SingleRecord.csv"), 
                     stringsAsFactors = F) %>%
  arrange(Postcode)

# Total number of records in file - should increase from previous version
# Would expect roughly the same increase each time - if numbers don't look right, raise with NRS
# Update numbers below.

# 2014_1 = 218,501
# 2014_2 = 218,883
# 2015_1 = 219,207
# 2015_2 = 219,607
# 2016_1 = 219,631
# 2016_2 = 220,094
# 2017_1 = 220,564
# 2017_2 = 221,032
# 2018_1 = 221,562
# 2018_1.5 = 221,812
# 2018_2 = 222,107
# 2019_1 = 222,687
# 2019_1.5 = 222,930
# 2019_2 = 223,826

# Postcode Type - expected around  S (80%) L (20%) - compare with previous numbers
# 2014_1 S (81.0%) L (19.0%).
# 2014_2 S (81.0%) L (19.0%).
# 2015_1 S (81.0%) L (19.0%).
# 2015_2 S (81.0%) L (19.0%).
# 2016_1 S (81.0%) L (19.0%).
# 2016_2 S (80.6%) L (18.4%).
# 2017_1 S (81.6%) L (18.4%).
# 2017_2 S (81.6%) L (18.4%).
# 2018_1 S (81.6%) L (18.4%).
# 2018_1.5 S (81.6%) L (18.4%).
# 2018_2 S (81.6%) L (18.4%).
# 2019_1 S (81.6%) L (18.4%).
# 2019_1.5 S (81.6%) L (18.4%).
# 2019_2 S (81.6%) L (18.4%).

SPD_data %>% group_by(PostcodeType) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)*100)

# Quick sense check of HB and CA variables and ensure there are no blanks
# Things are fine if the outputs are all with value 0

SPD_data %>% summarise(missing = sum(is.na(HealthBoardArea2019Code)))
SPD_data %>% summarise(missing = sum(is.na(CouncilArea2019Code)))
SPD_data %>% summarise(missing = sum(is.na(IntegrationAuthority2019Code)))



### 3 - Check columns are correctly aligned ----

# As a result of the 2019 boundary change, 8 postcodes moved from Glasgow City
# Council to North Lanarkshire Council
# Check that these postcodes have moved to the correct Council Area

# List of postcodes that have moved
# G33 6GS, G33 6GT, G33 6GU, G33 6GW, G33 6GX, G33 6GY, G33 6GZ, G33 6NS

# 0 postcodes removed from North Lanarkshire

SPD_data %>% 
  filter(CouncilArea2018Code == 'S12000044' & 
         CouncilArea2019Code != 'S12000050') %>% 
  select(Postcode)

# 8 postcodes now in North Lanarkshire

SPD_data %>% 
  filter(CouncilArea2018Code == 'S12000046' & 
         CouncilArea2019Code != 'S12000049') %>% 
  select(Postcode)

# 8 postcodes now in North Lanarkshire

SPD_data %>% 
  filter(CouncilArea2018Code == 'S12000046' & 
         CouncilArea2019Code == 'S12000050') %>% 
  select(Postcode)

# Check that the Health Board 1995 and Health Board 2006 are correctly aligned
# All should be same Health Board apart from Argyll & Clyde should be split 
# between GG&C and Highland for HB2006
# Output should be tibble with all values as NA

SPD_data %>%
  mutate(HB_issue = case_when(HealthBoardArea1995Code == '01' & 
                              HealthBoardArea2006Code != 'S08000008' ~ 1, 
                              HealthBoardArea1995Code == '02' & 
                              HealthBoardArea2006Code != 'S08000006' ~ 2, 
                              HealthBoardArea1995Code == '03' & 
                              HealthBoardArea2006Code != 'S08000013' ~ 3,
                              HealthBoardArea1995Code == '04' & 
                              HealthBoardArea2006Code != 'S08000004' ~ 4,
                              HealthBoardArea1995Code == '05' & 
                              HealthBoardArea2006Code != 'S08000010' ~ 5,
                              HealthBoardArea1995Code == '06' & 
                              HealthBoardArea2006Code != 'S08000002' ~ 6,
                              HealthBoardArea1995Code == '07' & 
                              HealthBoardArea2006Code != 'S08000005' ~ 7,
                              HealthBoardArea1995Code == '08' & 
                              HealthBoardArea2006Code != 'S08000008' & 
                              HealthBoardArea2006Code != 'S08000007' ~ 8,
                              HealthBoardArea1995Code == '09' & 
                              HealthBoardArea2006Code != 'S08000007' ~ 9,
                              HealthBoardArea1995Code == '10' & 
                              HealthBoardArea2006Code != 'S08000009' ~ 10,
                              HealthBoardArea1995Code == '11' & 
                              HealthBoardArea2006Code != 'S08000001' ~ 11,
                              HealthBoardArea1995Code == '12' & 
                              HealthBoardArea2006Code != 'S08000003' ~ 12,
                              HealthBoardArea1995Code == '13' & 
                              HealthBoardArea2006Code != 'S08000011' ~ 13,
                              HealthBoardArea1995Code == '14' & 
                              HealthBoardArea2006Code != 'S08000012' ~ 14,
                              HealthBoardArea1995Code == '15' & 
                              HealthBoardArea2006Code != 'S08000014' ~ 15)) %>% 
  count(HB_issue)


# Check that the Health Board 2019 and Council Area are correctly aligned
# Output should be tibble with all values as NA

SPD_data %>%
  mutate(CA_HB_issue = case_when(CouncilArea2019Code == 'S12000005' & 
                                 HealthBoardArea2019Code != 'S08000019' ~ 1,
                                 CouncilArea2019Code == 'S12000006' & 
                                 HealthBoardArea2019Code != 'S08000017' ~ 2,
                                 CouncilArea2019Code == 'S12000008' & 
                                 HealthBoardArea2019Code != 'S08000015' ~ 3,
                                 CouncilArea2019Code == 'S12000010' &
                                 HealthBoardArea2019Code != 'S08000024' ~ 4,
                                 CouncilArea2019Code == 'S12000011' & 
                                 HealthBoardArea2019Code != 'S08000031' ~ 5,
                                 CouncilArea2019Code == 'S12000013' &
                                 HealthBoardArea2019Code != 'S08000028' ~ 6,
                                 CouncilArea2019Code == 'S12000014' & 
                                 HealthBoardArea2019Code != 'S08000019' ~ 7,
                                 CouncilArea2019Code == 'S12000017' & 
                                 HealthBoardArea2019Code != 'S08000022' ~ 8,
                                 CouncilArea2019Code == 'S12000018' & 
                                 HealthBoardArea2019Code != 'S08000031' ~ 9,
                                 CouncilArea2019Code == 'S12000019' & 
                                 HealthBoardArea2019Code != 'S08000024' ~ 10,
                                 CouncilArea2019Code == 'S12000020' & 
                                 HealthBoardArea2019Code != 'S08000020' ~ 11,
                                 CouncilArea2019Code == 'S12000021' & 
                                 HealthBoardArea2019Code != 'S08000015' ~ 12,
                                 CouncilArea2019Code == 'S12000023' & 
                                 HealthBoardArea2019Code != 'S08000025' ~ 13,
                                 CouncilArea2019Code == 'S12000026' & 
                                 HealthBoardArea2019Code != 'S08000016' ~ 14,
                                 CouncilArea2019Code == 'S12000027' & 
                                 HealthBoardArea2019Code != 'S08000026' ~ 15,
                                 CouncilArea2019Code == 'S12000028' & 
                                 HealthBoardArea2019Code != 'S08000015' ~ 16,
                                 CouncilArea2019Code == 'S12000029' & 
                                 HealthBoardArea2019Code != 'S08000032' ~ 17,
                                 CouncilArea2019Code == 'S12000030' & 
                                 HealthBoardArea2019Code != 'S08000019' ~ 18,
                                 CouncilArea2019Code == 'S12000033' & 
                                 HealthBoardArea2019Code != 'S08000020' ~ 19,
                                 CouncilArea2019Code == 'S12000034' & 
                                 HealthBoardArea2019Code != 'S08000020' ~ 20,
                                 CouncilArea2019Code == 'S12000035' & 
                                 HealthBoardArea2019Code != 'S08000022' ~ 21,
                                 CouncilArea2019Code == 'S12000036' & 
                                 HealthBoardArea2019Code != 'S08000024' ~ 22,
                                 CouncilArea2019Code == 'S12000038' & 
                                 HealthBoardArea2019Code != 'S08000031' ~ 23,
                                 CouncilArea2019Code == 'S12000039' & 
                                 HealthBoardArea2019Code != 'S08000031' ~ 24,
                                 CouncilArea2019Code == 'S12000040' & 
                                 HealthBoardArea2019Code != 'S08000024' ~ 25,
                                 CouncilArea2019Code == 'S12000041' & 
                                 HealthBoardArea2019Code != 'S08000030' ~ 26,
                                 CouncilArea2019Code == 'S12000042' & 
                                 HealthBoardArea2019Code != 'S08000030' ~ 27,
                                 CouncilArea2019Code == 'S12000045' & 
                                 HealthBoardArea2019Code != 'S08000031' ~ 28,
                                 CouncilArea2019Code == 'S12000047' & 
                                 HealthBoardArea2019Code != 'S08000029' ~ 29,
                                 CouncilArea2019Code == 'S12000048' & 
                                 HealthBoardArea2019Code != 'S08000030' ~ 30, 
                                 CouncilArea2019Code == 'S12000049' & 
                                 HealthBoardArea2019Code != 'S08000031' ~ 31,
                                 CouncilArea2019Code == 'S12000050' & 
                                 HealthBoardArea2019Code != 'S08000032' ~ 32)) %>% 
  count(CA_HB_issue)

# Check that HSCP and Council Area are correctly aligned
# Output should be tibble with all values as NA

SPD_data %>%
  mutate(CA_HSCP_issue = case_when(IntegrationAuthority2019Code == "S37000001" & 
                                   CouncilArea2019Code != 'S12000033' ~ 1,
                                   IntegrationAuthority2019Code == "S37000002" & 
                                   CouncilArea2019Code != 'S12000034' ~ 2,
                                   IntegrationAuthority2019Code == "S37000003" & 
                                   CouncilArea2019Code != 'S12000041' ~ 3,
                                   IntegrationAuthority2019Code == "S37000004" & 
                                   CouncilArea2019Code != 'S12000035' ~ 4,
                                   IntegrationAuthority2019Code == "S37000005" & 
                                   CouncilArea2019Code != 'S12000005' & 
                                   CouncilArea2019Code != 'S12000030' ~ 5,
                                   IntegrationAuthority2019Code == "S37000006" & 
                                   CouncilArea2019Code != 'S12000006' ~ 6,
                                   IntegrationAuthority2019Code == "S37000007" & 
                                   CouncilArea2019Code != 'S12000042' ~ 7,
                                   IntegrationAuthority2019Code == "S37000008" & 
                                   CouncilArea2019Code != 'S12000008' ~ 8,
                                   IntegrationAuthority2019Code == "S37000009" & 
                                   CouncilArea2019Code != 'S12000045' ~ 9,
                                   IntegrationAuthority2019Code == "S37000010" & 
                                   CouncilArea2019Code != 'S12000010' ~ 10,
                                   IntegrationAuthority2019Code == "S37000011" & 
                                   CouncilArea2019Code != 'S12000011' ~ 11,
                                   IntegrationAuthority2019Code == "S37000012" & 
                                   CouncilArea2019Code != 'S12000036' ~ 12,
                                   IntegrationAuthority2019Code == "S37000013" & 
                                   CouncilArea2019Code != 'S12000014' ~ 13,
                                   IntegrationAuthority2019Code == "S37000016" & 
                                   CouncilArea2019Code != 'S12000017' ~ 14,
                                   IntegrationAuthority2019Code == "S37000017" & 
                                   CouncilArea2019Code != 'S12000018' ~ 15,
                                   IntegrationAuthority2019Code == "S37000018" & 
                                   CouncilArea2019Code != 'S12000019' ~ 16,
                                   IntegrationAuthority2019Code == "S37000019" & 
                                   CouncilArea2019Code != 'S12000020' ~ 17,
                                   IntegrationAuthority2019Code == "S37000020" &
                                   CouncilArea2019Code != 'S12000021' ~ 18,
                                   IntegrationAuthority2019Code == "S37000022" & 
                                   CouncilArea2019Code != 'S12000023' ~ 19,
                                   IntegrationAuthority2019Code == "S37000024" & 
                                   CouncilArea2019Code != 'S12000038' ~ 20,
                                   IntegrationAuthority2019Code == "S37000025" & 
                                   CouncilArea2019Code != 'S12000026' ~ 21,
                                   IntegrationAuthority2019Code == "S37000026" & 
                                   CouncilArea2019Code != 'S12000027' ~ 22,
                                   IntegrationAuthority2019Code == "S37000027" & 
                                   CouncilArea2019Code != 'S12000028' ~ 23,
                                   IntegrationAuthority2019Code == "S37000028" & 
                                   CouncilArea2019Code != 'S12000029' ~ 24,
                                   IntegrationAuthority2019Code == "S37000029" & 
                                   CouncilArea2019Code != 'S12000039' ~ 25,
                                   IntegrationAuthority2019Code == "S37000030" & 
                                   CouncilArea2019Code != 'S12000040' ~ 26,
                                   IntegrationAuthority2019Code == "S37000031" & 
                                   CouncilArea2019Code != 'S12000013' ~ 27,
                                   IntegrationAuthority2019Code == "S37000032" & 
                                   CouncilArea2019Code != 'S12000047' ~ 28,
                                   IntegrationAuthority2019Code == "S37000033" &
                                   CouncilArea2019Code != 'S12000048' ~ 29, 
                                   IntegrationAuthority2019Code == "S37000034" & 
                                   CouncilArea2019Code != 'S12000049' ~ 30,
                                   IntegrationAuthority2019Code == "S37000035" & 
                                   CouncilArea2019Code != 'S12000050' ~ 31)) %>% 
  count(CA_HSCP_issue)


# Check that urban rural variables are aligned correctly
# Output should be tibble with all values as NA

SPD_data %>% 
  mutate(UR_check_16 = case_when(UrbanRural8Fold2016Code  == 1 & 
                                 UrbanRural6Fold2016Code != 1 ~ 1,
                                 UrbanRural8Fold2016Code  == 2 & 
                                 UrbanRural6Fold2016Code != 2 ~ 2, 
                                 UrbanRural8Fold2016Code  == 3 & 
                                 UrbanRural6Fold2016Code != 3 ~ 3, 
                                 UrbanRural8Fold2016Code  == 4 & 
                                 UrbanRural6Fold2016Code != 4 ~ 4, 
                                 UrbanRural8Fold2016Code  == 5 & 
                                 UrbanRural6Fold2016Code != 4 ~ 5, 
                                 UrbanRural8Fold2016Code  == 6 & 
                                 UrbanRural6Fold2016Code != 5 ~ 6, 
                                 UrbanRural8Fold2016Code  == 7 & 
                                 UrbanRural6Fold2016Code != 6 ~ 7, 
                                 UrbanRural8Fold2016Code  == 8 & 
                                 UrbanRural6Fold2016Code != 6 ~ 8)) %>% 
  count(UR_check_16)



### 4 - Check that there are no incorrectly blank cells ----

checks <- function(variable1, variable2){
  
  SPD_data %>% 
    mutate(check = case_when(variable1 == "" & variable2 != "" ~ 1, 
                             variable1 != "" & variable2 == "" ~ 2)) %>% 
    count(check) %>% 
    print()
  
}



### 4.1 - Check Output Areas ----

# Check that if Output Area 2001 is blank, Data Zone 2001 is also blank
# Check that if Output Area 2001 is populated, Data Zone 2001 is also populated

OA2001_DZ2001 <- checks("OutputArea2001Code", "DataZone2001Code")

# Check that if Output Area 2011 is blank, Data Zone 2011 is also blank
# Check that if Output Area 2011 is populated, Data Zone 2011 is also populated

OA2011_DZ2011 <- checks("OutputArea2011Code", "DataZone2011Code")




### 4.2 - Check Data Zones and Intermediate Zones ----

# Check that if Data Zone 2001 is blank, Intermediate Zone 2001 is also blank
# Check that if Data Zone 2001 is populated, 
# Intermediate Zone 2001 is also populated

DZ2001_IZ2001 <- checks("DataZone2001Code", "IntermediateZone2001Code")

# Check that if Data Zone 2011 is blank, Intermediate Zone 2011 is also blank
# Check that if Data Zone 2011 is populated, 
# Intermediate Zone 2011 is also populated

DZ2011_IZ2011 <- checks("DataZone2011Code", "IntermediateZone2011Code")

# Check that if Data Zone 2001 is blank, Data Zone 2011 is also blank
# Check that if Data Zone 2001 is populated, Data Zone 2011 is also populated

DZ2001_DZ2011 <- checks("DataZone2001Code", "DataZone2011Code")

# Check that if Intermediate Zone 2001 is blank, 
# Intermediate Zone 2011 is also blank
# Check that if Intermediate Zone 2001 is populated, 
# Intermediate Zone 2011 is also populated

IZ2001_IZ2011 <- checks("IntermediateZone2001Code", "IntermediateZone2011Code")



### 4.3 - Check for blank fields ----

# Check that there are no postcodes with a blank 2001 Data Zone, 2011 Data Zone, 
# 2001 Intermediate Zone or 2011 Intermediate Zone
# Things are fine if the outputs are all with value 0

SPD_data %>% summarise(missing = sum(is.na(DataZone2001Code)))
SPD_data %>% summarise(missing = sum(is.na(DataZone2011Code)))
SPD_data %>% summarise(missing = sum(is.na(IntermediateZone2001Code)))
SPD_data %>% summarise(missing = sum(is.na(IntermediateZone2011Code)))


# Check Scottish Parliamentary Constituency and Scottish Parliamentary Region 
# fields have no blanks
# Things are fine if the outputs are all with value 0

SPD_data %>% 
  summarise(missing = sum(is.na(ScottishParliamentaryConstituency2014Code)))
SPD_data %>% 
  summarise(missing = sum(is.na(ScottishParliamentaryRegion2014Code)))



### 5 - Check Postcode Types ----

# Check that the 17 English Voting Code (TD15 9xx postcodes) are not included
# If so delete them and raise with NRS

eng_voting_codes <- c("TD15 9SA", "TD15 9SB", "TD15 9SD", "TD15 9SE", 
                      "TD15 9SF", "TD15 9SG", "TD15 9SH", "TD15 9SJ", 
                      "TD15 9SL", "TD15 9SN", "TD15 9SP", "TD15 9SQ", 
                      "TD15 9SR", "TD15 9SS", "TD15 9ST", "TD15 9SU", 
                      "TD15 9SW") 

eng_voting_codes %in% SPD_data$Postcode


# Check split_indicator against postcode type.
# Should be more small user postcodes than large user postcodes.
# Should be more non-split postcodes than split postcodes.

SPD_data %>% 
  group_by(SplitIndicator, PostcodeType) %>% 
  count()


# Ensure all small user split postcodes with a split indicator have 
# split character A

SPD_data %>% 
  filter(PostcodeType == "S") %>% 
  group_by(SplitChar, SplitIndicator) %>% 
  count()

# Ensure all large user postcodes have a "Y" in the imputed field

SPD_data %>% 
  filter(PostcodeType == "L") %>% 
  group_by(Imputed) %>% 
  count()

# Ensure that most small user postcodes are not imputed
# Small user total can be seen in Global Environment tab

SPD_data %>% 
  filter(PostcodeType == "S") %>% 
  group_by(Imputed) %>% 
  count()

# On SPD 2016_2, LinkedSmallUserPostcodeSplitChar are blank and included in 
# field LinkedSmallUserPostcode
# Issue fixed on source data and raised with NRS 26/10/2016.

SPD_data %>% 
  group_by(LinkedSmallUserPostcodeSplitChar) %>% 
  count()

# Output should say <0 rows> (or 0-length row.names)

SPD_data %>% filter(substring(LinkedSmallUserPostcode, 9, 1) != "" | 
                   (substring(LinkedSmallUserPostcode, 8, 1) != "" & 
                    substring(LinkedSmallUserPostcode, 4, 1) != "") | 
                   (substring(LinkedSmallUserPostcode, 7, 1) != "" & 
                    substring(LinkedSmallUserPostcode, 3, 1) != "" & 
                    substring(LinkedSmallUserPostcode, 1, 2) != "NO"))


end <- Sys.time()

end - start