### 1 - Information ----

# Codename - Create Urban Rural Classification Lookup Files
# Original Author - Calum Purdie
# Original Date - 28/03/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
#
# Description - This document is based on the SPSS syntax for creating urban rural classification files. 
#               It is designed to allow for the same data to be inputted and to provide the same output files.
# Approximate run time - 22 seconds

setwd("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/1_Geography/Urban Rural Classification/")

# Read in packages from library
library(tidyr)
library(dplyr)
library(readxl)
library(readr)

### 2 - Create Lookup for Urban Rural Columns ----
UR_Lookup <- read_csv("Source Data/2016/DZ2011_SGUR2016_Lookup.csv") %>% 
  select(-DZ_CODE) %>% 
  distinct() %>% 
  rename(UR8_2016 = UR8FOLD, 
         UR6_2016 = UR6FOLD, 
         UR3_2016 = UR3FOLD, 
         UR2_2016 = UR2FOLD) %>% 
  mutate(UR8_2016Name = case_when(UR8_2016 == 1 ~ "Large Urban Areas", 
                                  UR8_2016 == 2 ~ "Other Urban Areas", 
                                  UR8_2016 == 3 ~ "Accessible Small Towns", 
                                  UR8_2016 == 4 ~ "Remote Small Towns", 
                                  UR8_2016 == 5 ~ "Very Remote Small Towns", 
                                  UR8_2016 == 6 ~ "Accessible Rural", 
                                  UR8_2016 == 7 ~ "Remote Rural", 
                                  UR8_2016 == 8 ~ "Very Remote Rural"), 
         UR6_2016Name = case_when(UR6_2016 == 1 ~ "Large Urban Areas", 
                                  UR6_2016 == 2 ~ "Other Urban Areas", 
                                  UR6_2016 == 3 ~ "Accessible Small Towns", 
                                  UR6_2016 == 4 ~ "Remote Small Towns", 
                                  UR6_2016 == 5 ~ "Accessible Rural", 
                                  UR6_2016 == 6 ~ "Remote Rural"), 
         UR3_2016Name = case_when(UR3_2016 == 1 ~ "Rest of Scotland", 
                                  UR3_2016 == 2 ~ "Accessible Rural", 
                                  UR3_2016 == 3 ~ "Remote Rural"), 
         UR2_2016Name = case_when(UR2_2016 == 1 ~ "Urban Areas", 
                                  UR2_2016 == 2 ~ "Remote Areas")) %>% 
  select(UR2_2016, UR2_2016Name, UR3_2016, UR3_2016Name, UR6_2016, UR6_2016Name, UR8_2016, UR8_2016Name)

### 3 - Create DataZone2011 to Urban Rural Lookup ----

# Get DZ2011_SGUR2016_Lookup file
# Rename variables and sort by DataZone2011
DZ2011_SGUR2016_Lookup <- read_csv("Source Data/2016/DZ2011_SGUR2016_Lookup.csv") %>% 
  rename(DataZone2011 = DZ_CODE, 
         UR8_2016 = UR8FOLD, 
         UR6_2016 = UR6FOLD, 
         UR3_2016 = UR3FOLD, 
         UR2_2016 = UR2FOLD) %>% 
  arrange(DataZone2011) %>% 
  left_join(UR_Lookup) %>% 
  select(DataZone2011, UR2_2016, UR2_2016Name, UR3_2016, UR3_2016Name, UR6_2016, UR6_2016Name, UR8_2016, UR8_2016Name)

saveRDS(DZ2011_SGUR2016_Lookup, "Lookup Files/R Files/DataZone2011_urban_rural_2016.rds")

### 4 - Create OutputArea2011 to Urban Rural Lookup ----

# Get OA2011_SGUR2016_Lookup file
# Rename variables and sort by OA2011
OA2011_SGUR2016_Lookup <- read_csv("Source Data/2016/OA2011_SGUR2016_Lookup.csv") %>% 
  rename(OA2011 = OUTPUTAREA, 
         UR8_2016 = UR8FOLD, 
         UR6_2016 = UR6FOLD, 
         UR3_2016 = UR3FOLD, 
         UR2_2016 = UR2FOLD) %>% 
  arrange(OA2011) %>% 
  left_join(UR_Lookup) %>% 
  select(OA2011, UR2_2016, UR2_2016Name, UR3_2016, UR3_2016Name, UR6_2016, UR6_2016Name, UR8_2016, UR8_2016Name)


saveRDS(OA2011_SGUR2016_Lookup, "Lookup Files/R Files/OA2011_urban_rural_2016.rds")

### 5 - Create Settlement to Urban Rural Lookup ----

# Get SETT2016_SGUR2016_Lookup
# Rename variables and sort by Settlement
SETT2016_SGUR2016_Lookup <- read_csv("Source Data/2016/SETT2016_SGUR2016_Lookup.csv") %>% 
  rename(Settlement = SETT_CODE, 
         Settlement_Name = SETT_NAME, 
         Population = POPEST2016,
         SettPopClass = POP_CLASS, 
         UR8_2016 = UR8FOLD, 
         UR6_2016 = UR6FOLD, 
         UR3_2016 = UR3FOLD, 
         UR2_2016 = UR2FOLD) %>% 
  arrange(Settlement) %>% 
  left_join(UR_Lookup) %>% 
  select(Settlement, Settlement_Name, Population, SettPopClass, UR2_2016, UR2_2016Name, UR3_2016, UR3_2016Name, UR6_2016, UR6_2016Name, UR8_2016, UR8_2016Name)

saveRDS(SETT2016_SGUR2016_Lookup, "Lookup Files/R Files/Settlement_urban_rural_2016.rds")

### 6 - Create Postcode to Urban Rural Lookup ----

# Get PC2017_2_SGUR2016_Lookup
# Rename variables and sort by pc8 ascending, Date_of_Introduction descending and Date_of_Deletion ascending.
PC2017_2_SGUR2016_Lookup <- read_csv("Source Data/2016/PC2017_2_SGUR2016_Lookup.csv") %>% 
  rename(pc8 = POSTCODE, 
         Date_of_Introduction = INT_DATE, 
         Date_of_Deletion = DEL_DATE, 
         UR8_2016 = UR8FOLD, 
         UR6_2016 = UR6FOLD, 
         UR3_2016 = UR3FOLD, 
         UR2_2016 = UR2FOLD, 
         SplitChar = TYPE) %>% 
  arrange(pc8, desc(Date_of_Introduction), Date_of_Deletion) %>% 
  left_join(UR_Lookup) %>% 
  select(pc8, SplitChar, Date_of_Introduction, Date_of_Deletion, UR2_2016, UR2_2016Name, UR3_2016, UR3_2016Name, UR6_2016, UR6_2016Name, UR8_2016, UR8_2016Name)


# Check for duplicates
PC2017_2_SGUR2016_Lookup <- PC2017_2_SGUR2016_Lookup %>% 
  mutate(duplicate = ifelse(pc8 == lag(pc8), 1, 0))

table(PC2017_2_SGUR2016_Lookup$duplicate)

# Remove duplicates
PC2017_2_SGUR2016_Lookup <- PC2017_2_SGUR2016_Lookup %>% 
  filter(duplicate == 0)

# Take out "A" for split postcodes
PC2017_2_SGUR2016_Lookup <- PC2017_2_SGUR2016_Lookup %>% 
  mutate(pc8_new = ifelse(SplitChar == "A", substr(pc8, 1, nchar(pc8)-1), pc8))

# Check if all "A" have been taken out
A_check <- PC2017_2_SGUR2016_Lookup %>% 
  filter(SplitChar == "A") %>% 
  select(pc8, pc8_new)

# Set PC8 as PC8_new for SplitChar = A
PC2017_2_SGUR2016_Lookup <- PC2017_2_SGUR2016_Lookup %>% 
  mutate(pc8 = ifelse(SplitChar == "A", pc8_new, pc8))

# Create 7 character postcode variable
# Sort variables
PC2017_2_SGUR2016_Lookup <- PC2017_2_SGUR2016_Lookup %>%
  mutate(pc7 = ifelse(nchar(pc8) == 6, gsub(" ","  ",pc8),
                      ifelse(nchar(pc8) == 7, pc8,
                             ifelse(nchar(pc8) == 8, gsub(" ","",pc8),NA)))) %>%
  arrange(pc7, desc(Date_of_Introduction), Date_of_Deletion)

# Remove pc7 duplicates
PC2017_2_SGUR2016_Lookup <- PC2017_2_SGUR2016_Lookup %>% 
  mutate(dup = ifelse(pc7 == lag(pc7), 1, 0))

table(PC2017_2_SGUR2016_Lookup$dup)

# Eliminate duplicates
PC2017_2_SGUR2016_Lookup <- PC2017_2_SGUR2016_Lookup %>% 
  filter(dup == 0)

# Remove the SplitChar types other than A
# Change date format and select relevant variables
PC2017_2_SGUR2016_Lookup <- PC2017_2_SGUR2016_Lookup %>% 
  mutate(SplitChar = ifelse(SplitChar == "A", "A", " "),
         Date_of_Introduction = as.Date(Date_of_Introduction, "%d/%m/%Y"), 
         Date_of_Deletion = as.Date(Date_of_Deletion, "%d/%m/%Y")) %>% 
  select(pc7, pc8, SplitChar, Date_of_Introduction, Date_of_Deletion, UR2_2016, UR2_2016Name, UR3_2016, UR3_2016Name, UR6_2016, UR6_2016Name, UR8_2016, UR8_2016Name)

saveRDS(PC2017_2_SGUR2016_Lookup, "Lookup Files/R Files/Postcode_urban_rural_2016.rds")
