### 1 - Information ----

# Codename - Create SIMD 2016 Population-Weighted Files
# Original Author - Calum Purdie
# Original Date - 02/10/2018
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
#
# Description - This document is based on the SPSS syntax for creating SIMD 2016 population weighted files. 
#               It is designed to allow for the same data to be inputted and to provide the same output files.
# Approximate run time - 51 seconds

# Read in packages from library
library(tidyr)
library(dplyr)

### 2 - Get Populations Data Ready for Matching ----

# Read in uncorrected populations
DZ2011_pop_est_2011_2014 <- read.csv("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Small Area Population estimates/Lookup Files/DataZone2011_pop_est_2011_2014.csv", fileEncoding = "UTF-8-BOM")

# Select only 2014 population estimates as these are the populations SIMD 2016 is based on
DZ2011_pop_est_2011_2014 <- subset(DZ2011_pop_est_2011_2014, Year == 2014)

# Read in DataZone2011 file
DZ2011 <- read.csv("//Isdsf00d03/cl-out/lookups/Unicode/Geography/DataZone2011/DataZone2011.csv", fileEncoding = "UTF-8-BOM")

# Match on higher geographies
# Sort by DataZone2011 for matching
DZ2011_pop_est_2011_2014 <- DZ2011_pop_est_2011_2014 %>%
  arrange(DataZone2011) %>%
  left_join(DZ2011, by="DataZone2011")

# Create a file with the Data Zone population
SAPE_2014 <- DZ2011_pop_est_2011_2014 %>%
  group_by(DataZone2011) %>%
  summarise(dz_pop = sum(total_pop)) %>%
  ungroup()

# Create a Scotland flag
DZ2011_pop_est_2011_2014 <- DZ2011_pop_est_2011_2014 %>%
  mutate(Scot = 1)

# Create a file with the Scotland population
Scot_2014_pop <- DZ2011_pop_est_2011_2014 %>%
  group_by(Scot) %>%
  summarise(scot_pop = sum(total_pop)) %>%
  ungroup

# Create a file with the Health Board populations
HB_2014_pop <- DZ2011_pop_est_2011_2014 %>%
  group_by(HB2014) %>%
  summarise(HB_pop = sum(total_pop)) %>%
  ungroup

# Create a file with the HSCP populations
HSCP_2014_pop <- DZ2011_pop_est_2011_2014 %>%
  group_by(HSCP2016) %>%
  summarise(HSCP_pop = sum(total_pop)) %>%
  ungroup

# Create a file with the Council Area populations
CA_2014_pop <- DZ2011_pop_est_2011_2014 %>%
  group_by(CA2011) %>%
  summarise(CA_pop = sum(total_pop)) %>%
  ungroup

### 3 - Get SIMD domains ready for matching ----

# Read in SIMD files
simd16_Access <- read.csv("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/3_Deprivation/simd/Source Data/simd 2016/SIMD16ACCESS.csv", fileEncoding = "UTF-8-BOM") %>% rename(DZ = dz)
simd16_Crime <- read.csv("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/3_Deprivation/SIMD/Source Data/SIMD 2016/SIMD16CRIME.csv", fileEncoding = "UTF-8-BOM")
simd16_Education <- read.csv("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/3_Deprivation/SIMD/Source Data/SIMD 2016/SIMD16EDUCATION.csv", fileEncoding = "UTF-8-BOM")
simd16_Employment <- read.csv("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/3_Deprivation/SIMD/Source Data/SIMD 2016/SIMD16EMPLOYMENT.csv", fileEncoding = "UTF-8-BOM")
simd16_Health <- read.csv("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/3_Deprivation/SIMD/Source Data/SIMD 2016/SIMD16HEALTH.csv", fileEncoding = "UTF-8-BOM")
simd16_Housing <- read.csv("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/3_Deprivation/SIMD/Source Data/SIMD 2016/SIMD16HOUSING.csv", fileEncoding = "UTF-8-BOM")
simd16_Income <- read.csv("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/3_Deprivation/SIMD/Source Data/SIMD 2016/SIMD16INCOME.csv", fileEncoding = "UTF-8-BOM")

# Match all SIMD domain files together
simd16_Domains <- simd16_Access %>%
  full_join(simd16_Crime) %>%
  full_join(simd16_Education) %>%
  full_join(simd16_Employment) %>%
  full_join(simd16_Health) %>%
  full_join(simd16_Housing) %>%
  full_join(simd16_Income) %>%
  rename(DataZone2011 = DZ)
  
### 4 - Calculate SIMD 2016 Scotland level population weighted categories ----

# Read in source data
simd2016 <- read.csv("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/3_Deprivation/SIMD/Source Data/SIMD 2016/simd2016.csv", fileEncoding = "UTF-8-BOM") %>%
  rename(DataZone2011 = DZ, simd2016rank = SIMD16_rank)

# Match on higher geographies
simd2016 <- simd2016 %>%
  arrange(DataZone2011) %>%
  left_join(DZ2011, by="DataZone2011")

# Match on the Data Zone populations
# Sort by DataZone2011 and match onto SAPE_2014
simd2016 <- simd2016 %>%
  arrange(DataZone2011) %>%
  left_join(SAPE_2014, by="DataZone2011")

# Create Scotland flag for matching
# Match on the total Scotland populations
# Sort cases in order of SIMD 2016 rank, i.e. from most to least deprived
simd2016 <- simd2016 %>%
  mutate(Scot = 1) %>%
  left_join(Scot_2014_pop, by="Scot") %>%
  arrange(simd2016rank)

# Create a variable to store cumulative population
# Calculate the cumulative population percentage
simd2016 <- simd2016 %>%
  mutate(cpop = cumsum(dz_pop)) %>%
  mutate(cpop_per = (cpop/scot_pop)*100)

# Create non population weighted datazone based vigintiles
simd2016 <- simd2016 %>%
  mutate(npw_vig = ifelse(simd2016rank>0 & simd2016rank <= 349, 1, 
                          ifelse(simd2016rank>349 & simd2016rank <= 698, 2, 
                          ifelse(simd2016rank>698 & simd2016rank <= 1046, 3,
                          ifelse(simd2016rank>1046 & simd2016rank <= 1395, 4,  
                          ifelse(simd2016rank>1395 & simd2016rank <= 1744, 5,
                          ifelse(simd2016rank>1744 & simd2016rank <= 2093, 6, 
                          ifelse(simd2016rank>2093 & simd2016rank <= 2442, 7,
                          ifelse(simd2016rank>2442 & simd2016rank <= 2790, 8, 
                          ifelse(simd2016rank>2790 & simd2016rank <= 3139, 9,
                          ifelse(simd2016rank>3139 & simd2016rank <= 3488, 10,
                          ifelse(simd2016rank>3488 & simd2016rank <= 3837, 11,
                          ifelse(simd2016rank>3837 & simd2016rank <= 4186, 12,
                          ifelse(simd2016rank>4186 & simd2016rank <= 4534, 13,
                          ifelse(simd2016rank>4534 & simd2016rank <= 4883, 14,
                          ifelse(simd2016rank>4883 & simd2016rank <= 5232, 15,
                          ifelse(simd2016rank>5232 & simd2016rank <= 5581, 16,
                          ifelse(simd2016rank>5581 & simd2016rank <= 5930, 17,
                          ifelse(simd2016rank>5930 & simd2016rank <= 6278, 18,
                          ifelse(simd2016rank>6278 & simd2016rank <= 6627, 19,
                          ifelse(simd2016rank>6627 & simd2016rank <= 6976, 20, 0
                          )))))))))))))))))))))

# Create Scotland level non population weighted deciles
simd2016 <- simd2016 %>%
  mutate(npw_dec = ifelse(npw_vig == 1 | npw_vig == 2, 1, 
                          ifelse(npw_vig == 3 | npw_vig == 4, 2,     
                          ifelse(npw_vig == 5 | npw_vig == 6, 3,
                          ifelse(npw_vig == 7 | npw_vig == 8, 4,
                          ifelse(npw_vig == 9 | npw_vig == 10, 5,
                          ifelse(npw_vig == 11 | npw_vig == 12, 6, 
                          ifelse(npw_vig == 13 | npw_vig == 14, 7,     
                          ifelse(npw_vig == 15 | npw_vig == 16, 8,
                          ifelse(npw_vig == 17 | npw_vig == 18, 9,
                          ifelse(npw_vig == 19 | npw_vig == 20, 10, 0
                          )))))))))))

# Create Scotland level non population weighted quintiles
simd2016 <- simd2016 %>%
  mutate(npw_quin = ifelse(npw_dec == 1 | npw_dec == 2, 1, 
                          ifelse(npw_dec == 3 | npw_dec == 4, 2,     
                          ifelse(npw_dec == 5 | npw_dec == 6, 3,
                          ifelse(npw_dec == 7 | npw_dec == 8, 4,
                          ifelse(npw_dec == 9 | npw_dec == 10, 5, 0
                          ))))))

# Create a variable for population weighted vigintiles
# Based on the cut off always being below the target cut off point
simd2016 <- simd2016 %>%
  mutate(vig = ifelse(cpop_per>0 & cpop_per <= 5, 1, 
                          ifelse(cpop_per>5 & cpop_per <= 10, 2, 
                          ifelse(cpop_per>10 & cpop_per <= 15, 3,
                          ifelse(cpop_per>15 & cpop_per <= 20, 4,  
                          ifelse(cpop_per>20 & cpop_per <= 25, 5,
                          ifelse(cpop_per>25 & cpop_per <= 30, 6, 
                          ifelse(cpop_per>30 & cpop_per <= 35, 7,
                          ifelse(cpop_per>35 & cpop_per <= 40, 8, 
                          ifelse(cpop_per>40 & cpop_per <= 45, 9,
                          ifelse(cpop_per>45 & cpop_per <= 50, 10,
                          ifelse(cpop_per>50 & cpop_per <= 55, 11,
                          ifelse(cpop_per>55 & cpop_per <= 60, 12,
                          ifelse(cpop_per>60 & cpop_per <= 65, 13,
                          ifelse(cpop_per>65 & cpop_per <= 70, 14,
                          ifelse(cpop_per>70 & cpop_per <= 75, 15,
                          ifelse(cpop_per>75 & cpop_per <= 80, 16,
                          ifelse(cpop_per>80 & cpop_per <= 85, 17,
                          ifelse(cpop_per>85 & cpop_per <= 90, 18,
                          ifelse(cpop_per>90 & cpop_per <= 95, 19,
                          ifelse(cpop_per>95 & cpop_per <= 100, 20, 0
                          )))))))))))))))))))))

# Create a variable to calculate the difference between the cumulative population %>% and the target cut off points
simd2016 <- simd2016 %>%
  mutate(d1 = ifelse((vig == 1 | vig == 2) & cpop_per>2.5 & cpop_per <= 7.5, 5-cpop_per, 
                     ifelse((vig == 2 | vig == 3) & cpop_per>7.5 & cpop_per <= 12.5, 10-cpop_per, 
                     ifelse((vig == 3 | vig == 4) & cpop_per>12.5 & cpop_per <= 17.5, 15-cpop_per, 
                     ifelse((vig == 4 | vig == 5) & cpop_per>17.5 & cpop_per <= 22.5, 20-cpop_per, 
                     ifelse((vig == 5 | vig == 6) & cpop_per>22.5 & cpop_per <= 27.5, 25-cpop_per, 
                     ifelse((vig == 6 | vig == 7) & cpop_per>27.5 & cpop_per <= 32.5, 30-cpop_per, 
                     ifelse((vig == 7 | vig == 8) & cpop_per>32.5 & cpop_per <= 37.5, 35-cpop_per, 
                     ifelse((vig == 8 | vig == 9) & cpop_per>37.5 & cpop_per <= 42.5, 40-cpop_per, 
                     ifelse((vig == 9 | vig == 10) & cpop_per>42.5 & cpop_per <= 47.5, 45-cpop_per, 
                     ifelse((vig == 10 | vig == 11) & cpop_per>47.5 & cpop_per <= 52.5, 50-cpop_per, 
                     ifelse((vig == 11 | vig == 12) & cpop_per>52.5 & cpop_per <= 57.5, 55-cpop_per, 
                     ifelse((vig == 12 | vig == 13) & cpop_per>57.5 & cpop_per <= 62.5, 60-cpop_per, 
                     ifelse((vig == 13 | vig == 14) & cpop_per>62.5 & cpop_per <= 67.5, 65-cpop_per, 
                     ifelse((vig == 14 | vig == 15) & cpop_per>67.5 & cpop_per <= 72.5, 70-cpop_per, 
                     ifelse((vig == 15 | vig == 16) & cpop_per>72.5 & cpop_per <= 77.5, 75-cpop_per, 
                     ifelse((vig == 16 | vig == 17) & cpop_per>77.5 & cpop_per <= 82.5, 80-cpop_per, 
                     ifelse((vig == 17 | vig == 18) & cpop_per>82.5 & cpop_per <= 87.5, 85-cpop_per, 
                     ifelse((vig == 18 | vig == 19) & cpop_per>87.5 & cpop_per <= 92.5, 90-cpop_per, 
                     ifelse((vig == 19 | vig == 20) & cpop_per>92.5 & cpop_per <= 97.5, 95-cpop_per, 0
                     ))))))))))))))))))))

# Flag the points where the vigintiles change
simd2016 <- simd2016 %>%
  mutate(flag = ifelse(Scot == lag(Scot) & vig != lag(vig), 1, 0)) 

# Set first row value for flag to 0 rather than NA
simd2016[1, 27] <- 0

# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted vigintiles
simd2016 <- simd2016 %>%
  mutate(d2 = ifelse(flag == 1 & Scot == lag(Scot), d1+lag(d1), 0))

### 5 - Population Weighted Vigintiles ----        
# Create a variable for population weighted vigintiles
simd2016 <- simd2016 %>%
  mutate(simd2016_sc_vig = ifelse(d2 <= 0, vig, 
                                  ifelse(d2>0 & Scot == lag(Scot), lag(vig), 0)))

# Create Scotland level deciles
simd2016 <- simd2016 %>%
  mutate(simd2016_sc_decile = ifelse(simd2016_sc_vig == 1 | simd2016_sc_vig == 2, 1, 
                                     ifelse(simd2016_sc_vig == 3 | simd2016_sc_vig == 4, 2,     
                                     ifelse(simd2016_sc_vig == 5 | simd2016_sc_vig == 6, 3,
                                     ifelse(simd2016_sc_vig == 7 | simd2016_sc_vig == 8, 4,
                                     ifelse(simd2016_sc_vig == 9 | simd2016_sc_vig == 10, 5,
                                     ifelse(simd2016_sc_vig == 11 | simd2016_sc_vig == 12, 6, 
                                     ifelse(simd2016_sc_vig == 13 | simd2016_sc_vig == 14, 7,     
                                     ifelse(simd2016_sc_vig == 15 | simd2016_sc_vig == 16, 8,
                                     ifelse(simd2016_sc_vig == 17 | simd2016_sc_vig == 18, 9,
                                     ifelse(simd2016_sc_vig == 19 | simd2016_sc_vig == 20, 10, 0
                                     )))))))))))

# Create Scotland level quintiles
simd2016 <- simd2016 %>%
  mutate(simd2016_sc_quintile = ifelse(simd2016_sc_decile == 1 | simd2016_sc_decile == 2, 1, 
                                       ifelse(simd2016_sc_decile == 3 | simd2016_sc_decile == 4, 2,     
                                       ifelse(simd2016_sc_decile == 5 | simd2016_sc_decile == 6, 3,
                                       ifelse(simd2016_sc_decile == 7 | simd2016_sc_decile == 8, 4,
                                       ifelse(simd2016_sc_decile == 9 | simd2016_sc_decile == 10, 5, 0
                                       ))))))

# Create a variable for 15% most deprived
simd2016 <- simd2016 %>%
  mutate(simd2016tp15 = ifelse(simd2016_sc_vig == 1 | simd2016_sc_vig == 2 | simd2016_sc_vig == 3, 1, 0))

# Create a variable for 15% least deprived
simd2016 <- simd2016 %>%
  mutate(simd2016bt15 = ifelse(simd2016_sc_vig == 18 | simd2016_sc_vig == 19 | simd2016_sc_vig == 20, 1, 0))

# Sort by DataZone2011
simd2016 <- simd2016 %>%
  arrange(DataZone2011)

### 6 - Checking ----

# Check vigintiles, deciles and quintiles align correctly
table(simd2016$simd2016_sc_quintile, simd2016$simd2016_sc_decile)
table(simd2016$simd2016_sc_decile, simd2016$simd2016_sc_vig)

# Check quintile populations look ok
simd2016 %>%
  group_by(simd2016_sc_quintile) %>%
  summarise(Sum = sum(dz_pop)) %>%
  mutate(Percentage = Sum/sum(Sum))

# Check decile populations look ok
simd2016 %>%
  group_by(simd2016_sc_decile) %>%
  summarise(Sum = sum(dz_pop)) %>%
  mutate(Percentage = Sum/sum(Sum))

# Check vigintile populations look ok
simd2016 %>%
  group_by(simd2016_sc_vig) %>%
  summarise(Sum = sum(dz_pop)) %>%
  mutate(Percentage = Sum/sum(Sum))

### 7 - Calculate SIMD 2016 Health Board level population weighted categories ----

# Match SIMD2016 file onto Health Board 2014
# Match SIMD2016 file onto HB_2014_pop
# Sort in order of Health Board and SIMD 2016 rank, i.e. from most to least deprived
simd2016_HB2014 <- simd2016 %>%
  arrange(HB2014) %>%
  left_join(HB_2014_pop, by="HB2014") %>%
  arrange(HB2014, simd2016rank)

# Calculate cumulative population within each health board
# Calculate the cumulative population percentage
simd2016_HB2014 <- simd2016_HB2014 %>%
  group_by(HB2014) %>%
  mutate(cpop = cumsum(dz_pop)) %>%
  mutate(cpop_per = (cpop/HB_pop)*100) %>%
  ungroup()

# Create a variable for non population weighted deciles
simd2016_HB2014 <- simd2016_HB2014 %>%
  mutate(dec = ifelse(cpop_per>0 & cpop_per<=10, 1, 
                          ifelse(cpop_per>10 & cpop_per<=20, 2,     
                          ifelse(cpop_per>20 & cpop_per<=30, 3,
                          ifelse(cpop_per>30 & cpop_per<=40, 4,
                          ifelse(cpop_per>40 & cpop_per<=50, 5,
                          ifelse(cpop_per>50 & cpop_per<=60, 6, 
                          ifelse(cpop_per>60 & cpop_per<=70, 7,     
                          ifelse(cpop_per>70 & cpop_per<=80, 8,
                          ifelse(cpop_per>80 & cpop_per<=90, 9,
                          ifelse(cpop_per>90 & cpop_per<=100, 10, 0
                          )))))))))))

# Create a variable to calculate the difference between the cumulative population % and the target cut off points
simd2016_HB2014 <- simd2016_HB2014 %>%
  mutate(d1 = ifelse((dec == 1 | dec == 2) & cpop_per>5 & cpop_per <= 15, 10-cpop_per, 
                     ifelse((dec == 2 | dec == 3) & cpop_per>15 & cpop_per <= 25, 20-cpop_per, 
                     ifelse((dec == 3 | dec == 4) & cpop_per>25 & cpop_per <= 35, 30-cpop_per, 
                     ifelse((dec == 4 | dec == 5) & cpop_per>35 & cpop_per <= 45, 40-cpop_per, 
                     ifelse((dec == 5 | dec == 6) & cpop_per>45 & cpop_per <= 55, 50-cpop_per, 
                     ifelse((dec == 6 | dec == 7) & cpop_per>55 & cpop_per <= 65, 60-cpop_per, 
                     ifelse((dec == 7 | dec == 8) & cpop_per>65 & cpop_per <= 75, 70-cpop_per, 
                     ifelse((dec == 8 | dec == 9) & cpop_per>75 & cpop_per <= 85, 80-cpop_per, 
                     ifelse((dec == 9 | dec == 10) & cpop_per>85 & cpop_per <= 95, 90-cpop_per, 0
                     ))))))))))

# Flag the points where the non populated deciles change
simd2016_HB2014 <- simd2016_HB2014 %>%
  mutate(flag = ifelse(HB2014 == lag(HB2014) & dec != lag(dec), 1, 0)) 

# Set first row value for flag to 0 rather than NA
simd2016_HB2014[1, 27] <- 0

# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted deciles
simd2016_HB2014 <- simd2016_HB2014 %>%
  mutate(d2 = ifelse(flag == 1 & HB2014 == lag(HB2014), d1+lag(d1), 0))

# Create a variable for population weighted decile
# If the difference is greater than zero, we change the cut off point for the population weighted vigintiles
# This ensures we have as close to the target cut off point as possible
simd2016_HB2014 <- simd2016_HB2014 %>%
  mutate(simd2016_HB2014_decile = ifelse(d2 <= 0, dec, 
                                         ifelse(d2>0 & HB2014 == lag(HB2014), lag(dec), 0)))

# Create Health Board level quintiles
simd2016_HB2014 <- simd2016_HB2014 %>%
  mutate(simd2016_HB2014_quintile = ifelse(simd2016_HB2014_decile == 1 | simd2016_HB2014_decile == 2, 1, 
                                           ifelse(simd2016_HB2014_decile == 3 | simd2016_HB2014_decile == 4, 2,     
                                           ifelse(simd2016_HB2014_decile == 5 | simd2016_HB2014_decile == 6, 3,
                                           ifelse(simd2016_HB2014_decile == 7 | simd2016_HB2014_decile == 8, 4,
                                           ifelse(simd2016_HB2014_decile == 9 | simd2016_HB2014_decile == 10, 5, 0
                                           ))))))

### 8 - Checking ----

# Check deciles and quintiles align correctly
table(simd2016_HB2014$simd2016_HB2014_quintile, simd2016_HB2014$simd2016_HB2014_decile)

# Check quintile populations look ok
simd2016_HB2014_quin <- simd2016_HB2014 %>%
  group_by(HB2014, simd2016_HB2014_quintile) %>%
  summarise(Sum = sum(dz_pop)) %>%
  mutate(Percentage = Sum/sum(Sum))

# Check decile populations look ok
simd2016_HB2014_dec <- simd2016_HB2014 %>%
  group_by(HB2014, simd2016_HB2014_decile) %>%
  summarise(Sum = sum(dz_pop)) %>%
  mutate(Percentage = Sum/sum(Sum))

# Sort cases by DataZone2011
# Select the relevant variables
simd2016_HB2014 <- simd2016_HB2014 %>%
  arrange(DataZone2011) %>%
  select(DataZone2011, HB2014, simd2016_HB2014_decile, simd2016_HB2014_quintile)


### 9 - Manuel Changes for Small NHS Boards ----

# From the custom tables above the following changes need to be made to allow the population weighted deciles to be
# as close to 10%/20% as they can be
# See checking spreadsheet for details
# file://freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/3_Deprivation/SIMD/Quality Assurance Checks/SIMD 2016 - Checking quintiles and deciles.xlsx

# Shetland
simd2016_HB2014 <- simd2016_HB2014 %>%
  mutate(simd2016_HB2014_decile = ifelse(DataZone2011 == "S01012399", 5, 
                                         ifelse(DataZone2011 == "S01012402", 8, 
                                         ifelse(DataZone2011 == "S01012411", 9, simd2016_HB2014_decile)))) %>%
  mutate(simd2016_HB2014_quintile = ifelse(DataZone2011 == "S01012399", 3, 
                                           ifelse(DataZone2011 == "S01012411", 5, simd2016_HB2014_quintile)))

# Western Isles
simd2016_HB2014 <- simd2016_HB2014 %>%
  mutate(simd2016_HB2014_decile = ifelse(DataZone2011 == "S01009029", 1, 
                                         ifelse(DataZone2011 == "S01009023", 3, 
                                         ifelse(DataZone2011 == "S01009022", 5, 
                                         ifelse(DataZone2011 == "S01009015", 6, simd2016_HB2014_decile))))) %>%
  mutate(simd2016_HB2014_quintile = ifelse(DataZone2011 == "S01009015", 3, simd2016_HB2014_quintile))

# Orkney
simd2016_HB2014 <- simd2016_HB2014 %>%
  mutate(simd2016_HB2014_decile = ifelse(DataZone2011 == "S01011830", 3, 
                                         ifelse(DataZone2011 == "S01011828", 4, 
                                         ifelse(DataZone2011 == "S01011812", 5, 
                                         ifelse(DataZone2011 == "S01011811", 7, simd2016_HB2014_decile))))) %>%
  mutate(simd2016_HB2014_quintile = ifelse(DataZone2011 == "S01011828", 2, simd2016_HB2014_quintile))

# Check deciles and quintiles align correctly
table(simd2016_HB2014$simd2016_HB2014_quintile, simd2016_HB2014$simd2016_HB2014_decile)


### 10 - Calculate SIMD 2016 HSCP Level Population Weighted Categories ----

# Match SIMD2016 on the HSCP populations
# Sort by HSCP2016 before matching
# Sort cases in order of HSCP2016 and SIMD2016rank, i.e. from most to least deprived
simd2016_HSCP2016 <- simd2016 %>%
  arrange(HSCP2016) %>%
  left_join(HSCP_2014_pop, by="HSCP2016") %>%
  arrange(HSCP2016, simd2016rank)

# Calculate cumulative population within each HSCP
# Calculate the cumulative population percentage
simd2016_HSCP2016 <- simd2016_HSCP2016 %>%
  group_by(HSCP2016) %>%
  mutate(cpop = cumsum(dz_pop)) %>%
  mutate(cpop_per = (cpop/HSCP_pop)*100) %>%
  ungroup()

# Create a variable for non population weighted deciles
simd2016_HSCP2016 <- simd2016_HSCP2016 %>%
  mutate(dec = ifelse(cpop_per>0 & cpop_per<=10, 1, 
                      ifelse(cpop_per>10 & cpop_per<=20, 2,     
                      ifelse(cpop_per>20 & cpop_per<=30, 3,
                      ifelse(cpop_per>30 & cpop_per<=40, 4,
                      ifelse(cpop_per>40 & cpop_per<=50, 5,
                      ifelse(cpop_per>50 & cpop_per<=60, 6, 
                      ifelse(cpop_per>60 & cpop_per<=70, 7,     
                      ifelse(cpop_per>70 & cpop_per<=80, 8,
                      ifelse(cpop_per>80 & cpop_per<=90, 9,
                      ifelse(cpop_per>90 & cpop_per<=100, 10, 0
                      )))))))))))

# Create a variable to calculate the difference between the cumulative population % and the target cut off points
simd2016_HSCP2016 <- simd2016_HSCP2016 %>%
  mutate(d1 = ifelse((dec == 1 | dec == 2) & cpop_per>5 & cpop_per <= 15, 10-cpop_per, 
                     ifelse((dec == 2 | dec == 3) & cpop_per>15 & cpop_per <= 25, 20-cpop_per, 
                     ifelse((dec == 3 | dec == 4) & cpop_per>25 & cpop_per <= 35, 30-cpop_per, 
                     ifelse((dec == 4 | dec == 5) & cpop_per>35 & cpop_per <= 45, 40-cpop_per, 
                     ifelse((dec == 5 | dec == 6) & cpop_per>45 & cpop_per <= 55, 50-cpop_per, 
                     ifelse((dec == 6 | dec == 7) & cpop_per>55 & cpop_per <= 65, 60-cpop_per, 
                     ifelse((dec == 7 | dec == 8) & cpop_per>65 & cpop_per <= 75, 70-cpop_per, 
                     ifelse((dec == 8 | dec == 9) & cpop_per>75 & cpop_per <= 85, 80-cpop_per, 
                     ifelse((dec == 9 | dec == 10) & cpop_per>85 & cpop_per <= 95, 90-cpop_per, 0
                     ))))))))))

# Flag the points where the non populated deciles change
simd2016_HSCP2016 <- simd2016_HSCP2016 %>%
  mutate(flag = ifelse(HSCP2016 == lag(HSCP2016) & dec != lag(dec), 1, 0)) 

# Set first row value for flag to 0 rather than NA
simd2016_HSCP2016[1, 27] <- 0

# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted deciles
simd2016_HSCP2016 <- simd2016_HSCP2016 %>%
  mutate(d2 = ifelse(flag == 1 & HSCP2016 == lag(HSCP2016), d1+lag(d1), 0))

# Create a variable for population weighted decile
# If the difference is greater than zero, we change the cut off point for the population weighted vigintiles
# This ensures we have as close to the target cut off point as possible
simd2016_HSCP2016 <- simd2016_HSCP2016 %>%
  mutate(simd2016_HSCP2016_decile = ifelse(d2 <= 0, dec, 
                                         ifelse(d2>0 & HSCP2016 == lag(HSCP2016), lag(dec), 0)))

# Create HSCP level quintiles
simd2016_HSCP2016 <- simd2016_HSCP2016 %>%
  mutate(simd2016_HSCP2016_quintile = ifelse(simd2016_HSCP2016_decile == 1 | simd2016_HSCP2016_decile == 2, 1, 
                                           ifelse(simd2016_HSCP2016_decile == 3 | simd2016_HSCP2016_decile == 4, 2,     
                                           ifelse(simd2016_HSCP2016_decile == 5 | simd2016_HSCP2016_decile == 6, 3,
                                           ifelse(simd2016_HSCP2016_decile == 7 | simd2016_HSCP2016_decile == 8, 4,
                                           ifelse(simd2016_HSCP2016_decile == 9 | simd2016_HSCP2016_decile == 10, 5, 0
                                           ))))))

### 10 - Checking ----

# Check deciles and quintiles align correctly
table(simd2016_HSCP2016$simd2016_HSCP2016_quintile, simd2016_HSCP2016$simd2016_HSCP2016_decile)

# Check quintile populations look ok
simd2016_HSCP2016_quin <- simd2016_HSCP2016 %>%
  group_by(HSCP2016, simd2016_HSCP2016_quintile) %>%
  summarise(Sum = sum(dz_pop)) %>%
  mutate(Percentage = Sum/sum(Sum))

# Check decile populations look ok
simd2016_HSCP2016_dec <- simd2016_HSCP2016 %>%
  group_by(HSCP2016, simd2016_HSCP2016_decile) %>%
  summarise(Sum = sum(dz_pop)) %>%
  mutate(Percentage = Sum/sum(Sum))

# Sort cases by DataZone2011
# Select the relevant variables
simd2016_HSCP2016 <- simd2016_HSCP2016 %>%
  arrange(DataZone2011) %>%
  select(DataZone2011, HSCP2016, simd2016_HSCP2016_quintile, simd2016_HSCP2016_decile)

### 11 - Manual Changes for Small HSCPs ----

# From the custom tables above the following changes need to be made to allow the population weighted deciles to be
# as close to 10%/20% as they can be
# See checking spreadsheet for details
# file://freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/3_Deprivation/SIMD/Quality Assurance Checks/SIMD 2016 - Checking quintiles and deciles.xlsx

# Shetland
simd2016_HSCP2016 <- simd2016_HSCP2016 %>%
  mutate(simd2016_HSCP2016_decile = ifelse(DataZone2011 == "S01012399", 5, 
                                         ifelse(DataZone2011 == "S01012402", 8, 
                                         ifelse(DataZone2011 == "S01012411", 9, simd2016_HSCP2016_decile)))) %>%
  mutate(simd2016_HSCP2016_quintile = ifelse(DataZone2011 == "S01012399", 3, 
                                           ifelse(DataZone2011 == "S01012411", 5, simd2016_HSCP2016_quintile)))

# Western Isles
simd2016_HSCP2016 <- simd2016_HSCP2016 %>%
  mutate(simd2016_HSCP2016_decile = ifelse(DataZone2011 == "S01009029", 1, 
                                         ifelse(DataZone2011 == "S01009023", 3, 
                                         ifelse(DataZone2011 == "S01009022", 5, 
                                         ifelse(DataZone2011 == "S01009015", 6, simd2016_HSCP2016_decile))))) %>%
  mutate(simd2016_HSCP2016_quintile = ifelse(DataZone2011 == "S01009015", 3, simd2016_HSCP2016_quintile))

# Orkney
simd2016_HSCP2016 <- simd2016_HSCP2016 %>%
  mutate(simd2016_HSCP2016_decile = ifelse(DataZone2011 == "S01011830", 3, 
                                         ifelse(DataZone2011 == "S01011828", 4, 
                                         ifelse(DataZone2011 == "S01011812", 5, 
                                         ifelse(DataZone2011 == "S01011811", 7, simd2016_HSCP2016_decile))))) %>%
  mutate(simd2016_HSCP2016_quintile = ifelse(DataZone2011 == "S01011828", 2, simd2016_HSCP2016_quintile))

# Check deciles and quintiles align correctly
table(simd2016_HSCP2016$simd2016_HSCP2016_quintile, simd2016_HSCP2016$simd2016_HSCP2016_decile)


### 12 - Calculate SIMD 2016 Council Area Level Population Weighted Categories ----

# Match SIMD2016 on the Council Area populations
# Sort by CA2011 before matching
# Sort cases in order of CA2011 and SIMD2016rank, i.e. from most to least deprived
simd2016_CA2011 <- simd2016 %>%
  arrange(CA2011) %>%
  left_join(CA_2014_pop, by="CA2011") %>%
  arrange(CA2011, simd2016rank)

# Calculate cumulative population within each Council Area
# Calculate the cumulative population percentage
simd2016_CA2011 <- simd2016_CA2011 %>%
  group_by(CA2011) %>%
  mutate(cpop = cumsum(dz_pop)) %>%
  mutate(cpop_per = (cpop/CA_pop)*100) %>%
  ungroup()

# Create a variable for non population weighted deciles
simd2016_CA2011 <- simd2016_CA2011 %>%
  mutate(dec = ifelse(cpop_per>0 & cpop_per<=10, 1, 
                      ifelse(cpop_per>10 & cpop_per<=20, 2,     
                      ifelse(cpop_per>20 & cpop_per<=30, 3,
                      ifelse(cpop_per>30 & cpop_per<=40, 4,
                      ifelse(cpop_per>40 & cpop_per<=50, 5,
                      ifelse(cpop_per>50 & cpop_per<=60, 6, 
                      ifelse(cpop_per>60 & cpop_per<=70, 7,     
                      ifelse(cpop_per>70 & cpop_per<=80, 8,
                      ifelse(cpop_per>80 & cpop_per<=90, 9,
                      ifelse(cpop_per>90 & cpop_per<=100, 10, 0
                      )))))))))))

# Create a variable to calculate the difference between the cumulative population % and the target cut off points
simd2016_CA2011 <- simd2016_CA2011 %>%
  mutate(d1 = ifelse((dec == 1 | dec == 2) & cpop_per>5 & cpop_per <= 15, 10-cpop_per, 
                     ifelse((dec == 2 | dec == 3) & cpop_per>15 & cpop_per <= 25, 20-cpop_per, 
                     ifelse((dec == 3 | dec == 4) & cpop_per>25 & cpop_per <= 35, 30-cpop_per, 
                     ifelse((dec == 4 | dec == 5) & cpop_per>35 & cpop_per <= 45, 40-cpop_per, 
                     ifelse((dec == 5 | dec == 6) & cpop_per>45 & cpop_per <= 55, 50-cpop_per, 
                     ifelse((dec == 6 | dec == 7) & cpop_per>55 & cpop_per <= 65, 60-cpop_per, 
                     ifelse((dec == 7 | dec == 8) & cpop_per>65 & cpop_per <= 75, 70-cpop_per, 
                     ifelse((dec == 8 | dec == 9) & cpop_per>75 & cpop_per <= 85, 80-cpop_per, 
                     ifelse((dec == 9 | dec == 10) & cpop_per>85 & cpop_per <= 95, 90-cpop_per, 0
                     ))))))))))

# Flag the points where the non populated deciles change
simd2016_CA2011 <- simd2016_CA2011 %>%
  mutate(flag = ifelse(CA2011 == lag(CA2011) & dec != lag(dec), 1, 0)) 

# Set first row value for flag to 0 rather than NA
simd2016_CA2011[1, 27] <- 0

# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted deciles
simd2016_CA2011 <- simd2016_CA2011 %>%
  mutate(d2 = ifelse(flag == 1 & CA2011 == lag(CA2011), d1+lag(d1), 0))

# Create a variable for population weighted decile
# If the difference is greater than zero, we change the cut off point for the population weighted vigintiles
# This ensures we have as close to the target cut off point as possible
simd2016_CA2011 <- simd2016_CA2011 %>%
  mutate(simd2016_CA2011_decile = ifelse(d2 <= 0, dec, 
                                           ifelse(d2>0 & CA2011 == lag(CA2011), lag(dec), 0)))

# Create HSCP level quintiles
simd2016_CA2011 <- simd2016_CA2011 %>%
  mutate(simd2016_CA2011_quintile = ifelse(simd2016_CA2011_decile == 1 | simd2016_CA2011_decile == 2, 1, 
                                           ifelse(simd2016_CA2011_decile == 3 | simd2016_CA2011_decile == 4, 2,     
                                           ifelse(simd2016_CA2011_decile == 5 | simd2016_CA2011_decile == 6, 3,
                                           ifelse(simd2016_CA2011_decile == 7 | simd2016_CA2011_decile == 8, 4,
                                           ifelse(simd2016_CA2011_decile == 9 | simd2016_CA2011_decile == 10, 5, 0
                                           ))))))

### 13 - Checking ----

# Check deciles and quintiles align correctly
table(simd2016_CA2011$simd2016_CA2011_quintile, simd2016_CA2011$simd2016_CA2011_decile)

# Compare Scotland deciles and quintiles within Council Area
table(simd2016_CA2011$simd2016_sc_quintile, simd2016_CA2011$simd2016_CA2011_quintile)
table(simd2016_CA2011$simd2016_sc_decile, simd2016_CA2011$simd2016_CA2011_decile)

# Check quintile populations look ok
simd2016_CA2011_quin <- simd2016_CA2011 %>%
  group_by(CA2011, simd2016_CA2011_quintile) %>%
  summarise(Sum = sum(dz_pop)) %>%
  mutate(Percentage = Sum/sum(Sum))

# Check decile populations look ok
simd2016_CA2011_dec <- simd2016_CA2011 %>%
  group_by(CA2011, simd2016_CA2011_decile) %>%
  summarise(Sum = sum(dz_pop)) %>%
  mutate(Percentage = Sum/sum(Sum))

# Sort cases by DataZone2011
# Select the relevant variables
simd2016_CA2011 <- simd2016_CA2011 %>%
  arrange(DataZone2011) %>%
  select(DataZone2011, CA2011, simd2016_CA2011_quintile, simd2016_CA2011_decile)

### 14 - Manual Changes for Small Council Areas ---- 

# From the custom tables above the following changes need to be made to allow the population weighted deciles to be
# as close to 10%/20% as they can be
# See checking spreadsheet for details
# file://freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/3_Deprivation/SIMD/Quality Assurance Checks/SIMD 2016 - Checking quintiles and deciles.xlsx

# Shetland
simd2016_CA2011 <- simd2016_CA2011 %>%
  mutate(simd2016_CA2011_decile = ifelse(DataZone2011 == "S01012399", 5, 
                                         ifelse(DataZone2011 == "S01012402", 8, 
                                         ifelse(DataZone2011 == "S01012411", 9, simd2016_CA2011_decile)))) %>%
  mutate(simd2016_CA2011_quintile = ifelse(DataZone2011 == "S01012399", 3, 
                                           ifelse(DataZone2011 == "S01012411", 5, simd2016_CA2011_quintile)))

# Western Isles
simd2016_CA2011 <- simd2016_CA2011 %>%
  mutate(simd2016_CA2011_decile = ifelse(DataZone2011 == "S01009029", 1, 
                                         ifelse(DataZone2011 == "S01009023", 3, 
                                         ifelse(DataZone2011 == "S01009022", 5, 
                                         ifelse(DataZone2011 == "S01009015", 6, simd2016_CA2011_decile))))) %>%
  mutate(simd2016_CA2011_quintile = ifelse(DataZone2011 == "S01009015", 3, simd2016_CA2011_quintile))

# Orkney
simd2016_CA2011 <- simd2016_CA2011 %>%
  mutate(simd2016_CA2011_decile = ifelse(DataZone2011 == "S01011830", 3, 
                                         ifelse(DataZone2011 == "S01011828", 4, 
                                         ifelse(DataZone2011 == "S01011812", 5, 
                                         ifelse(DataZone2011 == "S01011811", 7, simd2016_CA2011_decile))))) %>%
  mutate(simd2016_CA2011_quintile = ifelse(DataZone2011 == "S01011828", 2, simd2016_CA2011_quintile))

# Check deciles and quintiles align correctly
table(simd2016_CA2011$simd2016_CA2011_quintile, simd2016_CA2011$simd2016_CA2011_decile)


### 15 - Put Everything Together to Create SIMD 2016 Lookup File ----

# Match HB2014, HSCP2016, CA2011 and SIMD Domains files onto SIMD2016
DZ2011_simd2016 <- simd2016 %>%
  left_join(simd2016_HB2014) %>%
  left_join(simd2016_HSCP2016) %>%
  left_join(simd2016_CA2011) %>%
  left_join(simd16_Domains)

# Rename Variables
DZ2011_simd2016 <- DZ2011_simd2016 %>%
  rename(simd2016_inc_rate = Income_rate, simd2016_inc_dep_N = Income_count, simd2016_inc_rank = incrank, 
         simd2016_emp_rate = Employment_rate, simd2016_emp_dep_N = Employment_count, simd2016_emp_rank = emprank,
         simd2016_hlth_score = healthscr, simd2016_hlth_rank = SIMD16_Health_Domain_Rank,
         simd2016_educ_score = Eduscr, simd2016_educ_rank = SIMD16_Education_domain_rank,
         simd2016_house_score = housingscr, simd2016_house_rank = housingrank, 
         simd2016_access_score = accessscr, simd2016_access_rank = accessrank, 
         simd2016_crime_rate = crimerate, simd2016_crime_rank = crimerank, 
         pop_2014 = dz_pop
         )

# Select Relevant Variables
DZ2011_simd2016 <- DZ2011_simd2016 %>%
  select(DataZone2011, IntZone2011, HB2014, HSCP2016, CA2011, simd2016rank, simd2016_sc_decile, 
         simd2016_sc_quintile, simd2016_HB2014_decile, simd2016_HB2014_quintile, simd2016_HSCP2016_decile, 
         simd2016_HSCP2016_quintile, simd2016_CA2011_decile, simd2016_CA2011_quintile, simd2016tp15, 
         simd2016bt15, simd2016_educ_rank, simd2016_educ_score, simd2016_emp_rate, simd2016_emp_dep_N, 
         simd2016_emp_rank, simd2016_hlth_score, simd2016_hlth_rank, simd2016_house_score, simd2016_house_rank, 
         simd2016_inc_rate, simd2016_inc_dep_N, simd2016_inc_rank, simd2016_access_score, simd2016_access_rank, 
         simd2016_crime_rate, simd2016_crime_rank, pop_2014
         )

# Shetland Checks
DZ2011_simd2016_check <- subset(DZ2011_simd2016, HB2014 == "S08000026")
table(DZ2011_simd2016_check$simd2016_HB2014_decile, DZ2011_simd2016_check$simd2016_CA2011_decile)
table(DZ2011_simd2016_check$simd2016_HB2014_quintile, DZ2011_simd2016_check$simd2016_CA2011_quintile)
table(DZ2011_simd2016_check$simd2016_HB2014_decile, DZ2011_simd2016_check$simd2016_HSCP2016_decile)
table(DZ2011_simd2016_check$simd2016_HB2014_quintile, DZ2011_simd2016_check$simd2016_HSCP2016_quintile)

# Orkney Checks
DZ2011_simd2016_check <- subset(DZ2011_simd2016, HB2014 == "S08000025")
table(DZ2011_simd2016_check$simd2016_HB2014_decile, DZ2011_simd2016_check$simd2016_CA2011_decile)
table(DZ2011_simd2016_check$simd2016_HB2014_quintile, DZ2011_simd2016_check$simd2016_CA2011_quintile)
table(DZ2011_simd2016_check$simd2016_HB2014_decile, DZ2011_simd2016_check$simd2016_HSCP2016_decile)
table(DZ2011_simd2016_check$simd2016_HB2014_quintile, DZ2011_simd2016_check$simd2016_HSCP2016_quintile)


# Western Isles Checks
DZ2011_simd2016_check <- subset(DZ2011_simd2016, HB2014 == "S08000028")
table(DZ2011_simd2016_check$simd2016_HB2014_decile, DZ2011_simd2016_check$simd2016_CA2011_decile)
table(DZ2011_simd2016_check$simd2016_HB2014_quintile, DZ2011_simd2016_check$simd2016_CA2011_quintile)
table(DZ2011_simd2016_check$simd2016_HB2014_decile, DZ2011_simd2016_check$simd2016_HSCP2016_decile)
table(DZ2011_simd2016_check$simd2016_HB2014_quintile, DZ2011_simd2016_check$simd2016_HSCP2016_quintile)

# Create flag for HSCP unequal to CA
DZ2011_simd2016_check <- DZ2011_simd2016 %>%
  mutate(flag = ifelse(simd2016_HSCP2016_decile != simd2016_CA2011_decile, 1, 
                       ifelse(simd2016_HSCP2016_quintile != simd2016_CA2011_quintile, 1, 0)))

# All inequalities should occur in Clackmannanshire and Stirling
DZ2011_simd2016_check <- subset(DZ2011_simd2016_check, flag == 1)


### 16 - Create Postcode-SIMD 2016 Lookup ----

# Most recent SPD at time of release of SIMD 2016 was 2016_1
# Select pc7 and DataZone2011 and sort by DataZone2011
SPD_2016_1 <- read.csv("//freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/1_Geography/Scottish Postcode Directory/Previous Versions/Archive/Scottish_Postcode_Directory_2016_1.csv", fileEncoding = "UTF-8-BOM") %>%
  select(pc7, DataZone2011) %>%
  arrange(DataZone2011)

# Match onto DZ2011_SIMD2016 
# Sort by pc7
# Drop pop_2014
Postcode_2016_1_simd2016 <- SPD_2016_1 %>%
  left_join(DZ2011_simd2016) %>%
  arrange(pc7) %>%
  select(-pop_2014)

### 17 - Add SIMD 2016 to Postcode All SIMD & Carstairs Lookup ----

# Remove IntZone2011, HB2014, HSCP2016 and CA2011
Postcode_2016_1_all_simd_Carstairs <- Postcode_2016_1_simd2016 %>%
  select(-c(IntZone2011, HB2014, HSCP2016, CA2011))

# Read in 


### 18 - Update SAPE Files for Single Year of Age ----

# Read in file
DZ2011_pop_est_2011_2015 <- read.csv("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Small Area Population estimates/Lookup Files/DataZone2011_pop_est_2011_2015.csv", fileEncoding = "UTF-8-BOM")

# Sort by DataZone2011
# Match onto DZ2011_SIMD2016
# Sort by Year, DataZone2011 and sex (descending)
# Remove pop_2014
DZ2011_pop_est_2011_2015 <- DZ2011_pop_est_2011_2015 %>%
  arrange(DataZone2011) %>%
  left_join(DZ2011_simd2016) %>%
  arrange(Year, DataZone2011, desc(sex)) %>%
  select(-pop_2014)


### 19 - Update SAPE Files for 5 Year Age Groups ----

# Read in file
DZ2011_pop_est_5year_agegroups_2011_2015 <- read.csv("//freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Small Area Population estimates/Lookup Files/DataZone2011_pop_est_5year_agegroups_2011_2015.csv", fileEncoding = "UTF-8-BOM")

# Sort by DataZone2011
# Match onto DZ2011_SIMD2016
# Sort by Year, DataZone2011 and sex (descending)
# Remove pop_2014
DZ2011_pop_est_5year_agegroups_2011_2015 <- DZ2011_pop_est_5year_agegroups_2011_2015 %>%
  arrange(DataZone2011) %>%
  left_join(DZ2011_simd2016) %>%
  arrange(Year, DataZone2011, desc(sex)) %>%
  select(-pop_2014)