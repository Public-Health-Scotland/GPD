### 1 - Information ----

# Codename - Create HSCP Population Estimates Files_all years
# Data release - Mid-year HSCP Population Estimates
# Original Author - Tina Fu
# Original Date - 05/03/2018
# Type - Preparation
# Written/run on - R Studio Desktop 
# Version - 3.3.2
#
# install.packages("readxl")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readr)
#
# Description - This document is based on the SPSS syntax for HSCP Population Estimates found within
#               GPD folders. It is designed to allow for the same data to be inputted and to provide the 
#               same output files. Each section of the SPSS syntax is contained in this file within different
#               subsections.
#
# Approximate run time - 14 seconds

# Read in packages from library
library(readxl)
library(tidyr)
library(dplyr)
library(readr)

# Set working directory to R Code folder
setwd("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Population Estimates/Lookup Files/R Files/")


### 2 - Read in CA2011 lookup file
CA2019_pop_est_1981_2018 <- readRDS("CA2019_pop_est_1981_2018.rds")

# Select the CA2018, CA2011, HSCP2018 and HSCP2016 columns and only keep unique rows
# This gives you all 32 Council Areas matched to HSCP


# Create an HSCP2018Name column
# Read in HSCP Locality lookup and take unique rows to get one row for each HSCP
HSCP_Locality <- read_csv("//Isdsf00d03/cl-out/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20180903.csv") %>%
  select(HSCP2018Name, CA2018, CA2011, HSCP2018, HSCP2016) %>%
  distinct() %>%
  arrange(CA2018)


### 3 - Create the HSCP2018 population estimate file ----
# Match the HSCP columns onto the CA2018 lookup
# Create column for SexName
# Select the relevant columns
# As there are 31 HSCPs and 32 CAs (Stirling and Clackmannanshire join for HSCP) this will produce population
# separately for both Stirling and Clackmannanshire even though they have the same HSCP code
# Need to recalculate the sums, so group by all variables bar Pop and calculate new Pop totals
# Arrange by Year, HSCP2018, Age and Sex to get the required format
HSCP2019_pop_est_1981_2018 <- CA2019_pop_est_1981_2018 %>%
  full_join(HSCP_Locality) %>%
  mutate(HSCP2019 = recode(HSCP2018, 'S37000015' = 'S37000034', 'S37000021' = 'S37000035'),
         SexName = recode(Sex, '1' = 'M', '2' = 'F')) %>%
  rename(HSCP2019Name = HSCP2018Name) %>% 
  select(Year, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, Age, Sex, SexName, Pop) %>%
  group_by(Year, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, Age, Sex, SexName) %>%
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  arrange(Year, HSCP2019, Age, Sex)

# Save file as .RDS
saveRDS(HSCP2019_pop_est_1981_2018, paste0("HSCP2019_pop_est_1981_2018.rds"))



### 4 - Create the HSCP2018 5 year age group population estimate file ----

# Create a file for 5 year age group and sex
CA2019_pop_est_5year_agegroups_1981_2018 <- readRDS("CA2019_pop_est_5year_agegroups_1981_2018.rds")

# Match the HSCP columns onto the CA2018 lookup
# Create column for SexName
# Select the relevant columns
# As there are 31 HSCPs and 32 CAs (Stirling and Clackmannanshire join for HSCP) this will produce population
# separately for both Stirling and Clackmannanshire even though they have the same HSCP code
# Need to recalculate the sums, so group by all variables bar Pop and calculate new Pop totals
# Arrange by Year, HSCP2018, Age and Sex to get the required format
HSCP2019_pop_est_5year_agegroups_1981_2018 <- CA2019_pop_est_5year_agegroups_1981_2018 %>%
  full_join(HSCP_Locality) %>%
  mutate(HSCP2019 = recode(HSCP2018, 'S37000015' = 'S37000034', 'S37000021' = 'S37000035'),
         SexName = recode(Sex, '1' = 'M', '2' = 'F')) %>%
  rename(HSCP2019Name = HSCP2018Name) %>% 
  select(Year, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, AgeGroup, AgeGroupName, Sex, SexName, Pop) %>%
  group_by(Year, HSCP2019, HSCP2019Name, HSCP2018,  HSCP2016, AgeGroup, AgeGroupName, Sex, SexName) %>%
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  arrange(Year, HSCP2019, AgeGroup, Sex)

# Save file as .RDS
saveRDS(HSCP2019_pop_est_5year_agegroups_1981_2018, paste0("HSCP2019_pop_est_5year_agegroups_1981_2018.rds"))



### 5 - Check HSCP2018 Files ----

### 5.1 - Check single age file ----

# Check that all years of the population estimates are there (last update 1981_2017)
# Check there are no missing values
# Check all years have the same % of records
as.data.frame(table(HSCP2019_pop_est_1981_2018$Year))

# Check that all 31 HSCPs are there
# Check there are no missing values
# Check all HSCps have the same % of records
as.data.frame(table(HSCP2019_pop_est_1981_2018$HSCP2019))
as.data.frame(table(HSCP2019_pop_est_1981_2018$HSCP2018))
as.data.frame(table(HSCP2019_pop_est_1981_2018$HSCP2016))

# Check that all 91 ages 0 to 90+ are there
# Check there are no missing values
# Check all ages have the same % of records
as.data.frame(table(HSCP2019_pop_est_1981_2018$Age))

# Check that both males and females are there
# Check there are no missing values
# Check both sexes have the same % of records (50/50)
as.data.frame(table(HSCP2019_pop_est_1981_2018$Sex))

# Check that population values are as expected
# i.e. no negative values or extremely high values etc
pop_test <- as.data.frame(table(HSCP2019_pop_est_1981_2018$Pop))
View(pop_test)


### 5.2 - Check 5 year age group file ----
# Check that all years of the population estimates are there (last update 1981_2017)
# Check there are no missing values
# Check all years have the same % of records
as.data.frame(table(HSCP2019_pop_est_5year_agegroups_1981_2018$Year))

# Check that all 31 HSCPs are there
# Check there are no missing values
# Check all HSCps have the same % of records
as.data.frame(table(HSCP2019_pop_est_5year_agegroups_1981_2018$HSCP2019))
as.data.frame(table(HSCP2019_pop_est_5year_agegroups_1981_2018$HSCP2018))
as.data.frame(table(HSCP2019_pop_est_5year_agegroups_1981_2018$HSCP2016))

# Check that all 20 age groups are there
# Check there are no missing values
# Check all ages have the same % of records
as.data.frame(table(HSCP2019_pop_est_5year_agegroups_1981_2018$AgeGroup))

# Check that both males and females are there
# Check there are no missing values
# Check both sexes have the same % of records (50/50)
as.data.frame(table(HSCP2019_pop_est_5year_agegroups_1981_2018$Sex))

# Check that population values are as expected
# i.e. no negative values or extremely high values etc
pop_test <- as.data.frame(table(HSCP2019_pop_est_5year_agegroups_1981_2018$Pop))
View(pop_test)
