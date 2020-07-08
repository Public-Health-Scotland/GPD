##########################################################
# Updating Open Data Geography Codes and Labels files
# Calum Purdie
# Original date 03/04/2020
# Latest update author - Calum Purdie
# Latest update date - 06/04/2020
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for updating all geography codes and labels open data files
# Approximate run time - <1 second
##########################################################

### 1 - Housekeeping ----

library(magrittr)
library(dplyr)
library(readr)
library(tidylog)
library(janitor)
library(glue)
library(haven)
library(sjlabelled)
library(data.table)

# filepaths

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI")
lookup_path <- glue("{base_filepath}/Referencing & Standards/GPD/1_Geography/", 
                    "HSCPs and Localities/Lookup Files/Locality/", 
                    "Update in 2019-12/HSCP Localities_DZ11_Lookup_20191216.rds")
dz_path <- glue("{base_filepath}/Referencing & Standards/GPD/1_Geography/", 
                "Data Zone/Lookup Files")
hb_path <- glue("{base_filepath}/Referencing & Standards/GPD/1_Geography/", 
                "Lookup Files")
open_data <- glue("{base_filepath}/Publications/Open Data (Non Health Topic)/", 
                  "Data/OD1700008 - Geography Codes")

# Set standard filename

file <- "codes_and_labels"

# Set date for open data filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")



### 2 - Read in data ----

# Read in HSCP Locality lookup and select relevant columns

dz11_lookup <- readRDS(lookup_path) %>% 
  select(datazone2011, datazone2011name, ca2011, ca2018, ca2019, 
         ca2019name, hscp2016, hscp2018, hscp2019, hscp2019name, hb2014, 
         hb2018, hb2019, hb2019name) %>% 
  mutate(Country = "S92000003")

# Match on Int Zone codes and names

# Get codes from DataZone2011 lookup

iz_code <- read_csv(glue("{dz_path}/DataZone2011.csv")) %>% 
  select(DataZone2011, IntZone2011)

# Get names from IntZone2011 lookup

iz_names <- read_csv(glue("{dz_path}/IntZone2011Names.csv"))

dz11_lookup %<>% 
  left_join(iz_code, by = c("datazone2011" = "DataZone2011")) %>% 
  left_join(iz_names)

colnames(dz11_lookup) <- c("DataZone", "DataZoneName", 
                          "CA2011", "CA2018", "CA", "CAName", "HSCP2016", 
                          "HSCP2018", "HSCP", "HSCPName", "HB2014", "HB2018", 
                          "HB", "HBName", "Country", "IntZone","IntZoneName")

rm(iz_code, iz_names)

# Read in Data Zone 2001 lookup

dz01_lookup <- read_sav(glue("{dz_path}/DataZone2001.sav")) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character) %>% 
  select(DataZone2001, IntZone2001, HB1995, HB2006, HB2014, CA2011, HSCP2016)

# Get DataZone2001 and IntZone2001 names

dz01_names <- readRDS(glue("{dz_path}/DataZone2001_names_lookup.rds"))
iz01_names <- readRDS(glue("{dz_path}/IntZone2001_names_lookup.rds"))

# Get HB1995 and HB2006 names

hb95_names <- read_csv(glue("{hb_path}/hb1995_lookup.csv"))
hb06_names <- read_csv(glue("{hb_path}/hb2006_lookup.csv"))

dz01_lookup %<>% 
  left_join(dz01_names, by = c("DataZone2001" = "DZ2001")) %>% 
  left_join(iz01_names, by = c("IntZone2001" = "IZ2001")) %>% 
  left_join(hb95_names) %>% 
  left_join(hb06_names)

colnames(dz01_lookup) <- c("DataZone", "IntZone", "HB1995", "HB2006", "HB2014", 
                           "CA2011", "HSCP2016", "DataZoneName", "IntZoneName", 
                           "HB1995Name", "HB2006Name")

# Add current geography names

dz01_lookup %<>% 
  left_join(select(dz11_lookup, CA2011, CA, CAName, HB2014, HB, HBName, 
                   HSCP2016, HSCP, HSCPName, Country)) %>% 
  distinct()

rm(dz01_names, iz01_names, hb06_names, hb95_names)



### 3 - 2011 level lookups ----

# Data Zone 2011

dz2011 <- dz11_lookup %>% 
  select(DataZone, DataZoneName, IntZone, IntZoneName, CA, CAName, HSCP, 
         HSCPName, HB, HBName, Country)

fwrite(dz2011, glue("{open_data}/dz2011_{file}_{date}.csv"))

rm(dz2011)


# Int Zone 2011

iz2011 <- dz11_lookup %>% 
  select(IntZone, IntZoneName, CA, CAName, HSCP, 
         HSCPName, HB, HBName, Country) %>% 
  distinct()

fwrite(iz2011, glue("{open_data}/iz2011_{file}_{date}.csv"))

rm(iz2011)


# Council Area 2011

ca2011 <- dz11_lookup %>% 
  select(CA2011, CAName, HSCP2016, HSCPName, HB2014, HBName, Country) %>% 
  distinct() %>% 
  rename(CA = CA2011, HSCP = HSCP2016, HB = HB2014)

fwrite(ca2011, glue("{open_data}/ca2011_{file}_{date}.csv"))

rm(ca2011)


# Council Area 2018

ca2018 <- dz11_lookup %>% 
  select(CA2018, CAName, HSCP2018, HSCPName, HB2018, HBName, Country) %>% 
  distinct() %>% 
  rename(CA = CA2018, HSCP = HSCP2018, HB = HB2018)

fwrite(ca2018, glue("{open_data}/ca2018_{file}_{date}.csv"))

rm(ca2018)


# Council Area 2019

ca2019 <- dz11_lookup %>% 
  select(CA, CAName, HSCP, HSCPName, HB, HBName, Country) %>% 
  distinct()

fwrite(ca2019, glue("{open_data}/ca2019_{file}_{date}.csv"))

rm(ca2019)


# HSCP 2016

hscp2016 <- dz11_lookup %>% 
  select(HSCP2016, HSCPName, HB2014, HBName, Country) %>% 
  distinct() %>% 
  rename(HSCP = HSCP2016, HB = HB2014)

fwrite(hscp2016, glue("{open_data}/hscp2016_{file}_{date}.csv"))

rm(hscp2016)


# HSCP 2018

hscp2018 <- dz11_lookup %>% 
  select(HSCP2018, HSCPName, HB2018, HBName, Country) %>% 
  distinct() %>% 
  rename(HSCP = HSCP2018, HB = HB2018)

fwrite(hscp2018, glue("{open_data}/hscp2018_{file}_{date}.csv"))

rm(hscp2018)


# HSCP 2019

hscp2019 <- dz11_lookup %>% 
  select(HSCP, HSCPName, HB, HBName, Country) %>% 
  distinct()

fwrite(hscp2019, glue("{open_data}/hscp2019_{file}_{date}.csv"))

rm(hscp2019)


# Health Board 2014

hb2014 <- dz11_lookup %>% 
  select(HB2014, HBName, Country) %>% 
  distinct() %>% 
  rename(HB = HB2014)

fwrite(hb2014, glue("{open_data}/hb2014_{file}_{date}.csv"))

rm(hb2014)


# Health Board 2018

hb2018 <- dz11_lookup %>% 
  select(HB2018, HBName, Country) %>% 
  distinct() %>% 
  rename(HB = HB2018)

fwrite(hb2018, glue("{open_data}/hb2018_{file}_{date}.csv"))

rm(hb2018)


# Health Board 2019

hb2019 <- dz11_lookup %>% 
  select(HB, HBName, Country) %>% 
  distinct()

fwrite(hb2019, glue("{open_data}/hb2019_{file}_{date}.csv"))

rm(hb2019)



### 4 - 2001 level lookups ----

# Data Zone 2011

dz2001 <- dz01_lookup %>% 
  select(DataZone, DataZoneName, IntZone, IntZoneName, CA, CAName, HSCP, 
         HSCPName, HB, HBName, Country)

fwrite(dz2001, glue("{open_data}/dz2001_{file}_{date}.csv"))

rm(dz2001)


# Int Zone 2011

iz2001 <- dz01_lookup %>% 
  select(IntZone, IntZoneName, CA, CAName, HSCP, 
         HSCPName, HB, HBName, Country) %>% 
  distinct()

fwrite(iz2001, glue("{open_data}/iz2001_{file}_{date}.csv"))

rm(iz2001)


# Health Board 1995

hb1995 <- dz01_lookup %>% 
  select(HB1995, HB1995Name, Country) %>% 
  distinct() %>% 
  rename(HB = HB1995, HBName = HB1995Name) %>% 
  arrange(HB)

fwrite(hb1995, glue("{open_data}/hb1995_{file}_{date}.csv"))

rm(hb1995)


# Health Board 2006

hb2006 <- dz01_lookup %>% 
  select(HB2006, HB2006Name, Country) %>% 
  distinct() %>% 
  rename(HB = HB2006, HBName = HB2006Name) %>% 
  arrange(HB)

fwrite(hb2006, glue("{open_data}/hb2006_{file}_{date}.csv"))

rm(hb2006)
