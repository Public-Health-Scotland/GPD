##########################################################
# Create SIMD 2016 Population-Weighted Files
# Calum Purdie
# Original date 02/12/2019
# Latest update author - Calum Purdie
# Latest update date - 18/12/2019
# Latest update description 
# Type of script - Creatioj
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for creating population-weighted SIMD 2016 Files
# Approximate run time - 6 minutes
##########################################################

### 1 - Housekeeping ----

library(magrittr)
library(tidyr)
library(dplyr)
library(readr)
library(tidylog)
library(glue)
library(ckanr)
library(here)
library(janitor)
library(data.table)

# set filepaths

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD")

pop_filepath <- file.path(base_filepath, "2_Population", 
                          "Small Area Population estimates", "Lookup Files", 
                          "Archive", "Data Zone", "2011 Data Zones")

geo_filepath <- file.path("//Isdsf00d03", "cl-out", "lookups", "Unicode", 
                          "Geography", "HSCP Locality")

simd_filepath <- file.path(base_filepath, "3_Deprivation", "SIMD", 
                           "Source Data", "SIMD 2016")

pc_simd_filepath <- file.path(base_filepath, "3_Deprivation", 
                              "Postcode Deprivation", "Lookup Files", "Archive", 
                              "All_SIMD_Carstairs")

spd_archive <- file.path(base_filepath, "1_Geography", 
                        "Scottish Postcode Directory", "Previous Versions", 
                        "Archive")

# Set file lists
# remove simd2016.csv file

simd_source_data <- list.files(simd_filepath, pattern = ".csv") %>% 
  magrittr::extract(.!= "simd2016.csv") %>% 
  tools::file_path_sans_ext()

# Read in functions

source(here::here("Deprivation", "SIMD", "Functions for Creating SIMD Files.R"))



### 2 Get Populations Data Ready for Matching ----

### 2.1 Read in data ----

# Read in uncorrected populations
# Select only 2014 population estimates as these are the populations SIMD 2016 
# is based on

DZ2011_pop_est <- read_csv(glue("{pop_filepath}/", 
                                "DataZone2011_pop_est_2011_2014.csv")) %>% 
  filter(Year == 2014) %>% 
  clean_names()



### 2.2 Add higher geographies ----

# Add columns for higher geographies

# Use the HSCP Locality lookup for HB, HSCP and CA

geo_names <- readRDS(glue("{geo_filepath}/", 
                          "HSCP Localities_DZ11_Lookup_20191216.rds")) %>% 
  select(data_zone2011, hb2014, hb2019name, hscp2016, hscp2019name, 
         ca2011, ca2019name) %>% 
  rename(hb2014name = hb2019name, 
         hscp2016name = hscp2019name, 
         ca2011name = ca2019name)

# Use the Geography Codes and Names open data file to get the IZ names
# First need to run the httr configuration script

source(here("Geography", "Scottish Postcode Directory", 
            "Set httr configuration for API.R"))

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id <- "395476ab-0720-4740-be07-ff4467141352"

dz_iz <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  select(DZ2011, DZ2011Name, IZ2011, IZ2011Name) %>% 
  rename(data_zone2011 = DZ2011, data_zone2011name = DZ2011Name, 
         int_zone2011 = IZ2011, int_zone2011name = IZ2011Name) %>%  
  as_tibble()

# Match on higher geographies
# Sort by DataZone2011 for matching
# Create a Scotland flag

DZ2011_pop_est %<>%
  arrange(data_zone2011) %>%
  left_join(geo_names) %>%
  left_join(dz_iz) %>% 
  mutate(scot = 1)



### 2.3 Create Geography Population Files ----

# scotland

scotland_pop <- pop_function("scot", "scot_pop")

# hb2014

hb2014_pop <- pop_function("hb2014", "hb2014_pop")

# hscp2016

hscp2016_pop <- pop_function("hscp2016", "hscp2016_pop")

# ca2011

ca2011_pop <- pop_function("ca2011", "ca2011_pop")

# dz2011

dz2011_pop <- pop_function("data_zone2011", "data_zone2011_pop")



### 3 - Get SIMD domains ready for matching ----

# Read in SIMD files into a list

simd_domains <- lapply(glue("{simd_filepath}/{simd_source_data}.csv"), read_csv)

# Set names

names(simd_domains) <- simd_source_data

# Reduce into one dataset

simd_domains <- Reduce(full_join, simd_domains) %>% 
  rename(data_zone2011 = DZ)



### 4 - Calculate SIMD 2016 Scotland level population weighted categories ----

# Read in source data
# Match on higher geographies and Data Zone populations
# Create Scotland flag for matching
# Match on the total Scotland populations
# Sort cases in order of SIMD 2016 rank, i.e. from most to least deprived
# Create a variable to store cumulative population
# Calculate the cumulative population percentage

simd2016 <- read_csv(glue("{simd_filepath}/simd2016.csv")) %>%
  rename(data_zone2011 = DZ, simd2016rank = SIMD16_rank) %>%
  left_join(geo_names) %>%
  left_join(dz2011_pop) %>%
  mutate(scot = 1) %>%
  left_join(scotland_pop) %>%
  arrange(simd2016rank) %>%
  mutate(cpop = cumsum(data_zone2011_pop), 
         cpop_per = (cpop/scot_pop)*100)

# Create datazone based non population weighted vigintiles

simd2016 <- vig_function(simd2016, "npw_vig", "simd2016rank")

# Create datazone based non population weighted deciles

simd2016 <- dec_function(simd2016, "npw_dec", "npw_vig")

# Create datazone based non population weighted quintiles

simd2016 <- quin_function(simd2016, "npw_quin", "npw_dec")

# Create a variable for population weighted vigintiles based on the cut off 
# always being below the target cut off point

simd2016 <- vig_cpop(simd2016)

# Create a variable to calculate the difference between the cumulative 
# population and the target cut off points

simd2016 <- vig_cut_off(simd2016)

# Flag the points where the vigintiles change
# Set first row value for flag to 0 rather than NA
# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted vigintiles

simd2016 %<>%
  mutate(flag = if_else(scot == lag(scot) & vig != lag(vig), 1, 0), 
         flag = if_else(is.na(flag), 0, flag), 
         d2 = if_else(flag == 1 & scot == lag(scot), d1 + lag(d1), 0))



### 5 - Population Weighted Vigintiles ----        

# Create a variable for population weighted vigintiles

simd2016 %<>%
  mutate(simd2016_sc_vig = if_else(d2 <= 0, vig, 
                           if_else(d2 > 0 & scot == lag(scot), lag(vig), 0)))

# Create Scotland level deciles

simd2016 <- dec_function(simd2016, "simd2016_sc_decile", "simd2016_sc_vig")

# Create Scotland level quintiles

simd2016 <- quin_function(simd2016, "simd2016_sc_quintile", 
                          "simd2016_sc_decile")


# Create variables for 15% most and least deprived
# Sort by data_zone2011

simd2016 %<>%
  mutate(simd2016tp15 = if_else(simd2016_sc_vig == 1 | 
                                simd2016_sc_vig == 2 | 
                                simd2016_sc_vig == 3, 1, 0), 
         simd2016bt15 = if_else(simd2016_sc_vig == 18 | 
                                simd2016_sc_vig == 19 | 
                                simd2016_sc_vig == 20, 1, 0)) %>%
  arrange(data_zone2011)

# Run checks on Scotland level data

scot_checks(data = simd2016)



### 6 - Calculate SIMD 2016 Health Board level population weighted categories ----

# Match simd2016 file onto hb2014_pop
# Arrange by health board and simd rank

simd2016_hb2014 <- simd2016 %>%
  left_join(hb2014_pop) %>%
  arrange(hb2014, simd2016rank)

# Calculate cumulative population within each health board
# Calculate the cumulative population percentage

simd2016_hb2014 %<>%
  group_by(hb2014) %>%
  mutate(cpop = cumsum(data_zone2011_pop)) %>%
  mutate(cpop_per = (cpop/hb2014_pop)*100) %>%
  ungroup()

# Create a variable for non population weighted deciles

simd2016_hb2014 <- geo_dec(simd2016_hb2014)

# Create a variable for non population weighted quintiles

simd2016_hb2014 <- geo_quin(simd2016_hb2014)

# Create a variable to calculate the difference between the cumulative 
# population % and the target cut off points

simd2016_hb2014 <- geo_cut_off(simd2016_hb2014)

# Flag the points where the non population weighted deciles change
# Set first row value for flag to 0 rather than NA
# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted vigintiles

simd2016_hb2014 %<>%
  mutate(flag = if_else(hb2014 == lag(hb2014) & dec != lag(dec), 1, 0), 
         flag = if_else(is.na(flag), 0, flag), 
         d2 = if_else(flag == 1 & hb2014 == lag(hb2014), d1 + lag(d1), 0))

# Create a variable for population weighted decile
# If the difference is greater than zero, we change the cut off point for the 
# population weighted vigintiles
# This ensures we have as close to the target cut off point as possible

simd2016_hb2014 %<>%
  mutate(simd2016_hb2014_decile = 
           if_else(d2 <= 0, 
                   dec, 
                   if_else(d2 > 0 & hb2014 == lag(hb2014), lag(dec), 0)))

# Create Health Board level quintiles

simd2016_hb2014 <- geo_quintile(simd2016_hb2014, "simd2016_hb2014_quintile", 
                                "simd2016_hb2014_decile")

# Run checks on Health Board level data

geo_checks(data = simd2016_hb2014, quin_col = "simd2016_hb2014_quintile", 
           dec_col = "simd2016_hb2014_decile", geography = "hb2014")



### 6.1 - Manual Changes for Small NHS Boards ----

# From the custom tables above the following changes need to be made to allow 
# the population weighted deciles to be as close to 10%/20% as they can be
# From the checks output we need to update Orkney, Shetland and Western Isles

simd2016_hb2014_orkney <- simd2016_hb2014 %>% 
  filter(hb2014 == "S08000025")

simd2016_hb2014_shetland <- simd2016_hb2014 %>% 
  filter(hb2014 == "S08000026")

simd2016_hb2014_western <- simd2016_hb2014 %>%
  filter(hb2014 == "S08000028")

simd2016_hb2014_unchanged <- simd2016_hb2014 %>% 
  filter(hb2014 != "S08000025" & 
         hb2014 != "S08000026" & 
         hb2014 != "S08000028")


### 6.1.1 - Orkney ----

# Change deciles

simd2016_hb2014_orkney <- manual_changes(simd2016_hb2014_orkney, 
                                         "simd2016_hb2014_decile")

# Recalculate Health Board level quintiles

simd2016_hb2014_orkney <- geo_quintile(simd2016_hb2014_orkney, 
                                       "simd2016_hb2014_quintile", 
                                       "simd2016_hb2014_decile")

# Check changes look ok

changes_check(simd2016_hb2014_orkney, "hb2014", "simd2016_hb2014_decile", 
              "simd2016_hb2014_quintile")


### 6.1.2 - Shetland ----

# Change deciles

simd2016_hb2014_shetland <- manual_changes(simd2016_hb2014_shetland, 
                                           "simd2016_hb2014_decile")

# Recalculate Health Board level quintiles

simd2016_hb2014_shetland <- geo_quintile(simd2016_hb2014_shetland, 
                                         "simd2016_hb2014_quintile", 
                                         "simd2016_hb2014_decile")

# Check changes look ok

changes_check(simd2016_hb2014_shetland, "hb2014", "simd2016_hb2014_decile", 
              "simd2016_hb2014_quintile")



### 6.1.3 - Western Isles ----

# Change deciles

simd2016_hb2014_western <- manual_changes(simd2016_hb2014_western, 
                                          "simd2016_hb2014_decile")

# Recalculate Health Board level quintiles

simd2016_hb2014_western <- geo_quintile(simd2016_hb2014_western, 
                                        "simd2016_hb2014_quintile", 
                                        "simd2016_hb2014_decile")

# Check changes look ok

changes_check(simd2016_hb2014_western, "hb2014", "simd2016_hb2014_decile", 
              "simd2016_hb2014_quintile")


### 6.2 - Create Final Health Board Output ----

# Join unchanged data with the small NHS boards
# Select the relevant variables
# Sort by hb2014, decile and data_zone2011

simd2016_hb2014 <- simd2016_hb2014_unchanged %>%
  bind_rows(simd2016_hb2014_orkney, 
            simd2016_hb2014_shetland, 
            simd2016_hb2014_western) %>% 
  select(data_zone2011, hb2014, hb2014name, simd2016_hb2014_decile, 
         simd2016_hb2014_quintile) %>% 
  arrange(hb2014, simd2016_hb2014_decile, data_zone2011)

# Remove dataframes

rm(simd2016_hb2014_orkney, simd2016_hb2014_shetland, simd2016_hb2014_western, 
   simd2016_hb2014_unchanged)



### 7 - Calculate SIMD 2016 HSCP Level Population Weighted Categories ----

# Match simd2016 file onto hscp2016_pop
# Arrange by hscp and simd rank

simd2016_hscp2016 <- simd2016 %>%
  left_join(hscp2016_pop) %>%
  arrange(hscp2016, simd2016rank)

# Calculate cumulative population within each hscp
# Calculate the cumulative population percentage

simd2016_hscp2016 %<>%
  group_by(hscp2016) %>%
  mutate(cpop = cumsum(data_zone2011_pop)) %>%
  mutate(cpop_per = (cpop/hscp2016_pop)*100) %>%
  ungroup()

# Create a variable for non population weighted deciles

simd2016_hscp2016 <- geo_dec(simd2016_hscp2016)

# Create a variable for non population weighted quintiles

simd2016_hscp2016 <- geo_quin(simd2016_hscp2016)

# Create a variable to calculate the difference between the cumulative 
# population % and the target cut off points

simd2016_hscp2016 <- geo_cut_off(simd2016_hscp2016) %>% 
  mutate(d1 = if_else(is.na(d1), 0, d1))

# Flag the points where the non population weighted deciles change
# Set first row value for flag to 0 rather than NA
# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted vigintiles

simd2016_hscp2016 %<>%
  mutate(flag = if_else(hscp2016 == lag(hscp2016) & dec != lag(dec), 1, 0), 
         flag = if_else(is.na(flag), 0, flag), 
         d2 = if_else(flag == 1 & hscp2016 == lag(hscp2016), d1 + lag(d1), 0))

# Create a variable for population weighted decile
# If the difference is greater than zero, we change the cut off point for the 
# population weighted vigintiles
# This ensures we have as close to the target cut off point as possible

simd2016_hscp2016 %<>%
  mutate(simd2016_hscp2016_decile = 
           if_else(d2 <= 0, 
                   dec, 
                   if_else(d2 > 0 & hscp2016 == lag(hscp2016), lag(dec), 0)))

# Create HSCP level quintiles

simd2016_hscp2016 <- geo_quintile(simd2016_hscp2016, 
                                  "simd2016_hscp2016_quintile", 
                                  "simd2016_hscp2016_decile")

# Run checks on HSCP level data

geo_checks(simd2016_hscp2016, "simd2016_hscp2016_quintile", 
           "simd2016_hscp2016_decile", "hscp2016")



### 7.1 - Manual Changes for Small HSCPs ----

# From the custom tables above the following changes need to be made to allow 
# the population weighted deciles to be as close to 10%/20% as they can be
# From the checks output we need to update Orkney, Shetland and Western Isles

simd2016_hscp2016_orkney <- simd2016_hscp2016 %>% 
  filter(hscp2016 == "S37000022")

simd2016_hscp2016_shetland <- simd2016_hscp2016 %>% 
  filter(hscp2016 == "S37000026")

simd2016_hscp2016_western <- simd2016_hscp2016 %>% 
  filter(hscp2016 == "S37000031")

simd2016_hscp2016_unchanged <- simd2016_hscp2016 %>% 
  filter(hscp2016 != "S37000022" & 
         hscp2016 != "S37000026" & 
         hscp2016 != "S37000031")


### 7.1.1 - Orkney ----

# Change deciles

simd2016_hscp2016_orkney <- manual_changes(simd2016_hscp2016_orkney, 
                                           "simd2016_hscp2016_decile")

# Recalculate HSCP level quintiles

simd2016_hscp2016_orkney <- geo_quintile(simd2016_hscp2016_orkney,
                                         "simd2016_hscp2016_quintile", 
                                         "simd2016_hscp2016_decile")

# Check changes look ok

changes_check(simd2016_hscp2016_orkney, "hscp2016", "simd2016_hscp2016_decile", 
              "simd2016_hscp2016_quintile")


### 7.1.2 - Shetland ----

# Change deciles

simd2016_hscp2016_shetland <- manual_changes(simd2016_hscp2016_shetland, 
                                             "simd2016_hscp2016_decile")

# Recalculate HSCP level quintiles

simd2016_hscp2016_shetland <- geo_quintile(simd2016_hscp2016_shetland, 
                                           "simd2016_hscp2016_quintile", 
                                           "simd2016_hscp2016_decile")

# Check changes look ok

changes_check(simd2016_hscp2016_shetland, "hscp2016", "simd2016_hscp2016_decile", 
              "simd2016_hscp2016_quintile")



### 7.1.3 - Western Isles ----

# Change deciles

simd2016_hscp2016_western <- manual_changes(simd2016_hscp2016_western, 
                                            "simd2016_hscp2016_decile")

# Recalculate HSCP level quintiles

simd2016_hscp2016_western <- geo_quintile(simd2016_hscp2016_western, 
                                          "simd2016_hscp2016_quintile", 
                                          "simd2016_hscp2016_decile")

# Check changes look ok

changes_check(simd2016_hscp2016_western, "hscp2016", "simd2016_hscp2016_decile", 
              "simd2016_hscp2016_quintile")



### 7.2 - Create Final HSCP2016 Output ----

# Join unchanged data with the small HSCPs
# Select the relevant variables
# Sort by hscp2016, decile and data_zone2011

simd2016_hscp2016 <- simd2016_hscp2016_unchanged %>%
  bind_rows(simd2016_hscp2016_orkney, 
            simd2016_hscp2016_shetland, 
            simd2016_hscp2016_western) %>% 
  select(data_zone2011, hscp2016, hscp2016name, simd2016_hscp2016_decile, 
         simd2016_hscp2016_quintile) %>% 
  arrange(hscp2016, simd2016_hscp2016_decile, data_zone2011)

# Remove dataframes

rm(simd2016_hscp2016_orkney, simd2016_hscp2016_shetland, 
   simd2016_hscp2016_western, simd2016_hscp2016_unchanged)



### 8 - Calculate SIMD2016 Council Area Level Population Weighted Categories ----

# Match simd2016 file onto ca2011_pop
# Arrange by council area and simd rank

simd2016_ca2011 <- simd2016 %>%
  left_join(ca2011_pop) %>%
  arrange(ca2011, simd2016rank)

# Calculate cumulative population within each council area
# Calculate the cumulative population percentage

simd2016_ca2011 %<>%
  group_by(ca2011) %>%
  mutate(cpop = cumsum(data_zone2011_pop), 
         cpop_per = (cpop/ca2011_pop)*100) %>%
  ungroup()

# Create a variable for non population weighted deciles

simd2016_ca2011 <- geo_dec(simd2016_ca2011)

# Create a variable for non population weighted quintiles

simd2016_ca2011 <- geo_quin(simd2016_ca2011)

# Create a variable to calculate the difference between the cumulative 
# population % and the target cut off points

simd2016_ca2011 <- geo_cut_off(simd2016_ca2011)

# Flag the points where the non population weighted deciles change
# Set first row value for flag to 0 rather than NA
# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted vigintiles

simd2016_ca2011 %<>%
  mutate(flag = if_else(ca2011 == lag(ca2011) & dec != lag(dec), 1, 0), 
         flag = if_else(is.na(flag), 0, flag), 
         d2 = if_else(flag == 1 & ca2011 == lag(ca2011), d1 + lag(d1), 0))

# Create a variable for population weighted decile
# If the difference is greater than zero, we change the cut off point for the 
# population weighted vigintiles
# This ensures we have as close to the target cut off point as possible

simd2016_ca2011 %<>%
  mutate(simd2016_ca2011_decile = 
           if_else(d2 <= 0, 
                   dec, 
                   if_else(d2 > 0 & ca2011 == lag(ca2011), lag(dec), 0)))

# Create Council Area level quintiles

simd2016_ca2011 <- geo_quintile(simd2016_ca2011, 
                                "simd2016_ca2011_quintile", 
                                "simd2016_ca2011_decile")

# Run checks on Scotland level data

geo_checks(simd2016_ca2011, "simd2016_ca2011_quintile", 
           "simd2016_ca2011_decile", "ca2011")


### 8.1 - Manual Changes for Small Council Areas ----

# From the custom tables above the following changes need to be made to allow 
# the population weighted deciles to be as close to 10%/20% as they can be
# From the checks output we need to update Orkney, Shetland and 
# Na h-Eileanan Siar

simd2016_ca2011_orkney <- simd2016_ca2011 %>% 
  filter(ca2011 == "S12000023")

simd2016_ca2011_shetland <- simd2016_ca2011 %>% 
  filter(ca2011 == "S12000027")

simd2016_ca2011_siar <- simd2016_ca2011 %>% 
  filter(ca2011 == "S12000013")

simd2016_ca2011_unchanged <- simd2016_ca2011 %>% 
  filter(ca2011 != "S12000023" & 
         ca2011 != "S12000027" & 
         ca2011 != "S12000013")


### 8.1.1 - Orkney ----

# Change deciles

simd2016_ca2011_orkney <- manual_changes(simd2016_ca2011_orkney, 
                                         "simd2016_ca2011_decile")

# Recalculate Council Area level quintiles

simd2016_ca2011_orkney <- geo_quintile(simd2016_ca2011_orkney, 
                                       "simd2016_ca2011_quintile", 
                                       "simd2016_ca2011_decile")

# Check changes look ok

changes_check(simd2016_ca2011_orkney, "ca2011", "simd2016_ca2011_decile", 
              "simd2016_ca2011_quintile")


### 8.1.2 - Shetland ----

# Change deciles

simd2016_ca2011_shetland <- manual_changes(simd2016_ca2011_shetland, 
                                           "simd2016_ca2011_decile")

# Recalculate Council Area level quintiles

simd2016_ca2011_shetland <- geo_quintile(simd2016_ca2011_shetland, 
                                         "simd2016_ca2011_quintile", 
                                         "simd2016_ca2011_decile")

# Check changes look ok

changes_check(simd2016_ca2011_shetland, "ca2011", "simd2016_ca2011_decile", 
              "simd2016_ca2011_quintile")



### 8.1.3 - Western Isles ----

# Change deciles

simd2016_ca2011_siar <- manual_changes(simd2016_ca2011_siar, 
                                       "simd2016_ca2011_decile")

# Recalculate Council Area level quintiles

simd2016_ca2011_siar <- geo_quintile(simd2016_ca2011_siar, 
                                     "simd2016_ca2011_quintile", 
                                     "simd2016_ca2011_decile")

# Check changes look ok

changes_check(simd2016_ca2011_siar, "ca2011", "simd2016_ca2011_decile", 
              "simd2016_ca2011_quintile")



### 8.2 - Select Columns for Output ----

# Join unchanged data with the small Council Areas
# Select the relevant variables
# Sort by ca2011, decile and data_zone2011

simd2016_ca2011 <- simd2016_ca2011_unchanged %>%
  bind_rows(simd2016_ca2011_orkney, 
            simd2016_ca2011_shetland, 
            simd2016_ca2011_siar) %>% 
  select(data_zone2011, ca2011, ca2011name, simd2016_ca2011_decile, 
         simd2016_ca2011_quintile) %>% 
  arrange(ca2011, simd2016_ca2011_decile, data_zone2011)

# Remove dataframes

rm(simd2016_ca2011_orkney, simd2016_ca2011_shetland, simd2016_ca2011_siar, 
   simd2016_ca2011_unchanged)



### 9 - Put Everything Together to Create SIMD 2016 Lookup File ----

# Match hb2014, hscp2016, ca2011 and SIMD simd16_domains files onto simd2016

DZ2011_simd2016 <- simd2016 %>%
  left_join(simd2016_hb2014) %>%
  left_join(simd2016_hscp2016) %>%
  left_join(simd2016_ca2011) %>%
  left_join(simd_domains)

# Rename variables

DZ2011_simd2016 %<>%
  rename(simd2016_inc_rate = Income_rate, simd2016_inc_dep_N = Income_count, 
         simd2016_inc_rank = incrank, simd2016_emp_rate = Employment_rate, 
         simd2016_emp_dep_N = Employment_count, simd2016_emp_rank = emprank,
         simd2016_hlth_score = healthscr, 
         simd2016_hlth_rank = SIMD16_Health_Domain_Rank,
         simd2016_educ_score = Eduscr, 
         simd2016_educ_rank = SIMD16_Education_domain_rank,
         simd2016_house_score = housingscr, simd2016_house_rank = housingrank, 
         simd2016_access_score = accessscr, simd2016_access_rank = accessrank, 
         simd2016_crime_rate = crimerate, simd2016_crime_rank = crimerank, 
         pop_2014 = data_zone2011_pop)

# Select relevant variables

DZ2011_simd2016 %<>%
  left_join(dz_iz) %>% 
  select(data_zone2011, data_zone2011name, int_zone2011, int_zone2011name, 
         hb2014, hb2014name, hscp2016, hscp2016name, ca2011, ca2011name, 
         simd2016rank, simd2016_sc_decile, simd2016_sc_quintile, 
         simd2016_hb2014_decile, simd2016_hb2014_quintile, 
         simd2016_hscp2016_decile, simd2016_hscp2016_quintile, 
         simd2016_ca2011_decile, simd2016_ca2011_quintile, simd2016tp15, 
         simd2016bt15, simd2016_educ_rank, simd2016_educ_score, 
         simd2016_emp_rate, simd2016_emp_dep_N, simd2016_emp_rank, 
         simd2016_hlth_score, simd2016_hlth_rank, simd2016_house_score, 
         simd2016_house_rank, simd2016_inc_rate, simd2016_inc_dep_N, 
         simd2016_inc_rank, simd2016_access_score, simd2016_access_rank, 
         simd2016_crime_rate, simd2016_crime_rank, pop_2014)



### 10 - Create postcode_simd 2016 lookup ----

# Most recent SPD at time of release of simd2016 was 2016_1
# Select pc7 and DataZone2011 and sort by DataZone2011

SPD_2016_1 <- fread(glue("{spd_archive}/", 
                         "Scottish_Postcode_Directory_2016_1.csv")) %>%
  select(pc7, DataZone2011) %>%
  arrange(DataZone2011) %>% 
  rename(data_zone2011 = DataZone2011)

# Match onto DZ2011_simd2016 
# Sort by pc7
# Drop pop_2014

postcode_2016_1_simd2016 <- SPD_2016_1 %>%
  left_join(DZ2011_simd2016) %>%
  arrange(pc7) %>%
  select(-pop_2014)



### 11 - Add SIMD 2016 to Postcode All SIMD & Carstairs Lookup ----

pc_carstairs <- fread(glue("{pc_simd_filepath}/", 
                           "postcode_2016_1_all_simd_carstairs.csv"))

postcode_2016_1_simd2016 %<>%
  select(-c(int_zone2011, int_zone2011name, hb2014, hb2014name, hscp2016, 
            hscp2016name, ca2011, ca2011name)) %>% 
  left_join(pc_carstairs)
  


### 12 - Update SAPE Files for Single Year of Age ----

# Read in file
# Update to RDS

DZ2011_pop_est_2011_2015 <- read_csv(glue("{pop_filepath}/", 
                                          "DataZone2011_pop_est_2011_2015.csv")) %>% 
  clean_names()

# Sort by data_zone2011
# Match onto DZ2011_simd2016
# Sort by year, data_zone2011 and sex (descending)
# Remove pop_2014

DZ2011_pop_est_2011_2015 %<>%
  arrange(data_zone2011) %>%
  left_join(DZ2011_simd2016) %>%
  arrange(year, data_zone2011, desc(sex)) %>%
  select(-pop_2014) %>% 
  clean_names()




### 13 - Update SAPE Files for 5 Year Age Groups ----

# Read in file
# Update to RDS

DZ2011_pop_est_2011_2015_5y <- read_csv(glue("{pop_filepath}/", 
                                             "DataZone2011_pop_est_5year_", 
                                             "agegroups_2011_2015.csv")) %>% 
  clean_names()

# Sort by data_zone2011
# Match onto DZ2011_SIMD2016
# Sort by year, data_zone2011 and sex (descending)
# Remove pop_2014

DZ2011_pop_est_2011_2015_5y %<>%
  arrange(data_zone2011) %>%
  left_join(DZ2011_simd2016) %>%
  arrange(year, data_zone2011, desc(sex)) %>%
  select(-pop_2014)
