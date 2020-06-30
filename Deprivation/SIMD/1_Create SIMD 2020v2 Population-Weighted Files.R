##########################################################
# Create SIMD 2020 Population-Weighted Files
# Calum Purdie
# Original date 28/01/2020
# Latest update author - Calum Purdie
# Latest update date - 28/05/2020
# Latest update description 
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for creating population-weighted SIMD 2020 Files
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
library(readxl)
library(haven)
library(sjlabelled)
library(openxlsx)

# set filepaths

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD")

pop_filepath <- glue("{base_filepath}/2_Population/", 
                     "Small Area Population estimates/Lookup Files")

pop_archive <- glue("{pop_filepath}/R Files/Archive")

simd_filepath <- glue("{base_filepath}/3_Deprivation/SIMD/Source Data/", 
                      "SIMD 2020v2")

simd_lookup <- glue("{base_filepath}/3_Deprivation/SIMD/Lookup Files/SIMD 2020")

simd_qa_filepath <- glue("{base_filepath}/3_Deprivation/SIMD/", 
                         "Quality Assurance Checks")

pc_simd_filepath <- glue("{base_filepath}/3_Deprivation/Postcode Deprivation/", 
                         "Lookup Files")

spd <- glue("{base_filepath}/1_Geography/Scottish Postcode Directory/", 
            "Lookup Files/R Files")

# Read in functions

source(here::here("Deprivation", "SIMD", "Functions for Creating SIMD Files.R"))




### 2 Get Populations Data Ready for Matching ----

### 2.1 Read in data ----

# Read in uncorrected populations
# Select only 2017 population estimates as these are the populations SIMD 2020 
# is based on

DZ2011_pop_est <- readRDS(glue("{pop_archive}/", 
                                "DataZone2011_pop_est_2011_2017.rds")) %>% 
  filter(Year == 2017) %>% 
  clean_names() %>% 
  rename(datazone2011 = data_zone2011, 
         intzone2011 = int_zone2011)



### 2.2 Add higher geographies ----

# Add columns for higher geographies

# Use the Geography Codes and Names open data file to get the IZ names
# First need to run the httr configuration script

source(here("Geography", "Scottish Postcode Directory", 
            "Set httr configuration for API.R"))

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id <- "395476ab-0720-4740-be07-ff4467141352"

dz_iz <- dplyr::tbl(src = ckan$con, from = res_id) %>%  
  as_tibble() %>% 
  select(DataZone, DataZoneName, IntZone, IntZoneName, CA, CAName, HSCP, 
         HSCPName, HB, HBName) %>% 
  rename(datazone2011 = DataZone, datazone2011name = DataZoneName, 
         intzone2011 = IntZone, intzone2011name = IntZoneName, 
         ca2019 = CA, ca2019name = CAName, 
         hscp2019 = HSCP, hscp2019name = HSCPName, 
         hb2019 = HB, hb2019name = HBName)

# Match on higher geographies
# Sort by DataZone2011 for matching
# Create a Scotland flag

DZ2011_pop_est %<>%
  arrange(datazone2011) %>%
  left_join(dz_iz) %>% 
  mutate(scot = 1)

# Select relevant geography names

geo_names <- DZ2011_pop_est %>% 
  select(datazone2011, datazone2011name, intzone2011, intzone2011name, 
         ca2019, ca2019name, ca2018, ca2011, hscp2019, hscp2019name, hscp2018, 
         hscp2016, hb2019, hb2019name, hb2018, hb2014) %>% 
  distinct()



### 2.3 Create Geography Population Files ----

# scotland

scotland_pop <- pop_function("scot", "scot_pop")

# hb2019

hb2019_pop <- pop_function("hb2019", "hb2019_pop")

# hscp2019

hscp2019_pop <- pop_function("hscp2019", "hscp2019_pop")

# ca2019

ca2019_pop <- pop_function("ca2019", "ca2019_pop")

# dz2011

dz2011_pop <- pop_function("datazone2011", "datazone2011_pop")



### 3 - Get SIMD domains ready for matching ----

# Read in SIMD files
# Some suppressed data has a * value
# Change all cells with a * to NA

simd_indicators <- read_xlsx(glue("{simd_filepath}/SIMD_2020v2_Indicators.xlsx"), 
                             sheet = "Data") %>% 
  select(-c(Intermediate_Zone, Council_area, Total_population, 
            Working_age_population)) %>% 
  rename(datazone2011 = Data_Zone) %>% 
  clean_names() %>% 
  mutate_all(~na_if(., "*"))

# Set columns still names simd2020_ to simd2020v2_

simd_ranks <- read_xlsx(glue("{simd_filepath}/SIMD_2020v2_Ranks_and_Domain_", 
                             "Ranks.xlsx"), sheet = "SIMD 2020v2 ranks") %>% 
  select(-c(Intermediate_Zone, Council_area, Total_population, 
            Working_age_population)) %>% 
  rename(datazone2011 = Data_Zone) %>% 
  clean_names() %>% 
  setnames(gsub("simd2020_", "simd2020v2_", names(.)))

simd_domains <- simd_ranks %>% 
  left_join(simd_indicators) 



### 4 - Calculate SIMD 2020 Scotland level population weighted categories ----

# Read in source data
# Match on higher geographies and Data Zone populations
# Create Scotland flag for matching
# Match on the total Scotland populations
# Sort cases in order of SIMD 2016 rank, i.e. from most to least deprived
# Create a variable to store cumulative population
# Calculate the cumulative population percentage

simd2020 <- simd_ranks %>%
  left_join(geo_names) %>%
  left_join(dz2011_pop) %>%
  mutate(scot = 1) %>%
  left_join(scotland_pop) %>%
  arrange(simd2020v2_rank) %>%
  mutate(cpop = cumsum(datazone2011_pop), 
         cpop_per = (cpop/scot_pop)*100)

# Create datazone based non population weighted vigintiles

simd2020 <- vig_function(simd2020, "npw_vig", "simd2020v2_rank")

# Create datazone based non population weighted deciles

simd2020 <- dec_function(simd2020, "npw_dec", "npw_vig")

# Create datazone based non population weighted quintiles

simd2020 <- quin_function(simd2020, "npw_quin", "npw_dec")

# Create a variable for population weighted vigintiles based on the cut off 
# always being below the target cut off point

simd2020 <- vig_cpop(simd2020)

# Create a variable to calculate the difference between the cumulative 
# population and the target cut off points

simd2020 <- vig_cut_off(simd2020)

# Flag the points where the vigintiles change
# Set first row value for flag to 0 rather than NA
# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted vigintiles

simd2020 %<>%
  mutate(flag = if_else(scot == lag(scot) & vig != lag(vig), 1, 0), 
         flag = if_else(is.na(flag), 0, flag), 
         d2 = if_else(flag == 1 & scot == lag(scot), d1 + lag(d1), 0))


### 5 - Population Weighted Vigintiles ----        

# Create a variable for population weighted vigintiles

simd2020 %<>%
  mutate(simd2020v2_sc_vig = if_else(d2 <= 0, vig, 
                                   if_else(d2 > 0 & 
                                          scot == lag(scot), lag(vig), 0)))

# Create Scotland level deciles

simd2020 <- dec_function(simd2020, "simd2020v2_sc_decile", "simd2020v2_sc_vig")

# Create Scotland level quintiles

simd2020 <- quin_function(simd2020, "simd2020v2_sc_quintile", 
                          "simd2020v2_sc_decile")


# Create variables for 15% most and least deprived
# Sort by dz2011

simd2020 %<>%
  mutate(simd2020v2tp15 = if_else(simd2020v2_sc_vig == 1 | 
                                  simd2020v2_sc_vig == 2 | 
                                  simd2020v2_sc_vig == 3, 1, 0), 
         simd2020v2bt15 = if_else(simd2020v2_sc_vig == 18 | 
                                  simd2020v2_sc_vig == 19 | 
                                  simd2020v2_sc_vig == 20, 1, 0)) %>%
  arrange(datazone2011)

# Run checks on Scotland level data

scot_checks(simd2020, "simd2020v2_sc_quintile", "simd2020v2_sc_decile", 
            "simd2020v2_sc_vig", "simd2020v2tp15", "simd2020v2bt15")



### 6 - Calculate SIMD 2020 Health Board level population weighted categories ----

# Match simd2020 file onto hb2019_pop
# Arrange by health board and simd rank

simd2020v2_hb2019 <- simd2020 %>%
  left_join(hb2019_pop) %>%
  arrange(hb2019, simd2020v2_rank)

# Calculate cumulative population within each health board
# Calculate the cumulative population percentage

simd2020v2_hb2019 %<>%
  group_by(hb2019) %>%
  mutate(cpop = cumsum(datazone2011_pop)) %>%
  mutate(cpop_per = (cpop/hb2019_pop)*100) %>%
  ungroup()

# Create a variable for non population weighted deciles

simd2020v2_hb2019 <- geo_dec(simd2020v2_hb2019)

# Create a variable for non population weighted quintiles

simd2020v2_hb2019 <- geo_quin(simd2020v2_hb2019)

# Create a variable to calculate the difference between the cumulative 
# population % and the target cut off points

simd2020v2_hb2019 <- geo_cut_off(simd2020v2_hb2019)

# Flag the points where the non population weighted deciles change
# Set first row value for flag to 0 rather than NA
# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted vigintiles

simd2020v2_hb2019 %<>%
  mutate(flag = if_else(hb2019 == lag(hb2019) & dec != lag(dec), 1, 0), 
         flag = if_else(is.na(flag), 0, flag), 
         d2 = if_else(flag == 1 & hb2019 == lag(hb2019), d1 + lag(d1), 0))

# Create a variable for population weighted decile
# If the difference is greater than zero, we change the cut off point for the 
# population weighted vigintiles
# This ensures we have as close to the target cut off point as possible

simd2020v2_hb2019 %<>%
  mutate(simd2020v2_hb2019_decile = if_else(d2 <= 0, dec, 
                                          if_else(d2 > 0 & 
                                                  hb2019 == lag(hb2019), 
                                                  lag(dec), 0)))

# Create Health Board level quintiles

simd2020v2_hb2019 <- geo_quintile(simd2020v2_hb2019, "simd2020v2_hb2019_quintile", 
                                "simd2020v2_hb2019_decile")

# Run checks on Health Board level data

geo_checks(simd2020v2_hb2019, "simd2020v2_hb2019_quintile", "simd2020v2_sc_quintile",  
           "simd2020v2_hb2019_decile", "simd2020v2_sc_decile", "hb2019")



### 6.1 - Manual Changes for Small NHS Boards ----

# From the custom tables above the following changes need to be made to allow 
# the population weighted deciles to be as close to 10%/20% as they can be
# From the checks output we need to update Orkney, Shetland and Western Isles

simd2020v2_hb2019_orkney <- simd2020v2_hb2019 %>% 
  filter(hb2019 == "S08000025")

simd2020v2_hb2019_shetland <- simd2020v2_hb2019 %>% 
  filter(hb2019 == "S08000026")

simd2020v2_hb2019_western <- simd2020v2_hb2019 %>%
  filter(hb2019 == "S08000028")

simd2020v2_hb2019_unchanged <- simd2020v2_hb2019 %>% 
  filter(hb2019 != "S08000025" & 
         hb2019 != "S08000026" & 
         hb2019 != "S08000028")

# Save as excel file for manual adjustment

list_of_datasets <- list("NHS Orkney" = simd2020v2_hb2019_orkney, 
                         "NHS Shetland" = simd2020v2_hb2019_shetland, 
                         "NHS Western Isles" = simd2020v2_hb2019_western)

# Select relevant columns

list_of_datasets <- lapply(list_of_datasets, 
                           function(x) x %>% 
                             select(datazone2011, simd2020v2_rank, 
                                    datazone2011_pop, cpop, cpop_per, d1, flag, 
                                    d2, hb2019_pop, dec, simd2020v2_hb2019_decile, 
                                    quin, simd2020v2_hb2019_quintile))


# write.xlsx(list_of_datasets, 
#            glue("{simd_qa_filepath}/", 
#                 "SIMD 2020v2 - Checking quintiles and deciles.xlsx"))


### 6.1.1 - Orkney ----

# No changes required


### 6.1.2 - Shetland ----

# Change deciles

simd2020v2_hb2019_shetland %<>%
  mutate(simd2020v2_hb2019_decile = case_when(datazone2011 == "S01012409" ~ 2, 
                                            datazone2011 == "S01012387" ~ 4,  
                                            TRUE ~ simd2020v2_hb2019_decile))

# Recalculate Health Board level quintiles

simd2020v2_hb2019_shetland <- geo_quintile(simd2020v2_hb2019_shetland, 
                                         "simd2020v2_hb2019_quintile", 
                                         "simd2020v2_hb2019_decile")

# Check changes look ok

changes_check(simd2020v2_hb2019_shetland, "hb2019", "simd2020v2_hb2019_decile", 
              "simd2020v2_hb2019_quintile")



### 6.1.3 - Western Isles ----

# No changes required



### 6.2 - Create Final Health Board Output ----

# Join unchanged data with the small NHS boards
# Select the relevant variables
# Sort by hb2019, decile and dz2011

simd2020v2_hb2019 <- simd2020v2_hb2019_unchanged %>%
  bind_rows(simd2020v2_hb2019_orkney, 
            simd2020v2_hb2019_shetland, 
            simd2020v2_hb2019_western) %>% 
  select(datazone2011, hb2019, hb2019name, simd2020v2_hb2019_decile, 
         simd2020v2_hb2019_quintile) %>% 
  arrange(hb2019, simd2020v2_hb2019_decile, datazone2011)

# Remove dataframes

rm(simd2020v2_hb2019_orkney, simd2020v2_hb2019_shetland, simd2020v2_hb2019_western, 
   simd2020v2_hb2019_unchanged)



### 7 - Calculate SIMD 2020 HSCP Level Population Weighted Categories ----

# Match simd2020 file onto hscp2019_pop
# Arrange by hscp and simd rank

simd2020v2_hscp2019 <- simd2020 %>%
  left_join(hscp2019_pop) %>%
  arrange(hscp2019, simd2020v2_rank)

# Calculate cumulative population within each hscp
# Calculate the cumulative population percentage

simd2020v2_hscp2019 %<>%
  group_by(hscp2019) %>%
  mutate(cpop = cumsum(datazone2011_pop)) %>%
  mutate(cpop_per = (cpop/hscp2019_pop)*100) %>%
  ungroup()


# Create a variable for non population weighted deciles

simd2020v2_hscp2019 <- geo_dec(simd2020v2_hscp2019)

# Create a variable for non population weighted quintiles

simd2020v2_hscp2019 <- geo_quin(simd2020v2_hscp2019)

# Create a variable to calculate the difference between the cumulative 
# population % and the target cut off points

simd2020v2_hscp2019 <- geo_cut_off(simd2020v2_hscp2019) %>% 
  mutate(d1 = if_else(is.na(d1), 0, d1))

# Flag the points where the non population weighted deciles change
# Set first row value for flag to 0 rather than NA
# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted vigintiles

simd2020v2_hscp2019 %<>%
  mutate(flag = if_else(hscp2019 == lag(hscp2019) & dec != lag(dec), 1, 0), 
         flag = if_else(is.na(flag), 0, flag), 
         d2 = if_else(flag == 1 & hscp2019 == lag(hscp2019), d1 + lag(d1), 0))

# Create a variable for population weighted decile
# If the difference is greater than zero, we change the cut off point for the 
# population weighted vigintiles
# This ensures we have as close to the target cut off point as possible

simd2020v2_hscp2019 %<>%
  mutate(simd2020v2_hscp2019_decile = 
           if_else(d2 <= 0, 
                   dec, 
                   if_else(d2 > 0 & hscp2019 == lag(hscp2019), lag(dec), 0)))

# Create HSCP level quintiles

simd2020v2_hscp2019 <- geo_quintile(simd2020v2_hscp2019, 
                                  "simd2020v2_hscp2019_quintile", 
                                  "simd2020v2_hscp2019_decile")

# Run checks on HSCP level data

geo_checks(simd2020v2_hscp2019, "simd2020v2_hscp2019_quintile", 
           "simd2020v2_sc_quintile", "simd2020v2_hscp2019_decile", 
           "simd2020v2_sc_decile", "hscp2019")



### 7.1 - Manual Changes for Small HSCPs ----

# From the custom tables above the following changes need to be made to allow 
# the population weighted deciles to be as close to 10%/20% as they can be
# From the checks output we need to update Shetland

simd2020v2_hscp2019_shetland <- simd2020v2_hscp2019 %>% 
  filter(hscp2019 == "S37000026")

simd2020v2_hscp2019_unchanged <- simd2020v2_hscp2019 %>% 
  filter(hscp2019 != "S37000026")



### 7.1.1 - Shetland ----

# Change deciles

simd2020v2_hscp2019_shetland %<>%
  mutate(simd2020v2_hscp2019_decile = case_when(datazone2011 == "S01012409" ~ 2, 
                                              datazone2011 == "S01012387" ~ 4, 
                                              TRUE ~ simd2020v2_hscp2019_decile))
# Recalculate HSCP level quintiles

simd2020v2_hscp2019_shetland <- geo_quintile(simd2020v2_hscp2019_shetland, 
                                           "simd2020v2_hscp2019_quintile", 
                                           "simd2020v2_hscp2019_decile")

# Check changes look ok

changes_check(simd2020v2_hscp2019_shetland, "hscp2019", 
              "simd2020v2_hscp2019_decile", "simd2020v2_hscp2019_quintile")



### 7.2 - Create Final HSCP2019 Output ----

# Join unchanged data with the small HSCPs
# Select the relevant variables
# Sort by hscp2019, decile and dz2011

simd2020v2_hscp2019 <- simd2020v2_hscp2019_unchanged %>%
  bind_rows(simd2020v2_hscp2019_shetland) %>% 
  select(datazone2011, hscp2019, hscp2019name, simd2020v2_hscp2019_decile, 
         simd2020v2_hscp2019_quintile) %>% 
  arrange(hscp2019, simd2020v2_hscp2019_decile, datazone2011)

# Remove dataframes

rm(simd2020v2_hscp2019_shetland, simd2020v2_hscp2019_unchanged)



### 8 - Calculate simd2020 Council Area Level Population Weighted Categories ----

# Match simd2020 file onto ca2019_pop
# Arrange by council area and simd rank

simd2020v2_ca2019 <- simd2020 %>%
  left_join(ca2019_pop) %>%
  arrange(ca2019, simd2020v2_rank)

# Calculate cumulative population within each council area
# Calculate the cumulative population percentage

simd2020v2_ca2019 %<>%
  group_by(ca2019) %>%
  mutate(cpop = cumsum(datazone2011_pop), 
         cpop_per = (cpop/ca2019_pop)*100) %>%
  ungroup()

# Create a variable for non population weighted deciles

simd2020v2_ca2019 <- geo_dec(simd2020v2_ca2019)

# Create a variable for non population weighted quintiles

simd2020v2_ca2019 <- geo_quin(simd2020v2_ca2019)

# Create a variable to calculate the difference between the cumulative 
# population % and the target cut off points

simd2020v2_ca2019 <- geo_cut_off(simd2020v2_ca2019)

# Flag the points where the non population weighted deciles change
# Set first row value for flag to 0 rather than NA
# Calculate the difference between the cut off point differences
# This allows us to set the cut off point for the population weighted vigintiles

simd2020v2_ca2019 %<>%
  mutate(flag = if_else(ca2019 == lag(ca2019) & dec != lag(dec), 1, 0), 
         flag = if_else(is.na(flag), 0, flag), 
         d2 = if_else(flag == 1 & ca2019 == lag(ca2019), d1 + lag(d1), 0))

# Create a variable for population weighted decile
# If the difference is greater than zero, we change the cut off point for the 
# population weighted vigintiles
# This ensures we have as close to the target cut off point as possible

simd2020v2_ca2019 %<>%
  mutate(simd2020v2_ca2019_decile = 
           if_else(d2 <= 0, 
                   dec, 
                   if_else(d2 > 0 & ca2019 == lag(ca2019), lag(dec), 0)))

# Create Council Area level quintiles

simd2020v2_ca2019 <- geo_quintile(simd2020v2_ca2019, 
                                "simd2020v2_ca2019_quintile", 
                                "simd2020v2_ca2019_decile")

# Run checks on Scotland level data

geo_checks(simd2020v2_ca2019, "simd2020v2_ca2019_quintile", "simd2020v2_sc_quintile", 
           "simd2020v2_ca2019_decile", "simd2020v2_sc_decile","ca2019")


### 8.1 - Manual Changes for Small Council Areas ----

# From the custom tables above the following changes need to be made to allow 
# the population weighted deciles to be as close to 10%/20% as they can be
# From the checks output we need to update Shetland

simd2020v2_ca2019_shetland <- simd2020v2_ca2019 %>% 
  filter(ca2019 == "S12000027")

simd2020v2_ca2019_unchanged <- simd2020v2_ca2019 %>% 
  filter(ca2019 != "S12000027")



### 8.1.1 - Shetland ----

# Change deciles

simd2020v2_ca2019_shetland %<>%
  mutate(simd2020v2_ca2019_decile = case_when(datazone2011 == "S01012409" ~ 2, 
                                            datazone2011 == "S01012387" ~ 4,  
                                            TRUE ~ simd2020v2_ca2019_decile))
# Recalculate ca level quintiles

simd2020v2_ca2019_shetland <- geo_quintile(simd2020v2_ca2019_shetland, 
                                         "simd2020v2_ca2019_quintile", 
                                         "simd2020v2_ca2019_decile")

# Check changes look ok

changes_check(simd2020v2_ca2019_shetland, "ca2019", "simd2020v2_ca2019_decile", 
              "simd2020v2_ca2019_quintile")



### 8.2 - Select Columns for Output ----

# Join unchanged data with the small Council Areas
# Select the relevant variables
# Sort by ca2019, decile and dz2011

simd2020v2_ca2019 <- simd2020v2_ca2019_unchanged %>%
  bind_rows(simd2020v2_ca2019_shetland) %>% 
  select(datazone2011, ca2019, ca2019name, simd2020v2_ca2019_decile, 
         simd2020v2_ca2019_quintile) %>% 
  arrange(ca2019, simd2020v2_ca2019_decile, datazone2011)

# Remove dataframes

rm(simd2020v2_ca2019_shetland, simd2020v2_ca2019_unchanged)



### 9 - Put Everything Together to Create SIMD 2020 Lookup File ----

# Match hb2019, hscp2019, ca2019 and simd_domains files onto simd2020

DZ2011_simd2020v2 <- simd2020 %>%
  left_join(simd2020v2_hb2019) %>%
  left_join(simd2020v2_hscp2019) %>%
  left_join(simd2020v2_ca2019) %>%
  left_join(simd_domains)

# Rename variables

DZ2011_simd2020v2 %<>%
  rename(simd2020v2_inc_rate = income_rate, 
         simd2020v2_inc_dep_n = income_count, 
         simd2020v2_inc_rank = simd2020v2_income_domain_rank, 
         simd2020v2_emp_rate = employment_rate, 
         simd2020v2_emp_dep_n = employment_count, 
         simd2020v2_emp_rank = simd2020v2_employment_domain_rank,
         simd2020v2_hlth_rank = simd2020v2_health_domain_rank,
         simd2020v2_educ_rank = simd2020v2_education_domain_rank, 
         simd2020v2_house_rank = simd2020v2_housing_domain_rank, 
         simd2020v2_access_rank = simd2020v2_access_domain_rank, 
         simd2020v2_crime_rate = crime_rate, 
         simd2020v2_crime_count = crime_count, 
         simd2020v2_crime_rank = simd2020v2_crime_domain_rank, 
         pop_2017 = datazone2011_pop)

# Select relevant variables

DZ2011_simd2020v2 %<>%
  select(datazone2011, datazone2011name, intzone2011, intzone2011name, 
         hb2019, hb2019name, hb2018, hb2014, hscp2019, hscp2019name, hscp2018, 
         hscp2016, ca2019, ca2019name, ca2018, ca2011, simd2020v2_rank, 
         simd2020v2_sc_decile, simd2020v2_sc_quintile, simd2020v2_hb2019_decile, 
         simd2020v2_hb2019_quintile, simd2020v2_hscp2019_decile, 
         simd2020v2_hscp2019_quintile, simd2020v2_ca2019_decile, 
         simd2020v2_ca2019_quintile, simd2020v2tp15, simd2020v2bt15, 
         simd2020v2_educ_rank, simd2020v2_emp_rate, simd2020v2_emp_dep_n, 
         simd2020v2_emp_rank, simd2020v2_hlth_rank, simd2020v2_house_rank, 
         simd2020v2_inc_rate, simd2020v2_inc_dep_n, simd2020v2_inc_rank,
         simd2020v2_access_rank, simd2020v2_crime_rate, simd2020v2_crime_count, 
         simd2020v2_crime_rank, pop_2017)

# Round simd2020v2_crime_rate and simd2020v2_crime_count to 6 decimal places

DZ2011_simd2020v2 %<>%
  mutate(simd2020v2_crime_rate = round_half_up(as.numeric(simd2020v2_crime_rate), 6), 
         simd2020v2_crime_count = round_half_up(as.numeric(simd2020v2_crime_count), 6))

# Format quintiles and deciles columns to be integers

DZ2011_simd2020v2 %<>%
  mutate_at(vars(dplyr::contains("_quintile")), as.integer) %>% 
  mutate_at(vars(dplyr::contains("_decile")), as.integer)

# Save DZ2011_simd2020 as rds

saveRDS(DZ2011_simd2020v2, glue("{simd_lookup}/DataZone2011_simd2020v2.rds"))

# Save DZ2011_simd2020 as csv

write_csv(DZ2011_simd2020v2, glue("{simd_lookup}/DataZone2011_simd2020v2.csv"))

# Save DZ2011_simd2020 as sav

DZ2011_simd2020v2_spss <- spss_names(DZ2011_simd2020v2)

write_sav(DZ2011_simd2020v2_spss, 
          glue("{simd_lookup}/DataZone2011_simd2020v2.sav"))

rm(simd2020, simd2020v2_ca2019, simd2020v2_hb2019, simd2020v2_hscp2019, 
   DZ2011_simd2020v2_spss)
