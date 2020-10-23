##########################################################
# Datamart Tests
# Calum Purdie
# Original date 30/03/2020
# Latest update author - Calum Purdie
# Latest update date - 13/05/2020
# Latest update description - formatting code
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for testing new population projections uploaded to the datamart
# Run these checks in R and Business Objects and check results match
# Approximate run time - 10 seconds
##########################################################

### 1 Housekeeping ----

library(magrittr)
library(dplyr)
library(tidylog)
library(glue)

# Set years to use

start <- "2018"
end <- "2043"

# Set filepath

filepath <- glue("//freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                 "2_Population/Population Projections/Lookup Files/R Files")

hb_filepath <- glue("{filepath}/HB2019_pop_proj_{start}_{end}.rds")
hscp_filepath <- glue("{filepath}/HSCP2019_pop_proj_{start}_{end}.rds")
ca_filepath <- glue("{filepath}/CA2019_pop_proj_{start}_{end}.rds")



### 2 HB2014 ----

# Read in Health Board projections

hb <- readRDS(hb_filepath)

# Check that all years of the population projections are there 
# Check that all totals match

hb %>% 
  group_by(year) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

# Choose a specific year and check that the totals by health board and sex match
# Use hb2014 as datamart uses old codes

hb %>% 
  filter(year == 2030) %>% 
  group_by(hb2014name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

hb %>% 
  filter(year == 2043) %>% 
  group_by(hb2014name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

# Choose a specific year and check that the totals by age

hb %>%  
  filter(year == 2018) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

hb %>%  
  filter(year == 2035) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)



### 3 HSCP2016 ----

# Read in Health Board projections

hscp <- readRDS(hscp_filepath)

# Check that all years of the population projections are there 
# Check that all totals match

hscp %>% 
  group_by(year) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

# Choose a specific year and check that the totals by hscp and sex match
# Use hscp2016 as datamart uses old codes

hscp %>% 
  filter(year == 2024) %>% 
  group_by(hscp2019name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

hscp %>% 
  filter(year == 2038) %>% 
  group_by(hscp2019name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

# Choose a specific year and check that the totals by age

hscp %>%  
  filter(year == 2020) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

hscp %>%  
  filter(year == 2029) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)



### 4 CA2011 ----

# Read in Health Board projections

ca <- readRDS(ca_filepath)

# Check that all years of the population projections are there 
# Check that all totals match

ca %>% 
  group_by(year) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

# Choose a specific year and check that the totals by health board and sex match
# Use ca2011 as datamart uses old codes

ca %>% 
  filter(year == 2027) %>% 
  group_by(ca2019name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

ca %>% 
  filter(year == 2034) %>% 
  group_by(ca2019name, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

# Choose a specific year and check that the totals by age

ca %>%  
  filter(year == 2026) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)

ca %>%  
  filter(year == 2041) %>% 
  group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  print(n = Inf)
