# 1_Rebased Only_Trim Council_and_HB Mid-year Estimate files
# Rebased Mid-Year Population Estimates Only
# Required only for Reebased Mid-year_population estimates
# July 2024
# Created by Alan Coventry
# Posit Workbench
# trims down previous mid-year population files to year before start of rebased estimates & saves new files


### Libraries ####
library(magrittr)
library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(tidylog)
library(glue)
library(stringr)
library(janitor)
library(data.table)

### Years ####
start <- "1981" #should always be 1981, this is the first year in the output file
old <- "2000"   #should the year before re-based estimates, this is the last year in the output file
prev <- "2023"  #should the end year for latest pop-estimates- this helps identify name of file to be trimmed


### Set filepaths ####
filepath <- glue("/data/geography",
                 "/Population/Population Estimates") # if on Posit
data_filepath <- glue("{filepath}/Source Data")
output_filepath <- glue("{filepath}/Lookup Files/R Files")


### Import Historical Releases ####
# imports 4 files, CA, HB, single-year and 5-year estimates

### COUNCIL AREAS - Single Year File 
CA2019_pop_est_hist <- readRDS(glue("{output_filepath}/", 
                                    "CA2019_pop_est_{start}_{prev}.rds")) %>% 
  clean_names()

### COUNCIL AREAS - 5 Year age Group File
CA2019_pop_est_5y_hist <- readRDS(
  glue("{output_filepath}/CA2019_pop_est_5year_agegroups_{start}_{prev}.rds")) %>% 
  clean_names()

# HEALTH BOARDS - Single Year File 
HB2019_pop_est_hist <- readRDS(glue("{output_filepath}/", 
                                    "HB2019_pop_est_{start}_{prev}.rds")) %>% 
  clean_names()

### HEALTH BOARDS - 5 Year Age Group File
HB2019_pop_est_5y_hist <- readRDS(
  glue("{output_filepath}/", 
       "HB2019_pop_est_5year_agegroups_{start}_{prev}.rds")) %>% 
  clean_names()

### Trim out unecessary Years ####
# keeps only years between "start" and "old" variables

### COUNCIL AREAS - Single Year File 
CA2019_pop_est_hist_2010 <- CA2019_pop_est_hist %>%
  filter(year >= start & year <= old)

### COUNCIL AREAS - 5 Year age Group File
CA2019_pop_est_5y_hist_2010 <- CA2019_pop_est_5y_hist %>%
  filter(year >= start & year <= old)

# HEALTH BOARDS - Single Year File 
HB2019_pop_est_hist_2010 <- HB2019_pop_est_hist %>%
  filter(year >= start & year <= old)

### HEALTH BOARDS - 5 Year Age Group File
HB2019_pop_est_5y_hist_2010 <- HB2019_pop_est_5y_hist %>%
  filter(year >= start & year <= old)


### Save Trimmed Files ####
# saves trimmed files
### COUNCIL AREAS - Single Year File 
saveRDS(CA2019_pop_est_hist_2010 , 
        glue("{output_filepath}/CA2019_pop_est_{start}_{old}.rds"))

### COUNCIL AREAS - 5 Year age Group File
saveRDS(CA2019_pop_est_5y_hist_2010 , 
        glue("{output_filepath}/CA2019_pop_est_5year_agegroups_{start}_{old}.rds"))

# HEALTH BOARDS - Single Year File
saveRDS(HB2019_pop_est_hist_2010 , 
        glue("{output_filepath}/HB2019_pop_est_{start}_{old}.rds"))

### HEALTH BOARDS - 5 Year Age Group File
saveRDS(HB2019_pop_est_5y_hist_2010 , 
        glue("{output_filepath}/HB2019_pop_est_5year_agegroups_{start}_{old}.rds")) 
