##########################################################
# Name of file: Create HB1995 POpulation Estimates Lookup
# Original author: Calum Purdie
# Original date: 02/04/2020
# Latest update author: Calum Purdie
# Latest update date: 02/04/2020
# Latest update description: Initial version
# Type of script: data preparation
# Written/run on: RStudio desktop
# Version of R that the script was most recently run on: R 3.5.1
# Description of content: Code for creating internal HB1995 population
# estimates file from open data
# Approximate run time: 30 seconds.
##########################################################


### 1 - Housekeeping ----

# Load libraries

library(magrittr)
library(dplyr)
library(data.table)
library(glue)
library(here)
library(tidylog)

# Set filepaths

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI")

data_filepath <- glue("{base_filepath}/Referencing & Standards/GPD/", 
                      "2_Population/Population Estimates/Lookup Files")

od_filepath <- glue("{base_filepath}/Publications/Open Data (Non Health Topic)/",
                    "Data/OD1700007 - Population Estimates")

# Set date and file to use

date <- strftime(Sys.Date(), format = "%d%m%Y")



### 2 - Read in Open Data File ----

hb1995_pop_est <- fread(glue("{data_filepath}/hb1995-pop-est.csv"))

# Update file for open data with new column names

hb1995_pop_est_od <- hb1995_pop_est %>% 
  rename(HB = HB1995, HBQF = HB1995QF)

# Save in open data folder

fwrite(hb1995_pop_est_od, glue("{od_filepath}/HB1995_pop_est_{date}.csv"))
