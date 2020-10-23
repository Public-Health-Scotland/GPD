##########################################################
# Update SIMD 2016 Within Area Shetland Ranks_ONE OFF
# Calum Purdie
# Original date 14/05/2020
# Latest update author - Calum Purdie
# Latest update date - 14/05/2020
# Latest update description - formatting code
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for correcting SIMD 2016 within area Shetland ranks
# Approximate run time - 10 seconds
##########################################################

### 1 Housekeeping ----

# Calum noticed that four data zones had incorrect values for SIMD quintiles
# and deciles for within health board, hscp and council area levels
# This code is to correct these for rds, csv and sav files for each resource


library(magrittr)
library(dplyr)
library(tidylog)
library(glue)
library(haven)
library(data.table)
library(tools)

# Set filepath

base_filepath <- glue("//freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD")
pop_filepath <- glue("{base_filepath}/2_Population/", 
                     "Small Area Population estimates/Lookup Files")
dep_filepath <- glue("{base_filepath}/3_Deprivation/Postcode Deprivation/", 
                     "Lookup Files")
simd_filepath <- glue("{base_filepath}/3_Deprivation/SIMD/Lookup Files/", 
                      "SIMD 2016")

# Set file names to use

dz_simd <- "DataZone2011_simd2016"
pc_simd <- "postcode_2019_2_simd2016"
pc_all_simd_carstairs <- "postcode_2019_2_all_simd_carstairs"
dz_pop_est <- "DataZone2011_pop_est_2011_2018"
dz_pop_est_5y <- "DataZone2011_pop_est_5year_agegroups_2011_2018"



### 2 Function ----

simd_change <- function(path, filename, ext, dz_col){
  
  if (ext == "csv"){
    
    data <- fread(glue("{path}/{filename}.{ext}"))
    
  } else if (ext == "rds"){ 
    
    data <- readRDS(glue("{path}/R Files/{filename}.{ext}"))
    
  } else if (ext == "sav"){ 
    
    data <- read_sav(glue("{path}/{filename}.{ext}"))
    
  } else {
    
    print("Define a correct file extension")
    
  }
  
  # data %<>%
  #   mutate(simd2016_HB2014_decile = case_when(!!as.name(dz_col) == "S01012411" ~ 2,
  #                                             !!as.name(dz_col) == "S01012389" ~ 9,
  #                                             !!as.name(dz_col) == "S01012390" ~ 5,
  #                                             !!as.name(dz_col) == "S01012396" ~ 6,
  #                                             TRUE ~ as.numeric(simd2016_HB2014_decile)),
  #          simd2016_HB2014_quintile = case_when(!!as.name(dz_col) == "S01012411" ~ 1,
  #                                               !!as.name(dz_col) == "S01012389" ~ 5,
  #                                               !!as.name(dz_col) == "S01012390" ~ 3,
  #                                               TRUE ~ as.numeric(simd2016_HB2014_quintile)),
  #          simd2016_HSCP2016_decile = case_when(!!as.name(dz_col) == "S01012411" ~ 2,
  #                                               !!as.name(dz_col) == "S01012389" ~ 9,
  #                                               !!as.name(dz_col) == "S01012390" ~ 5,
  #                                               !!as.name(dz_col) == "S01012396" ~ 6,
  #                                               TRUE ~ as.numeric(simd2016_HSCP2016_decile)),
  #          simd2016_HSCP2016_quintile = case_when(!!as.name(dz_col) == "S01012411" ~ 1,
  #                                                 !!as.name(dz_col) == "S01012389" ~ 5,
  #                                                 !!as.name(dz_col) == "S01012390" ~ 3,
  #                                                 TRUE ~ as.numeric(simd2016_HSCP2016_quintile)),
  #          simd2016_CA2011_decile = case_when(!!as.name(dz_col) == "S01012411" ~ 2,
  #                                             !!as.name(dz_col) == "S01012389" ~ 9,
  #                                             !!as.name(dz_col) == "S01012390" ~ 5,
  #                                             !!as.name(dz_col) == "S01012396" ~ 6,
  #                                             TRUE ~ as.numeric(simd2016_CA2011_decile)),
  #          simd2016_CA2011_quintile = case_when(!!as.name(dz_col) == "S01012411" ~ 1,
  #                                               !!as.name(dz_col) == "S01012389" ~ 5,
  #                                               !!as.name(dz_col) == "S01012390" ~ 3,
  #                                               TRUE ~ as.numeric(simd2016_CA2011_quintile)))
  
  
  data %<>%
    mutate(simd2016_hb2014_decile = case_when(!!as.name(dz_col) == "S01012411" ~ 2,
                                              !!as.name(dz_col) == "S01012389" ~ 9,
                                              !!as.name(dz_col) == "S01012390" ~ 5,
                                              !!as.name(dz_col) == "S01012396" ~ 6,
                                              TRUE ~ as.numeric(simd2016_hb2014_decile)),
           simd2016_hb2014_quintile = case_when(!!as.name(dz_col) == "S01012411" ~ 1,
                                                !!as.name(dz_col) == "S01012389" ~ 5,
                                                !!as.name(dz_col) == "S01012390" ~ 3,
                                                TRUE ~ as.numeric(simd2016_hb2014_quintile)),
           simd2016_hscp2016_decile = case_when(!!as.name(dz_col) == "S01012411" ~ 2,
                                                !!as.name(dz_col) == "S01012389" ~ 9,
                                                !!as.name(dz_col) == "S01012390" ~ 5,
                                                !!as.name(dz_col) == "S01012396" ~ 6,
                                                TRUE ~ as.numeric(simd2016_hscp2016_decile)),
           simd2016_hscp2016_quintile = case_when(!!as.name(dz_col) == "S01012411" ~ 1,
                                                  !!as.name(dz_col) == "S01012389" ~ 5,
                                                  !!as.name(dz_col) == "S01012390" ~ 3,
                                                  TRUE ~ as.numeric(simd2016_hscp2016_quintile)),
           simd2016_ca2011_decile = case_when(!!as.name(dz_col) == "S01012411" ~ 2,
                                              !!as.name(dz_col) == "S01012389" ~ 9,
                                              !!as.name(dz_col) == "S01012390" ~ 5,
                                              !!as.name(dz_col) == "S01012396" ~ 6,
                                              TRUE ~ as.numeric(simd2016_ca2011_decile)),
           simd2016_ca2011_quintile = case_when(!!as.name(dz_col) == "S01012411" ~ 1,
                                                !!as.name(dz_col) == "S01012389" ~ 5,
                                                !!as.name(dz_col) == "S01012390" ~ 3,
                                                TRUE ~ as.numeric(simd2016_ca2011_quintile)))

  # Format columns to be same data type
  # Using as.integer above produced an error
  
  data %<>%
    mutate_at(vars(dplyr::contains("_quintile")), as.integer) %>% 
    mutate_at(vars(dplyr::contains("_decile")), as.integer)
  
  if (ext == "csv"){
    
    fwrite(data, glue("{path}/{filename}.{ext}"))
    
  } else if (ext == "rds"){ 
    
    saveRDS(data, glue("{path}/R Files/{filename}.{ext}"))
    
  } else if (ext == "sav"){ 
    
    write_sav(data, glue("{path}/{filename}.{ext}"))
    
  } else {
    
    print("Define a correct file extension")
    
  }
  
  
}


### 3 DataZone2011_SIMD2016 ----

# SPSS

simd_change(path = simd_filepath, filename = dz_simd, ext = "sav", 
            dz_col = "DataZone2011")

# CSV

simd_change(path = simd_filepath, filename = dz_simd, ext = "csv", 
            dz_col = "DataZone2011")

# R

simd_change(path = simd_filepath, filename = dz_simd, ext = "rds", 
            dz_col = "DataZone2011")



### 4 postcode_2019_2_simd2016 ----

# SPSS

simd_change(path = dep_filepath, filename = pc_simd, ext = "sav", 
            dz_col = "DataZone2011")

# CSV

simd_change(path = dep_filepath, filename = pc_simd, ext = "csv", 
            dz_col = "DataZone2011")

# R

simd_change(path = dep_filepath, filename = pc_simd, ext = "rds", 
            dz_col = "DataZone2011")



### 5 postcode_2019_2_all_simd_carstairs ----

# SPSS

simd_change(path = dep_filepath, filename = pc_all_simd_carstairs, ext = "sav", 
            dz_col = "DataZone2011_simd2016")

# CSV

simd_change(path = dep_filepath, filename = pc_all_simd_carstairs, ext = "csv", 
            dz_col = "DataZone2011_simd2016")

# R

simd_change(path = dep_filepath, filename = pc_all_simd_carstairs, ext = "rds", 
            dz_col = "datazone2011_simd2016")



### 6 DataZone2011_pop_est_2011_2018 ----

# SPSS

simd_change(path = pop_filepath, filename = dz_pop_est, ext = "sav", 
            dz_col = "DataZone2011")

# R

simd_change(path = pop_filepath, filename = dz_pop_est, ext = "rds", 
            dz_col = "datazone2011")



### 7 DataZone2011_pop_est_5year_agegroups_2011_2018 ----

# SPSS

simd_change(path = pop_filepath, filename = dz_pop_est_5y, ext = "sav", 
            dz_col = "DataZone2011")

# R

simd_change(path = pop_filepath, filename = dz_pop_est_5y, ext = "rds", 
            dz_col = "datazone2011")
