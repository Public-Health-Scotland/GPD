#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# RStudio Workbench is strictly for use by Public Health Scotland staff and     
# authorised users only, and is governed by an <Acceptable Usage Policy>.
#
# This is a shared resource and is hosted on a pay-as-you-go cloud computing
# platform.  Your usage will incur direct financial cost to Public Health
# Scotland.  As such, please ensure
#
#   1. that this session is appropriately sized with the minimum number of CPUs
#      and memory required for the size and scale of your analysis;
#   2. the code you write in this script is optimal and only writes out the
#      data required, nothing more.
#   3. you close this session when not in use; idle sessions still cost PHS
#      money!
#
# For further guidance, please see <insert link>.
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


rm(list = ls())

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}

pacman::p_load( magrittr,tidyr,dplyr, tidylog,readr, glue, here, xfun)


# set Estimate start & end dates 
start <- "2023" #update
end <- "2024" #update
# Set filepath



# Set filepaths

#base_filepath <- glue("/data/geography", 
#                "/Population/Population Estimates") # if on Posit



base_filepath <- glue("/data/geography", "/Population/Population Estimates")

lookups_filepath <- glue("{base_filepath}/",  "Lookup Files/R Files")

datamart_filepath <- glue("/data/geography/Population/Population Datamart/Lookup Files/", 
                          "Other Geographies")


# Getting main script location for working directory
path_main_script_location = dirname(rstudioapi::getActiveDocumentContext()$path)

setwd(path_main_script_location)

#geography folder
gpd_base_path<-"/conf/linkage/output/lookups/Unicode/"

# spd
simd_path <- glue(gpd_base_path,  "Deprivation/")
spd_path <- glue(gpd_base_path,"Geography/Scottish Postcode Directory/" )

simd<- readRDS(glue(simd_path, "postcode_2025_2_simd2020v2.rds")) 


spd <- readRDS(glue(spd_path, "Scottish_Postcode_Directory_2025_2.rds")) 
data_zone_2022_spd <- spd  %>% 
  distinct(datazone2022, .keep_all = TRUE ) %>% 
  select (datazone2022,datazone2022name,
          intzone2022, intzone2022name,
          hb2019, hb2019name, hb2018, hb2014,
          hscp2019, hscp2019name, hscp2018, hscp2016,
          ca2019, ca2019name, ca2018, ca2011)

