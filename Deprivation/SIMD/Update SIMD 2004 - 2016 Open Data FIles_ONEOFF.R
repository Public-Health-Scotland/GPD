##########################################################
# Name of file: Update SIMD 2004 - 2016 Open Data Files
# Original author: Calum Purdie
# Original date: 02/04/2020
# Latest update author: Calum Purdie
# Latest update date: 02/04/2020
# Latest update description: Initial version
# Type of script: data preparation
# Written/run on: RStudio desktop
# Version of R that the script was most recently run on: R 3.5.1
# Description of content: To update column names for SIMD 2004-2016 open data.
# It is an one-off script and doesn't need to be run once the files are uploaded.
# Approximate run time: 30 seconds.
##########################################################

### 1 - Housekeeping ----

# Load libraries

library(magrittr)
library(dplyr)
library(data.table)
library(glue)
library(here)
library(ckanr)
library(tidylog)

# Set filepaths

od_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Publications", 
                    "Open Data (Non Health Topic)/Data/OD1700038 - SIMD")

# Set date and file to use

date <- strftime(Sys.Date(), format = "%d%m%Y")

# Set httr configuration for connecting to API

source(here::here(glue("Geography/Scottish Postcode Directory/", 
                       "Set httr configuration for API.R")))

# Set url and id

ckan <- src_ckan("https://www.opendata.nhs.scot")

simd04 <- "a97fca71-ebbb-4897-a611-88024a76ff21"
simd06 <- "6f871d03-d2af-4fe2-a615-d2d2ca76c3a5"
simd09 <- "d9738550-4cf9-428e-8453-c2aad463ff68"
simd12 <- "dd4b13d3-066b-4714-bb1f-730e1a1ee692"
simd16 <- "cadf715a-c365-4dcf-a6e0-acd7e3af21ec"


### SIMD 2004 ----

data <- dplyr::tbl(src = ckan$con, from = simd04) %>% 
  as_tibble() %>% 
  select(DZ2001, IZ2001, CA2011, HSCP2016, HB2014, 
         SIMD2004Rank, SIMD2004CountryDecile, SIMD2004CountryQuintile, 
         SIMD2004HB2014Decile, SIMD2004HB2014Quintile, SIMD2004HSCP2016Decile, 
         SIMD2004HSCP2016Quintile, SIMD2004CA2011Decile, SIMD2004CA2011Quintile, 
         SIMD2004Most15pc, SIMD2004Least15pc)

colnames(data) <- c("DataZone", "IntZone", "CA", "HSCP", "HB", "SIMD2004Rank", 
                    "SIMD2004CountryDecile", "SIMD2004CountryQuintile", 
                    "SIMD2004HBDecile", "SIMD2004HBQuintile", 
                    "SIMD2004HSCPDecile", "SIMD2004HSCPQuintile", 
                    "SIMD2004CADecile", "SIMD2004CAQuintile", 
                    "SIMD2004Most15pc", "SIMD2004Least15pc")

fwrite(data, glue("{od_filepath}/simd2004_{date}.csv"), na = "")



### SIMD 2006 ----

data <- dplyr::tbl(src = ckan$con, from = simd06) %>%  
  as_tibble() %>% 
  select(DZ2001, IZ2001, CA2011, HSCP2016, HB2014, 
         SIMD2006Rank, SIMD2006CountryDecile, SIMD2006CountryQuintile, 
         SIMD2006HB2014Decile, SIMD2006HB2014Quintile, SIMD2006HSCP2016Decile, 
         SIMD2006HSCP2016Quintile, SIMD2006CA2011Decile, SIMD2006CA2011Quintile, 
         SIMD2006Most15pc, SIMD2006Least15pc)

colnames(data) <- c("DataZone", "IntZone", "CA", "HSCP", "HB", "SIMD2006Rank", 
                    "SIMD2006CountryDecile", "SIMD2006CountryQuintile", 
                    "SIMD2006HBDecile", "SIMD2006HBQuintile", 
                    "SIMD2006HSCPDecile", "SIMD2006HSCPQuintile", 
                    "SIMD2006CADecile", "SIMD2006CAQuintile", 
                    "SIMD2006Most15pc", "SIMD2006Least15pc")

fwrite(data, glue("{od_filepath}/simd2006_{date}.csv"), na = "")



### SIMD 2009 ----

data <- dplyr::tbl(src = ckan$con, from = simd09) %>% 
  as_tibble() %>% 
  select(DZ2001, IZ2001, CA2011, HSCP2016, HB2014, 
         SIMD2009Rank, SIMD2009CountryDecile, SIMD2009CountryQuintile, 
         SIMD2009HB2014Decile, SIMD2009HB2014Quintile, SIMD2009HSCP2016Decile, 
         SIMD2009HSCP2016Quintile, SIMD2009CA2011Decile, SIMD2009CA2011Quintile, 
         SIMD2009Most15pc, SIMD2009Least15pc)

colnames(data) <- c("DataZone", "IntZone", "CA", "HSCP", "HB", "SIMD2009Rank", 
                    "SIMD2009CountryDecile", "SIMD2009CountryQuintile", 
                    "SIMD2009HBDecile", "SIMD2009HBQuintile", 
                    "SIMD2009HSCPDecile", "SIMD2009HSCPQuintile", 
                    "SIMD2009CADecile", "SIMD2009CAQuintile", 
                    "SIMD2009Most15pc", "SIMD2009Least15pc")

fwrite(data, glue("{od_filepath}/simd2009_{date}.csv"), na = "")



### SIMD 2012 ----

data <- dplyr::tbl(src = ckan$con, from = simd12) %>%  
  as_tibble() %>% 
  select(DZ2001, IZ2001, CA2011, HSCP2016, HB2014, 
         SIMD2012Rank, SIMD2012CountryDecile, SIMD2012CountryQuintile, 
         SIMD2012HB2014Decile, SIMD2012HB2014Quintile, SIMD2012HSCP2016Decile, 
         SIMD2012HSCP2016Quintile, SIMD2012CA2011Decile, SIMD2012CA2011Quintile, 
         SIMD2012Most15pc, SIMD2012Least15pc)

colnames(data) <- c("DataZone", "IntZone", "CA", "HSCP", "HB", "SIMD2012Rank", 
                    "SIMD2012CountryDecile", "SIMD2012CountryQuintile", 
                    "SIMD2012HBDecile", "SIMD2012HBQuintile", 
                    "SIMD2012HSCPDecile", "SIMD2012HSCPQuintile", 
                    "SIMD2012CADecile", "SIMD2012CAQuintile", 
                    "SIMD2012Most15pc", "SIMD2012Least15pc")

fwrite(data, glue("{od_filepath}/simd2012_{date}.csv"), na = "")



### SIMD 2016 ----

data <- dplyr::tbl(src = ckan$con, from = simd16) %>%  
  select(DZ2011, IZ2011, CA2011, HSCP2016, HB2014, 
         SIMD2016Rank, SIMD2016CountryDecile, SIMD2016CountryQuintile, 
         SIMD2016HB2014Decile, SIMD2016HB2014Quintile, SIMD2016HSCP2016Decile, 
         SIMD2016HSCP2016Quintile, SIMD2016CA2011Decile, SIMD2016CA2011Quintile, 
         SIMD2016Most15pc, SIMD2016Least15pc) %>% 
  as_tibble()

colnames(data) <- c("DataZone", "IntZone", "CA", "HSCP", "HB", "SIMD2016Rank", 
                    "SIMD2016CountryDecile", "SIMD2016CountryQuintile", 
                    "SIMD2016HBDecile", "SIMD2016HBQuintile", 
                    "SIMD2016HSCPDecile", "SIMD2016HSCPQuintile", 
                    "SIMD2016CADecile", "SIMD2016CAQuintile", 
                    "SIMD2016Most15pc", "SIMD2016Least15pc")

fwrite(data, glue("{od_filepath}/simd2016_{date}.csv"), na = "")
