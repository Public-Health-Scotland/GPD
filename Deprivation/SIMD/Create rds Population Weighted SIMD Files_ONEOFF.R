### 1 - Information ----

# Codename - Create R Population Weighted Deprivation Lookup Files
# Original Author - Calum Purdie
# Original Date - 19/06/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readr")
#
# Description - Creating .rds versions of all population weighted DataZone-SIMD lookup files.
#
# Approximate run time - <1 minute

# Set working directory
base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD", "3_Deprivation", "SIMD", "Lookup Files")
SIMD2004_filepath <- file.path(base_filepath, "SIMD 2004")
SIMD2006_filepath <- file.path(base_filepath, "SIMD 2006")
SIMD2009_filepath <- file.path(base_filepath, "SIMD 2009")
SIMD2012_filepath <- file.path(base_filepath, "SIMD 2012")
SIMD2016_filepath <- file.path(base_filepath, "SIMD 2016")
AllSIMD_filepath <- file.path(base_filepath, "All SIMD")


# Read in packages from library
library(tidyr)
library(dplyr)
library(readr)
library(haven)
library(sjlabelled)

### 2 - Read in csv files ----

R_data <- function(filepath, input_file, output_file){
  
  data <- read_csv(file.path(filepath, input_file))
  
  saveRDS(data, file.path(filepath, output_file))
  
}

DataZone2001_simd2004 <- R_data(SIMD2004_filepath, "DataZone2001_simd2004.csv", "DataZone2001_simd2004.rds")

DataZone2001_simd2006 <- R_data(SIMD2006_filepath, "DataZone2001_simd2006.csv", "DataZone2001_simd2006.rds")

DataZone2001_simd2009v2 <- R_data(SIMD2009_filepath, "DataZone2001_simd2009v2.csv", "DataZone2001_simd2009v2.rds")

DataZone2001_simd2012 <- R_data(SIMD2012_filepath, "DataZone2001_simd2012.csv", "DataZone2001_simd2012.rds")

DataZone2011_simd2016 <- R_data(SIMD2016_filepath, "DataZone2011_simd2016.csv", "DataZone2011_simd2016.rds")

DataZone2001_all_simd <- R_data(AllSIMD_filepath, "DataZone2001_all_simd.csv", "DataZone2001_all_simd.rds")


DataZone2001_domain_level_simd <- read_sav(file.path(AllSIMD_filepath, "DataZone2001_domain_level_simd.sav"), user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character)

saveRDS(DataZone2001_domain_level_simd, file.path(AllSIMD_filepath, "DataZone2001_domain_level_simd.rds"))
