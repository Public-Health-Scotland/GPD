### 1 - Information ----

# Codename - Correcting Codes for Data Zone 2011 Open Data Codes and Labels Files
# Data release - Updated Standard Geography Code Register
# Original Author - Calum Purdie
# Original Date - 02/09/2019
# Type - Preparation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("dplyr")
# install.packages("tidylog")
#
# Description - Updating Health Board and Council Area codes for Data Zone 2001
#               and 2011 NHSScotland open data codes and labels files
#
# Approximate run time - 10 seconds

library(dplyr)
library(tidylog)

od_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                         "Open Data (Non Health Topic)", "Data", "OD1700008 - Geography Codes")

### 2 - Data Zone 2011 file ----

# Read in Data Zone 2011 file from open data website

geo_names <- read.csv(file = "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/395476ab-0720-4740-be07-ff4467141352/download/geography_codes_and_labels_dz2011_19082019.csv", stringsAsFactors = F)

# Recode HB values for 2019 boundary change
# Create qualifier column for HB2014 and  set it to "r" for revised HB2014 codes

geo_names <- geo_names %>%
  mutate(HB2014 = recode(HB2014, 
                         "S08000021" = "S08000031", 
                         "S08000023" = "S08000032")) %>%
  mutate(HB2014QF = case_when(HB2014 == "S08000029" ~ "r", 
                              HB2014 == "S08000030" ~ "r", 
                              HB2014 == "S08000031" ~ "r", 
                              HB2014 == "S08000032" ~ "r"))

# Check changes have been applied

geo_names %>% count(HB2014, HB2014QF)  %>% print(n = Inf)

# Save in open data folder

write.csv(geo_names, file.path(od_filepath, "geography_codes_and_labels_DZ2011_02092019.csv"), row.names = FALSE, na = "")


### 3 - Data Zone 2001 File ----

# Read in Data Zone 2001 file from open data website

geo_names <- read.csv(file = "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/e92d19d4-ced7-40c8-b628-e28e4528fc41/download/geography_codes_and_labels_dz2001_19082019.csv", stringsAsFactors = F)

# Recode Glasgow City council area for 2019 boundary change
# Create qualifier column for CA2011 and  set it to "r" for revised CA2011 codes

geo_names <- geo_names %>%
  mutate(CA2011 = recode(CA2011, "S12000046" = "S12000049")) %>%
  mutate(CA2011QF = case_when(CA2011 == "S12000047" ~ "r", 
                              CA2011 == "S12000048" ~ "r", 
                              CA2011 == "S12000049" ~ "r", 
                              CA2011 == "S12000050" ~ "r"))

# Check changes have been applied

geo_names %>% count(CA2011, CA2011QF) %>% print(n = Inf)

# Save in open data folder

write.csv(geo_names, file.path(od_filepath, "geography_codes_and_labels_DZ2001_02092019.csv"), row.names = FALSE, na = "")
