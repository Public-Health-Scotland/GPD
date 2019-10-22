### 1 - Information ----

# Codename - Updating DZ and IZ Open Data Codes and Labels Files
# Data release - Updated Standard Geography Code Register
# Original Author - Calum Purdie
# Original Date - 19/08/2019
# Type - Preparation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("readr")
# install.packages("dplyr")
#
# Description - Updating Corrected Data Zone and Intermediate Zone names on
#               NHSScotland open data codes and labels files
#
# Approximate run time - 10 seconds

library(readr)
library(dplyr)

# Open Data filepath
od_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", "Open Data (Non Health Topic)", 
                         "Data", "OD1700008 - Geography Codes")

# Two Intermediate Zone names were updated in February and July 2019
# These updates were simply due to typo's and the codes are unchanged
# This code was used to update the files on the open data website

### 2 - DataZone2011 ----

# Read in current DataZone2011 file
DataZone2011_lookup <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/395476ab-0720-4740-be07-ff4467141352/download/geography_codes_and_labels_dz2011_01042019.csv", stringsAsFactors = F)

# Update the names for DZ2011 and IZ2011
DataZone2011_lookup <- DataZone2011_lookup %>% 
  mutate(DZ2011Name = gsub("Siverknowes", "Silverknowes", DZ2011Name), 
         DZ2011Name = gsub("Lochlash", "Lochalsh", DZ2011Name),
         IZ2011Name = gsub("Siverknowes", "Silverknowes", IZ2011Name), 
         IZ2011Name = gsub("Lochlash", "Lochalsh", IZ2011Name))

# Save the updated file in the Open Data folder
write_csv(DataZone2011_lookup, file.path(od_filepath, "geography_codes_and_labels_DZ2011_19082019.csv"))


### 3 - DataZone2001 ----

# Read in current DataZone2001 file

DataZone2001_lookup <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/e92d19d4-ced7-40c8-b628-e28e4528fc41/download/geography_codes_and_labels_dz2001_01042019.csv", stringsAsFactors = F)

# Update the names for IZ2001
DataZone2001_lookup <- DataZone2001_lookup %>% 
  mutate(IZ2001Name = gsub("Lochlash", "Lochalsh", IZ2001Name))

# Save the updated file in the Open Data folder
write_csv(DataZone2001_lookup, file.path(od_filepath, "geography_codes_and_labels_DZ2001_19082019.csv"))


### 4 - IntZone2011 ----

# Read in current DataZone2011 file
IntZone2011_lookup <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/e3e885cc-2530-4b3c-bead-9eda9782264f/download/geography_codes_and_labels_iz2011_01042019.csv", stringsAsFactors = F)

# Update the names for DZ2011, IZ2001 and IZ2011
IntZone2011_lookup <- IntZone2011_lookup %>% 
  mutate(IZ2011Name = gsub("Siverknowes", "Silverknowes", IZ2011Name), 
         IZ2011Name = gsub("Lochlash", "Lochalsh", IZ2011Name))

# Save the updated file in the Open Data folder
write_csv(IntZone2011_lookup, file.path(od_filepath, "geography_codes_and_labels_IZ2011_19082019.csv"))


### 5 - IntZone2011

# Read in current IntZone2001 file
IntZone2001_lookup <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/84f6061a-6f15-4e8f-a4f9-aeb96e142b83/download/geography_codes_and_labels_iz2001_01042019.csv", stringsAsFactors = F)

# Update the names for DZ2011, IZ2001 and IZ2011
IntZone2001_lookup <- IntZone2001_lookup %>% 
  mutate(IZ2001Name = gsub("Lochlash", "Lochalsh", IZ2001Name))

# Save the updated file in the Open Data folder
write_csv(IntZone2001_lookup, file.path(od_filepath, "geography_codes_and_labels_IZ2001_19082019.csv"))
