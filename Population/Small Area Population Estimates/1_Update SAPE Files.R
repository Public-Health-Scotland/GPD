### 1 - Information ----

# Codename - Update SAPE Files
# Data release - Small Area Population Estimates for 2011 Data Zones
# Original Author - Calum Purdie
# Original Date - 24/08/2018
# Updated - 05/11/2019
# Type - Updating
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("magrittr")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("readr)
# install.packages("tidylog")
# install.packages("jantior")
# install.packages("glue")
# install.packages("here")
# install.packages("ckanr")
#
# Description - Updating Small Area Population Estimates files 
#               (Data Zone and Int Zone) for yearly NRS release
#
# Approximate run time - 5 minutes

# Read in packages from library

library(magrittr)
library(tidyr)
library(dplyr)
library(readxl)
library(readr)
library(tidylog)
library(janitor)
library(glue)
library(here)
library(ckanr)

# Set filepaths

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", 
                           "Referencing & Standards", "GPD", "2_Population", 
                           "Small Area Population estimates")
data_filepath <- file.path(base_filepath, "Source Data")
lookup_filepath <- file.path(base_filepath, "Lookup Files", "R Files")
simd_filepath <- file.path("//Isdsf00d03", "cl-out", "lookups", "Unicode", 
                              "Deprivation")
od_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", 
                         "Open Data (Non Health Topic)", "Data", 
                         "OD1700007 - Population Estimates")

# Set date for open data filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")


### 2 - Read in Source Data for Males and Females ----


# Read in female data

f_2018 <- read_excel(glue("{data_filepath}/Datazone2011_2018_f.xlsx"), 
                     sheet = 1, range = "A6:CR6982") %>% 
  mutate(Year = 2018) %>%
  mutate(Sex = 'F')

# Read in male data

m_2018 <- read_excel(glue("{data_filepath}/Datazone2011_2018_m.xlsx"), 
                     sheet = 1, range = "A6:CR6982") %>% 
  mutate(Year = 2018) %>%
  mutate(Sex = 'M')

# Create list of the two data frames

mandf <- list(f_2018, m_2018)

# Remove unnecessary columns

mandf <- lapply(mandf, function(x) x[,-c(3,5)])

# Rename Columns
# Function changes each column name to the respective name listed in the 
# function, in order

ChangeNames <- function(x) {
  names(x) <- c("DataZone2011", "DataZone2011Name", "total_pop",  
                "age0", "age1", "age2", "age3", "age4", "age5", "age6", "age7", 
                "age8", "age9", "age10", "age11", "age12", "age13", "age14", 
                "age15", "age16", "age17", "age18", "age19", "age20", "age21", 
                "age22", "age23", "age24", "age25", "age26", "age27", "age28", 
                "age29", "age30", "age31", "age32", "age33", "age34", "age35", 
                "age36", "age37", "age38", "age39", "age40", "age41", "age42", 
                "age43", "age44", "age45", "age46", "age47", "age48", "age49",
                "age50", "age51", "age52", "age53", "age54", "age55", "age56", 
                "age57", "age58", "age59", "age60", "age61", "age62", "age63", 
                "age64", "age65", "age66", "age67", "age68", "age69", "age70", 
                "age71", "age72", "age73", "age74", "age75", "age76", "age77", 
                "age78", "age79", "age80", "age81", "age82", "age83", "age84", 
                "age85", "age86", "age87", "age88", "age89", "age90plus", 
                "Year", "Sex")
  return(x)
}

# Apply new column names

mandf <- lapply(mandf, ChangeNames)

# Combine the lists into a dataframe

mandf_2018 <- do.call(rbind.data.frame, mandf)



### 3 - Add to DataZone Files ----

### 3.1 - Match to DataZone2011_SIMD2016 ----

# Read in DZ2011_SIMD2016 file

DZ2011_simd2016 <- readRDS(glue("{simd_filepath}/DataZone2011_simd2016.rds"))

# Match to DataZone2011_SIMD2016 and HSCPLocality to get other geography level 
# and SIMD information
# Remove pop_2014 and rearrange variables

DZ2011_2018 <- mandf_2018 %>%
  left_join(DZ2011_simd2016, by = "DataZone2011") %>% 
  select(-pop_2014) %>% 
  select(Year, DataZone2011, DataZone2011Name, Sex, age0:age90plus, total_pop, 
         everything())



### 3.2 - Add to Previous Years File ----

# Read in previous years file
# UPDATE FILEPATH FOR LATEST FILE

# DZ2011_pop_est_2011_2017 <- readRDS(glue("{lookup_filepath}/", 
#                                          "DataZone2011_pop_est_2011_2017.rds"))

DZ2011_pop_est_2011_2017 <- readRDS(glue("{base_filepath}/Lookup Files/R Files/", 
                                         "Archive/", 
                                         "DataZone2011_pop_est_2011_2017.rds"))

### THIS IS ONLY REQUIRED ONCE ###
# Need to add in the updated 2019 geographies in the DZ2011_2018 and 
# DataZone2011_pop_est_2011_2017 files

# Use the Geography Codes and Names open data file to get the names
# First need to run the httr configuration script

source(here("Geography", "Scottish Postcode Directory", 
            "Set httr configuration for API.R"))

ckan <- src_ckan("https://www.opendata.nhs.scot")
res_id <- "395476ab-0720-4740-be07-ff4467141352"

geo_names <- dplyr::tbl(src = ckan$con, from = res_id) %>% 
  select(DZ2011, IZ2011Name, CA2011Name, HSCP2016Name, 
         HB2014Name) %>% 
  rename(DataZone2011 = DZ2011, IntZone2011Name = IZ2011Name, 
         CA2019Name = CA2011Name, HSCP2019Name = HSCP2016Name, 
         HB2019Name = HB2014Name) %>%  
  as_tibble()

# Update DZ2011_2018

DZ2011_2018 %<>% 
  left_join(geo_names)

# Update DZ2011_pop_est_2011_2017

DZ2011_pop_est_2011_2017 %<>%
  left_join(geo_names) %>%
  arrange(Year, DataZone2011, desc(Sex))

# Combine DataZone2011_pop_est_2011_2017 and DZ2011_2018

DZ2011_pop_est_2011_2018 <- DZ2011_pop_est_2011_2017 %>%
  full_join(DZ2011_2018) %>%
  select(Year, DataZone2011, DataZone2011Name, Sex, age0:IntZone2011, 
         IntZone2011Name, HB2019, HB2019Name, HB2018, HB2014, HSCP2019, 
         HSCP2019Name, HSCP2018, HSCP2016, CA2019, CA2019Name, CA2018, CA2011, 
         simd2016rank:simd2016_crime_rank) %>% 
  arrange(Year, DataZone2011, desc(Sex))

# Save as .RDS file

saveRDS(DZ2011_pop_est_2011_2018, glue("{lookup_filepath}/", 
                                       "DataZone2011_pop_est_2011_2018.rds"))



### 3.3 - Compute Age Groups and Add to 5 Year Age Group File ----

# Read in latest 5 year age group file

# DZ2011_pop_est_2011_2017 <- readRDS(
#   glue("{lookup_filepath}/DataZone2011_pop_est_5year_agegroups_2011_2017.rds"))

DZ2011_pop_est_2011_2017_5y <- readRDS(
  glue("{base_filepath}/Lookup Files/R Files/Archive/", 
       "DataZone2011_pop_est_5year_agegroups_2011_2017.rds"))

# Need to add in the updated 2019 geographies in the 
# DZ2011_pop_est_2011_2017_5y file
# Do this using the geo_names lookup file

DZ2011_pop_est_2011_2017_5y %<>%
  left_join(geo_names) %>%
  arrange(Year, DataZone2011, desc(Sex))

# Compute age groups for DZ2011_2017
# Remove age0 to age89 and join with DZ2011_pop_est_2011_2016_5year_agegroups
# Rearrange variables and sort by Year, DataZone2011 and Sex(descending)

DZ2011_pop_est_2011_2018_5y <- DZ2011_2018 %>%
  mutate(ageg04 = rowSums(.[5:9]), 
         ageg59 = rowSums(.[10:14]),
         ageg1014 = rowSums(.[15:19]),
         ageg1519 = rowSums(.[20:24]), 
         ageg2024 = rowSums(.[25:29]),
         ageg2529 = rowSums(.[30:34]), 
         ageg3034 = rowSums(.[35:39]),
         ageg3539 = rowSums(.[40:44]), 
         ageg4044 = rowSums(.[45:49]),
         ageg4549 = rowSums(.[50:54]), 
         ageg5054 = rowSums(.[55:59]),
         ageg5559 = rowSums(.[60:64]), 
         ageg6064 = rowSums(.[65:69]),
         ageg6569 = rowSums(.[70:74]), 
         ageg7074 = rowSums(.[75:79]),
         ageg7579 = rowSums(.[80:84]), 
         ageg8084 = rowSums(.[85:89]),
         ageg8589 = rowSums(.[90:94])) %>% 
  select(-c(age0:age89)) %>% 
  rename(ageg90plus = age90plus) %>% 
  full_join(DZ2011_pop_est_2011_2017_5y) %>% 
  select(Year, DataZone2011, DataZone2011Name, Sex, ageg04:ageg8589, ageg90plus, 
         total_pop, IntZone2011, IntZone2011Name, HB2019, HB2019Name, HB2018, 
         HB2014, HSCP2019, HSCP2019Name, HSCP2018, HSCP2016, CA2019, CA2019Name, 
         CA2018, CA2011, everything()) %>%
  arrange(Year, DataZone2011, desc(Sex))

# Save as .RDS file

saveRDS(DZ2011_pop_est_2011_2018_5y, 
        glue("{lookup_filepath}/", 
             "DataZone2011_pop_est_5year_agegroups_2011_2018.rds"))



### 4 - Create SAPE Files for 2011 Intermediate Zones ----

### 4.1 - Save IZ2011_pop_est_2011_2017 File ----

# Group by Year, IntZone2011 and Sex
# Sum ageg columns and total_pop across grouped variables
# Ungroup to turn the grouped data back to a normal dataframe
# Sort data by Year, IntZone2011 and Sex

IZ2011_pop_est_2011_2018 <- DZ2011_pop_est_2011_2018 %>%
  group_by(Year, IntZone2011, Sex) %>%
  summarise_at(vars(age0:total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(Year, IntZone2011, desc(Sex))

# Match on IntZone2011Name from open data codes and names file

iz_names <- read.csv(paste0("https://www.opendata.nhs.scot/dataset/", 
                            "9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/", 
                            "e3e885cc-2530-4b3c-bead-9eda9782264f/download/", 
                            "geography_codes_and_labels_iz2011_19082019.csv")) %>% 
  select(IZ2011, IZ2011Name) %>% 
  mutate_if(is.factor, as.character) %>% 
  rename(IntZone2011 = IZ2011, IntZone2011Name = IZ2011Name)

IZ2011_pop_est_2011_2018 %<>%
  left_join(iz_names) %>% 
  select(Year, IntZone2011, IntZone2011Name, Sex, age0:total_pop)

# Save file as .RDS

saveRDS(IZ2011_pop_est_2011_2018, 
        glue("{lookup_filepath}/IntZone2011_pop_est_2011_2018.rds"))



### 4.2 - Save IntZone2011_pop_est_5year_agegroups_2011_2017 File ----

# Group by Year, IntZone2011 and Sex
# Sum ageg columns and total_pop across grouped variables
# Ungroup to to turn the grouped data back to a normal dataframe
# Sort data by Year, IntZone2011 and Sex

IZ2011_pop_est_2011_2018_5y <- DZ2011_pop_est_2011_2018_5y %>%
  group_by(Year, IntZone2011, Sex) %>%
  summarise_at(vars(ageg04:ageg90plus, total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(Year, IntZone2011, desc(Sex))

# Match on IntZone2011Name from open data codes and names file

IZ2011_pop_est_2011_2018_5y %<>%
  left_join(iz_names) %>% 
  select(Year, IntZone2011, IntZone2011Name, Sex, ageg04:total_pop)

# Save file as .RDS
saveRDS(IZ2011_pop_est_2011_2018_5y, 
        glue("{lookup_filepath}/", 
             "IntZone2011_pop_est_5year_agegroups_2011_2018.rds"))



### 5 - Check Final Files ----

# Create check function

data_check <- function(input, column){
  
  # Check that the frequencies for each area is the same.
  # Check that the frequencies for each area is equal to the number of years of 
  # SAPE data x 2, i.e. for 12 years of data, would expect 24
  # Here we expect the value to be 16 for 8 years and both gender
  # UPDATE THIS FOR NEW RELEASE
  
  input %>% count({{column}} != 16) %>% print()
  
  # Check sums add up to total_pop
  # Calculate the sum for all the age columns and check to see if the sums add 
  # up to the total_pop
  # Filter for rows where the sums don't add up
  input %>% 
    mutate(sums = if_else(
      rowSums(select(., starts_with("age"))) - total_pop != 0, 1, 0)) %>% 
    select(sums) %>% 
    filter(sums != 0) %>% 
    print()
  
  # Check that the frequencies for each year are the same
  
  input %>% count(Year) %>% print()
  
  # Check that the frequencies for males and females is equal
  
  input %>% count(Sex) %>% print()
  
}

### 5.1 - Check DataZone Single Year Files ----

data_check(DZ2011_pop_est_2011_2018, "DataZone2011")


### 5.2 - Check DataZone 5 Year Age Group File ----

data_check(DZ2011_pop_est_2011_2018_5y, "DataZone2011")


### 5.3 - Check IntZone Single Year File ----

data_check(IZ2011_pop_est_2011_2018, "IntZone2011")


### 5.4 - Check IntZone 5 Year Age Group File ----

data_check(IZ2011_pop_est_2011_2018_5y, "IntZone2011")


### 5.5 - Check Scotland Totals ----

# Run full section of syntax to end
# Check all files have same Scotland total for all years
# Conatct GPD analyst if there are any issues

# Data Zones
# Create single year totals for DataZone
DZ_total <- DZ2011_pop_est_2011_2018 %>%
  group_by(Year) %>%
  summarise(DZ_total_pop = sum(total_pop)) %>%
  ungroup()

# Create 5 year age group totals for DataZone
DZ_5y_total <- DZ2011_pop_est_2011_2018_5y %>%
  group_by(Year) %>%
  summarise(DZ_5y_total_pop = sum(total_pop)) %>%
  ungroup()

# Intermediate Zones
# Create single year totals for IntZone
IZ_total <- IZ2011_pop_est_2011_2018 %>%
  group_by(Year) %>%
  summarise(IZ_total_pop = sum(total_pop)) %>%
  ungroup()

# Create 5 year age group totals for IntZone
IZ_5y_total <- IZ2011_pop_est_2011_2018_5y %>%
  group_by(Year) %>%
  summarise(IZ_5y_total_pop = sum(total_pop)) %>%
  ungroup()

# Match all files together
DZ_total %>%
  full_join(DZ_5y_total, by="Year") %>%
  full_join(IZ_total, by="Year") %>%
  full_join(IZ_5y_total, by="Year")



### 6 - Convert files to comply with PHI R style guide ----

# Use clean_names in earlier sections for future files

# Convert DataZone2011

DZ2011_lower <- readRDS(glue("{lookup_filepath}/", 
                             "DataZone2011_pop_est_2011_2018.rds")) %>%
  clean_names() %>%
  rename(datazone2011 = data_zone2011, datazone2011name = data_zone2011name, 
         intzone2011 = int_zone2011, intzone2011name = int_zone2011name)

saveRDS(DZ2011_lower, glue("{lookup_filepath}/", 
                           "DataZone2011_pop_est_2011_2018.rds"))


# Convert DataZone2011_5y

DZ2011_5y_lower <- readRDS(
  glue("{lookup_filepath}/", 
       "DataZone2011_pop_est_5year_agegroups_2011_2018.rds")) %>%
  clean_names() %>%
  rename(datazone2011 = data_zone2011, datazone2011name = data_zone2011name, 
         intzone2011 = int_zone2011, intzone2011name = int_zone2011name)

saveRDS(DZ2011_5y_lower, 
        glue("{lookup_filepath}/", 
             "DataZone2011_pop_est_5year_agegroups_2011_2018.rds"))

# Convert IntZone2011

IZZ2011_lower <- readRDS(glue("{lookup_filepath}/", 
                              "IntZone2011_pop_est_2011_2018.rds")) %>%
  clean_names() %>%
  rename(intzone2011 = int_zone2011, intzone2011name = int_zone2011name)

saveRDS(IZZ2011_lower, glue("{lookup_filepath}/", 
                            "IntZone2011_pop_est_2011_2018.rds"))


# Convert IntZone2011_5y

IZ2011_5y_lower <- readRDS(
  glue("{lookup_filepath}/", 
       "IntZone2011_pop_est_5year_agegroups_2011_2018.rds")) %>%
  clean_names() %>%
  rename(intzone2011 = int_zone2011, intzone2011name = int_zone2011name)

saveRDS(IZ2011_5y_lower, 
        glue("{lookup_filepath}/", 
             "IntZone2011_pop_est_5year_agegroups_2011_2018.rds"))




### 7 - Create Open Data 2011 DZ Population Estimates Files ----

### 7.1 - 2011 DataZone - Single Year of Age ----

# Read in DataZone2011_pop_est_2001_2018 and drop DataZone2011Name
DZ2011_pop_est_2011_2018 <- readRDS(
  glue("{lookup_filepath}/DataZone2011_pop_est_2011_2018.rds")) %>% 
  select(-c(intzone2011:simd2016_crime_rank)) %>% 
  rename(Year = year, DZ2011 = datazone2011, Sex = sex, AllAges = total_pop)

# Need to add in the rebased 2001 - 2010 DZ estimates for the open data file
# It was agreed to keep all estimates in one open data file for consistency
# Read in DataZone2011_pop_est_2011_2018.rds and select relevant columns

DZ2011_pop_est_2001_2010 <- readRDS(
  glue("{lookup_filepath}/DataZone2011_pop_est_2001_2010.rds")) %>% 
  select(-c(datazone2011name)) %>% 
  rename(Year = year, DZ2011 = datazone2011, Sex = sex, AllAges = total_pop)

# Add DZ2011_pop_est_2001_2010 and DZ2011_pop_est_2011_2018

DZ2011_pop_est_OD <- DZ2011_pop_est_2001_2010 %>% 
  bind_rows(DZ2011_pop_est_2011_2018)

# Capitalise Age for each age variable

names(DZ2011_pop_est_OD) <- gsub("age", "Age", names(DZ2011_pop_est_OD))



### 7.2 - Create Scotland Totals and Add to Single Year File ----

# Group by Year and Sex for all single year of age

Scot_total <- DZ2011_pop_est_OD %>%
  group_by(Year, Sex) %>%
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>%
  arrange(Year, desc(Sex))

# Combine DZ2011_pop_est and Scot_total together
# Recode Sex variable. Sort by Year, DataZone2011 and Sex(descending)
# Scotland total is at the end for each year so move this to start using 
# !is.na(DataZone2011)

DZ2011_pop_est_OD %<>%
  full_join(Scot_total) %>%
  mutate(Sex = recode(Sex, 'M' = 'Male', 'F' = 'Female')) %>% 
  arrange(Year, !is.na(DZ2011))


### 7.3 - Tidy DZ2011_pop_est Data ----

# Attach Scotland national code
# Create qualifier column for DZ2011 and set it to "d" for Scotland totals

DZ2011_pop_est_OD %<>%
  mutate(DZ2011 = if_else(is.na(DZ2011), "S92000003", DZ2011), 
         DZ2011QF = if_else(DZ2011 == "S92000003", "d", "")) %>%
  select(Year, DZ2011, DZ2011QF, Sex, AllAges, Age0:Age90plus)

# Save as csv

write_csv(DZ2011_pop_est_OD, glue("{od_filepath}/DZ2011-pop-est_{date}.csv"))




### 8 - Create Open Data 2011 IZ Population Estimates Files ----

### 8.1 - 2011 IntZone - Single Year of Age ----

# Read in IntZone2011_pop_est_2001_2018.rds and drop intzone2011name

IZ2011_pop_est_2011_2018 <- readRDS(glue("{lookup_filepath}/", 
                                         "IntZone2011_pop_est_2011_2018.rds")) %>% 
  rename(Year = year, IZ2011 = intzone2011, Sex = sex, AllAges = total_pop)

# Need to add in the rebased 2001 - 2010 DZ estimates for the open data file
# It was agreed to keep all estimates in one open data file for consistency
# Read in IntZone2011_pop_est_2001_2010.rds and select relevant columns

IZ2011_pop_est_2001_2010 <- readRDS(glue("{lookup_filepath}/", 
                                         "IntZone2011_pop_est_2001_2010.rds")) %>% 
  select(-intzone2011name) %>% 
  rename(Year = year, IZ2011 = intzone2011, Sex = sex, AllAges = total_pop)

# Add IZ2011_pop_est_2001_2010 and IZ2011_pop_est_2011_2018

IZ2011_pop_est_OD <- IZ2011_pop_est_2001_2010 %>% 
  bind_rows(IZ2011_pop_est_2011_2018)

# Capitalise Age for each age variable

names(IZ2011_pop_est_OD) <- gsub("age", "Age", names(IZ2011_pop_est_OD))



### 8.2 - Create Scotland Totals and Add to Single Year File ----

# Group by Year and Sex for all single year of age

Scot_total <- IZ2011_pop_est_OD %>%
  group_by(Year, Sex) %>%
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>%
  arrange(Year, desc(Sex))

# Combine IZ2011_pop_est_OD and Scot_total together
# Recode Sex variable. Sort by Year, IntZone2011 and Sex(descending order)
# Scotland total is at the end for each year so move this to start using 
# !is.na(DataZone2011)

IZ2011_pop_est_OD %<>%
  full_join(Scot_total) %>%
  mutate(Sex = recode(Sex, 'M' = 'Male', 'F' = 'Female')) %>% 
  arrange(Year, !is.na(IZ2011))


### 8.3 - Tidy IZ2011_pop_est Data ----

# Attach Scotland national code
# Create qualifier column for IZ2011 and set it to "d" for Scotland totals

DZ2011_pop_est_OD %<>%
  mutate(IZ2011 = if_else(is.na(IZ2011), "S92000003", IZ2011), 
         IZ2011QF = if_else(IZ2011QF == "S92000003", "d", "")) %>%
  select(Year, IZ2011, IZ2011QF, Sex, AllAges, Age0:Age90plus)

# Save as csv

write_csv(DZ2011_pop_est_OD, glue("{od_filepath}/DZ2011-pop-est_{date}.csv"))
