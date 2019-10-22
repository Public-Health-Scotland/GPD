### 1 - Information ----

# Codename - Time Series Data for 2011 Data Zones
# Data release - 2001-2010 Small Area Population Estimates for 2011 Data Zones
# Original Author - Calum Purdie
# Original Date - 27/08/2019
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("readr")
#
# Description - Creating time series SAPE for 2011 Data Zones from 2001-2018.
# Approximate run time - 1 minutes

# Read in packages from library

library(tidyr)
library(dplyr)
library(readxl)
library(readr)
library(tidylog)
library(haven)
library(sjlabelled)
library(janitor)

# set filepath

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", "GPD")
pop_filepath <- file.path(base_filepath, "2_Population", "Small Area Population estimates")
data_filepath <- file.path(pop_filepath, "Source Data", "Data Zone 2011 2001-2010")
lookup_filepath <- file.path(pop_filepath, "Lookup Files", "R Files")
geo_filepath <- file.path(base_filepath, "1_Geography", "HSCPs and Localities", "Lookup Files", "Locality")
open_data_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Publications", "Open Data (Non Health Topic)", "Data", "OD1700007 - Population Estimates")

### 2 - Read in data ----

filelist_f <- list.files(data_filepath, pattern = "f.*\\.xlsx")
filelist_m <- list.files(data_filepath, pattern = "m.*\\.xlsx")

# Create a list to populate with xlsx data (if you want to bind all the rows together)

allxlsx.files_f <- list()  
allxlsx.files_m <- list()


### 2.1 - Female Data ----

# Initialise the value
count <- 1
year <- 2001

for (file in filelist_f) {
  dat <- read_excel(file.path(data_filepath, file), sheet = 1, range = "A6:CR6982") %>% 
    select(-c(CouncilArea2011Name, ...5)) %>%
    mutate(Year = year) %>% 
    mutate(Sex = 'F')
    
    allxlsx.files_f[[count]] <- dat # creat a list of rows from xls files
    count <- count + 1
    year <- year + 1
}

### 2.2 - Male Data ----

# Initialise the value
count <- 1
year <- 2001

for (file in filelist_m) {
  dat <- read_excel(file.path(data_filepath, file), sheet = 1, range = "A6:CR6982") %>% 
    select(-c(CouncilArea2011Name, ...5)) %>%
    mutate(Year = year) %>% 
    mutate(Sex = 'M')
  
  allxlsx.files_m[[count]] <- dat # creat a list of rows from xls files
  count <- count + 1
  year <- year + 1
}


### 3 - Add Files Together ----

### 3.1 - Combine the Lists Into a Dataframe ----

allfiles_f <- do.call(bind_rows, allxlsx.files_f)
allfiles_m <- do.call(bind_rows, allxlsx.files_m)

# Combine all files together

allfiles <- bind_rows(allfiles_m, allfiles_f)

# Rename variables

names(allfiles) <- c("DataZone2011", "DataZone2011Name", "total_pop",  
              "age0", "age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", 
              "age10", "age11", "age12", "age13", "age14", "age15", "age16", "age17", "age18", "age19",
              "age20", "age21", "age22", "age23", "age24", "age25", "age26", "age27", "age28", "age29",
              "age30", "age31", "age32", "age33", "age34", "age35", "age36", "age37", "age38", "age39",
              "age40", "age41", "age42", "age43", "age44", "age45", "age46", "age47", "age48", "age49",
              "age50", "age51", "age52", "age53", "age54", "age55", "age56", "age57", "age58", "age59",
              "age60", "age61", "age62", "age63", "age64", "age65", "age66", "age67", "age68", "age69",
              "age70", "age71", "age72", "age73", "age74", "age75", "age76", "age77", "age78", "age79",
              "age80", "age81", "age82", "age83", "age84", "age85", "age86", "age87", "age88", "age89", 
              "age90plus", "Year", "Sex")

# Arrange the columns and order

DZ2011_pop_est_2001_2010 <- allfiles %>% 
  arrange(Year, DataZone2011, desc(Sex)) %>% 
  select(Year, DataZone2011, DataZone2011Name, Sex, age0:age90plus, total_pop)


# Save file as .RDS
saveRDS(DZ2011_pop_est_2001_2010, file.path(lookup_filepath, "DataZone2011_pop_est_2001_2010.rds"))



### 3.2 - Compute Age Groups and save DataZone2011_pop_est_5year_agegroups_2001_2010 File ----

DZ2011_pop_est_5year_agegroups_2001_2010 <- DZ2011_pop_est_2001_2010 %>%
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
  select(Year, DataZone2011, DataZone2011Name, Sex, ageg04:ageg8589, ageg90plus, total_pop)

# Save file as .RDS
saveRDS(DZ2011_pop_est_5year_agegroups_2001_2010, file.path(lookup_filepath, "DataZone2011_pop_est_5year_agegroups_2001_2010.rds"))



### 4 - Create SAPE Files for 2011 Intermediate Zones ----

# Create Data Zone and Int Zone lookup and match onto both files
# Use the Geography Codes and Names open data file to get the names
# read_csv producing a timeout error when Calum tried to run it so best using read.csv

geo_names <- read.csv(file = "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/395476ab-0720-4740-be07-ff4467141352/download/geography_codes_and_labels_dz2011_01042019.csv") %>% 
  select(DZ2011, IZ2011, IZ2011Name) %>% 
  rename(DataZone2011 = DZ2011, IntZone2011 = IZ2011, IntZone2011Name = IZ2011Name) %>% 
  mutate_if(is.factor, as.character)

# Join the name columns onto the SPD by DataZone2011

DZ2011_pop_est_2001_2010 <- DZ2011_pop_est_2001_2010 %>% 
  left_join(geo_names)

DZ2011_pop_est_5year_agegroups_2001_2010 <- DZ2011_pop_est_5year_agegroups_2001_2010 %>% 
  left_join(geo_names)


### 4.1 - Save IZ2011_pop_est_2001_2010 File ----

# Group by Year, IntZone2011 and Sex
# Sum ageg columns and total_pop across grouped variables
# Ungroup to turn the grouped data back to a normal dataframe
# Sort data by Year, IntZone2011 and Sex
IZ2011_pop_est_2001_2010 <- DZ2011_pop_est_2001_2010 %>%
  group_by(Year, IntZone2011, IntZone2011Name, Sex) %>%
  summarise_at(vars(age0:total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(Year, IntZone2011, desc(Sex))

# Save file as .RDS
saveRDS(IZ2011_pop_est_2001_2010, file.path(lookup_filepath, "IntZone2011_pop_est_2001_2010.rds"))



### 4.2 - Save IntZone2011_pop_est_5year_agegroups_2001_2010 File ----

# Group by Year, IntZone2011 and Sex
# Sum ageg columns and total_pop across grouped variables
# Ungroup to to turn the grouped data back to a normal dataframe
# Sort data by Year, IntZone2011 and Sex
IZ2011_pop_est_5year_agegroups_2001_2010 <- DZ2011_pop_est_5year_agegroups_2001_2010 %>%
  group_by(Year, IntZone2011, IntZone2011Name, Sex) %>%
  summarise_at(vars(ageg04:ageg90plus, total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(Year, IntZone2011, desc(Sex))

# Save file as .RDS
saveRDS(IZ2011_pop_est_5year_agegroups_2001_2010, file.path(lookup_filepath, "IntZone2011_pop_est_5year_agegroups_2001_2010.rds"))


### 4.3 - Remove IntZone columns from DataZone files ----

DZ2011_pop_est_2001_2010 <- DZ2011_pop_est_2001_2010 %>% 
  select(-c(IntZone2011, IntZone2011Name))

DZ2011_pop_est_5year_agegroups_2001_2010 <- DZ2011_pop_est_5year_agegroups_2001_2010  %>% 
  select(-c(IntZone2011, IntZone2011Name))

### 5 - Check Final Files ----

# Check that the frequencies for each area is the same.
# Check that the frequencies for each area is equal to the number of years of SAPE data x 2 
# i.e. for 12 years of data, would expect 24
# Here we expect the value to be 14 for 7 years and both gender - UPDATE THIS FOR NEW RELEASE

# Create check function

data_check <- function(input, column){
  
  
  # Check that the frequencies for each datazone is the same.
  # Check that the frequencies for each datazone is equal to the number of years of SAPE data x 2 
  # i.e. for 12 years of data, would expect 24
  # Here we expect the value to be 14 for 7 years and both gender - UPDATE THIS FOR NEW RELEASE
  
  input %>% count({{column}} != 20) %>% print()
  
  # Check sums add up to total_pop
  # Calculate the sum for all the age columns and check to see if the sums add up to the total_pop
  # Filter for rows where the sums don't add up
  input %>% 
    mutate(sums = ifelse(rowSums(select(., starts_with("age"))) - total_pop != 0, 1, 0)) %>% 
    select(sums) %>% 
    filter(sums != 0) %>% 
    print()
  
  # Check that the frequencies for each year are the same
  
  input %>% count(Year) %>% print()
  
  # Check that the frequencies for males and females is equal
  
  input %>% count(Sex) %>% print()
  
}

### 5.1 - Check DataZone Single Year Files ----

data_check(DZ2011_pop_est_2001_2010, "DataZone2011")


### 5.2 - Check DataZone 5 Year Age Group File ----

data_check(DZ2011_pop_est_5year_agegroups_2001_2010, "DataZone2011")


### 5.3 - Check IntZone Single Year File ----

data_check(IZ2011_pop_est_2001_2010, "IntZone2011")


### 5.4 - Check IntZone 5 Year Age Group File ----

data_check(IZ2011_pop_est_5year_agegroups_2001_2010, "IntZone2011")


### 5.5 - Check Scotland Totals ----

# Run full section of syntax to end
# Check all files have same Scotland total for all years
# Conatct GPD analyst if there are any issues

# Data Zones
# Create single year totals for DataZone
DZ_total <- DZ2011_pop_est_2001_2010 %>%
  group_by(Year) %>%
  summarise(DZ_total_pop = sum(total_pop)) %>%
  ungroup()

# Create 5 year age group totals for DataZone
DZ_5y_total <- DZ2011_pop_est_5year_agegroups_2001_2010 %>%
  group_by(Year) %>%
  summarise(DZ_5y_total_pop = sum(total_pop)) %>%
  ungroup()

# Intermediate Zones
# Create single year totals for IntZone
IZ_total <- IZ2011_pop_est_2001_2010 %>%
  group_by(Year) %>%
  summarise(IZ_total_pop = sum(total_pop)) %>%
  ungroup()

# Create 5 year age group totals for IntZone
IZ_5y_total <- IZ2011_pop_est_5year_agegroups_2001_2010 %>%
  group_by(Year) %>%
  summarise(IZ_5y_total_pop = sum(total_pop)) %>%
  ungroup()

# Match all files together
DZ_total %>%
  full_join(DZ_5y_total, by="Year") %>%
  full_join(IZ_total, by="Year") %>%
  full_join(IZ_5y_total, by="Year")




### 6 - Compare R and SPSS Output ----

SPSS_filepath <- file.path(pop_filepath, "Lookup Files")

# Comparison Function for DZ

compare_DZ <- function(SPSS, R){
  
  SPSS_file <- read_sav(file.path(SPSS_filepath, SPSS), user_na=F) %>%
    zap_formats() %>%
    zap_widths() %>%
    remove_all_labels() %>% 
    mutate_if(is.factor, as.character)

    R_file <- readRDS(file.path(lookup_filepath, R)) %>%  
    select(-DataZone2011Name)
  
  print(all_equal(R_file, SPSS_file))
  
}

DataZone <- compare_DZ("DataZone2011_pop_est_2001_2010.sav", "DataZone2011_pop_est_2001_2010.rds")
DataZone_5y <- compare_DZ("DataZone2011_pop_est_5year_agegroups_2001_2010.sav", "DataZone2011_pop_est_5year_agegroups_2001_2010.rds")

# Comparison Function for IZ

compare_IZ <- function(SPSS, R){
  
  SPSS_file <- read_sav(file.path(SPSS_filepath, SPSS), user_na=F) %>%
    zap_formats() %>%
    zap_widths() %>%
    remove_all_labels() %>% 
    mutate_if(is.factor, as.character)
  
  R_file <- readRDS(file.path(lookup_filepath, R)) %>% 
    select(-IntZone2011Name)
  
  print(all_equal(R_file, SPSS_file))
  
}

IntZone <- compare_IZ("IntZone2011_pop_est_2001_2010.sav", "IntZone2011_pop_est_2001_2010.rds")
IntZone_5y <- compare_IZ("IntZone2011_pop_est_5year_agegroups_2001_2010.sav", "IntZone2011_pop_est_5year_agegroups_2001_2010.rds")




### 7 - Convert files to comply with PHI R style guide ----

# Convert DataZone2011

DZ2011_lower <- readRDS(file.path(lookup_filepath, "DataZone2011_pop_est_2001_2010.rds")) %>%
  clean_names() %>%
  rename(datazone2011 = data_zone2011, 
         datazone2011name = data_zone2011name)

saveRDS(DZ2011_lower, file.path(lookup_filepath, "DataZone2011_pop_est_2001_2010.rds"))


# Convert DataZone2011_5y

DZ2011_5y_lower <- readRDS(file.path(lookup_filepath, "DataZone2011_pop_est_5year_agegroups_2001_2010.rds")) %>%
  clean_names() %>%
  rename(datazone2011 = data_zone2011, 
         datazone2011name = data_zone2011name)

saveRDS(DZ2011_5y_lower, file.path(lookup_filepath, "DataZone2011_pop_est_5year_agegroups_2001_2010.rds"))

# Convert IntZone2011

IZZ2011_lower <- readRDS(file.path(lookup_filepath, "IntZone2011_pop_est_2001_2010.rds")) %>%
  clean_names() %>%
  rename(intzone2011 = int_zone2011, 
         intzone2011name = int_zone2011name)

saveRDS(IZZ2011_lower, file.path(lookup_filepath, "IntZone2011_pop_est_2001_2010.rds"))


# Convert IntZone2011_5y

IZ2011_5y_lower <- readRDS(file.path(lookup_filepath, "IntZone2011_pop_est_5year_agegroups_2001_2010.rds")) %>%
  clean_names() %>%
  rename(intzone2011 = int_zone2011, 
         intzone2011name = int_zone2011name)

saveRDS(IZ2011_5y_lower, file.path(lookup_filepath, "IntZone2011_pop_est_5year_agegroups_2001_2010.rds"))




### 8 - Create Open Data 2011 DZ Population Estimates Files ----

### 8.1 - 2011 DataZone - Single Year of Age ----

# Read in DataZone2011_pop_est_2001_2010.rds and drop DataZone2011Name
DZ2011_pop_est_2001_2010 <- readRDS(file.path(lookup_filepath, "DataZone2011_pop_est_2001_2010.rds")) %>% 
  select(-datazone2011name) %>% 
  rename(Year = year, DZ2011 = datazone2011, Sex = sex, AllAges = total_pop)

# We have decided to keep 2001-2010 data in the same file for 2011 onwards.
# This is because the open data title can be too long and not show completely  
# to the users if specifying the years on CKAN. 
# For better user experience, GPD and open data team have agreed on this.
# Need to add in the 2011 - 2018 DZ estimates for the open data file
# Read in DataZone2011_pop_est_2011_2018.rds and select relevant columns
DZ2011_pop_est_2011_2018 <- readRDS(file.path(lookup_filepath, "DataZone2011_pop_est_2011_2018.rds")) %>% 
  select(-c(intzone2011:simd2016_crime_rank)) %>% 
  rename(Year = year, DZ2011 = datazone2011, Sex = sex, AllAges = total_pop)

# Add DZ2011_pop_est_2001_2010 and DZ2011_pop_est_2011_2018
DZ2011_pop_est_OD <- DZ2011_pop_est_2001_2010 %>% 
  bind_rows(DZ2011_pop_est_2011_2018)
  
# Capitalise Age for each Age variable
names(DZ2011_pop_est_OD) <- gsub("age", "Age", names(DZ2011_pop_est_OD))

### 8.2 - Create Scotland Totals and Add to Single Year File ----

# Group by Year and Sex for all single year of age
Scot_total <- DZ2011_pop_est_OD %>%
  group_by(Year, Sex) %>%
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>%
  arrange(Year, desc(Sex))

# Combine DZ2011_pop_est and Scot_total together
# Recode Sex variable. Sort by Year, DataZone2011 and Sex(descending)
# Scotland total is at the end for each year so move this to start using !is.na(DataZone2011)
DZ2011_pop_est_OD <- DZ2011_pop_est_OD %>%
  full_join(Scot_total) %>%
  mutate(Sex = recode(Sex, 'M' = 'Male', 'F' = 'Female')) %>% 
  arrange(Year, !is.na(DZ2011))


### 8.3 - Tidy DZ2011_pop_est Data ----

# Attach Scotland national code
DZ2011_pop_est_OD$DZ2011[is.na(DZ2011_pop_est_OD$DZ2011)] <- 'S92000003'

# Rename DataZone2011
# Create qualifier column for DZ2011 and set it to "d" for Scotland totals
DZ2011_pop_est_OD <- DZ2011_pop_est_OD %>%
  mutate(DZ2011QF = ifelse(DZ2011 == "S92000003", "d", "")) %>%
  select(Year, DZ2011, DZ2011QF, Sex, AllAges, Age0:Age90plus)

# Save as csv - UPDATE FILE PATH AND DATE
write_csv(DZ2011_pop_est_OD, file.path(open_data_filepath, "DZ2011-pop-est_30082019.csv"))




### 9 - Create Open Data 2011 IZ Population Estimates Files ----

### 9.1 - 2011 IntZone - Single Year of Age ----

# Read in IntZone2011_pop_est_2001_2010.rds and drop DataZone2011Name
IZ2011_pop_est_2001_2010 <- readRDS(file.path(lookup_filepath, "IntZone2011_pop_est_2001_2010.rds")) %>% 
  select(-intzone2011name) %>% 
  rename(Year = year, IZ2011 = intzone2011, Sex = sex, AllAges = total_pop)

# Need to add in the 2011 - 2018 DZ estimates for the open data file
# Read in DataZone2011_pop_est_2011_2018.rds and select relevant columns
IZ2011_pop_est_2011_2018 <- readRDS(file.path(lookup_filepath, "IntZone2011_pop_est_2011_2018.rds")) %>% 
  rename(Year = year, IZ2011 = intzone2011, Sex = sex, AllAges = total_pop)

# Add IZ2011_pop_est_2001_2010 and IZ2011_pop_est_2011_2018
IZ2011_pop_est_OD <- IZ2011_pop_est_2001_2010 %>% 
  bind_rows(IZ2011_pop_est_2011_2018)

# Capitalise Age for each Age variable
names(IZ2011_pop_est_OD) <- gsub("age", "Age", names(IZ2011_pop_est_OD))

### 9.2 - Create Scotland Totals and Add to Single Year File ----

# Group by Year and Sex for all single year of age
Scot_total <- IZ2011_pop_est_OD %>%
  group_by(Year, Sex) %>%
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>%
  arrange(Year, desc(Sex))

# Combine IZ2011_pop_est_OD and Scot_total together
# Recode Sex variable. Sort by Year, IntZone2011 and Sex(descending order)
# Scotland total is at the end for each year so move this to start using !is.na(DataZone2011)
IZ2011_pop_est_OD <- IZ2011_pop_est_OD %>%
  full_join(Scot_total) %>%
  mutate(Sex = recode(Sex, 'M' = 'Male', 'F' = 'Female')) %>% 
  arrange(Year, !is.na(IZ2011))

### 9.3 - Tidy IZ2011_pop_est Data ----

# Attach Scotland national code
IZ2011_pop_est_OD$IZ2011[is.na(IZ2011_pop_est_OD$IZ2011)] <- 'S92000003'

# Rename IntZone2011
# Create qualifier column for IZ2011 and set it to "d" for Scotland totals
IZ2011_pop_est_OD <- IZ2011_pop_est_OD %>%
  mutate(IZ2011QF = ifelse(IZ2011 == "S92000003", "d", "")) %>%
  select(Year, IZ2011, IZ2011QF, Sex, AllAges, Age0:Age90plus)

# Save as csv - UPDATE FILE PATH AND DATE
write_csv(IZ2011_pop_est_OD, file.path(open_data_filepath, "IZ2011-pop-est_30082019.csv"))
