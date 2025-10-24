##########################################################
# Update SAPE Files
# Calum Purdie EDITED / AMENDED BY MR
# Original date 24/08/2018
# Latest update author - Iain MacKinnon
# Latest update date - 01/09/2022
# Latest update description 
# ADDED HAVEN AND SPSS WRITE OUTS HERE TOO IN 2021
# Rewrite section 2 upload of spreadsheets in 2022
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Updating Small Area Population Estimates files (Data Zone and Int Zone) for 
# yearly NRS release
# Approximate run time - 2021 UPDATE 1 minute - NOT DURING 2020 CONNECTIVITY FORM HOME MAKES IT MUCH LONGER

##########################################################


### 1 - Housekeeping ----

# Read in packages from library
#install.packages("haven")
library(magrittr)
library(haven)  #ADDED IN 2021 BY MR
library(tidyr)
library(dplyr)
library(readxl)
library(tidylog)
library(janitor)
library(glue)
library(data.table)
library(readr)

           
# Set filepaths

base_filepath <- glue("//data/geography")
data_filepath <- glue("{base_filepath}/Population/Small Area Population estimates/Source Data")
lookup_filepath <- glue("{base_filepath}/Population/Small Area Population estimates/Lookup Files/R Files")
codes_names_filepath <- glue("/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Codes and Names") 
simd2020v2_filepath <- glue("{base_filepath}/Deprivation/SIMD/Lookup Files/SIMD 2020")
simd2016_filepath <- glue("{base_filepath}/Deprivation/SIMD/Lookup Files/SIMD 2016/R Files")

# Set date for open data filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")

# Set years

new <- 2022
old <- 2021

# Set datasets to use
new_sape_file        <- glue("sape-{new}.xlsx")
#female_pop    <- glue("sape-{new}-female")
#male_pop <- glue("sape-{new}-male")
datazone_simd2016    <- glue("DataZone2011_simd2016")
datazone_simd2020v2  <- glue("DataZone2011_simd2020v2")
prev_dz_estimates    <- glue("DataZone2011_pop_est_2011_{old}")
prev_dz_estimates_5y <- glue("DataZone2011_pop_est_5year_agegroups_2011_{old}")
new_dz_estimates     <- glue("DataZone2011_pop_est_2011_{new}")
new_dz_estimates_5y  <- glue("DataZone2011_pop_est_5year_agegroups_2011_{new}")
new_iz_estimates     <- glue("IntZone2011_pop_est_2011_{new}")
new_iz_estimates_5y  <- glue("IntZone2011_pop_est_5year_agegroups_2011_{new}")


### 2 - Read in Source Data for Males and Females ----
# added duplicate segments to open up and wrangle source file and removed function
# NRS table structure had changed and function had stopped working
# potential to simplify i.e. upload two excel source info add male female variable
 # then take merge these 2  tables and do remove variables and set name commands
# consider this for next year if table format "stablises"#

# read in Source Data for Females, remove column and catgorise by gender
female <- read_excel(glue("{data_filepath}/{new_sape_file}"), 
                     sheet = "Females", range = "A4:CR6980") %>%
  mutate(sex = "F") 

#read in Source Data for Males, remove column and catgorise by gender 
male <- read_excel(glue("{data_filepath}/{new_sape_file}"), 
                   sheet = "Males", range = "A4:CR6980") %>%
  mutate(sex = "M") 

# Combine the lists into one dataframe and wrangle to fit prev years' format

male_and_female <- bind_rows(female, male) %>%  #sort by dz to check gender totals are different
  select(-c("Data zone name":"Council area name")) %>% 
  relocate(sex, .after = `Data zone code`) %>% 
  set_names(., c("datazone2011","sex", "total_pop","age0", "age1", "age2", 
                 "age3", "age4", "age5", "age6", "age7", "age8", "age9", 
                 "age10", "age11", "age12", "age13", "age14", "age15", 
                 "age16", "age17", "age18", "age19", "age20", "age21", 
                 "age22", "age23", "age24", "age25", "age26", "age27", 
                 "age28", "age29", "age30", "age31", "age32", "age33", 
                 "age34", "age35", "age36", "age37", "age38", "age39", 
                 "age40", "age41", "age42", "age43", "age44", "age45", 
                 "age46", "age47", "age48", "age49", "age50", "age51", 
                 "age52", "age53", "age54", "age55", "age56", "age57", 
                 "age58", "age59", "age60", "age61", "age62", "age63", 
                 "age64", "age65", "age66", "age67", "age68", "age69", 
                 "age70", "age71", "age72", "age73", "age74", "age75", 
                 "age76", "age77", "age78", "age79", "age80", "age81", 
                 "age82", "age83", "age84", "age85", "age86", "age87", 
                 "age88", "age89", "age90plus"))%>% 
  mutate(year = new) %>% 
  filter(!is.na(datazone2011)) 



### 3 - Add Previous Years Data ----

# Read in previous years file
# Drop higher geographies and SIMD variables as we add these on later

dz2011_pop_est <- readRDS(glue("{lookup_filepath}/", 
                               "{prev_dz_estimates}.rds")) %>% 
  select(year:datazone2011, sex:total_pop)

# Combine previous years data with new data

dz2011_pop_est %<>% 
  bind_rows(male_and_female) %>% 
  arrange(year, datazone2011, desc(sex))



### 3.1 - Add SIMD Data ----

# Read in SIMD 2020v2 data
# Drop pop_2017 column

dz2011_simd2020v2 <- readRDS(glue("{simd2020v2_filepath}/", 
                                  "DataZone2011_simd2020v2.rds")) %>% 
  select(-pop_2017)

# Read in SIMD 2016 data
# Drop pop_2014 column

dz2011_simd2016 <- readRDS(glue("{simd2016_filepath}/", 
                                "DataZone2011_simd2016.rds")) %>% 
  select(-pop_2014) %>% 
  clean_names() %>% 
  rename(datazone2011 = data_zone2011, 
         intzone2011 = int_zone2011, 
         simd2016_rank = simd2016rank)

# Join simd data onto population estimates 

# Select columns
dz2011_pop_est %<>% 
  left_join(dz2011_simd2020v2) %>% #left_join(dz2011_pop_est,dz2011_simd2020v2,by=?)
  left_join(dz2011_simd2016) %>% 
  select(-datazone2011name, -intzone2011name) %>% 
  select(year, datazone2011,  sex, age0:total_pop, 
         intzone2011, hb2019, hb2019name, hb2018, hb2014, 
         hscp2019, hscp2019name, hscp2018, hscp2016, ca2019, ca2019name, ca2018, 
         ca2011, everything()) 
  

# Add dz and iz labels
dz_names <- read.csv(glue("{codes_names_filepath}/Data Zone 2011 Lookup.csv")) %>% 
  rename("datazone2011" = "DataZone2011Code", 
         "datazone2011name" = "DataZone2011Name")
iz_names <- read.csv(glue("{codes_names_filepath}/Intermediate Zone 2011 Lookup.csv")) %>% 
  rename("intzone2011" = "IntermediateZone2011Code", 
         "intzone2011name" = "IntermediateZone2011Name")

dz2011_pop_est %<>% 
  left_join(dz_names) %>% 
  relocate(datazone2011name, .after = datazone2011) %>% 
  left_join(iz_names) %>% 
  relocate(intzone2011name, .after = intzone2011)


### 3.2 - Compute Age Groups and Add to 5 Year Age Group File ----

# Read in latest 5 year age group file
# Drop higher geographies and SIMD variables as we add these on later

dz2011_pop_est_5y <- readRDS(glue("{lookup_filepath}/", 
                                  "{prev_dz_estimates_5y}.rds")) %>% 
  select(year:datazone2011, sex:total_pop)


# Compute age groups for male_and_female
male_and_female_5y <- male_and_female %>%
  mutate(ageg04   = rowSums(select(., age0:age4)),
         ageg59   = rowSums(select(., age5:age9)),
         ageg1014 = rowSums(select(., age10:age14)),
         ageg1519 = rowSums(select(., age15:age19)),
         ageg2024 = rowSums(select(., age20:age24)),
         ageg2529 = rowSums(select(., age25:age29)),
         ageg3034 = rowSums(select(., age30:age34)),
         ageg3539 = rowSums(select(., age35:age39)),
         ageg4044 = rowSums(select(., age40:age44)),
         ageg4549 = rowSums(select(., age45:age49)),
         ageg5054 = rowSums(select(., age50:age54)),
         ageg5559 = rowSums(select(., age55:age59)),
         ageg6064 = rowSums(select(., age60:age64)),
         ageg6569 = rowSums(select(., age65:age69)), 
         ageg7074 = rowSums(select(., age70:age74)),
         ageg7579 = rowSums(select(., age75:age79)),
         ageg8084 = rowSums(select(., age80:age84)),
         ageg8589 = rowSums(select(., age85:age89))) %>% 
  select(-c(age0:age89)) %>% 
  rename(ageg90plus = age90plus)


# Combine previous years data with new data
# Match on SIMD data
# Select columns

dz2011_pop_est_5y %<>%
  bind_rows(male_and_female_5y) %>% 
  arrange(year, datazone2011, desc(sex)) %>% 
  left_join(dz2011_simd2020v2) %>% 
  left_join(dz2011_simd2016) %>% 
  select(-datazone2011name, -intzone2011name) %>% 
  select(year, datazone2011, sex, ageg04:total_pop, 
         intzone2011, hb2019, hb2019name, hb2018, hb2014, 
         hscp2019, hscp2019name, hscp2018, hscp2016, ca2019, ca2019name, ca2018, 
         ca2011, everything())

dz2011_pop_est_5y %<>% 
  left_join(dz_names) %>% 
  relocate(datazone2011name, .after = datazone2011) %>% 
  left_join(iz_names) %>% 
  relocate(intzone2011name, .after = intzone2011)



### 4 - Create SAPE Files for 2011 Intermediate Zones ----

### 4.1 - IntZone Single Year File ----

# Group by year, intzone2011 and sex
# Sum age columns and total_pop across grouped variables
# Ungroup to turn the grouped data back to a normal dataframe
# Sort data by year, intzone2011 and sex

iz2011_pop_est <- dz2011_pop_est %>%
  group_by(year, intzone2011, intzone2011name, sex) %>%
  summarise_at(vars(age0:total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(year, intzone2011, desc(sex))

### 4.2 - create IntZone2011_pop_est_5year_agegroups File ----

# Group by year, intzone2011 and sex
# Sum age columns and total_pop across grouped variables
# Ungroup to turn the grouped data back to a normal dataframe
# Sort data by year, intzone2011 and sex

iz2011_pop_est_5y <- dz2011_pop_est_5y %>%
  group_by(year, intzone2011, intzone2011name, sex) %>%
  summarise_at(vars(ageg04:total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(year, intzone2011, desc(sex))


### 5 - Check Final Files ----

# Create check function

data_check <- function(input, column){
  
  # Check that the frequencies for each area is the same.
  # Check that the frequencies for each area is equal to the number of years of 
  # SAPE data x 2, i.e. for 12 years of data, would expect 24
  # Here we expect the value to be 16 for 8 years and both gender
  # UPDATE THIS FOR NEW RELEASE
  
  input %>% 
    group_by(!!as.name(column)) %>% 
    count() %>% 
    filter(n != 24) %>% #updated this to 22 for 11 yrs
    print()
  
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
  
  input %>% count(year) %>% print()
  
  # Check that the frequencies for males and females is equal
  
  input %>% count(sex) %>% print()
  
}

### 5.1 - Check DataZone Single Year Files ----
# no of years of data x2 (i.e 2 genders) for each DZ
# confirm  with no rows and columns
# 6976 DZ x no of years for F and M

data_check(dz2011_pop_est, "datazone2011")


### 5.2 - Check DataZone 5 Year Age Group File ----
# 5 year aggregation check  
# 6976 DZ x no of years for F and M

data_check(dz2011_pop_est_5y, "datazone2011")


### 5.3 - Check IntZone Single Year File ----
#1279 *2 =2558
# *1279 *12= 15348
data_check(iz2011_pop_est, "intzone2011")


### 5.4 - Check IntZone 5 Year Age Group File ----
#1279 *2 =2558
# *1279 *12= 15348

data_check(iz2011_pop_est_5y, "intzone2011")


### 5.5 - Check Scotland Totals ----

# Run full section of syntax to end to get console printout#
# i.e. (don't need to check indivdual outputs)
# Check all files have same Scotland total for all years
# Conatct GPD analyst if there are any issues

# Data Zones
# Create single year totals for data zones

dz_total <- dz2011_pop_est %>%
  group_by(year) %>%
  summarise(dz_total_pop = sum(total_pop)) %>%
  ungroup()

# Create 5 year age group totals for DataZone

dz_5y_total <- dz2011_pop_est_5y %>%
  group_by(year) %>%
  summarise(dz_5y_total_pop = sum(total_pop)) %>%
  ungroup()

# Intermediate Zones
# Create single year totals for IntZone

iz_total <- iz2011_pop_est %>%
  group_by(year) %>%
  summarise(iz_total_pop = sum(total_pop)) %>%
  ungroup()

# Create 5 year age group totals for IntZone

iz_5y_total <- iz2011_pop_est_5y %>%
  group_by(year) %>%
  summarise(iz_5y_total_pop = sum(total_pop)) %>%
  ungroup()

# Match all files together

dz_total %>%
  full_join(dz_5y_total, by = "year") %>%
  full_join(iz_total, by = "year") %>%
  full_join(iz_5y_total, by = "year")


#### 6.0 export outputs (once checks complete)-----

# 6.1 Data zone yearly estimates
saveRDS(dz2011_pop_est, glue("{lookup_filepath}/{new_dz_estimates}.rds"))
fwrite(dz2011_pop_est, glue("{lookup_filepath}/{new_dz_estimates}.csv", na = ""))

# 6.2 Data zone 5 yearly estimates
saveRDS(dz2011_pop_est_5y, glue("{lookup_filepath}","/{new_dz_estimates_5y}.rds"))
fwrite(dz2011_pop_est_5y, glue("{lookup_filepath}/{new_dz_estimates_5y}.csv", na = ""))

# 6.3 IntZone Single Year File
saveRDS(iz2011_pop_est, glue("{lookup_filepath}/{new_iz_estimates}.rds"))
fwrite(iz2011_pop_est, glue("{lookup_filepath}/{new_iz_estimates}.csv", na = ""))

# 6.4 IntZone2011_pop_est_5year File 
saveRDS(iz2011_pop_est_5y, glue("{lookup_filepath}/{new_iz_estimates_5y}.rds"))
fwrite(iz2011_pop_est_5y, glue("{lookup_filepath}/{new_iz_estimates_5y}.csv", na = ""))
