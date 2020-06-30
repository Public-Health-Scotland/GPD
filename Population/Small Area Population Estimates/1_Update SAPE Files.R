##########################################################
# Update SAPE Files
# Calum Purdie
# Original date 24/08/2018
# Latest update author - Calum Purdie
# Latest update date - 05/06/2020
# Latest update description 
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Updating Small Area Population Estimates files (Data Zone and Int Zone) for 
# yearly NRS release
# Approximate run time - 1 minute
##########################################################


### 1 - Housekeeping ----

# Read in packages from library

library(magrittr)
library(tidyr)
library(dplyr)
library(readxl)
library(tidylog)
library(janitor)
library(glue)
library(data.table)

# Set filepaths

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/")
data_filepath <- glue("{base_filepath}/2_Population/Small Area Population ", 
                      "estimates/Source Data")
lookup_filepath <- glue("{base_filepath}/2_Population/Small Area Population ", 
                        "estimates/Lookup Files/R Files")
simd2020v2_filepath <- glue("{base_filepath}/3_Deprivation/SIMD/Lookup Files/",
                            "SIMD 2020")
simd2016_filepath <- glue("{base_filepath}/3_Deprivation/SIMD/Lookup Files/",
                          "SIMD 2016/R Files")

# Set date for open data filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")

# Set datasets to use

female_2018_pop <- "Datazone2011_2018_f"
male_2018_pop <- "Datazone2011_2018_m"
datazone_simd2016 <- "DataZone2011_simd2016"
datazone_simd2020v2 <- "DataZone2011_simd2020v2"
prev_dz_estimates <- "DataZone2011_pop_est_2011_2017"
prev_dz_estimates_5y <- "DataZone2011_pop_est_5year_agegroups_2011_2017"
new_dz_estimates <- "DataZone2011_pop_est_2011_2018"
new_dz_estimates_5y <- "DataZone2011_pop_est_5year_agegroups_2011_2018"
new_iz_estimates <- "IntZone2011_pop_est_2011_2018"
new_iz_estimates_5y <- "IntZone2011_pop_est_5year_agegroups_2011_2018"



### 2 - Read in Source Data for Males and Females ----

read_data <- function(pop_data, pop_year, pop_gender){
  
  data <- read_excel(glue("{data_filepath}/{pop_data}.xlsx"), 
                     sheet = 1, range = "A6:CR6982") %>% 
    mutate(Year = pop_year) %>%
    mutate(Sex = pop_gender) %>% 
    select(-c(DataZone2011Name, CouncilArea2018Name, ...5)) %>% 
    set_names(., c("datazone2011", "total_pop", "age0", "age1", "age2", 
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
                   "age88", "age89", "age90plus", "year", "sex"))
  
}

# Read in female data

female <- read_data(female_2018_pop, 2018, "F")

# Read in male data

male <- read_data(male_2018_pop, 2018, "M")

# Combine the lists into one dataframe

male_and_female <- bind_rows(female, male)



### 3 - Add Previous Years Data ----

# Read in previous years file
# Drop higher geographies and SIMD variables as we add these on later

dz2011_pop_est <- readRDS(glue("{lookup_filepath}/Archive/", 
                               "{prev_dz_estimates}.rds")) %>% 
  select(Year:total_pop) %>% 
  clean_names() %>% 
  rename(datazone2011 = data_zone2011)

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
  left_join(dz2011_simd2020v2) %>% 
  left_join(dz2011_simd2016) %>% 
  select(year, datazone2011, datazone2011name, sex, age0:total_pop, 
         intzone2011, intzone2011name, hb2019, hb2019name, hb2018, hb2014, 
         hscp2019, hscp2019name, hscp2018, hscp2016, ca2019, ca2019name, ca2018, 
         ca2011, everything())

# Save as .RDS file

saveRDS(dz2011_pop_est, glue("{lookup_filepath}/{new_dz_estimates}.rds"))



### 3.2 - Compute Age Groups and Add to 5 Year Age Group File ----

# Read in latest 5 year age group file
# Drop higher geographies and SIMD variables as we add these on later

dz2011_pop_est_5y <- readRDS(glue("{lookup_filepath}/Archive/", 
                                  "{prev_dz_estimates_5y}.rds")) %>% 
  select(Year:total_pop) %>% 
  clean_names() %>% 
  rename(datazone2011 = data_zone2011)

# Compute age groups for male_and_female

male_and_female_5y <- male_and_female %>%
  mutate(ageg04 = rowSums(.[3:7]), 
         ageg59 = rowSums(.[8:12]),
         ageg1014 = rowSums(.[13:17]),
         ageg1519 = rowSums(.[18:22]), 
         ageg2024 = rowSums(.[23:27]),
         ageg2529 = rowSums(.[28:32]), 
         ageg3034 = rowSums(.[33:37]),
         ageg3539 = rowSums(.[38:42]), 
         ageg4044 = rowSums(.[43:47]),
         ageg4549 = rowSums(.[48:52]), 
         ageg5054 = rowSums(.[53:57]),
         ageg5559 = rowSums(.[58:62]), 
         ageg6064 = rowSums(.[63:67]),
         ageg6569 = rowSums(.[68:72]), 
         ageg7074 = rowSums(.[73:77]),
         ageg7579 = rowSums(.[78:82]), 
         ageg8084 = rowSums(.[83:87]),
         ageg8589 = rowSums(.[88:92])) %>% 
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
  select(year, datazone2011, datazone2011name, sex, ageg04:total_pop, 
         intzone2011, intzone2011name, hb2019, hb2019name, hb2018, hb2014, 
         hscp2019, hscp2019name, hscp2018, hscp2016, ca2019, ca2019name, ca2018, 
         ca2011, everything())

# Save as .RDS file

saveRDS(dz2011_pop_est_5y, glue("{lookup_filepath}/{new_dz_estimates_5y}.rds"))



### 4 - Create SAPE Files for 2011 Intermediate Zones ----

### 4.1 - Save IntZone Single Year File ----

# Group by year, intzone2011 and sex
# Sum age columns and total_pop across grouped variables
# Ungroup to turn the grouped data back to a normal dataframe
# Sort data by year, intzone2011 and sex

iz2011_pop_est <- dz2011_pop_est %>%
  group_by(year, intzone2011, intzone2011name, sex) %>%
  summarise_at(vars(age0:total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(year, intzone2011, desc(sex))

# Save file as .RDS

saveRDS(iz2011_pop_est, 
        glue("{lookup_filepath}/{new_iz_estimates}.rds"))



### 4.2 - Save IntZone2011_pop_est_5year_agegroups_2011_2017 File ----

# Group by year, intzone2011 and sex
# Sum ageg columns and total_pop across grouped variables
# Ungroup to turn the grouped data back to a normal dataframe
# Sort data by year, intzone2011 and sex

iz2011_pop_est_5y <- dz2011_pop_est_5y %>%
  group_by(year, intzone2011, intzone2011name, sex) %>%
  summarise_at(vars(ageg04:total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(year, intzone2011, desc(sex))

# Save file as .RDS

saveRDS(iz2011_pop_est_5y, 
        glue("{lookup_filepath}/{new_iz_estimates_5y}.rds"))



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
    filter(n != 16) %>% 
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

data_check(dz2011_pop_est, "datazone2011")


### 5.2 - Check DataZone 5 Year Age Group File ----

data_check(dz2011_pop_est_5y, "datazone2011")


### 5.3 - Check IntZone Single Year File ----

data_check(iz2011_pop_est, "intzone2011")


### 5.4 - Check IntZone 5 Year Age Group File ----

data_check(iz2011_pop_est_5y, "intzone2011")


### 5.5 - Check Scotland Totals ----

# Run full section of syntax to end
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
