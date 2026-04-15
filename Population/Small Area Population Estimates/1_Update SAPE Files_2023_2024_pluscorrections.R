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

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}
# Read in packages from library

pacman::p_load( glue, janitor, tidylog, tidyr, readxl, data.table, magrittr,
                dplyr, arrow)


           
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

new <- 2024

# Set datasets to use
new_dz_estimates     <- glue("DataZone2011_pop_est_2011_{new}")
new_dz_estimates_5y  <- glue("DataZone2011_pop_est_5year_agegroups_2011_{new}")
new_iz_estimates     <- glue("IntZone2011_pop_est_2011_{new}")
new_iz_estimates_5y  <- glue("IntZone2011_pop_est_5year_agegroups_2011_{new}")

# Set file paths
SAPE_filepath   <- glue("//data/geography/Population/Small Area Population estimates")
data_filepath   <- glue("{SAPE_filepath}/Source Data")
lookup_filepath <- glue("{SAPE_filepath}/Lookup Files/R Files")

# Read in SIMD & geography lookups
# take last datazone pop estimate file, create simple lookup with  all SIMD and geo parameters
# join to DF as required
simd_df <- readRDS(glue("{lookup_filepath}/","DataZone2011_pop_est_2011_2022_REBASED.rds")) %>% 
  select(datazone2011, intzone2011:simd2016_crime_rank) %>% 
  distinct(datazone2011, .keep_all = TRUE)


### 2.0 import source data ####
# 2011 to 2024 individual data tables re-based summer 2025 and and slight corrections Feb 2026
# 2022 has slight corrections to small number of Data Zones, republished Feb 2026
# 2023 and 2024 first publication  24 Feb 2026, data split into femal and male, so need to be 
# imported separately. 
#  This is quite longwinded and will be revised when data import settles down to a routine
i_2011 <- read_excel(glue("{data_filepath}/Rebased SAPE 2011 (11DZ) - corrected.xlsx"),
                           sheet = "2011", range = "A3:CS20931") %>% 
  filter(Sex != "Persons") %>% 
  mutate(year =2011)

i_2012 <- read_excel(glue("{data_filepath}/Rebased SAPE 2012 (11DZ) - corrected.xlsx"),
                           sheet = "2012", range = "A3:CS20931") %>% 
  filter(Sex != "Persons") %>% 
  mutate(year =2012)

i_2013 <- read_excel(glue("{data_filepath}/Rebased SAPE 2013 (11DZ) - corrected.xlsx"),
                           sheet = "2013", range = "A3:CS20931") %>% 
  filter(Sex != "Persons") %>% 
  mutate(year =2013)

i_2014 <- read_excel(glue("{data_filepath}/Rebased SAPE 2014 (11DZ) - corrected.xlsx"),
                           sheet = "2014", range = "A3:CS20931") %>% 
  filter(Sex != "Persons") %>% 
  mutate(year =2014)

i_2015 <- read_excel(glue("{data_filepath}/Rebased SAPE 2015 (11DZ) - corrected.xlsx"),
                           sheet = "2015", range = "A3:CS20931") %>% 
  filter(Sex != "Persons") %>% 
  mutate(year =2015)

i_2016 <- read_excel(glue("{data_filepath}/Rebased SAPE 2016 (11DZ) - corrected.xlsx"),
                           sheet = "2016", range = "A3:CS20931") %>% 
  filter(Sex != "Persons")  %>% 
  mutate(year =2016)

i_2017 <- read_excel(glue("{data_filepath}/Rebased SAPE 2017 (11DZ) - corrected.xlsx"),
                           sheet = "2017", range = "A3:CS20931") %>% 
  filter(Sex != "Persons")  %>% 
  mutate(year =2017)

i_2018 <- read_excel(glue("{data_filepath}/Rebased SAPE 2018 (11DZ) - corrected.xlsx"),
                           sheet = "2018", range = "A3:CS20931") %>% 
  filter(Sex != "Persons")  %>% 
  mutate(year =2018)

i_2019 <- read_excel(glue("{data_filepath}/Rebased SAPE 2019 (11DZ) - corrected.xlsx"),
                           sheet = "2019", range = "A3:CS20931") %>% 
  filter(Sex != "Persons") %>% 
  mutate(year =2019)

i_2020 <- read_excel(glue("{data_filepath}/Rebased SAPE 2020 (11DZ) - corrected.xlsx"),
                           sheet = "2020", range = "A3:CS20931") %>% 
  filter(Sex != "Persons")  %>% 
  mutate(year =2020)

i_2021 <- read_excel(glue("{data_filepath}/Rebased SAPE 2021 (11DZ) - corrected.xlsx"),
                           sheet = "2021", range = "A3:CS20931") %>% 
  filter(Sex != "Persons")  %>% 
  mutate(year =2021)

i_2022 <- read_excel(glue("{data_filepath}/sape-2022-11dz-corrected.xlsx"),
                           sheet = "Table 1", range = "A4:CS20932") %>% 
  filter(Sex != "Persons")  %>% 
  mutate(year =2022)

i_2023_female <- read_excel(glue("{data_filepath}/small-area-population-estimates-mid-2023-11-dz.xlsx"), 
                     sheet = "Females", range = "A4:CR6980") %>%
  mutate(Sex = "Females", year = 2023) 

i_2023_male <- read_excel(glue("{data_filepath}/small-area-population-estimates-mid-2023-11-dz.xlsx"), 
                            sheet = "Males", range = "A4:CR6980") %>%
  mutate(Sex = "Males", year = 2023) 

i_2024_female <- read_excel(glue("{data_filepath}/small-area-population-estimates-mid-2024-11-dz.xlsx"), 
                            sheet = "Females", range = "A4:CR6980") %>%
  mutate(Sex = "Females", year = 2024) 

i_2024_male <- read_excel(glue("{data_filepath}/small-area-population-estimates-mid-2024-11-dz.xlsx"), 
                            sheet = "Males", range = "A4:CR6980") %>%
  mutate(Sex = "Males", year = 2024) 

### 3.1 create annual Data Zone lookup ####
# bind input tables
g_2011_2024 <- rbind(i_2011, i_2012, i_2013 ,i_2014, i_2015, i_2016, i_2017, 
                       i_2018, i_2019, i_2020 ,i_2021, i_2022,
                       i_2023_female, i_2023_male, i_2024_female, i_2024_male) %>% 
  #reorganis and set names
    set_names(., c("datazone2011","datazone2011name", "ca19","ca19name","sex",
                 "total_pop", "age0", "age1", "age2", 
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
                 "age88", "age89", "age90plus","year", "sex" )) %>%  
  mutate(sex= case_when (sex == "Females"~ "F",
                         sex == "Males" ~ "M",
                         TRUE~"check") ) %>% 
  select(c(year, datazone2011,datazone2011name, sex,
           age0:age90plus, total_pop )  ) %>% 
  filter(!is.na(datazone2011)) %>% 
  arrange(year, datazone2011, desc(sex))

# remove input source files
rm(i_2011, i_2012, i_2013 ,i_2014, i_2015, i_2016, i_2017, 
   i_2018, i_2019, i_2020 ,i_2021, i_2022,
   i_2023_female, i_2023_male, i_2024_female, i_2024_male) 


# add lookups

dz2011_pop_est <- g_2011_2024 %>%
  full_join(simd_df, by =c("datazone2011"))

### 3.2 -Data Zone  Age Groups and Add to 5 Year Age Group File ----

# Read in latest 5 year age group file
# Drop higher geographies and SIMD variables as we add these on later

# Compute age groups for male_and_female
male_and_female_5y <- g_2011_2024 %>% 
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
  select(year, datazone2011,datazone2011name, sex,
         ageg04:ageg8589, ageg90plus = age90plus, total_pop)

rm(g_2011_2024)

# Match on SIMD data
# Select columns

dz2011_pop_est_5y <- male_and_female_5y %>% 
  full_join(simd_df, by =c("datazone2011")) 


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
write_parquet(dz2011_pop_est,
              sink = glue("{lookup_filepath}/{new_dz_estimates}.parquet"),
              compression = "zstd")

# 6.2 Data zone 5 yearly estimates
saveRDS(dz2011_pop_est_5y , glue("{lookup_filepath}","/{new_dz_estimates_5y}.rds"))
fwrite(dz2011_pop_est_5y, glue("{lookup_filepath}/{new_dz_estimates_5y}.csv", na = ""))
write_parquet(dz2011_pop_est_5y,
              sink = glue("{lookup_filepath}/{new_dz_estimates_5y}.parquet"),
              compression = "zstd")

# 6.3 IntZone Single Year File
saveRDS(iz2011_pop_est, glue("{lookup_filepath}/{new_iz_estimates}.rds"))
fwrite(iz2011_pop_est, glue("{lookup_filepath}/{new_iz_estimates}.csv", na = ""))
write_parquet(iz2011_pop_est,
              sink = glue("{lookup_filepath}/{new_iz_estimates}.parquet"),
              compression = "zstd")

# 6.4 IntZone2011_pop_est_5year File 
saveRDS(iz2011_pop_est_5y, glue("{lookup_filepath}/{new_iz_estimates_5y}.rds"))
fwrite(iz2011_pop_est_5y, glue("{lookup_filepath}/{new_iz_estimates_5y}.csv", na = ""))
write_parquet(iz2011_pop_est_5y,
              sink = glue("{lookup_filepath}/{new_iz_estimates_5y}.parquet"),
              compression = "zstd")


