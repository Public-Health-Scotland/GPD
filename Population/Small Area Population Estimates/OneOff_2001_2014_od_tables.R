##########################################################
# Create Open Data SAPE Files
################################

### 1 - Housekeeping ----
rm(list = ls())

#### libaries 
if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}
# Read in packages from library

pacman::p_load( glue, janitor, tidylog, data.table, magrittr,
               dplyr, arrow, readr)


# Set filepaths
gpd_base_path<-"/conf/linkage/output/lookups/Unicode/"

pop_path <- glue(gpd_base_path,"Populations/Estimates/")

od_filepath     <- "//data/geography/Population/Small Area Population estimates/Lookup Files/CKAN Open Data"


# Set date for open data filenames

date <- strftime(Sys.Date(), format = "%d%m%Y")

# Create check function
data_check <- function(input, column){
    # Check that the frequencies for each area is the same.
  # Check that the frequencies for each area is equal to the number of years of 
  # SAPE data x 2, i.e. for 12 years of data, would expect 24
  # Here we expect the value to be 16 for 8 years and both gender
  input %>% 
    group_by(!!as.name(column)) %>% 
    count() %>% 
    filter(n != 14) %>% #2001-2014
    print()
  # Check sums add up to total_pop
  # Calculate the sum for all the age columns and check to see if the sums add 
  # up to the total_pop
  # Filter for rows where the sums don't add up
  input %>% 
    mutate(sums = if_else(
      rowSums(select(., starts_with("Age"))) - AllAges != 0, 1, 0)) %>% 
    select(sums) %>% 
    filter(sums != 0) %>% 
    print()
    # Check that the frequencies for each year are the same
  input %>% count(Year) %>% print()
  # Check that the frequencies for males and females is equal
  input %>% count(Sex) %>% print()
}

### 2 - Create Open Data 2001 DZ Population Estimates Files ----
# import cl-out table
dz2001_pop_est_2001_2014  <- read_rds(glue(pop_path, "DataZone2001_pop_est_2001_2014.rds")) %>% 
  rename(DataZone = DataZone2001, Sex = SEX,Age90plus= AGE90PLUS, AllAges = total_pop) %>% 
  set_colnames(gsub("AGE", "Age", names(.))) %>% 
  select(1:95)

# Create totals for male and female combined
dz2001_all_total <- dz2001_pop_est_2001_2014 %>% 
  group_by(Year, DataZone) %>% 
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>% 
  mutate(Sex = "All")

# Add all sex data zone pops to dz2011_pop_est_od
dz2001_od <- dz2001_pop_est_2001_2014 %>% 
  bind_rows(dz2001_all_total)

# create  Scot Male, Female All data 
dz_scot_dz2001_all_total <-  dz2001_pop_est_2001_2014 %>%
  group_by(Year) %>%
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>%
  mutate(Sex = "All") 

dz_scot_total <-  dz2001_pop_est_2001_2014 %>%
  group_by(Year, Sex) %>%
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>% 
  full_join(dz_scot_dz2001_all_total) %>% 
  arrange(Year, desc(Sex))

# Combine DZ2001_pop_est and scot_total together
# Recode Sex variable. Sort by Year and Sex
# Scotland total is at the end for each year so move this to start using 
# setorder()

dz2001_od %<>%
  full_join(dz_scot_total) %>%
  mutate(Sex = recode(Sex, 'M' = 'Male', 'F' = 'Female')) %>% 
  arrange(Year, Sex) %>%
  setorder(na.last = F)


#  Tidy dz2011_pop_est_od ----

# Attach Scotland national code
# Create qualifier column for DataZone and set it to "d" for Scotland totals

dz2001_od %<>%
  mutate(DataZone = if_else(is.na(DataZone), "S92000003", DataZone), 
         DataZoneQF = case_when(DataZone == "S92000003" ~ "d"), 
         SexQF =  case_when(Sex == "All" ~ "d")) %>%
  select(Year, DataZone, DataZoneQF, Sex, SexQF, AllAges, Age0:Age90plus)

#### checks

#  Check DataZone Single Year Files #
# no of years of data x2 (i.e 2 genders) for each DZ
# confirm  with no rows and columns
# 6506 DZ x no of years for F and M

od_checks<- dz2001_od %>% 
  filter(DataZone!= "S92000003")

data_check(od_checks, "DataZone")

# Save as csv

fwrite( dz2001_od, glue("{od_filepath}/DZ2001-pop-est_{date}.csv"),  na = "")

rm(dz_scot_dz2001_all_total,dz_scot_total, dz2001_all_total, dz2001_od, 
   dz2001_pop_est_2001_2014, od_checks)

### 3 Create Open Data 2001 IZ Population Estimates Files ----

iz2001_pop_est_2001_2014  <- read_rds(glue(pop_path, "IntZone2001_pop_est_2001_2014.rds")) %>% 
  rename(IntZone = IntZone2001, Sex = SEX, Age90plus= AGE90PLUS, AllAges = Total_pop) %>% 
  set_colnames(gsub("AGE", "Age", names(.))) %>% 
  select(1:95)


# Create totals for male and female combined
iz2001_all_total <- iz2001_pop_est_2001_2014 %>% 
  group_by(Year, IntZone) %>% 
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>% 
  mutate(Sex = "All")

# Add all_total 
iz2001_od <- iz2001_pop_est_2001_2014 %>% 
  bind_rows(iz2001_all_total)

# Scotal and value for combined sex 
iz_scot_iz2001_all_total <- iz2001_pop_est_2001_2014 %>%
  group_by(Year) %>%
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>%
  mutate(Sex = "All") 

# Scot total for M and F
iz_scot_total <-  iz2001_pop_est_2001_2014 %>%
  group_by(Year, Sex) %>%
  summarise_at(vars(Age0:AllAges), list(sum)) %>%
  ungroup() %>% 
  full_join(iz_scot_iz2001_all_total) %>% 
  arrange(Year, desc(Sex))

# Combine IZ2001_pop_est and scot_total together
# Recode Sex variable. Sort by Year and Sex
# Scotland total is at the end for each year so move this to start using 
# setorder()
iz2001_od %<>%
  full_join(iz_scot_total) %>%
  mutate(Sex = recode(Sex, 'M' = 'Male', 'F' = 'Female')) %>% 
  arrange(Year, Sex) %>%
  setorder(na.last = F)

# Attach Scotland national code
# Create qualifier column for DataZone and set it to "d" for Scotland totals

iz2001_od %<>%
  mutate(IntZone = if_else(is.na(IntZone), "S92000003", IntZone), 
         IntZoneQF = case_when(IntZone == "S92000003" ~ "d"), 
         SexQF =  case_when(Sex == "All" ~ "d")) %>%
  select(Year, IntZone, IntZoneQF, Sex, SexQF, AllAges, Age0:Age90plus)


#### checks

od_checks<- iz2001_od %>% 
  filter(IntZone!= "S92000003")

data_check(od_checks, "IntZone")

fwrite(iz2001_od, glue("{od_filepath}/IZ2001-pop-est_{date}.csv"), 
       na = "")

rm(iz_scot_iz2001_all_total,iz_scot_total, iz2001_all_total, iz2001_od, 
   iz2001_pop_est_2001_2014, od_checks)
