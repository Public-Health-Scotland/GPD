##########################################################
# Creating Population Projection Files
# Calum Purdie
# Original date 27/03/2019
# Latest update author - Gerald Leung
# Latest update date - 19/04/2023
# Latest update description - 
# Updating Scotland population due to revised version from Jan 2023 (Section 3)
# Added csv files
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for creating  look-up files for NRS population projections for 
# Scotland (section 3, Health Board (Section 5)
#  HSCP ( section 6) and Council Area (section 4) and sanity checks (section 7)
# Approximate run time - 5 minutes - (read excel)
##########################################################

#### 1.0 - Housekeeping ----
#### 1.1 Read in packages from library #####

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}

pacman::p_load(haven, #still needed?
               magrittr,tidyr,readxl, readr, janitor,tidylog, glue, stringr, dplyr,data.table)

# set projection start & end dates 
start <- "2022" #update
end <- "2047" #update

### 1.2  Set filepaths ######

base_filepath <- ("/data/geography/Population/Population Projections/")

#base_filepath <- glue("F:/PHI/Referencing & Standards/GPD/", 
#                      "2_Population/Population Projections")
lookup_filepath <- glue("{base_filepath}/Lookup Files/R Files")
data_filepath <- glue({base_filepath},"Source Data/")


gpd_base_path<-"/conf/linkage/output/lookups/Unicode/"
geog_lookup_path <-  glue(gpd_base_path, "Geography/HSCP Locality/")


### 1.3 Set files to use #####


source_data_table <- glue(data_filepath, "2022-based-snpp-data-tables.xlsx")

# Read in geography names look up
# This is used for Council Area, Health Board and HSCP Files

# geo_lookup <- readRDS(geo_lookup_file) %>%
#   select(ca2019, ca2019name, ca2018, ca2011, hscp2019, hscp2019name, hscp2018, 
#          hscp2016, hb2019, hb2019name, hb2018, hb2014) %>%
#   distinct()


#IM addition to reference what it should ;looklike
#prev_lookup<- readRDS("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Population Projections/Lookup Files/R Files/scot_pop_proj_2018_2043.rds")

geog_lookup <- readRDS(glue(geog_lookup_path,"HSCP Localities_DZ11_Lookup_20240513.rds"))

ca_lookup <- geog_lookup %>% 
  select(ca2019, ca2019name, ca2018, ca2011) %>% 
  distinct()

hb_lookup <- geog_lookup %>% 
  select(hb2019, hb2019name, hb2018, hb2014) %>% 
  distinct()

hscp_lookup <- geog_lookup %>% 
  select(ca2019, hscp2019, hscp2019name, hscp2018, 
         hscp2016) %>% 
  distinct()

rm(geog_lookup)


##### 2.0  Set functions #####
source ("/data/geography/GitHub/GPD-Population/Population Projections/pop_proj_functions.R")
#### 3.0 - Create Scotland population projection files####
### 3.1  Read in NRS data ####
  # Load data from Excel sheet. The 2020 -2045 datasets structure is 
  # different from the prev year, so a different approach was needed
  # have removed thr part of the script that produced the 2018-2043 source data
  # and replaced with a different version
  # Use read Excel to select the cells. Process is slow, therefore
  # separate the read from BCS step from the wrangling
  # call the Excel load xls, the working scot_proj_m or scot_proj_f

# read in male & female excel data
#male_xls<- read_excel(glue("{data_filepath}/scot_pop_proj_2020_2045.xlsx"), 
 #                   sheet = "Table 2", range = "A115:AA223")

# revised version, Jan 2023
columnnames <- read_excel(scot_pop_data,
                          sheet = "Population", range = "B1:AB1")
male_xls<- read_excel(scot_pop_data, 
                      sheet = "Population", range = "B1:AB108")

male_df <- as.data.frame(male_xls)

female <- read_excel(scot_pop_data, 
                         sheet = "Population", col_names = FALSE, range = "B109:AB215")

female_xls <- rbind(columnnames,setNames(female,names(columnnames)))
female_df <- as.data.frame(female_xls)

#female_xls <- read_excel(glue("{data_filepath}/scot_pop_proj_2020_2045.xlsx"), 
 #                        sheet = "Table 2", range = "A225:AA333")

# GL: just to compare with previous version as a sanity check
x = readRDS(glue("{lookup_filepath}/scot_pop_proj_2020_2045.rds"))
#y = readRDS(glue("{lookup_filepath}/scot_pop_proj_5year_agegroups_2020_2045.rds"))

### 3.2 wrangle male Scotland data projections by single year of age #####
# variable names had unusual formatting in Excel instead of the funtion approach 
# for 2018-2043 data had to use a long winded approach to renaming them.

scot_proj_m<-male_df %>% 
  # !! the block of code below is not needed for the revised version
#if(FALSE) {
  #filter(Age != "All ages") %>% 
  #  mutate(Age = as.numeric(Age)) %>% 
# rename("2020" = "Males\r\nMid-2020",
#        "2021" = "Males\r\nMid-2021",
#        "2022" = "Males\r\nMid-2022",
#        "2023" = "Males\r\nMid-2023",
#        "2024" = "Males\r\nMid-2024",
#        "2025" = "Males\r\nMid-2025",
#        "2026" = "Males\r\nMid-2026",
#        "2027" = "Males\r\nMid-2027",
#        "2028" = "Males\r\nMid-2028",
#        "2029" = "Males\r\nMid-2029",
#        "2030" = "Males\r\nMid-2030",
#        "2031" = "Males\r\nMid-2031",
#        "2032" = "Males\r\nMid-2032",
#        "2033" = "Males\r\nMid-2033",
#        "2034" = "Males\r\nMid-2034",
#        "2035" = "Males\r\nMid-2035",
#        "2036" = "Males\r\nMid-2036",
#        "2037" = "Males\r\nMid-2037",
#        "2038" = "Males\r\nMid-2038",
#        "2039" = "Males\r\nMid-2039",
#        "2040" = "Males\r\nMid-2040",
#        "2041" = "Males\r\nMid-2041",
#        "2042" = "Males\r\nMid-2042",
#        "2043" = "Males\r\nMid-2043",
#        "2044" = "Males\r\nMid-2044",
#        "2045" = "Males\r\nMid-2045") #%>%
#}
#pivot the table so have 1 column of pop per age and year
  pivot_longer("2020":"2045",names_to = "Year",values_to = "pop")%>% 
  mutate( #add/amend the  following
    age=recode(Age, "105 - 109"="150", "110 and over"="200"),#arbitrary ages >109
    age=as.numeric(age),
    age = if_else(age > 90, 90,age), #convert all ages> 90 to 90
    sex = 1 # male =1 female =2
    ) %>% 
  select(-Age) %>% 
  clean_names()
           
  
### 3.3 wrangle female Scotland data projections by single year of age ####
scot_proj_f<-female_xls %>% 
  # similarly to male the block below is not needed for revised version
  #  filter(Age != "All ages") %>% 
  # #  mutate(Age = as.numeric(Age)) %>% 
  # rename("2020" = "Females\r\nMid-2020",
  #        "2021" = "Females\r\nMid-2021",
  #        "2022" = "Females\r\nMid-2022",
  #        "2023" = "Females\r\nMid-2023",
  #        "2024" = "Females\r\nMid-2024",
  #        "2025" = "Females\r\nMid-2025",
  #        "2026" = "Females\r\nMid-2026",
  #        "2027" = "Females\r\nMid-2027",
  #        "2028" = "Females\r\nMid-2028",
  #        "2029" = "Females\r\nMid-2029",
  #        "2030" = "Females\r\nMid-2030",
  #        "2031" = "Females\r\nMid-2031",
  #        "2032" = "Females\r\nMid-2032",
  #        "2033" = "Females\r\nMid-2033",
  #        "2034" = "Females\r\nMid-2034",
  #        "2035" = "Females\r\nMid-2035",
  #        "2036" = "Females\r\nMid-2036",
  #        "2037" = "Females\r\nMid-2037",
  #        "2038" = "Females\r\nMid-2038",
  #        "2039" = "Females\r\nMid-2039",
  #        "2040" = "Females\r\nMid-2040",
  #        "2041" = "Females\r\nMid-2041",
  #        "2042" = "Females\r\nMid-2042",
  #        "2043" = "Females\r\nMid-2043",
  #        "2044" = "Females\r\nMid-2044",
  #        "2045" = "Females\r\nMid-2045" ) %>% 
  #pivot the table so have 1 column of pop per age and year
  pivot_longer("2020":"2045",names_to = "Year",values_to = "pop")%>% 
  mutate( #add/amend the  following
    age=recode(Age, "105 - 109"="150", "110 and over"="200"),#arbitrary ages >109
    age=as.numeric(age),
    age = if_else(age > 90, 90,age), #convert all ages> 90 to 90
    sex = 2 # male =1 female =2
  ) %>% 
  select(-Age) %>% 
  clean_names()


### 3.4 Merge Scotland population projections by single year of age  ####
    # Add the male and female files together
  # Sort by year, Age and sex
  # Set all variables as integers
  # Create sex_name column
  # Recode all Age values greater than 90 to 90 for consistency with previous 
  # projections
  # group_by all columns except Pop and calculate new Pop values for Age = 90
  # Ungroup and set age_group as an integer
 #
 # scot_pop_proj <- bind_rows(scot_proj_m, scot_proj_f) %>% 
 #    arrange(year, age, sex) %>% 
 #    mutate(year = as.integer(year), 
 #           pop = as.integer(pop), 
 #           sex = as.integer(sex), 
 #           sex_name = recode(sex, "1" = "Male", "2" = "Female"), 
 #           age = if_else(age > 90, 90, age)) %>%
 #    group_by(year, age, sex, sex_name) %>% 
 #    summarise(pop = sum(pop)) %>% 
 #    ungroup() %>% 
 #    mutate(age = as.integer(age))
  
# GL: not sure why this shows up here. Should check in the end.
### 7.1 - Check Scotland files ----
#checks(input = scot_pop_proj, geography = "scot", age_column = "age")
#checks(input = scot_pop_proj_5y, geography = "scot", age_column = "age_group")
### 3.5 - Create Scotland population projections by single year of age ####

# Add the male and female files together
# Sort by year, Age and sex
# Set all variables as integers
# Create sex_name column
# Recode all Age values greater than 90 to 90 for consistency with previous 
# projections
# group_by all columns except Pop and calculate new Pop values for Age = 90
# Ungroup and set age_group as an integer

scot_pop_proj <- bind_rows(scot_proj_m, scot_proj_f) %>% 
  arrange(year, age, sex) %>% 
  mutate(year = as.integer(year), 
         pop = as.integer(pop), 
         sex = as.integer(sex), 
         sex_name = recode(sex, "1" = "Male", "2" = "Female"), 
         age = if_else(age > 90, 90, age)) %>%
  group_by(year, age, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(age = as.integer(age))

saveRDS(scot_pop_proj, 
      glue("{lookup_filepath}/scot_pop_proj_{start}_{end}_v2.rds"))

fwrite(scot_pop_proj,
       glue("{lookup_filepath}/scot_pop_proj_{start}_{end}_v2.csv"))

# GL: should stop running this as SPSS not supported
# write .sav version. 
# temporary addition as SPSS will be phased out soon
  #write_sav(scot_pop_proj,
   #         glue("{lookup_filepath}/scot_pop_proj_{start}_{end}.sav"))

### 3.6 - Create file for 5 year age groups ----
  # Create age_group column for 5 year age groups using function: age_group_fun
  # Group by year, age_group and sex - these are the columns you want to keep
  # Summarise by pop to get the population for each age group
  # Ungroup and set age_group as an integer

scot_pop_proj_5y <- age_group_fun(scot_pop_proj) %>% 
  group_by(year, age_group, age_group_name, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(age_group = as.integer(age_group))

saveRDS(scot_pop_proj_5y, 
        glue("{lookup_filepath}/scot_pop_proj_5year_agegroups_{start}_{end}_v2.rds"))


fwrite(scot_pop_proj_5y,
       glue("{lookup_filepath}/scot_pop_proj_5year_{start}_{end}_v2.csv"))

y = read.csv(glue("{lookup_filepath}/scot_pop_proj_5year_agegroups_2020_2120.csv"))

# GL: shoud stop running this as SPSS not supported
# write .sav version. 
# temporary addition as SPSS will be phased out soon
#write_sav(scot_pop_proj_5y,
 #         glue("{lookup_filepath}/scot_pop_proj_5year_agegroups_{start}_{end}_v2.sav"))

rm(scot_proj_f, scot_proj_m, male_xls, female_xls)



#### 4.0 - Create Council Area population projections files ----
### 4.1 - Use lower geography population function to read in data ----

ca_source_data <- read_excel(source_data_table, sheet = "Table 2",  range = "A6:CR2580")

### 4.2 - Create Council Area population projections by single year of age ----

# pivot # Rename code to ca2019 and join on ca columns from geo_lookup
# Set age as an integer and create sex_name column
# Select relevant columns and arrange

ca_pop_proj <- ca_source_data %>% 
  clean_names() %>% 
  filter(area_name !="Scotland" & sex !="Persons") %>% 
  rename(year = year_to_30_june, ca2019= area_code) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer( cols = 5:96,  names_to = "age",  values_to = "pop" ) %>% 
  filter(age!= "all_ages") %>% 
  mutate(sex_name =  case_when (sex == "Females" ~"Female",
                           sex =="Males" ~ "Male",  TRUE~ "check"),
         sex = case_when(sex_name == "Male"~"1",
                         sex_name == "Female" ~ "2", TRUE~"check"),
         age= case_when(age== "x90_and_over"~ "x90", TRUE~ age),
         age = as.integer(substr(age, 2, nchar(age))),
         pop = as.integer(pop)) %>% 
  left_join(ca_lookup, by = "ca2019") %>% 
  select(year, ca2019, ca2019name, ca2018, ca2011, age, sex, sex_name, pop) %>% 
  arrange(year, ca2019, age, sex)


saveRDS(ca_pop_proj, glue("{lookup_filepath}/CA2019_pop_proj_{start}_{end}.rds"))

rm(ca_lookup, ca_source_data)

### 4.3 - Create file for 5 year age groups ----

# Create age_group column for 5 year age groups
# Group data and summarise by pop to get the population for each age group

ca_pop_proj_5y <- age_group_fun(ca_pop_proj) %>% 
  group_by(year, ca2019, ca2019name, ca2018, ca2011, age_group, age_group_name, 
           sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

saveRDS(ca_pop_proj_5y, 
        glue("{lookup_filepath}/CA2019_pop_proj_5year_agegroups_{start}_{end}.rds"))

rm(ca_proj_f, ca_proj_m)




#### 5.0 - Create Health Board population projections files ----


### 5.2 - Create Council Area population projections by single year of age ----

# Add the male and female files together
# Rename code to hb2019 and join on hb columns from geo_lookup
# Take distinct rows to remove duplicates
# Set age as an integer and create sex_name column
# Select relevant columns and arrange


hb_source_data <- read_excel(source_data_table, sheet = "Table 3",  range = "A6:CR1176")

hb_pop_proj <- hb_source_data %>% 
  clean_names() %>% 
  filter(area_name !="Scotland" & sex !="Persons") %>% 
  rename(year = year_to_30_june, hb2019= area_code) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer( cols = 5:96,  names_to = "age",  values_to = "pop" ) %>% 
  filter(age!= "all_ages") %>% 
  mutate(sex_name =  case_when (sex == "Females" ~"Female",
                                sex =="Males" ~ "Male",  TRUE~ "check"),
         sex = case_when(sex_name == "Male"~"1",
                         sex_name == "Female" ~ "2", TRUE~"check"),
         age= case_when(age== "x90_and_over"~ "x90", TRUE~ age),
         age = as.integer(substr(age, 2, nchar(age))),
         pop = as.integer(pop)) %>% 
  left_join(hb_lookup, by = "hb2019") %>% 
  select(year, hb2019, hb2019name, hb2018, hb2014, age, sex, sex_name, pop) %>% 
  arrange(year, hb2019, age, sex)


saveRDS(hb_pop_proj, 
        glue("{lookup_filepath}/HB2019_pop_proj_{start}_{end}.rds"))



### 5.3 - Create file for 5 year age groups ----
# Create age_group column for 5 year age groups
# Group and summarise by pop to get the population for each age group

hb_pop_proj_5y <- age_group_fun(hb_pop_proj) %>% 
  group_by(year, hb2019, hb2019name, hb2018, hb2014, age_group, age_group_name, 
           sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

saveRDS(hb_pop_proj_5y, 
        glue("{lookup_filepath}/HB2019_pop_proj_5year_agegroups_{start}_{end}.rds"))

rm(hb_proj_f, hb_proj_m)




#### 6.0 - Create HSCP population projection files ----

### 6.1 - Create HSCP population projections by single year of age ----
# Take ca_pop_proj and join hscp columns on from geo_lookup
# Group and summarise by pop to get the population for each age group

hscp_pop_proj <- ca_pop_proj %>% 
  left_join(hscp_lookup, by="ca2019") %>% 
  group_by(year, hscp2019, hscp2019name, hscp2018, hscp2016, age, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

saveRDS(hscp_pop_proj, 
        glue("{lookup_filepath}/HSCP2019_pop_proj_{start}_{end}.rds"))



### 6.2 - Create file for 5 year age groups ----

# Take ca_pop_proj_5y and join hscp columns on from geo_lookup
# Group and summarise by pop to get the population for each age group

hscp_pop_proj_5y <- ca_pop_proj_5y %>% 
  left_join(hscp_lookup, by="ca2019") %>% 
  group_by(year, hscp2019, hscp2019name, hscp2018, hscp2016, age_group, 
           age_group_name, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

saveRDS(hscp_pop_proj_5y, 
        glue("{lookup_filepath}/HSCP2019_pop_proj_5year_agegroups_{start}_{end}.rds"))


#### 7.0 - Check files ----
### 7.1 - Check Scotland files ----
checks(input = scot_pop_proj, geography = "scot", age_column = "age")
checks(input = scot_pop_proj_5y, geography = "scot", age_column = "age_group")

### 7.2 - Check Health Board files ----

checks(input = hb_pop_proj, geography = "hb", age_column = "age")
checks(input = hb_pop_proj_5y, geography = "hb", age_column = "age_group")


### 7.3 - Check HSCP files ----

checks(input = hscp_pop_proj, geography = "hscp", age_column = "age")
checks(input = hscp_pop_proj_5y, geography = "hscp", age_column = "age_group")


### 7.4 - Check Council Area files ----

checks(input = ca_pop_proj, geography = "ca", age_column = "age")
checks(input = ca_pop_proj_5y, geography = "ca", age_column = "age_group")
