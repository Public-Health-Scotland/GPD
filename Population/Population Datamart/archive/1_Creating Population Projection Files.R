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
# library(haven)
# library(magrittr)
# library(tidyr)
# library(readxl)
# library(readr)
# library(janitor)
# library(tidylog)
# library(glue)
# library(stringr)
# library(dplyr)
# library(data.table)

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}

pacman::p_load(haven, #still needed?
               magrittr,tidyr,readxl, readr, janitor,tidylog, glue, stringr, dplyr,data.table)


### 1.2  Set filepaths ######

base_filepath <- ("//stats/geography/Population/Population Projections")

#base_filepath <- glue("F:/PHI/Referencing & Standards/GPD/", 
#                      "2_Population/Population Projections")
lookup_filepath <- glue("{base_filepath}/Lookup Files/R Files")
data_filepath <- glue("{base_filepath}/Source Data")

### 1.3 Set files to use #####
#updates to current years span
scot_pop_data <- glue("{data_filepath}/scot_pop_proj_2020_2045.xlsx")
# ca_pop_data <- glue("{data_filepath}/ca_pop_proj_2018_2043.xlsx") #not update
# hb_pop_data <- glue("{data_filepath}/hb_pop_proj_2018_2043.xlsx") #not updated

#unclear why taking this from cl-out and others taken from Freddy. IM
# updated to 2020 version
 # geo_lookup_file <- glue("//Isdsf00d03/cl-out/lookups/Unicode/Geography/",
 #                     "HSCP Locality/HSCP Localities_DZ11_Lookup_20220630.rds")

start <- "2020" #update
end <- "2045" #update

# Read in geography names look up
# This is used for Council Area, Health Board and HSCP Files

# geo_lookup <- readRDS(geo_lookup_file) %>%
#   select(ca2019, ca2019name, ca2018, ca2011, hscp2019, hscp2019name, hscp2018, 
#          hscp2016, hb2019, hb2019name, hb2018, hb2014) %>%
#   distinct()


#IM addition to reference what it should ;looklike
#prev_lookup<- readRDS("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Population Projections/Lookup Files/R Files/scot_pop_proj_2018_2043.rds")



##### 2.0  Set functions #####
### 2.1 Create Scotland function to read in the data ####

# wasn't used for Scotland projections due to formatting issues with NRS spreadsheet
# retain to see if it can be fixed or used for sub-national creation
# 2018 to 2043 NRS table set-up is different to the 2020-2045.  
 #scot_pop <- function(filepath, cells, gender, start_year, end_year){
  
#  scot_proj <- read_xlsx(path = filepath, sheet = "Table 2", # changed sheet from Principal to Table 2 IM
#                         range = cells)
  
  # Rename variables
 # names(scot_proj) <- c("Age", start_year:end_year)
  
#  scot_proj %>% 
 #   gather("year", "Pop", start_year:end_year) %>% 
  #  mutate(sex = gender) %>% 
   # filter(Age != "All ages") %>% #does this remove age 0 cohort? IM
  #  mutate(Age = as.numeric(Age))
#}


### 2.2 Create lower geography population function to read in the data####

# lower_geo_pop <- function(filepath, cells, gender, start_year, end_year){
#   
#   # Read in the sheet names within the excel file
#   sheet_names <- excel_sheets(path = filepath)
#   
#   # Remove unnecessary names - only keep years
#   sheet_names <- sheet_names[5:30]
#   
#   # Read in the data for each year using sheet_names
#   list_all <- lapply(sheet_names, 
#                      function(i) read_xlsx(path = filepath, sheet = i, 
#                                            range = cells))
#   
#   # Add a column for sex to each year's data and create a year column
#   lower_geo <- mapply(cbind, list_all, "sex" = gender, 
#                       "Year" = seq.int(start_year, end_year), SIMPLIFY = F)
#   
#   # Turn the list into a dataframe
#   lower_geo_proj <- do.call(rbind.data.frame, lower_geo)
#   
#   lower_geo_proj %<>% 
#     rename("90"= "90+") %>% 
#     gather("Age", "Pop", 'All ages':'90') %>% 
#     filter(Area != "SCOTLAND" & Age != "All ages")
# }
# 

### 2.3 Age Group Function ####

age_group_fun <- function(data){
  
  data %<>%
    mutate(age_group = case_when(age == 0 ~ 0, 
                                 age >= 1 & age <= 4 ~ 1, 
                                 age >= 5 & age <= 9 ~ 2, 
                                 age >= 10 & age <= 14 ~ 3, 
                                 age >= 15 & age <= 19 ~ 4, 
                                 age >= 20 & age <= 24 ~ 5, 
                                 age >= 25 & age <= 29 ~ 6, 
                                 age >= 30 & age <= 34 ~ 7, 
                                 age >= 35 & age <= 39 ~ 8, 
                                 age >= 40 & age <= 44 ~ 9, 
                                 age >= 45 & age <= 49 ~ 10, 
                                 age >= 50 & age <= 54 ~ 11, 
                                 age >= 55 & age <= 59 ~ 12, 
                                 age >= 60 & age <= 64 ~ 13, 
                                 age >= 65 & age <= 69 ~ 14, 
                                 age >= 70 & age <= 74 ~ 15, 
                                 age >= 75 & age <= 79 ~ 16, 
                                 age >= 80 & age <= 84 ~ 17, 
                                 age >= 85 & age <= 89 ~ 18, 
                                 age >= 90 ~ 19), 
           age_group_name = case_when(age_group == 0 ~ "0", 
                                      age_group == 1 ~ "1-4", 
                                      age_group == 2 ~ "5-9", 
                                      age_group == 3 ~ "10-14", 
                                      age_group == 4 ~ "15-19", 
                                      age_group == 5 ~ "20-24", 
                                      age_group == 6 ~ "25-29", 
                                      age_group == 7 ~ "30-34", 
                                      age_group == 8 ~ "35-39", 
                                      age_group == 9 ~ "40-44", 
                                      age_group == 10 ~ "45-49", 
                                      age_group == 11 ~ "50-54", 
                                      age_group == 12 ~ "55-59", 
                                      age_group == 13 ~ "60-64", 
                                      age_group == 14 ~ "65-69", 
                                      age_group == 15 ~ "70-74", 
                                      age_group == 16 ~ "75-79", 
                                      age_group == 17 ~ "80-84", 
                                      age_group == 18 ~ "85-89", 
                                      age_group == 19 ~ "90+"))
  
}

### 2.4 Check functions ####

checks <- function(input, geography, age_column){
  
  # Check that all years of the population projections are there 
  # Check that there are no missing values
  # Check all years have the same % of records
  
  input %>% group_by(year) %>% count() %>% print(n = Inf)
  
  # Check that all ages 0 to 90+ are there
  # Check there are no missing values
  # Check all ages have the same % of records
  
  input %>% group_by({{age_column}}) %>% count() %>% print(n = Inf)
  
  # Check that both males and females are there
  # Check there are no missing values
  # Check both sexes have the same % of records (50/50)
  
  input %>% group_by(sex) %>% count() %>% print(n = Inf)
  
  # Check Scotland totals against NRS source data
  
  input %>%
    group_by(year) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()
  
  # Check that the population values are as expected
  # i.e. no negative values or extremely high values etc
  
  input %>%
    group_by(pop) %>%
    count() %>%
    arrange(n) %>%
    filter(n <= 0 | n >= 250)
  
  if(geography == "scot"){
    
  } else if (geography == "hb"){
    
    # Check that all 14 Health Boards are there
    # Check there are no missing values
    # Check all HBs have the same % of records
    
    input %>% group_by(hb2019) %>% count() %>% print(n = Inf)
    input %>% group_by(hb2018) %>% count() %>% print(n = Inf)
    input %>% group_by(hb2014) %>% count() %>% print(n = Inf)
    
  } else if (geography == "hscp"){
    
    # Check that all 31 HSCPs are there
    # Check there are no missing values
    # Check all HSCPs have the same % of records
    
    input %>% group_by(hscp2019) %>% count() %>% print(n = Inf)
    input %>% group_by(hscp2018) %>% count() %>% print(n = Inf)
    input %>% group_by(hscp2016) %>% count() %>% print(n = Inf)
    
  } else if (geography == "ca"){
    
    # Check that all 32 Council Areas are there
    # Check there are no missing values
    # Check all CAs have the same % of records
    
    input %>% group_by(ca2019) %>% count() %>% print(n = Inf)
    input %>% group_by(ca2018) %>% count() %>% print(n = Inf)
    input %>% group_by(ca2011) %>% count() %>% print(n = Inf)
    
  } else {
    
    print("Define correct projections data")
    
  }
  
}


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

# Create the male population projections

ca_proj_m <- lower_geo_pop(ca_pop_data, 
                           cells = "A44:CP77", gender = 1, 
                           start_year = start, 
                           end_year = end) %>% 
  clean_names()

# Create the female population projections

ca_proj_f <- lower_geo_pop(ca_pop_data, 
                           cells = "A83:CP116", gender = 2, 
                           start_year = start, 
                           end_year = end) %>% 
  clean_names()


### 4.2 - Create Council Area population projections by single year of age ----

# Add the male and female files together
# Rename code to ca2019 and join on ca columns from geo_lookup
# Set age as an integer and create sex_name column
# Select relevant columns and arrange

ca_pop_proj <- bind_rows(ca_proj_m, ca_proj_f) %>% 
  rename(ca2019 = code) %>% 
  left_join(select(geo_lookup, ca2019, ca2019name, ca2018, ca2011)) %>% 
  mutate(age = as.integer(age),
         sex_name = recode(sex, "1" = "Male", "2" = "Female")) %>% 
  select(year, ca2019, ca2019name, ca2018, ca2011, age, sex, sex_name, pop) %>% 
  arrange(year, ca2019, age, sex)

saveRDS(ca_pop_proj, glue("{lookup_filepath}/CA2019_pop_proj_{start}_{end}.rds"))



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

### 5.1 - Use lower geography population function to read in data ----

# Create the male population projections

hb_proj_m <- lower_geo_pop(hb_pop_data, cells = "A26:CP41", gender = 1, 
                           start_year = start, end_year = end) %>% 
  clean_names()

# Create the female population projections

hb_proj_f <- lower_geo_pop(hb_pop_data, cells = "A47:CP62", gender = 2, 
                           start_year = start, end_year = end) %>% 
  clean_names()



### 5.2 - Create Council Area population projections by single year of age ----

# Add the male and female files together
# Rename code to hb2019 and join on hb columns from geo_lookup
# Take distinct rows to remove duplicates
# Set age as an integer and create sex_name column
# Select relevant columns and arrange

hb_pop_proj <- bind_rows(hb_proj_m, hb_proj_f) %>% 
  rename(hb2019 = code) %>% 
  left_join(select(geo_lookup, hb2019, hb2019name, hb2018, hb2014)) %>% 
  distinct() %>% 
  mutate(age = as.integer(age), 
         sex_name = recode(sex, "1" = "Male", "2" = "Female")) %>% 
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
  left_join(select(geo_lookup, ca2019, hscp2019, hscp2019name, hscp2018, 
                   hscp2016)) %>% 
  group_by(year, hscp2019, hscp2019name, hscp2018, hscp2016, age, sex, sex_name) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

saveRDS(hscp_pop_proj, 
        glue("{lookup_filepath}/HSCP2019_pop_proj_{start}_{end}.rds"))



### 6.2 - Create file for 5 year age groups ----

# Take ca_pop_proj_5y and join hscp columns on from geo_lookup
# Group and summarise by pop to get the population for each age group

hscp_pop_proj_5y <- ca_pop_proj_5y %>% 
  left_join(select(geo_lookup, ca2019, hscp2019, hscp2019name, hscp2018, 
                   hscp2016)) %>% 
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
