##########################################################
# CHECK OF DATAMART UAT OUTPUT 
# EDITED / AMENDED BY: Iain MacKinnon, Gerald Leung 
# Original date 14/09/2022
# Latest update author - Gerald Leung
# Latest update date - 25/04/2024
# Latest update description 
# UAT check for 2022 mid year estimates. BI team provided extract in a different format and only contained columns we sent
# (e.g no dz, iz columns). Original codes included for testing purposes - you do not have to run them if you don't wish to
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 4.0.2
# Purpose is to run a few simple checks of an Oracle extract provided by the BI
# team following upload to the UAT environment. Once checks are complete inform 
# and acceptable inform NSS BI that to upload to Production (See SOP)

##########################################################


### 1 - Housekeeping ----


library(magrittr)
library(tidyr)
library(dplyr)
library(readxl)
library(tidylog)
library(janitor)
library(glue)
library(data.table)
library(readr)

           
# Set filepaths
#base_filepath <- glue("/data/geography", 
#                "/Population/Population Estimates") # if on Posit



base_filepath <- glue("//stats/geography", 
                      "/Population")

#base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
 #                   "2_Population")

extracts <- glue("{base_filepath}/Population Datamart/Lookup Files/", 
                          "Other Geographies/uat_extracts")

other_geog<-glue("{base_filepath}/Population Datamart/Lookup Files/", 
                "Other Geographies")

# potential checks
# check age range : 0-90
# check unique number of dz, iz, etc
# check pop grouped by dz, iz, etc
# check for blanks or na in pops - no nas
# check 2 genders- check

#### load extract  ####
# the extracts are in different text format, until standardised I've used the Import
# dataset to sort the formatting issues

# old_uat_ext <- read_delim ("//stats/geography/Population/Population Datamart/Lookup Files/Other Geographies/uat_extracts/population_estimates.txt",
#                       "|", escape_double = FALSE, trim_ws = TRUE)

old_uat_ext <- read_csv("/data/geography/Population/Population Datamart/Lookup Files/Other Geographies/uat_extracts/SCTASK0536351_2023.csv")

# uat_ext <- read_delim ("//stats/geography/Population/Population Datamart/Lookup Files/Other Geographies/uat_extracts/2022_estimates_extract.txt", 
#                            ",", escape_double = FALSE, trim_ws = TRUE)

uat_ext <- read_csv("/data/geography/Population/Population Datamart/Lookup Files/Other Geographies/uat_extracts/CORU_extracts_SCTASK0586852_pop_2024.csv")


#uat_ext <- read_delim ("//nssdrdb02.csa.scot.nhs.uk/corufiles/business_files/populations/extracts/2022_estimates_extract.txt", 
 #                      "|", escape_double = FALSE, trim_ws = TRUE)

# 2021_20220715 in loaded folder
# old_uat_data <- read_csv("//stats/geography/Population/Population Datamart/Lookup Files/Other Geographies/POPULATION_CA_ESTIMATES_2021_20230405.csv", 
#                                                   skip = 1)
# 
# uat_data <- read_csv("//stats/geography/Population/Population Datamart/Lookup Files/Other Geographies/POPULATION_CA_ESTIMATES_2022_20240401.csv", 
#                      skip = 1)

# 2023 old UAT data
old_uat_data <- read_csv("/data/geography/Population/Population Datamart/Lookup Files/Other Geographies/POPULATION_CA_ESTIMATES_2023_20241015.csv",
                                                  skip = 1)

uat_data <- read_csv("/data/geography/Population/Population Datamart/Lookup Files/Other Geographies/POPULATION_CA_ESTIMATES_2024_20250827.csv",
                     skip = 1)


uat_data<-uat_data %>% 
filter(`Population Name`!="Y") 

### 2.0  age checks ####
#check age ranges of al dataframes  
# Answer should be:0-90

oldage_check<-old_uat_ext %>% 
  summarise(ages = unique(AGE_BAND))

age_check<-uat_ext %>% 
  summarise(ages = unique(AGE_BAND))

rm(oldage_check)

# check age ranges of rows with Null geography
# Answer: Using council codes set of blanks with pop values 
# GL: should also show 0-90. This checks agebands of non-CA populations?

oldage_blnks<-old_uat_ext  %>% 
  filter(is.na(COUNCIL_AREA_CODE_9))%>%  
  summarise(ages = unique(AGE_BAND)) %>%
  arrange(ages)

age_blnks<-uat_ext  %>% 
  filter(is.na(COUNCIL_AREA_CONFIG_CODE_9))%>%  
  summarise(ages = unique(AGE_BAND)) %>%
  arrange(ages)

rm(oldage_blnks)

# age range of data that was supplied to NSS BI 
# Answer: 0-90
#age_check_uat<-uat_data %>% 
 # summarise(ages = unique(`Age Band`))

# check age ranges of blank cells in UAT
# Answer:data provided had no blanks
# GL: This should be checking if there are any blank age bands
# Original script only checks previously loaded data. Added extra
# lines to also check the extract
oldage_blnks_uat<-old_uat_data %>% 
  filter(!is.na("Age Band"))%>% 
  summarise(ages = unique("Age Band"))

rm(oldage_blnks_uat)

# This is the CSV we provided
age_blnks_uat<-uat_data %>% 
  filter(!is.na("Age Band"))%>% 
  summarise(ages = unique("Age Band"))

# Current extract
ext_age_blnks_uat<-uat_ext %>% 
  filter(!is.na("Age Band"))%>% 
  summarise(ages = unique("Age Band"))

####pop totals by admins blank and non-blank #####

# pop no supplied by GPD to UAT
# 2021 Answer= 5,479,900; 2022 answer for tot_pop_uat = 5447700
# GL: added na.rm = TRUE as previous code only returned NA caused by first row of data
# Again also added codes to check extract since previous codes only checked loaded data
# However, this extract contains all 5 populations (CA, HB, HBevent1, HBevent2, HSCP),
# the total you get would be 27238500. Divide this by 5, you will get 5447700. Unsure if  this is a good check!
# Can possibly do this separately for each population?
oldtot_pop_uat<-old_uat_data %>% 
 mutate(Population=as.numeric(Population)) %>% 
 summarise(uat_pop= sum(Population, na.rm = TRUE))
print(oldtot_pop_uat$uat_pop)
rm(oldtot_pop_uat)

tot_pop_uat<-uat_data %>% 
  mutate(Population=as.numeric(Population)) %>% 
  summarise(uat_pop= sum(Population, na.rm=TRUE))
print(tot_pop_uat$uat_pop)
rm(tot_pop_uat)


ext_tot_pop_uat<-uat_ext %>% 
  mutate(Population=as.numeric(POPULATION)) %>% 
  summarise(uat_pop= sum(POPULATION, na.rm=TRUE))

  print(ext_tot_pop_uat$uat_pop)
  rm(ext_tot_pop_uat)
  
# are there any blank POPULATION entries in extract? 
# Answer: None
oldPOP_blanks<-old_uat_ext %>%
  filter(is.na(POPULATION))

rm(oldPOP_blanks)

POP_blanks<-uat_ext %>%
  filter(is.na(POPULATION))

rm(POP_blanks)

# sum of population in entire extract (no filtering) 
# 2021 Answer= 27999500 (GL: 27399500?)
oldtot_pop<-old_uat_ext %>% 
  summarise(ext_pop= sum(POPULATION))
print(oldtot_pop$ext_pop)

rm(oldtot_pop)

# Answer = 27238500, consistent with the previous check
tot_pop<-uat_ext %>% 
  summarise(ext_pop= sum(POPULATION))
print(tot_pop$ext_pop)
rm(tot_pop)

# pop no where admin is not NULL
# Answer=  = 5,479,900
oldtot_pop_no_blanks<-old_uat_ext %>%
  filter(!is.na(COUNCIL_AREA_CODE_9)) %>% 
  summarise(geog_pop= sum(POPULATION))
print(oldtot_pop_no_blanks$geog_pop)
rm(oldtot_pop_no_blanks)


# 2022: 5447700
tot_pop_no_blanks<-uat_ext %>%
  filter(!is.na(COUNCIL_AREA_CONFIG_CODE_9)) %>% 
  summarise(geog_pop= sum(POPULATION))

print(tot_pop_no_blanks$geog_pop)
rm(tot_pop_no_blanks)

#pop where admins is Null 
# GL: this is checking all non-CA populations
# Answer = 21919600
oldtot_pop_blanks<-old_uat_ext %>%
  filter(is.na(COUNCIL_AREA_CODE_9)) %>% 
  summarise(blank_pop= sum(POPULATION))

print(oldtot_pop_blanks$blank_pop)
rm(oldtot_pop_blanks)

# 2022: should get 21790800 
# From previous section total population is 27238500
# Subtract 5447700 (since we dont count CA population here) indeed we get 21790800
tot_pop_blanks<-uat_ext %>%
  filter(is.na(COUNCIL_AREA_CONFIG_CODE_9)) %>% 
  summarise(blank_pop= sum(POPULATION))
print(tot_pop_blanks$blank_pop)
rm(tot_pop_blanks)


# at this point probably too many variables. May consider removing some 

### admin number checks###

# number of data zones 
# Answer = null
olddz_no<-old_uat_ext %>% 
  summarise(ages = unique(DATAZONE))

# 2022 extract no datazone column
#dz_no<-uat_ext %>% 
 # summarise(ages = unique(DATAZONE))

# number of data zones that are not blank
# Answer = null
olddz_no_blanks<-old_uat_ext %>% 
    filter(!is.na(DATAZONE))%>% 
  summarise(ages = unique(DATAZONE))

# 2022 extract no datazone column 
#dz_no_blanks<-uat_ext %>% 
 # filter(!is.na(DATAZONE))%>% 
  #summarise(ages = unique(DATAZONE))


# number of IZ not blank
# Answer = null
oldiz_no_blanks<-old_uat_ext %>% 
  filter(!is.na(INTERMEDIATE_ZONE))%>% 
  summarise(ages = unique(INTERMEDIATE_ZONE))

# 2022 extract no iz
#iz_no_blanks<-uat_ext %>% 
 # filter(!is.na(INTERMEDIATE_ZONE))%>% 
  #summarise(ages = unique(INTERMEDIATE_ZONE))

# number of HB that are not blank 
# Answer = 14
oldhb_no_blanks<-old_uat_ext %>% 
  filter(!is.na(HEALTH_BOARD_CODE_9))%>% 
  summarise(ages = unique(HEALTH_BOARD_CODE_9))
print(oldhb_no_blanks$ages)
rm(oldhb_no_blanks)

hb_no_blanks<-uat_ext %>% 
  filter(!is.na(HEALTH_BOARD_CODE_9))%>% 
  summarise(ages = unique(HEALTH_BOARD_CODE_9))

rm(hb_no_blanks)

# number of CA19 that are not blank
# Answer = 32
oldca_no_blanks<-old_uat_ext %>% 
  filter(!is.na(COUNCIL_AREA_CODE_9))%>% 
  summarise(ages = unique(COUNCIL_AREA_CODE_9))
rm(oldca_no_blanks)

ca_no_blanks<-uat_ext %>% 
  filter(!is.na(COUNCIL_AREA_CONFIG_CODE_9))%>% 
  summarise(ages = unique(COUNCIL_AREA_CONFIG_CODE_9))

rm(ca_no_blanks)
# number of CA19 that are blank
# Answer = 1 with 13286 rows of null CA
oldca_no_blanks<-old_uat_ext %>% 
  filter(is.na(COUNCIL_AREA_CODE_9))#%>% 
  #summarise(ages = unique(COUNCIL_AREA_CONFIG_CODE_9))

ca_no_blanks<-uat_ext %>% 
  filter(is.na(COUNCIL_AREA_CONFIG_CODE_9))

# number of hscp that are not blank 
# Answer = 31
oldhscp_no_blanks<-old_uat_ext %>% 
  filter(!is.na(HSCP_CODE))%>% 
  summarise(ages = unique(HSCP_CODE))

hscp_no_blanks<-uat_ext %>% 
  filter(!is.na(HSCP_CODE))%>% 
  summarise(ages = unique(HSCP_CODE))
