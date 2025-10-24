##########################################################
# CHECK OF DATAMART UAT OUTPUT 
#Iain MacKinnon EDITED / AMENDED BY:
# Original date 14/09/2022
# Latest update author - Iain MacKinnon
# Latest update date - 14/09/2022
# Latest update description 
#
# 
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.6.1
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

base_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                    "2_Population")

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

sape_ext <- read_csv(glue("{extracts}/population_estimates_14092022.txt"))

uat_data <- read_csv("//freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Population Datamart/Lookup Files/Other Geographies/POPULATION_DATAZONE2011_ESTIMATES_2021_20220905.csv", 
                     skip = 1)
uat_data <- uat_data %>% 
  filter(`Population Name`!="Y")



### 2.0  age checks ####
#check age ranges of al dataframes  
# Answer:0-90
age_check<-sape_ext %>% 
  summarise(ages = unique(AGE_BAND))

# check age ranges of rows with Null geography
# Answer: 0-90
age_blnks<-sape_ext %>% 
  filter(is.na(HEALTH_BOARD_CODE_9))%>%  
  summarise(ages = unique(AGE_BAND)) %>%
  arrange(ages)


# age range of data that was supplied to NSS BI 
# Answer: 0
age_check_uat<-uat_data %>% 
  summarise(ages = unique(`Age Band`))

# check age ranges of nblank cells in UAT
# Answer:data provided had no blanks  
age_blnks_uat<-uat_data %>% 
  filter(!is.na("Age Band"))%>% 
  summarise(ages = unique("Age Band"))

####pop totals by admins blank and non-blank #####

# pop no supplied by GPD to UAT
# Answer= 5,479,900
tot_pop_uat<-uat_data %>% 
  mutate(Population=as.numeric(Population)) %>% 
  summarise(uat_pop= sum(Population))

# are there any blank POPULATION entries in extract? 
# Answer: None
POP_blanks<-sape_ext %>%
  filter(is.na(POPULATION))

# sum of population in entire extract (no filtering) 
# Answer= 5,479,900
tot_pop<-sape_ext %>% 
  summarise(ext_pop= sum(POPULATION))

# pop no where admin is not NULL
# Answer=  = 5,479,900
tot_pop_no_blanks<-sape_ext %>%
  filter(!is.na(HEALTH_BOARD_CODE_9)) %>% 
  summarise(geog_pop= sum(POPULATION))

#pop where admins is Null
# Answer = 0
tot_pop_blanks<-sape_ext %>%
  filter(is.na(HEALTH_BOARD_CODE_9)) %>% 
  summarise(blank_pop= sum(POPULATION))


### admin number checks###

# number of data zones 
# Answer = 6977
dz_no<-sape_ext %>% 
  summarise(ages = unique(DATAZONE))

# number of data zones that are not blank
# Answer = 6976
dz_no_blanks<-sape_ext %>% 
    filter(!is.na(DATAZONE))%>% 
  summarise(ages = unique(DATAZONE))

# number of IZ not blank
# Answer = 1279
iz_no_blanks<-sape_ext %>% 
  filter(!is.na(INTERMEDIATE_ZONE))%>% 
  summarise(ages = unique(INTERMEDIATE_ZONE))

# number of HB that are not blank 
# Answer = 14
hb_no_blanks<-sape_ext %>% 
  filter(!is.na(HEALTH_BOARD_CODE_9))%>% 
  summarise(ages = unique(HEALTH_BOARD_CODE_9))

# number of CA19 that are not blank
# Answer = 32
ca_no_blanks<-sape_ext %>% 
  filter(!is.na(COUNCIL_AREA_CODE_9))%>% 
  summarise(ages = unique(COUNCIL_AREA_CODE_9))

# number of hscp that are not blank 
# Answer = 31
hscp_no_blanks<-sape_ext %>% 
  filter(!is.na(HSCP_CODE))%>% 
  summarise(ages = unique(HSCP_CODE))