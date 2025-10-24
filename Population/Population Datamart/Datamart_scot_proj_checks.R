##########################################################
# CHECK OF DATAMART UAT OUTPUT 
#Iain MacKinnon EDITED / AMENDED BY:
# Original date 14/09/2022
# Latest update author - Iain MacKinnon
# Latest update date - 14/09/2022
# Latest update description 
# suspect that this pop estimates was from the CA file creation
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

base_filepath <- ("/data/geography/Population/Population Projections/")

extracts <- glue("{base_filepath}/Population Datamart/Lookup Files/", 
                          "Other Geographies/uat_extracts")
other_geog<-glue("{base_filepath}/Population Datamart/Lookup Files/", 
                "UAT_test_files")

# potential checks
# check age range : 0-90
# check unique number of dz, iz, etc
# check pop grouped by dz, iz, etc
# check for blanks or na in pops - no nas
# check 2 genders- check

#### load extract  ####
# the extracts are in different text format, until standardised I've used the Import
# dataset to sort the formatting issues

uat_ext <- read_delim("//freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Population Datamart/Lookup Files/Other Geographies/uat_extracts/population_projections.txt", 
                      "|", escape_double = FALSE, trim_ws = TRUE)


uat_data <- read_rds('//freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/2_Population/Population Projections/Lookup Files/R Files/scot_pop_proj_2020_2045.rds')



uat_data <- uat_data %>% 
  filter(Gender!='Y')

### 2.0  age checks ####
#check age ranges of al dataframes  
# Answer should be:0-90
age_check<-uat_ext %>% 
  summarise(ages = unique(AGE_BAND))%>%
  arrange(ages)
# check age ranges of rows with Null geography
# Answer: Using council codes set of blanks with pop values 
age_blnks<-uat_ext  %>% 
  filter(is.na(FINANCIAL_YEAR))%>%  
  summarise(ages = sum(POPULATION)) 

# check age ranges of nblank cells in UAT
# Answer:data provided had no blanks  
age_blnks_uat<-uat_data %>% 
  filter(!is.na("Age Band"))%>% 
  summarise(ages = unique("Age Band"))

#years year ranges   
# Answer should be:25 years from first year
# answer = 25 sequential years
year_check<-uat_ext %>% 
  summarise(years = unique(FINANCIAL_YEAR))%>%
  arrange(years)


# Choose a specific year and check that the totals by health board and sex match

#UAT extract POP in 2030 =5481527
one_year_pop_ext <-uat_ext %>% 
  filter(FINANCIAL_YEAR==2030) %>% 
  summarise(pop = sum(POPULATION))

  one_year_pop_uat<-uat_data %>% 
  filter(year ==2030) %>% 
   # group_by(hb2014name, sex) %>% 
    summarise(tot_pop = sum(pop))

