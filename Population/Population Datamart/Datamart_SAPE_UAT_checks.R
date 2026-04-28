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

rm(list = ls())

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}

pacman::p_load(phsmethods,
               magrittr,tidyr,readxl, readr, janitor,tidylog, glue, stringr, dplyr,data.table)

           
# Set filepaths & variable
# set Estimate start & end dates 
start <- "2011" #update
end <- "2024" #update
uat_extract <- "SCTASK0615356_CORU_output" # update


source_filepath <- ("//data/geography/Population/Small Area Population estimates/Lookup Files/R Files/")

datamart_filepath <- ("/data/geography/Population/Population Datamart/")


#### UAT extract
uat_filepath <- glue("{datamart_filepath}", "Lookup Files/UAT_test_files/")



#### Import UAT extract ####
uat_test_orig <-read_csv(glue({uat_filepath},{uat_extract},".csv"))

# clean up

uat_test_clean <- uat_test_orig %>% 
  select(geog= POPULATION_NAME, geog=POPULATION_NAME, year= FINANCIAL_YEAR, pop =POPULATION,
         age=AGE_BAND, ca2019=COUNCIL_AREA_CODE_9, gender = GENDER,
         datazone2011= DATAZONE, intzone2011=  INTERMEDIATE_ZONE,
         hb2019= HEALTH_BOARD_CODE_9, hscp2019= HSCP_CODE) 

rm(uat_test_orig)

year_check <- uat_test_clean %>% 
  distinct(year, .keep_all = FALSE) %>% 
  print(n = Inf)
sex_check <- uat_test_clean %>% 
  distinct(gender, .keep_all = FALSE) %>% 
  print(n = Inf)
hb_check <- uat_test_clean %>% 
  distinct(hb2019, .keep_all = FALSE) %>% 
  print(n = Inf)
ca_check <- uat_test_clean %>% 
  distinct(ca2019, .keep_all = FALSE) %>% 
  print(n = Inf)
hscp_check <- uat_test_clean %>% 
  distinct(hscp2019, .keep_all = FALSE) %>% 
  print(n = Inf)
iz_check <- uat_test_clean %>% 
  distinct(intzone2011, .keep_all = FALSE) %>% 
  print(n = Inf)
dz_check <- uat_test_clean %>% 
  distinct(datazone2011, .keep_all = FALSE) %>% 
  print(n = Inf)

rm(year_check,sex_check,hb_check,ca_check,hscp_check,iz_check, dz_check)

### source data   ----
#- data that NSS data are checked against

check_years =c("2016", "2023", "2024")

source_data <- readRDS(glue("{source_filepath}/DataZone2011_pop_est_{start}_{end}.rds"))%>% 
  select(year:age90plus, intzone2011, hb2019, ca2019, hscp2019) %>% 
  pivot_longer(cols = age0:age90plus,
               names_to = "age",
               values_to = "pop")  %>% 
  mutate(age = gsub("age", "", age),
         age = as.numeric(case_when(age == "90plus" ~ "90", TRUE~ age)),
         gender = as.numeric(recode(sex, "M" = "1", "F" = "2"))  ) %>% 
    filter (year %in% check_years) 
  
  
dz_source_total_pop = source_data %>% 
  group_by(datazone2011,year) %>% 
  summarise(source_pop= sum(pop)) 

dz_source_sex_pop= source_data  %>% 
  group_by(datazone2011, year, gender) %>% 
  summarise(source_sex_pop= sum(pop))

dz_source_age_pop= source_data  %>% 
  group_by(datazone2011, year, age) %>% 
  summarise(source_age_pop= sum(pop))

iz_source_total_pop = source_data %>% 
  group_by(intzone2011,year) %>% 
  summarise(source_pop= sum(pop)) 

iz_source_sex_pop= source_data  %>% 
  group_by(intzone2011, year, gender) %>% 
  summarise(source_sex_pop= sum(pop))

iz_source_age_pop= source_data  %>% 
  group_by(intzone2011, year, age) %>% 
  summarise(source_age_pop= sum(pop))

ca_source_total_pop = source_data %>% 
  group_by(ca2019,year) %>% 
  summarise(source_pop= sum(pop)) 

ca_source_sex_pop= source_data  %>% 
  group_by(ca2019, year, gender) %>% 
  summarise(source_sex_pop= sum(pop))

ca_source_age_pop= source_data  %>% 
  group_by(ca2019, year, age) %>% 
  summarise(source_age_pop= sum(pop))

hb_source_total_pop = source_data %>% 
  group_by(hb2019,year) %>% 
  summarise(source_pop= sum(pop)) 

hb_source_sex_pop= source_data  %>% 
  group_by(hb2019, year, gender) %>% 
  summarise(source_sex_pop= sum(pop))

hb_source_age_pop= source_data  %>% 
  group_by(hb2019, year, age) %>% 
  summarise(source_age_pop= sum(pop))

# hscp
hscp_source_total_pop = source_data %>% 
  group_by(hscp2019,year) %>% 
  summarise(source_pop= sum(pop)) 

hscp_source_sex_pop= source_data  %>% 
  group_by(hscp2019, year, gender) %>% 
  summarise(source_sex_pop= sum(pop))

hscp_source_age_pop= source_data  %>% 
  group_by(hscp2019, year, age) %>% 
  summarise(source_age_pop= sum(pop))

rm(source_data)

### DZ checks

# uat CA data
dz_uat_total_pop <- uat_test_clean %>% 
  group_by(year, datazone2011) %>% 
  summarise(uat_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(dz_source_total_pop, by=c("year","datazone2011")) %>% 
  mutate(check = case_when(uat_pop == source_pop ~"total match",
                           TRUE ~ "mismatch - check data")) %>% 
  filter(check== "mismatch- check data") %>% 
  print(n = Inf)

dz_uat_sex_pop <-  uat_test_clean %>% 
  group_by(datazone2011, year, gender) %>% 
  summarise(uat_sex_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(dz_source_sex_pop, by=c("gender", "year","datazone2011")) %>% 
  mutate(check =case_when (uat_sex_pop== source_sex_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check== "mismatch - check data") %>% 
  print(n = Inf)

dz_uat_age_pop <- uat_test_clean %>% 
  group_by(datazone2011, year, age) %>% 
  summarise(uat_age_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(dz_source_age_pop, by=c("age", "year","datazone2011")) %>% 
  mutate(check =case_when (uat_age_pop== source_age_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check == "mismatch - check data") %>% 
  print(n = Inf)


rm(dz_uat_total_pop, dz_uat_sex_pop, dz_uat_age_pop   )
rm(dz_source_total_pop, dz_source_age_pop, dz_source_sex_pop)
 ### end dz checks 
## iz checks 
iz_uat_total_pop <- uat_test_clean %>% 
  group_by(year, intzone2011) %>% 
  summarise(uat_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(iz_source_total_pop, by=c("year","intzone2011")) %>% 
  mutate(check = case_when(uat_pop == source_pop ~"total match",
                           TRUE ~ "mismatch - check data")) %>% 
  filter(check== "mismatch- check data") %>% 
  print(n = Inf)

iz_uat_sex_pop <-  uat_test_clean %>% 
  group_by(intzone2011, year, gender) %>% 
  summarise(uat_sex_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(iz_source_sex_pop, by=c("gender", "year","intzone2011")) %>% 
  mutate(check =case_when (uat_sex_pop== source_sex_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check== "mismatch - check data") %>% 
  print(n = Inf)

iz_uat_age_pop <- uat_test_clean %>% 
  group_by(intzone2011, year, age) %>% 
  summarise(uat_age_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(iz_source_age_pop, by=c("age", "year","intzone2011")) %>% 
  mutate(check =case_when (uat_age_pop== source_age_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check == "mismatch - check data") %>% 
  print(n = Inf)

rm(iz_uat_total_pop, iz_uat_sex_pop, iz_uat_age_pop   )
rm(iz_source_total_pop, iz_source_age_pop, iz_source_sex_pop)

### end iz checks 

# uat CA  checks 
ca_uat_total_pop <- uat_test_clean %>% 
  group_by(year, ca2019) %>% 
  summarise(uat_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(ca_source_total_pop, by=c("year","ca2019")) %>% 
  mutate(check = case_when(uat_pop == source_pop ~"total match",
                           TRUE ~ "mismatch - check data")) %>% 
  filter(check== "mismatch- check data") %>% 
  print(n = Inf)

ca_uat_sex_pop <-  uat_test_clean %>% 
  group_by(ca2019, year, gender) %>% 
  summarise(uat_sex_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(ca_source_sex_pop, by=c("gender", "year","ca2019")) %>% 
  mutate(check =case_when (uat_sex_pop== source_sex_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check== "mismatch - check data") %>% 
  print(n = Inf)

ca_uat_age_pop <- uat_test_clean %>% 
  group_by(ca2019, year, age) %>% 
  summarise(uat_age_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(ca_source_age_pop, by=c("age", "year","ca2019")) %>% 
  mutate(check =case_when (uat_age_pop== source_age_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check == "mismatch - check data") %>% 
  print(n = Inf)

rm(ca_uat_total_pop, ca_uat_sex_pop, ca_uat_age_pop )
rm(ca_source_total_pop, ca_source_age_pop, ca_source_sex_pop)


#### end CA checks ####
### HB projections Checks  ----

#source data - data that NSS data are checked against

hb_uat_total_pop <- uat_test_clean %>% 
  group_by(year, hb2019) %>% 
  summarise(uat_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(hb_source_total_pop, by=c("year","hb2019")) %>% 
  mutate(check = case_when(uat_pop == source_pop ~"total match",
                           TRUE ~ "mismatch - check data")) %>% 
  filter(check== "mismatch- check data") %>% 
  print(n = Inf)

hb_uat_sex_pop <-  uat_test_clean %>% 
  group_by(hb2019, year, gender) %>% 
  summarise(uat_sex_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(hb_source_sex_pop, by=c("gender", "year","hb2019")) %>% 
  mutate(check =case_when (uat_sex_pop== source_sex_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check== "mismatch - check data") %>% 
  print(n = Inf)

hb_uat_age_pop <- uat_test_clean %>% 
  group_by(hb2019, year, age) %>% 
  summarise(uat_age_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(hb_source_age_pop, by=c("age", "year","hb2019")) %>% 
  mutate(check =case_when (uat_age_pop== source_age_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check == "mismatch - check data") %>% 
  print(n = Inf)

rm(hb_uat_total_pop, hb_uat_sex_pop, hb_uat_age_pop )
rm(hb_source_total_pop, hb_source_age_pop, hb_source_sex_pop)


#### end HB checks ####
### HSCP projections Checks  ----

hscp_uat_total_pop <- uat_test_clean %>% 
  group_by(year, hscp2019) %>% 
  summarise(uat_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(hscp_source_total_pop, by=c("year","hscp2019")) %>% 
  mutate(check = case_when(uat_pop == source_pop ~"total match",
                           TRUE ~ "mismatch - check data")) %>% 
  filter(check== "mismatch- check data") %>% 
  print(n = Inf)

hscp_uat_sex_pop <-  uat_test_clean %>% 
  group_by(hscp2019, year, gender) %>% 
  summarise(uat_sex_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(hscp_source_sex_pop, by=c("gender", "year","hscp2019")) %>% 
  mutate(check =case_when (uat_sex_pop== source_sex_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check== "mismatch - check data") %>% 
  print(n = Inf)

hscp_uat_age_pop <- uat_test_clean %>% 
  group_by(hscp2019, year, age) %>% 
  summarise(uat_age_pop= sum(pop)) %>% 
  ungroup() %>% 
  left_join(hscp_source_age_pop, by=c("age", "year","hscp2019")) %>% 
  mutate(check =case_when (uat_age_pop== source_age_pop ~ "match",
                           TRUE~"mismatch - check data"))%>% 
  filter(check == "mismatch - check data") %>% 
  print(n = Inf)

rm(hscp_uat_total_pop, hscp_uat_sex_pop, hscp_uat_age_pop )
rm(hscp_source_total_pop, hscp_source_age_pop, hscp_source_sex_pop)
rm(uat_test_clean)
