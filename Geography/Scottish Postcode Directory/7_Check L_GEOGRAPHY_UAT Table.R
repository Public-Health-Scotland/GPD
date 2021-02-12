##########################################################
# Check L_GEOGRAPHY_UAT Table
# Csilla Scharle
# 05/01/2021
# Written/run on RStudio Server Pro
# Version of R that the script was most recently run on - 3.6.1
# Code for checking the L_GEOGRAPHY_UAT table extract
##########################################################



### 1 Housekeeping ----

library(magrittr)
library(dplyr)
library(janitor)
library(tidylog)
library(stringr)
library(readr)
library(glue)
library(lubridate)

# Set filepath

cdw_filepath <- file.path("//conf", "CDW_dimension_extracts")

# Set APXU Connection
APXU_connection <- odbc::dbConnect(
  drv = odbc::odbc(),
  dsn = "APXU",
  uid = rstudioapi::askForPassword("APXU Username:"),
  pwd = rstudioapi::askForPassword("APXU Password:"))



### 2 Check Geography codes ----

# Read in l_geography_uat file
# note column names missing from csv.gs
# order by postcode

geo_cdw <- read_csv(glue("{cdw_filepath}", "/l_geography_uat.csv.gz"), col_names = FALSE) %>% 
  set_colnames(c("POSTCODE", "SOURCE_FLAG", "INTRODUCTION_DATE", 
                 "DELETION_DATE", "COUNCIL_AREA_CODE", "COUNCIL_AREA_CODE_9", 
                 "COUNCIL_AREA_NAME", "COUNCIL_AREA_CODE_CURR", 
                 "COUNCIL_AREA_CODE_9_CURR", "COUNCIL_AREA_NAME_CURR", 
                 "HEALTH_BOARD_CODE", "HEALTH_BOARD_CODE_9", "HEALTH_BOARD_NO", 
                 "HEALTH_BOARD_NAME", "HEALTH_BOARD_CODE_9_CURR", 
                 "HEALTH_BOARD_NO_CURR", "HEALTH_BOARD_NAME_CURR", 
                 "SUB_HEALTH_BOARD_CODE", "CHP_CODE", "CHP_NAME", 
                 "CHP_CODE_CURR", "CHP_NAME_CURR", "HSCP_CODE", "HSCP_NAME", 
                 "HSCP_CODE_CURR", "HSCP_NAME_CURR", "CDW_INSERT_DATE", 
                 "CDW_UPDATE_DATE", "START_DATE", "END_DATE", 
                 "CURRENT_RECORD_FLAG", "COUNCIL_AREA", "COUNCIL_AREA_DESC", 
                 "COUNCIL_AREA_CURRENT", "COUNCIL_AREA_DESC_CURRENT", 
                 "HEALTH_BOARD_OF_RESIDENCE", "HEALTH_BOARD_OF_RESIDENCE_NO", 
                 "HEALTH_BOARD_OF_RESIDENCE_DESC", "HEALTH_BOARD_AREA", 
                 "UPDATED_ON")) %>% 
  arrange(POSTCODE)

# Remove postcodes from before 1900, these are not valid for testing

geo_cdw %<>%
  mutate(START_DATE = as.Date(START_DATE, "%d-%m-%Y"), 
         END_DATE = as.Date(END_DATE, "%d-%m-%Y")) %>% 
  filter(END_DATE >= as.Date("1900-01-01"))

# 7 entries for 1V360XZ, source ACND, NAs and dates only - 1900-01-01

### check 9-digit geography codes: 

# X6 - S12 
# x9 - S12 
# x12 - S08 
# x15 - S08
# x19 - S03
# x21 - S03
# x23 - S37
# x25 - S37

gss_cols <- c("COUNCIL_AREA_CODE_9", "COUNCIL_AREA_CODE_9_CURR", 
              "HEALTH_BOARD_CODE_9", "HEALTH_BOARD_CODE_9_CURR", 
              "CHP_CODE", "CHP_CODE_CURR", "HSCP_CODE", "HSCP_CODE_CURR")

for (i in gss_cols){
  
  geo_cdw %>% 
    mutate(!!as.name(i) := str_sub(!!as.name(i), 1, 3)) %>% 
    distinct(!!as.name(i)) %>% 
    filter(!is.na(!!as.name(i))) %>% 
    print()
  
}

### manually check S27 in X23 &X25 columns are Not applicable: 9 entries, all outside UK

s27s <- geo_cdw %>%
  filter(str_sub(HSCP_CODE, 1, 3) == "S27" | str_sub(HSCP_CODE_CURR, 1, 3) == "S27")

# Check for old geography codes in current values

old_hb <- geo_cdw %>% filter(HEALTH_BOARD_CODE_9_CURR %in% c("S08000018", "S08000021", 
                                                             "S08000023", "S08000027"))

old_hb_2 <- old_hb %>% 
  mutate(START_DATE = as_date(START_DATE, format = "%d-%m-%Y")) %>% 
  filter(START_DATE > as_date("31-12-1899", format = "%d-%m-%Y")) %>% 
  count(POSTCODE)

old_hb %>% count(SOURCE_FLAG)
old_hb %>% count(START_DATE)

old_hscp <- geo_cdw %>% filter(HSCP_CODE_CURR %in% c("S37000014", "S37000015", 
                                                     "S37000021", "S37000023"))

old_hscp %>% count(SOURCE_FLAG)
old_hscp %>% count(START_DATE)

old_ca <- geo_cdw %>% filter(COUNCIL_AREA_CODE_9_CURR %in% c("S12000015", "S12000024", 
                                                             "S12000044", "S12000046"))

old_ca %>% count(SOURCE_FLAG)
old_ca %>% count(START_DATE)


### 3 Frequencies ----

freq_cols <- c("COUNCIL_AREA_CODE_9", "COUNCIL_AREA_CODE_9_CURR", 
               "HEALTH_BOARD_CODE_9", "HEALTH_BOARD_CODE_9_CURR", 
               "CHP_CODE", "CHP_CODE_CURR", "HSCP_CODE", "HSCP_CODE_CURR", 
               "COUNCIL_AREA_CODE", "COUNCIL_AREA_CODE_CURR", 
               "HEALTH_BOARD_CODE", "HEALTH_BOARD_NO", "HEALTH_BOARD_NO_CURR", 
               "SUB_HEALTH_BOARD_CODE", "CURRENT_RECORD_FLAG", 
               "COUNCIL_AREA", "COUNCIL_AREA_CURRENT", 
               "HEALTH_BOARD_OF_RESIDENCE", "HEALTH_BOARD_OF_RESIDENCE_NO", 
               "HEALTH_BOARD_AREA")


# Check the below codes match the code range on the data dictionary.
# Check that leading zeros are included as expected

for (i in freq_cols){
  
  geo_cdw %>% group_by(!!as.name(i)) %>% count() %>% print(n = Inf)
  
}

# special categories using old codes S08200001:4, S08299999, S08900001:3
# CHP column using S039999984 - S039999999 ??

# check nonstandard codes

nonstx15 <- geo_cdw %>%
  filter(HEALTH_BOARD_CODE_9_CURR == "S08299999") # dummy Scotland HB

nonstx19 <- geo_cdw %>%
  filter(str_sub(CHP_CODE, 1, 4) == "S039") # unknown CHP categories

geo_cdw %>%
  filter(str_sub(HSCP_CODE, 1, 4) == "S379") %>% 
  count(HSCP_NAME) # unknown HSCP, England/Wales/NI


### 4 Postcodes ----

# Check postcode info

geo_cdw %>% group_by(POSTCODE) %>% count() %>% View()

# check postcodes not in pc7 or pc8 format

geo_cdw %>% 
  filter(nchar(POSTCODE) != 7) %>% 
  # filter(nchar(POSTCODE) != 6) %>% 
  count(POSTCODE, SOURCE_FLAG) %>% 
  View()

# check number of unique postcodes 

geo_cdw %>%
  distinct(POSTCODE) %>%
  nrow()

# 2,641,189 rows (2,632,837 in 2019_2 postcode file)

# compare with n records in POSTCODE table:

postcode <- tbl(APXU_connection, "POSTCODE") %>%
  collect() %>% 
  nrow()

#2,633,179


### 5 Dates ----

# Check date fields look ok
# date fields are: X3, X4, X27, X28, X29, X30, X40

date_cols <- c("INTRODUCTION_DATE", "DELETION_DATE", "CDW_INSERT_DATE", 
               "CDW_UPDATE_DATE", "START_DATE", "END_DATE", "UPDATED_ON")


for (i in date_cols){
  
  geo_cdw %>% group_by(!!as.name(i)) %>% count() %>% print(n = Inf)
  
}

# UPDATED_ON: range 2017-2020
# END_DATE & START_DATE: 2001, 2006, 2007, 2011, 2012, 2014, 2018 & 2019 changes. 
#      1899, 4000 ?
# CDW_UPDATE_DATE, CDW_INSERT_DATE: range 2017-2020
# DELETION_DATE: range 1977 - 2019
# INTRODUCTION_DATE: range - 2020

# check deletion date follows introduction date

geo_cdw %>% 
  filter(!(is.na(INTRODUCTION_DATE)|is.na(DELETION_DATE))) %>% 
  filter(as.numeric(str_sub(INTRODUCTION_DATE, 7, 10)) > as.numeric(str_sub(DELETION_DATE, 7,10)))%>% 
  View()

# one ONS entry where X3 = 01-01-1983 & X4 = 01-12-1982

# check end date follows start date

geo_cdw %>% 
  filter(!(is.na(START_DATE)|is.na(END_DATE))) %>% 
  filter(as.numeric(str_sub(START_DATE, 7, 10)) > as.numeric(str_sub(END_DATE, 7,10)))%>% 
  View()


## check current records have appropriate end date

geo_cdw %>% 
  filter(CURRENT_RECORD_FLAG == "Y") %>% 
  group_by(END_DATE) %>% 
  count()

## non-current records have appropriate end date

geo_cdw %>% 
  #filter(X2 == "GROS" | X2 == "GROL") %>% 
  filter(CURRENT_RECORD_FLAG == "N") %>% 
  group_by(END_DATE) %>% 
  count()


### 6 Source_flag ----
# Check frequencies of postcode source flag

geo_cdw %>% 
  group_by(SOURCE_FLAG) %>% 
  summarise(n = n()) %>% 
  mutate(pc = n/sum(n)*100)

# unique postcodes only

geo_cdw %>% 
  distinct(POSTCODE, .keep_all = TRUE) %>% 
  group_by(SOURCE_FLAG) %>% 
  summarise(n = n()) %>% 
  mutate(pc = n/sum(n)*100)

# similar to postcode table

# Compare number of Scotland postcodes with the number on the single record file
# SPD 2019_2 contains 223,286 postcodes

geo_cdw %>% distinct(POSTCODE, .keep_all = TRUE) %>% filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% count()

#223748


### 7 Geographies ----

# Check Health Board 9 digit codes against 2 digit code
# X12, X13, X14 (labels)
hb_all <- geo_cdw %>% 
  filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  group_by(HEALTH_BOARD_CODE_9, HEALTH_BOARD_NO, HEALTH_BOARD_NAME) %>% 
  count()

# Check Health Board against the Health Board cypher

geo_cdw %>% filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  group_by(HEALTH_BOARD_NO, HEALTH_BOARD_CODE) %>% 
  count()


# Check CURRENT Health Board 9 digit codes against 2 digit code
# current should only contain HB2014-2019 codes
# X15, X16, X17 (labels), X18 (cypher)
hb_curr <- geo_cdw %>% 
  filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  group_by(HEALTH_BOARD_CODE_9_CURR, HEALTH_BOARD_NO_CURR, 
           HEALTH_BOARD_NAME_CURR, SUB_HEALTH_BOARD_CODE) %>% 
  count()


# check entries for S08000032 with cypher G are from boundary changes
# S08900002 is Greater Glasgow

geo_cdw %>% 
  filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  filter(HEALTH_BOARD_CODE_9_CURR == "S08000032" & SUB_HEALTH_BOARD_CODE == "G") %>% 
  count(HEALTH_BOARD_CODE_9, END_DATE)

# Check Health Board RES codes and cypher
# X36(cypher), X37, X38 (labels), X39 
hb_res <- geo_cdw %>% 
  filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  group_by(HEALTH_BOARD_OF_RESIDENCE, HEALTH_BOARD_OF_RESIDENCE_NO, 
           HEALTH_BOARD_OF_RESIDENCE_DESC, HEALTH_BOARD_AREA) %>% 
  count()

#X37 vs X39???



# Check CA 9 digit codes against 2 digit code
# X5, X6, X7 (labels)
ca_all <- geo_cdw %>% 
  filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  group_by(COUNCIL_AREA_CODE, COUNCIL_AREA_CODE_9, COUNCIL_AREA_NAME) %>% 
  count()

# Check CURRENT CA 9 digit codes against 2 digit code
# X8, X9, X10 (labels)
ca_curr <- geo_cdw %>% 
  filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  group_by(COUNCIL_AREA_CODE_CURR, COUNCIL_AREA_CODE_9_CURR, COUNCIL_AREA_NAME_CURR) %>% 
  count()

# Check CA 9 digit codes against 2 digit code
# X32, X33, X34, X35 
ca_desc <- geo_cdw %>% 
  filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  count(COUNCIL_AREA, COUNCIL_AREA_DESC, COUNCIL_AREA_CURRENT, 
        COUNCIL_AREA_DESC_CURRENT)



# Check CHP 9 digit codes against CHP CURR codes
# X19, X20, X21, X22 
chp <- geo_cdw %>% 
  filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  count(CHP_CODE, CHP_NAME, CHP_CODE_CURR, CHP_NAME_CURR)


# Check HSCP 9 digit codes against HSCP CURR codes
# X23, X24, X25, X26 
hscp <- geo_cdw %>% 
  filter(SOURCE_FLAG == "GROS" | SOURCE_FLAG == "GROL") %>% 
  count(HSCP_CODE, HSCP_NAME, HSCP_CODE_CURR, HSCP_NAME_CURR)



# Check missing Health Boards & CA (current) are English postcodes (ONS), 
# manual additions (MNUL) or dummy postcodes (DUMM) or ACND

geo_cdw %>% 
  filter(is.na(HEALTH_BOARD_CODE_9_CURR)| is.na(COUNCIL_AREA_CODE_9_CURR)) %>% 
  group_by(SOURCE_FLAG) %>% 
  count()

