##########################################################
# Compare R and SPSS Outputs for HSCP Locality Lookup
# Calum Purdie
# Original date 16/12/2019
# Latest update author - Calum Purdie
# Latest update date - 16/12/2019
# Latest update description 
# Type of script - Update
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for comparing R and SPSS HSCP Locality lookups
# Approximate run time - <1 second
##########################################################


### 1 Housekeeping ----

library(dplyr)
library(tidylog)
library(glue)
library(haven)
library(sjlabelled)
library(janitor)

filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                 "1_Geography/HSCPs and Localities/Lookup Files/Locality/", 
                 "Update in 2019-12") 

lookup <- "HSCP Localities_DZ11_Lookup_20191216" 



### 2 - Compare Files ----

# Read in SPSS file
# Remove variable labels, formats and widths from SPSS
# Set factors to characters

spss_lookup <- read_sav(glue("{filepath}/{lookup}.sav"), user_na=F) %>%
  zap_formats() %>%
  zap_widths() %>%
  remove_all_labels() %>% 
  mutate_if(is.factor, as.character) %>% 
  clean_names()

# Read in R file

r_lookup <- readRDS(glue("{filepath}/{lookup}.rds")) %>% 
  select(-c(data_zone2011name))

# Compare files

all_equal(r_lookup, spss_lookup)
