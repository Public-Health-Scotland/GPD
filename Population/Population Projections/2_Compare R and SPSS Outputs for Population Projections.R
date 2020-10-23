##########################################################
# Compare R and SPSS Outputs for Population Projections
# Calum Purdie
# Original date 25/04/2019
# Latest update author - Calum Purdie
# Latest update date - 18/03/2020
# Latest update description - formatting code
# Type of script - Comparison
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for comparing R and SPSS lookup files for population projections
# Approximate run time - 30 seconds
##########################################################

### 1 - Housekeeping ----

# Read in packages from library

library(tidyr)
library(dplyr)
library(stringr)
library(haven)
library(sjlabelled)
library(janitor)
library(tidylog)
library(glue)

# Set working directory

spss_filepath <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                      "2_Population/Population Projections/Lookup Files")
r_filepath <- glue("{spss_filepath}/R Files")



### 2 - Create Function for Outputs ----

comparison <- function(spss_data, r_data, spss_5y_data, r_5y_data){
  
  # Read in SPSS file
  # Remove variable labels, formats and widths from SPSS
  # Mutate any factors to characters
  # Use clean_names so column names match
  
  spss <- read_sav(glue("{spss_filepath}/{spss_data}"), user_na=F) %>%
    zap_formats() %>%
    zap_widths() %>%
    remove_all_labels() %>% 
    mutate_if(is.factor, as.character) %>% 
    clean_names()
  
  # Read in R file
  # Remove sex_name and set all integers to numeric
  
  r <- readRDS(glue("{r_filepath}/{r_data}")) %>% 
    select(-sex_name) %>% 
    mutate_if(is.integer, as.numeric)
  
  # Read in SPSS 5 year file
  # Remove variable labels, formats and widths from SPSS
  # Mutate any factors to characters
  # Use clean_names so column names match
  
  spss_5y <- read_sav(glue("{spss_filepath}/{spss_5y_data}"), user_na=F) %>%
    zap_formats() %>%
    zap_widths() %>%
    remove_all_labels() %>% 
    mutate_if(is.factor, as.character) %>% 
    clean_names()
  
  # Read in R 5 year file
  # Remove sex_name and set all integers to numeric
  
  r_5y <- readRDS(glue("{r_filepath}/{r_5y_data}")) %>% 
    select(-c(sex_name, age_group_name)) %>% 
    mutate_if(is.integer, as.numeric)
  
  if((str_extract(spss_data, "[^_]*") == "scot" & 
      str_extract(r_data, "[^_]*") == "scot") | 
     (str_extract(spss_5y_data, "[^_]*") == "scot" & 
      str_extract(r_5y_data, "[^_]*") == "scot")){
    
  } else if((str_extract(spss_data, "[^_]*") == "CA2019" & 
             str_extract(r_data, "[^_]*") == "CA2019") | 
            (str_extract(spss_5y_data, "[^_]*") == "CA2019" & 
             str_extract(r_5y_data, "[^_]*") == "CA2019")){
    
    r %<>% select(-ca2019name)
    r_5y %<>% select(-ca2019name)
    
  } else if((str_extract(spss_data, "[^_]*") == "HB2019" & 
             str_extract(r_data, "[^_]*") == "HB2019") | 
            (str_extract(spss_5y_data, "[^_]*") == "HB2019" & 
             str_extract(r_5y_data, "[^_]*") == "HB2019")){
    
    r %<>% select(-hb2019name)
    r_5y %<>% select(-hb2019name)
    
  } else if ((str_extract(spss_data, "[^_]*") == "HSCP2019" & 
              str_extract(r_data, "[^_]*") == "HSCP2019") | 
             (str_extract(spss_5y_data, "[^_]*") == "HSCP2019" & 
              str_extract(r_5y_data, "[^_]*") == "HSCP2019")){
    
    r %<>% select(-hscp2019name)
    r_5y %<>% select(-hscp2019name)
    
  } else {
    
    print("Define correct projections data")
    
  }
  
  # Compare files
  
  all_equal(spss, r) %>% print()
  all_equal(spss_5y, r_5y) %>% print()
  
}



### 3 - Compare Files ----

### 3.1 - Scotland ----

spss_data <- "scot_pop_proj_2018_2043.sav"
r_data <- "scot_pop_proj_2018_2043.rds"
spss_5y_data <- "scot_pop_proj_5year_agegroups_2018_2043.sav"
r_5y_data <- "scot_pop_proj_5year_agegroups_2018_2043.rds"

comparison(spss_data, r_data, spss_5y_data, r_5y_data)


### 3.2 - Council Area ----

spss_data <- "CA2019_pop_proj_2018_2043.sav"
r_data <- "CA2019_pop_proj_2018_2043.rds"
spss_5y_data <- "CA2019_pop_proj_5year_agegroups_2018_2043.sav"
r_5y_data <- "CA2019_pop_proj_5year_agegroups_2018_2043.rds"

comparison(spss_data, r_data, spss_5y_data, r_5y_data)


### 3.3 - Health Board ----

spss_data <- "HB2019_pop_proj_2018_2043.sav"
r_data <- "HB2019_pop_proj_2018_2043.rds"
spss_5y_data <- "HB2019_pop_proj_5year_agegroups_2018_2043.sav"
r_5y_data <- "HB2019_pop_proj_5year_agegroups_2018_2043.rds"

comparison(spss_data, r_data, spss_5y_data, r_5y_data)


### 3.4 - HSCP ----

spss_data <- "HSCP2019_pop_proj_2018_2043.sav"
r_data <- "HSCP2019_pop_proj_2018_2043.rds"
spss_5y_data <- "HSCP2019_pop_proj_5year_agegroups_2018_2043.sav"
r_5y_data <- "HSCP2019_pop_proj_5year_agegroups_2018_2043.rds"

comparison(spss_data, r_data, spss_5y_data, r_5y_data)
