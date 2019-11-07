### 1 - Information ----

# Codename - Create R Population Datamart Templates
# Original Author - Calum Purdie
# Original Date - 23/10/2019
# Type - Creation
# Written/run on - R Studio Desktop 
# Version - 3.5.1
#
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("haven")
# install.packages(sjlabelled)
# install.packages(tidylog)
#
# Description - Code for creating ISD files for NRS population projections for Scotland, 
#               Health Board, HSCP and Council Area
# Approximate run time - 30 seconds

base_filepath <- file.path("//Freddy", "DEPT", "PHIBCS", "PHI", "Referencing & Standards", 
                           "GPD", "2_Population", "Population Datamart", "Creation of Files", 
                           "Templates")

# Read in packages from library

library(tidyr)
library(dplyr)
library(haven)
library(sjlabelled)
library(tidylog)

### 2 - Create function for reading SPSS templates ----

# Create a list of all .sav templates

file_list <- list.files(base_filepath, pattern = ".sav")

for(i in 1:length(file_list)){
  
  # Read in the templates and remove all SPSS formatting, widths and labels
  # Change all factors to characters
  
  template <- read_sav(file = file.path(base_filepath, file_list[i])) %>% 
    zap_formats() %>%
    zap_widths() %>%
    remove_all_labels() %>% 
    mutate_if(is.factor, as.character)
  
  # Set the filename to be the orignal name with rds instead of sav
  
  filename <- gsub("sav", "rds", file_list[i])
  
  # Save the template as an RDS file using the filename
  saveRDS(template, file.path(base_filepath, "R Templates", filename))
  
}


