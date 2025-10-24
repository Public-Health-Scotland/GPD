#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# RStudio Workbench is strictly for use by Public Health Scotland staff and     
# authorised users only, and is governed by an <Acceptable Usage Policy>.
#
# This is a shared resource and is hosted on a pay-as-you-go cloud computing
# platform.  Your usage will incur direct financial cost to Public Health
# Scotland.  As such, please ensure
#
#   1. that this session is appropriately sized with the minimum number of CPUs
#      and memory required for the size and scale of your analysis;
#   2. the code you write in this script is optimal and only writes out the
#      data required, nothing more.
#   3. you close this session when not in use; idle sessions still cost PHS
#      money!
#
# For further guidance, please see <insert link>.
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



pop_proj <- function(filepath, col_start, col_end, variable){
  
  # Read in rds version and rename columns for open data format
  # Arrange by Year, Sex and Age and spread data by Age and Pop
  # Rename Age variables to have Age prefix
  
  geo_pop_proj <- readRDS(filepath) %>%
    select(-sex) %>%
    rename(Sex = sex_name,
           Year = year,
           Age = age,
           Pop = pop) %>%
    arrange(Year, Sex, Age) %>%
    spread(Age, Pop) %>% 
    rename_at(col_start:col_end, function(x) paste("Age", x, sep=""))
  
  # Create totals for male and female combined
  # If Scotland projections group by Year
  # If smaller geography group by Year and geography
  
  if (col_start == 3 & col_end == 93){
    
    all_total <- geo_pop_proj %>% 
      group_by(Year) %>% 
      summarise_at(vars(Age0:Age90), list(sum)) %>%
      ungroup() %>% 
      mutate(Sex = "All")
    
  } else {
    
    all_total <- geo_pop_proj %>% 
      group_by(Year, !!as.name(variable)) %>% 
      summarise_at(vars(Age0:Age90), list(sum)) %>%
      ungroup() %>% 
      mutate(Sex = "All")
    
  }
  
  # Add all_total to geo_pop_est
  
  geo_pop_proj %<>% 
    bind_rows(all_total)
  
  # Group by Year and Sex for single years of age for calculating Scotland total
  
  scot_total <- geo_pop_proj %>%
    group_by(Year, Sex) %>%
    summarise_at(vars(Age0:Age90), list(sum)) %>%
    ungroup()
  
  # Join geo_pop_proj and scot_total
  # Sum across all ages to get totals
  # Sort by Year and Sex
  # Reorder to set missing data first
  
  geo_pop_proj %<>%
    full_join(scot_total) %>%
    mutate(AllAges = rowSums(.[col_start:col_end]), 
           SexQF = case_when(Sex == "All" ~ "d")) %>%
    arrange(Year, Sex) %>%
    rename(Age90plus = Age90) %>% 
    setorder(na.last = F)
  
}
