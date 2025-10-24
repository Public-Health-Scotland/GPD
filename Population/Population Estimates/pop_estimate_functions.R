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

# create age bands
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

ca_checks <- function(input){
  
  # Check that all years of the population estimates are there
  # Check that there are no missing values
  # Check all years have the same % of records
  input %>% group_by(year) %>% count() %>% print(n = Inf)
  
  # Check that all 32 Council Areas are there
  # Check there are no missing values
  # Check all CAs have the same % of records
  input %>% group_by(ca2019) %>% count() %>% print(n = Inf)
  input %>% group_by(ca2018) %>% count() %>% print(n = Inf)
  input %>% group_by(ca2011) %>% count() %>% print(n = Inf)
  input %>% group_by(ca2019name) %>% count() %>% print(n = Inf)
  
  if (str_extract(deparse(substitute(input)), "(..)$") == "5y"){
    
    # Check that all age groups are there
    # Check there are no missing values
    # Check all age groups have the same % of records
    input %>% count(age_group) %>% print(n = Inf)
  } else {
    
    # Check that all 91 ages 0 to 90+ are there
    # Check there are no missing values
    # Check all ages have the same % of records
    input %>% count(age) %>% print(n = Inf)
  }
  
  # Check that both males and females are there
  # Check there are no missing values
  # Check both sexes have the same % of records (50/50)
  input %>% count(sex) %>% print(n = Inf)
  
  # Check that the population values are as expected
  # i.e. no negative values or extremely high values etc
  input %>%
    group_by(pop) %>%
    count() %>%
    arrange(n) %>%
    filter(n <= 0 | n >= 300)
  
  # Select last 10 years of data to check trend / nothing unexpected / outlandish
  new_years <- input %>%
    filter(year > 2014)
  
  # Check Council Area totals against NRS source data
  new_years %<>%
    group_by(year, ca2019) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()
  
  View(new_years)
  
  # Check Scotland totals against NRS source data
  new_years %>%
    group_by(year) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()
  
}

hb_checks <- function(input){
  
  # Check that all years of the population estimates are there 
  # Check that there are no missing values
  # Check all years have the same % of records
  
  input %>% group_by(year) %>% count() %>% print(n = Inf)
  
  # Check that all 14 health boards are there
  # Check there are no missing values
  # Check all HBs have the same % of records
  
  input %>% group_by(hb2019) %>% count() %>% print(n = Inf)
  input %>% group_by(hb2018) %>% count() %>% print(n = Inf)
  input %>% group_by(hb2014) %>% count() %>% print(n = Inf)
  input %>% group_by(hb2019name) %>% count() %>% print(n = Inf)
  
  if (str_extract(deparse(substitute(input)), "(..)$") == "5y"){
    
    # Check that all age groups are there
    # Check there are no missing values
    # Check all age groups have the same % of records
    
    input %>% count(age_group) %>% print(n = Inf)
    
  } else {
    
    # Check that all 91 ages 0 to 90+ are there
    # Check there are no missing values
    # Check all ages have the same % of records
    
    input %>% count(age) %>% print(n = Inf)
    
  }
  
  # Check that both males and females are there
  # Check there are no missing values
  # Check both sexes have the same % of records (50/50)
  
  input %>% group_by(sex) %>% count() %>% print(n = Inf)
  
  # Check that the population values are as expected
  # i.e. no negative values or extremely high values etc
  
  input %>%
    group_by(pop) %>%
    count() %>%
    arrange(n) %>%
    filter(n <= 0 | n >= 250)
  
  # Select only the new year(s) of data
  
  new_years <- input %>%
    filter(year > 2014)
  # 
  # Check Council Area totals against NRS source data
  
  new_years %<>%
    group_by(year, hb2019) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()
  
  View(new_years)
  
  # Check Scotland totals against NRS source data
  
  new_years %>%
    group_by(year) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()
  
}



hscp_checks <- function(input){
  
  # Check that all years of the population estimates are there
  # Check that there are no missing values
  # Check all years have the same % of records
  
  input %>% group_by(year) %>% count() %>% print(n = Inf)
  
  # Check that all 31 HSCPs are there
  # Check there are no missing values
  # Check all HSCPs have the same % of records
  
  input %>% group_by(hscp2019) %>% count() %>% print(n = Inf)
  input %>% group_by(hscp2018) %>% count() %>% print(n = Inf)
  input %>% group_by(hscp2016) %>% count() %>% print(n = Inf)
  input %>% group_by(hscp2019name) %>% count() %>% print(n = Inf)
  
  if (str_extract(deparse(substitute(input)), "(..)$") == "5y"){
    
    # Check that all age groups are there
    # Check there are no missing values
    # Check all age groups have the same % of records
    
    input %>% count(age_group) %>% print(n = Inf)
    
  } else {
    
    # Check that all 91 ages 0 to 90+ are there
    # Check there are no missing values
    # Check all ages have the same % of records
    
    input %>% count(age) %>% print(n = Inf)
    
  }
  
  # Check that both males and females are there
  # Check there are no missing values
  # Check both sexes have the same % of records (50/50)
  
  input %>% group_by(sex) %>% count() %>% print(n = Inf)
  
  # Check that the population values are as expected
  # i.e. no negative values or extremely high values etc
  
  input %>%
    group_by(pop) %>%
    count() %>%
    arrange(n) %>%
    filter(n <= 0 | n >= 300)
  
  # Select only the new year(s) of data
  
  new_years <- input %>%
    filter(year > 2014)
  
}
