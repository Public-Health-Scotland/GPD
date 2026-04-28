## create age groups
age_group_fun <- function(data){
  
  data %<>%
    mutate(age_group_name = cut( age,
                                 breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 
                                            59, 64, 69, 74, 79, 84, 89, Inf),
                                 labels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                            "25-29", "30-34", "35-39", "40-44", "45-49", 
                                            "50-54", "55-59", "60-64", "65-69", "70-74", 
                                            "75-79", "80-84", "85-89", "90+"),
                                 right = TRUE,  include.lowest = TRUE  ),
           age_group_name = case_when(age==0~"0", age<=4~ "1-4",
                                      TRUE ~ age_group_name),
           age_group = cut( age,
                            breaks = c(1, 4, 9, 14, 19, 24, 29, 34, 
                                       39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                            labels = FALSE,       right = TRUE, include.lowest = TRUE),
           age_group = case_when(age_group_name == "0" ~ 0, TRUE ~ age_group)     ) 
  
}

### Check functions #

checks <- function(input, geography, age_column){
  
  # Check that all years of the population projections are there 
  # Check that there are no missing values
  # Check all years have the same % of records
  
  input %>% group_by(year) %>% count() %>% print(n = Inf)
  
  # Check that all ages 0 to 90+ are there
  # Check there are no missing values
  # Check all ages have the same % of records
  
  input %>% group_by({{age_column}}) %>% count() %>% print(n = Inf)
  
  # Check that both males and females are there #
  # Check there are no missing values 
  # Check both sexes have the same % of records (50/50)
  
  input %>% group_by(sex) %>% count() %>% print(n = Inf)
  
  # Check Scotland totals against NRS source data
  
  input %>%
    group_by(year) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()
  
  # Check that the population values are as expected
  # i.e. no negative values or extremely high values etc
  
  input %>%
    group_by(pop) %>%
    count() %>%
    arrange(n) %>%
    filter(n <= 0 | n >= 250)
  
  if(geography == "scot"){
    
  } else if (geography == "hb"){
    
    # Check that all 14 Health Boards are there
    # Check there are no missing values
    # Check all HBs have the same % of records
    
    input %>% group_by(hb2019) %>% count() %>% print(n = Inf)
    input %>% group_by(hb2018) %>% count() %>% print(n = Inf)
    input %>% group_by(hb2014) %>% count() %>% print(n = Inf)
    
  } else if (geography == "hscp"){
    
    # Check that all 31 HSCPs are there
    # Check there are no missing values
    # Check all HSCPs have the same % of records
    
    input %>% group_by(hscp2019) %>% count() %>% print(n = Inf)
    input %>% group_by(hscp2018) %>% count() %>% print(n = Inf)
    input %>% group_by(hscp2016) %>% count() %>% print(n = Inf)
    
  } else if (geography == "ca"){
    
    # Check that all 32 Council Areas are there
    # Check there are no missing values
    # Check all CAs have the same % of records
    
    input %>% group_by(ca2019) %>% count() %>% print(n = Inf)
    input %>% group_by(ca2018) %>% count() %>% print(n = Inf)
    input %>% group_by(ca2011) %>% count() %>% print(n = Inf)
    
  } else {
    
    print("Define correct projections data")
    
  }
  
}

