library(magrittr)
library(dplyr)


### 1 Calculating Populations ----

# Calculate populations for geography

pop_function <- function(column, new_column){
  
  DZ2011_pop_est %>% 
    group_by(!!as.name(column)) %>% 
    summarise(!!new_column := sum(total_pop)) %>% 
    ungroup()
  
}



### 2 Non Population Weighted ----

# Calculate datazone based non population weighted vigintiles

vig_function <- function(data, vig_col, col){
  
  data %<>% 
    mutate(!!as.name(vig_col) := 
             case_when(!!as.name(col) > 0 & !!as.name(col) <= 349 ~ 1, 
                       !!as.name(col) > 349 & !!as.name(col) <= 698 ~ 2, 
                       !!as.name(col) > 698 & !!as.name(col) <= 1046 ~ 3, 
                       !!as.name(col) > 1046 & !!as.name(col) <= 1395 ~ 4,
                       !!as.name(col) > 1395 & !!as.name(col) <= 1744 ~ 5,
                       !!as.name(col) > 1744 & !!as.name(col) <= 2093 ~ 6,
                       !!as.name(col) > 2093 & !!as.name(col) <= 2442 ~ 7,
                       !!as.name(col) > 2442 & !!as.name(col) <= 2790 ~ 8,
                       !!as.name(col) > 2790 & !!as.name(col) <= 3139 ~ 9,
                       !!as.name(col) > 3139 & !!as.name(col) <= 3488 ~ 10,
                       !!as.name(col) > 3488 & !!as.name(col) <= 3837 ~ 11,
                       !!as.name(col) > 3837 & !!as.name(col) <= 4186 ~ 12,
                       !!as.name(col) > 4186 & !!as.name(col) <= 4534 ~ 13,
                       !!as.name(col) > 4534 & !!as.name(col) <= 4883 ~ 14,
                       !!as.name(col) > 4883 & !!as.name(col) <= 5232 ~ 15,
                       !!as.name(col) > 5232 & !!as.name(col) <= 5581 ~ 16,
                       !!as.name(col) > 5581 & !!as.name(col) <= 5930 ~ 17,
                       !!as.name(col) > 5930 & !!as.name(col) <= 6278 ~ 18,
                       !!as.name(col) > 6278 & !!as.name(col) <= 6627 ~ 19,
                       !!as.name(col) > 6627 & !!as.name(col) <= 6976 ~ 20))
  
}

# Create datazone based non population weighted deciles

dec_function <- function(data, dec_col, col){
  
  data %<>% 
    mutate(!!as.name(dec_col) := 
             case_when(!!as.name(col) == 1 | !!as.name(col) == 2 ~ 1, 
                       !!as.name(col) == 3 | !!as.name(col) == 4 ~ 2, 
                       !!as.name(col) == 5 | !!as.name(col) == 6 ~ 3, 
                       !!as.name(col) == 7 | !!as.name(col) == 8 ~ 4,
                       !!as.name(col) == 9 | !!as.name(col) == 10 ~ 5,
                       !!as.name(col) == 11 | !!as.name(col) == 12 ~ 6,
                       !!as.name(col) == 13 | !!as.name(col) == 14 ~ 7,
                       !!as.name(col) == 15 | !!as.name(col) == 16 ~ 8,
                       !!as.name(col) == 17 | !!as.name(col) == 18 ~ 9,
                       !!as.name(col) == 19 | !!as.name(col) == 20 ~ 10))
  
}

# Create datazone based non population weighted quintiles

quin_function <- function(data, quin_col, col){
  
  data %<>% 
    mutate(!!as.name(quin_col) := 
             case_when(!!as.name(col) == 1 | !!as.name(col) == 2 ~ 1, 
                       !!as.name(col) == 3 | !!as.name(col) == 4 ~ 2, 
                       !!as.name(col) == 5 | !!as.name(col) == 6 ~ 3, 
                       !!as.name(col) == 7 | !!as.name(col) == 8 ~ 4,
                       !!as.name(col) == 9 | !!as.name(col) == 10 ~ 5))
  
}

# Create a variable for non population weighted deciles

geo_dec <- function(data){
  
  data %<>%
    mutate(dec = case_when(cpop_per > 0 & cpop_per <= 10 ~ 1, 
                           cpop_per > 10 & cpop_per <= 20 ~ 2, 
                           cpop_per > 20 & cpop_per <= 30 ~ 3,
                           cpop_per > 30 & cpop_per <= 40 ~ 4,
                           cpop_per > 40 & cpop_per <= 50 ~ 5,
                           cpop_per > 50 & cpop_per <= 60 ~ 6, 
                           cpop_per > 60 & cpop_per <= 70 ~ 7, 
                           cpop_per > 70 & cpop_per <= 80 ~ 8,
                           cpop_per > 80 & cpop_per <= 90 ~ 9,
                           cpop_per > 90 & cpop_per <= 100 ~ 10))
  
}

# Create a variable for non population weighted quintiles

geo_quin <- function(data){
  
  data %<>%
    mutate(quin = case_when(dec == 1 | dec == 2 ~ 1, 
                            dec == 3 | dec == 4 ~ 2, 
                            dec == 5 | dec == 6 ~ 3, 
                            dec == 7 | dec == 8 ~ 4, 
                            dec == 9 | dec == 10 ~ 5))
  
}

# Create geography level quintiles

geo_quintile <- function(data, quin_col, dec_col){
  
  data %<>%
    mutate(!!as.name(quin_col) := case_when(!!as.name(dec_col) == 1 | 
                                              !!as.name(dec_col) == 2 ~ 1, 
                                            !!as.name(dec_col) == 3 | 
                                              !!as.name(dec_col) == 4 ~ 2,
                                            !!as.name(dec_col) == 5 | 
                                              !!as.name(dec_col) == 6 ~ 3,
                                            !!as.name(dec_col) == 7 | 
                                              !!as.name(dec_col) == 8 ~ 4,
                                            !!as.name(dec_col) == 9 | 
                                              !!as.name(dec_col) == 10 ~ 5))
  
}


### 3 Population Weighted ----

# Calcuate population weighted vigintiles

vig_cpop <- function(data){
  
  data %<>%
    mutate(vig = case_when(cpop_per > 0 & cpop_per <= 5 ~ 1, 
                           cpop_per > 5 & cpop_per <= 10 ~ 2, 
                           cpop_per > 10 & cpop_per <= 15 ~ 3, 
                           cpop_per > 15 & cpop_per <= 20 ~ 4, 
                           cpop_per > 20 & cpop_per <= 25 ~ 5, 
                           cpop_per > 25 & cpop_per <= 30 ~ 6, 
                           cpop_per > 30 & cpop_per <= 35 ~ 7, 
                           cpop_per > 35 & cpop_per <= 40 ~ 8, 
                           cpop_per > 40 & cpop_per <= 45 ~ 9, 
                           cpop_per > 45 & cpop_per <= 50 ~ 10, 
                           cpop_per > 50 & cpop_per <= 55 ~ 11, 
                           cpop_per > 55 & cpop_per <= 60 ~ 12, 
                           cpop_per > 60 & cpop_per <= 65 ~ 13, 
                           cpop_per > 65 & cpop_per <= 70 ~ 14, 
                           cpop_per > 70 & cpop_per <= 75 ~ 15, 
                           cpop_per > 75 & cpop_per <= 80 ~ 16, 
                           cpop_per > 80 & cpop_per <= 85 ~ 17, 
                           cpop_per > 85 & cpop_per <= 90 ~ 18, 
                           cpop_per > 90 & cpop_per <= 95 ~ 19, 
                           cpop_per > 95 & cpop_per <= 100 ~ 20))
  
}



### 4 Cut-Off Points ----

# Calculate the difference between the cumulative population and the target 
# cut off points

vig_cut_off <- function(data){ 
  
  data %<>%
    mutate(d1 = case_when((vig == 1 | vig == 2) & 
                            cpop_per > 2.5 & cpop_per <= 7.5 ~ 5 - cpop_per,
                          (vig == 2 | vig == 3) & 
                            cpop_per > 7.5 & cpop_per <= 12.5 ~ 10 - cpop_per, 
                          (vig == 3 | vig == 4) & 
                            cpop_per > 12.5 & cpop_per <= 17.5 ~ 15 - cpop_per, 
                          (vig == 4 | vig == 5) & 
                            cpop_per > 17.5 & cpop_per <= 22.5 ~ 20 - cpop_per, 
                          (vig == 5 | vig == 6) & 
                            cpop_per > 22.5 & cpop_per <= 27.5 ~ 25 - cpop_per, 
                          (vig == 6 | vig == 7) & 
                            cpop_per > 27.5 & cpop_per <= 32.5 ~ 30 - cpop_per, 
                          (vig == 7 | vig == 8) & 
                            cpop_per > 32.5 & cpop_per <= 37.5 ~ 35 - cpop_per, 
                          (vig == 8 | vig == 9) & 
                            cpop_per > 37.5 & cpop_per <= 42.5 ~ 40 - cpop_per, 
                          (vig == 9 | vig == 10) & 
                            cpop_per > 42.5 & cpop_per <= 47.5 ~ 45 - cpop_per, 
                          (vig == 10 | vig == 11) & 
                            cpop_per > 47.5 & cpop_per <= 52.5 ~ 50 - cpop_per, 
                          (vig == 11 | vig == 12) & 
                            cpop_per > 52.5 & cpop_per <= 57.5 ~ 55 - cpop_per, 
                          (vig == 12 | vig == 13) & 
                            cpop_per > 57.5 & cpop_per <= 62.5 ~ 60 - cpop_per, 
                          (vig == 13 | vig == 14) & 
                            cpop_per > 62.5 & cpop_per <= 67.5 ~ 65 - cpop_per, 
                          (vig == 14 | vig == 15) & 
                            cpop_per > 67.5 & cpop_per <= 72.5 ~ 70 - cpop_per, 
                          (vig == 15 | vig == 16) & 
                            cpop_per > 72.5 & cpop_per <= 77.5 ~ 75 - cpop_per, 
                          (vig == 16 | vig == 17) & 
                            cpop_per > 77.5 & cpop_per <= 82.5 ~ 80 - cpop_per, 
                          (vig == 17 | vig == 18) & 
                            cpop_per > 82.5 & cpop_per <= 87.5 ~ 85 - cpop_per, 
                          (vig == 18 | vig == 19) & 
                            cpop_per > 87.5 & cpop_per <= 92.5 ~ 90 - cpop_per,
                          (vig == 19 | vig == 20) & 
                            cpop_per > 92.5 & cpop_per <= 97.5 ~ 95 - cpop_per)) %>% 
    mutate(d1 = if_else(is.na(d1), 0, d1))
  
}

# Calculate the difference between the cumulative population % and the target 
# cut off points

geo_cut_off <- function(data){
  
  data %<>% 
    mutate(d1 = case_when((dec == 1 | dec == 2) & 
                            cpop_per > 5 & cpop_per <= 15 ~ 10 - cpop_per,
                          (dec == 2 | dec == 3) & 
                            cpop_per > 15 & cpop_per <= 25 ~ 20 - cpop_per,
                          (dec == 3 | dec == 4) & 
                            cpop_per > 25 & cpop_per <= 35 ~ 30 - cpop_per,
                          (dec == 4 | dec == 5) & 
                            cpop_per > 35 & cpop_per <= 45 ~ 40 - cpop_per,
                          (dec == 5 | dec == 6) & 
                            cpop_per > 45 & cpop_per <= 55 ~ 50 - cpop_per,
                          (dec == 6 | dec == 7) & 
                            cpop_per > 55 & cpop_per <= 65 ~ 60 - cpop_per, 
                          (dec == 7 | dec == 8) & 
                            cpop_per > 65 & cpop_per <= 75 ~ 70 - cpop_per, 
                          (dec == 8 | dec == 9) & 
                            cpop_per > 75 & cpop_per <= 85 ~ 80 - cpop_per, 
                          (dec == 9 | dec == 10) & 
                            cpop_per > 85 & cpop_per <= 95 ~ 90 - cpop_per), 
           d1 = if_else(is.na(d1), 0, d1))
  
}



### 5 Check Functions ----

# Run checks on Scotland level data

scot_checks <- function(data, quin_col, dec_col, vig_col, tp15, bt15){
  
  # Check vigintiles, deciles and quintiles align correctly
  
  data %>% 
    group_by(!!as.name(quin_col), !!as.name(dec_col)) %>% 
    count() %>% 
    print()
  
  data %>% 
    group_by(!!as.name(dec_col), !!as.name(vig_col)) %>% 
    count() %>% 
    print()
  
  # Check quintile populations look ok
  
  data %>%
    group_by(!!as.name(quin_col)) %>%
    summarise(total = sum(datazone2011_pop)) %>%
    mutate(percentage = total*100/sum(total)) %>% 
    print()
  
  # Check decile populations look ok
  
  data %>%
    group_by(!!as.name(dec_col)) %>%
    summarise(total = sum(datazone2011_pop)) %>%
    mutate(percentage = total*100/sum(total)) %>% 
    print()
  
  # Check vigintile populations look ok
  
  data %>%
    group_by(!!as.name(vig_col)) %>%
    summarise(total = sum(datazone2011_pop)) %>%
    mutate(percentage = total*100/sum(total)) %>% 
    print()
  
  # Check top 15% is correct
  
  data %>% 
    group_by(!!as.name(vig_col), !!as.name(tp15)) %>% 
    count() %>% 
    print()
  
  # Check bottom 15% is correct
  
  data %>% 
    group_by(!!as.name(vig_col), !!as.name(bt15)) %>% 
    count() %>% 
    print()
  
}



# Run checks on smaller geography level data

geo_checks <- function(data, quin_col, scot_quin, dec_col, scot_dec, geography){
  
  # Check deciles and quintiles align correctly
  
  data %>% 
    group_by(!!as.name(quin_col), !!as.name(dec_col)) %>% 
    count() %>% 
    print(n = Inf)
  
  # Compare Scotland deciles and quintiles within geography
  
  data %>% 
    group_by(!!as.name(scot_quin), !!as.name(quin_col)) %>% 
    count() %>% 
    print(n = Inf)
  
  data %>% 
    group_by(!!as.name(scot_dec), !!as.name(dec_col)) %>% 
    count() %>% 
    print(n = Inf)
  
  # Check quintile populations look ok
  
  data %>%
    group_by(!!as.name(geography), !!as.name(quin_col)) %>%
    summarise(total = sum(datazone2011_pop)) %>%
    mutate(percentage = total*100/sum(total)) %>% 
    print(n = Inf)
  
  # Check decile populations look ok
  
  data %>%
    group_by(!!as.name(geography), !!as.name(dec_col)) %>%
    summarise(total = sum(datazone2011_pop)) %>%
    mutate(percentage = total*100/sum(total)) %>% 
    print(n = Inf)  
}



### 6 Manual Changes ----

# Change deciles for small areas

manual_changes <- function(data, decile){
  
  data %<>%  
    mutate(!!as.name(decile) := if_else(d2 < 0,
                                        !!as.name(decile) - 1,
                                        !!as.name(decile)))
  
}

# Check changes look ok

changes_check <- function(data, geography, decile, quintile){
  
  data %>%
    group_by(!!as.name(geography), !!as.name(decile)) %>%
    summarise(total = sum(datazone2011_pop)) %>%
    mutate(percentage = total*100/sum(total)) %>% 
    print(n = Inf) 
  
  data %>%
    group_by(!!as.name(geography), !!as.name(quintile)) %>%
    summarise(total = sum(datazone2011_pop)) %>%
    mutate(percentage = total*100/sum(total)) %>% 
    print(n = Inf) 
  
}



### 7 SPSS ----

# Rename SPSS columns

spss_names <- function(data){
  
  data %>% 
    select(-c(datazone2011name, intzone2011name, ca2019name, hscp2019name, 
              hb2019name)) %>% 
    rename(DataZone2011 = datazone2011, IntZone2011 = intzone2011, CA2019 = ca2019, 
           CA2018 = ca2018, CA2011 = ca2011, HB2019 = hb2019, HB2018 = hb2018, 
           HB2014 = hb2014, HSCP2019 = hscp2019, HSCP2018 = hscp2018, 
           HSCP2016 = hscp2016)
}

pc_simd_spss_names <- function(data){
  
  data %>% 
    rename(DataZone2011_simd2020 = datazone2011_simd2020, 
           DataZone2011_simd2016 = datazone2011_simd2016, 
           DataZone2001_simd2012 = datazone2001_simd2012, 
           DataZone2001_simd2009v2 = datazone2001_simd2009v2, 
           DataZone2001_simd2006 = datazone2001_simd2006, 
           DataZone2001_simd2004 = datazone2001_simd2004,
           OA2011 = oa2011, OA2001 = oa2001, OA1991 = oa1991) %>% 
    select(-datazone2011name)
}

