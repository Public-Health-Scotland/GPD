### 1 - Housekeeping ----

rm(list = ls())

# Read in packages from library

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}
# Read in packages from library

pacman::p_load( glue, janitor, tidylog, tidyr, readxl, data.table, magrittr,
                dplyr, arrow)


# Set file paths
SAPE_filepath   <- glue("//data/geography/Population/Small Area Population estimates")
data_filepath   <- glue("{SAPE_filepath}/Source Data")
lookup_filepath <- glue("{SAPE_filepath}/Lookup Files/R Files")



#### Feb-26 revisions ####



i_fm_df_2011 <- read_excel(glue("{data_filepath}/Rebased SAPE 2011 (11DZ) - corrected.xlsx"),
                           sheet = "2011", range = "A3:F20931") %>% 
  filter(Sex == "Persons") %>% 
  mutate(year =2011)

i_fm_df_2012 <- read_excel(glue("{data_filepath}/Rebased SAPE 2012 (11DZ) - corrected.xlsx"),
                           sheet = "2012", range = "A3:F20931") %>% 
  filter(Sex == "Persons")%>% 
  mutate(year =2012)

i_fm_df_2013 <- read_excel(glue("{data_filepath}/Rebased SAPE 2013 (11DZ) - corrected.xlsx"),
                           sheet = "2013", range = "A3:F20931") %>% 
  filter(Sex == "Persons")%>% 
  mutate(year =2013)

i_fm_df_2014 <- read_excel(glue("{data_filepath}/Rebased SAPE 2014 (11DZ) - corrected.xlsx"),
                           sheet = "2014", range = "A3:F20931") %>% 
  filter(Sex == "Persons")  %>% 
  mutate(year =2014)

i_fm_df_2015 <- read_excel(glue("{data_filepath}/Rebased SAPE 2015 (11DZ) - corrected.xlsx"),
                           sheet = "2015", range = "A3:F20931") %>% 
  filter(Sex == "Persons") %>% 
  mutate(year =2015)


i_fm_df_2016 <- read_excel(glue("{data_filepath}/Rebased SAPE 2016 (11DZ) - corrected.xlsx"),
                           sheet = "2016", range = "A3:F20931") %>% 
  filter(Sex == "Persons")  %>% 
  mutate(year =2016)

i_fm_df_2017 <- read_excel(glue("{data_filepath}/Rebased SAPE 2017 (11DZ) - corrected.xlsx"),
                           sheet = "2017", range = "A3:F20931") %>% 
  filter(Sex == "Persons")  %>% 
  mutate(year =2017)

i_fm_df_2018 <- read_excel(glue("{data_filepath}/Rebased SAPE 2018 (11DZ) - corrected.xlsx"),
                           sheet = "2018", range = "A3:F20931") %>% 
  filter(Sex == "Persons")  %>% 
  mutate(year =2018)

i_fm_df_2019 <- read_excel(glue("{data_filepath}/Rebased SAPE 2019 (11DZ) - corrected.xlsx"),
                           sheet = "2019", range = "A3:F20931") %>% 
  filter(Sex == "Persons")  %>% 
  mutate(year =2019)

i_fm_df_2020 <- read_excel(glue("{data_filepath}/Rebased SAPE 2020 (11DZ) - corrected.xlsx"),
                           sheet = "2020", range = "A3:F20931") %>% 
  filter(Sex == "Persons")  %>% 
  mutate(year =2020)

i_fm_df_2021 <- read_excel(glue("{data_filepath}/Rebased SAPE 2021 (11DZ) - corrected.xlsx"),
                           sheet = "2021", range = "A3:F20931") %>% 
  filter(Sex == "Persons")  %>% 
  mutate(year =2021)

i_fm_df_2022 <- read_excel(glue("{data_filepath}/sape-2022-11dz-corrected.xlsx"),
                           sheet = "Table 1", range = "A4:F20932") %>% 
  filter(Sex == "Persons")  %>% 
  mutate(year =2022)

new_2011_2022 <- rbind(i_fm_df_2011 , i_fm_df_2012 , i_fm_df_2013 ,i_fm_df_2014,
                        i_fm_df_2015,   i_fm_df_2016 , i_fm_df_2017, i_fm_df_2018, 
                        i_fm_df_2019, i_fm_df_2020 ,i_fm_df_2021,  i_fm_df_2022)  %>%  
  clean_names() %>% 
  mutate(sex="All") %>% 
  select(year, ca2019name= council_area_name, datazone2011 = data_zone_code,
         sex, feb_pop = total_population)



rm(i_fm_df_2011 , i_fm_df_2012 , i_fm_df_2013 ,i_fm_df_2014,
i_fm_df_2015,   i_fm_df_2016 , i_fm_df_2017, i_fm_df_2018, 
i_fm_df_2019, i_fm_df_2020 ,i_fm_df_2021,  i_fm_df_2022) 

# 
# rm(i_fm_df_2022, 
#    #fm_revised, 
#    new_2022,feb_col_names)


#### previous files

# Read in previous years file

i_orig_file   <- readRDS(glue("{lookup_filepath}/DataZone2011_pop_est_2011_2022.rds"))
g_orig_file <- i_orig_file %>% 
  group_by(year, datazone2011, datazone2011name) %>% 
  summarise(orig_pop = sum (total_pop))

i_july_file   <- readRDS(glue("{lookup_filepath}/DataZone2011_pop_est_2011_2022_REBASED.rds"))
g_july_file <- i_july_file %>% 
  group_by(year, datazone2011) %>% 
  summarise(july_pop = sum (total_pop))

#i_feb_file   <- readRDS(glue("{lookup_filepath}/IntZone2011_pop_est_2011_2022_REBASED_v3.rds"))

# take the version from loval env after running "ONEOFF_Create Rebased Population Estimates for 2011-2021 for 2011 Data Zones.R"

   # feb_orig_diff    = feb_pop - orig_pop, # diff in pop in Feb dataset to orig pre rebase dataset
    # feb_orig_diff_pc = round(feb_orig_diff / orig_pop * 100, digits = 0),

#remove very small pops that give too high a percentage difference
zero_dz_pop <- c("S01010206", "S01010226","S01010227" ) 

com_pop <- g_orig_file %>% 
  left_join(g_july_file, by = c("year", "datazone2011")) %>% 
  left_join(new_2011_2022,  by = c("year", "datazone2011")) %>% 
  mutate( july_orig_diff   = july_pop - orig_pop, # rebase difference
          july_orig_diff_pc = round(july_orig_diff / orig_pop * 100, digits = 0)) %>% 
   filter(datazone2011 %in% zero_dz_pop)



   #%>% 
# filter (july_orig_diff==1)

jul_zeros<- com_pop %>% 
  filter( july_orig_diff_pc == 0) %>% 
  mutate( july_pc_rank  = "No diff")


jul_negatives <- com_pop %>% 
 # filter( july_orig_diff_pc !=0) %>% 
    filter( july_orig_diff_pc <=-1) %>% 
  mutate (july_orig_diff_pc= july_orig_diff_pc*-1) %>% 
  mutate(  july_pc_rank   = case_when(
    july_orig_diff_pc == 0  ~ "No diff",
    between(july_orig_diff_pc, 1, 5)  ~ "neg_1_5",
    between(july_orig_diff_pc, 6, 25) ~ "neg_6_25",
    between(july_orig_diff_pc, 26,49) ~ "neg_26_50",
    july_orig_diff_pc >= 51 ~  "neg_51_100",
                                       TRUE  ~ "check") ) %>% 
  mutate (july_orig_diff_pc= july_orig_diff_pc*-1)



jul_positives <-com_pop %>% 
#  filter( july_orig_diff_pc !=0) %>% 
    filter( july_orig_diff_pc >=1) %>% 
  mutate(  july_pc_rank   = case_when( 
    july_orig_diff_pc == 0  ~ "No diff",
    between(july_orig_diff_pc, 1, 5)~ "pos_1_5",
    between(july_orig_diff_pc, 6, 25) ~ "pos_6_25",
    between(july_orig_diff_pc, 26,50) ~ "pos_26_50",
    july_orig_diff_pc >= 51 ~  "pos_51_100",
    TRUE  ~ "check") )  


pc_rank_order <- (c( "No diff", "pos_1-5","neg_1-5", "pos_6_25","neg_6_25",
                     "pos_26_50", "neg_26_50","pos_51_100","neg_51_100"))


jul_comp_difference <- rbind(jul_zeros, jul_positives, jul_negatives) %>% 
  mutate(pc_ranking = case_when(july_pc_rank  ==  "neg_1_5" ~ "Between 0 and 5", 
                                july_pc_rank  ==  "pos_1_5" ~ "Between 0 and 5", 
                                july_pc_rank  ==  "No diff" ~ "Between 0 and 5", 
                                july_pc_rank  ==  "pos_6_25" ~ "Between 6 and 25", 
                                july_pc_rank  ==  "neg_6_25" ~ "Between 6 and 25", 
                                july_pc_rank  ==  "pos_26_50" ~ "Between 26 and 50", 
                                july_pc_rank  ==  "neg_26_50" ~ "Between 26 and 50", 
                                july_pc_rank  ==  "pos_51_100" ~ "Between 51 and 100", 
                                july_pc_rank  ==  "neg_51_100" ~ "Between 51 and 100", 
                                TRUE~"check") ) %>% 
  arrange(year, datazone2011)

fwrite(jul_comp_difference, glue({SAPE_filepath},"/jul_2011_rebase_table.csv", na = ""))

jul_sum <- jul_comp_difference%>% 
  group_by(year, pc_ranking) %>% 
  summarise(count = n()  )  %>% 
  ungroup() %>% 
  pivot_wider(names_from = pc_ranking,
            values_from = count)  

fwrite(jul_sum, glue({SAPE_filepath},"/jul_rebase_sum.csv", na = ""))

ca2019_differences <- jul_comp_difference %>% 
  group_by(ca2019name, year) %>% 
  summarise(pre_rebase_pop =sum(orig_pop),
            rebase_pop =sum(july_pop),
            diff_pop = sum(july_orig_diff)) %>% 
  arrange(ca2019name, year)

fwrite(ca2019_differences, glue({SAPE_filepath},"/ca2019_differences.csv", na = ""))


##### compare 2026 revisions ####
feb_differences <-com_pop %>% 
  select( year,ca2019name, sex, datazone2011:feb_pop) %>% 
 mutate(feb_orig_diff = feb_pop - orig_pop,
        feb_orig_diff_pc = round(feb_orig_diff /orig_pop * 100, digits = 0),
        feb_jul_diff = feb_pop - july_pop,
        feb_jul_diff_pc = round(feb_jul_diff /orig_pop * 100, digits = 0) ) 

feb_jul_zeros<- feb_differences %>% 
  filter(feb_jul_diff == 0) %>% 
  mutate( feb_jul_pc_rank  = "No diff")


feb_jul_negatives <- feb_differences %>% 
 # filter( feb_jul_diff !=0) %>% 
    filter( feb_jul_diff  <= -1) %>% 
  mutate (feb_jul_diff_pc = feb_jul_diff_pc *-1) %>%   # make +ve for ranking
    mutate(feb_jul_pc_rank   = case_when(
      feb_jul_diff_pc  == 0 ~  "No diff",
      between(feb_jul_diff_pc, 1, 5)  ~ "neg_1_5",
      between(feb_jul_diff_pc, 6, 25) ~ "neg_6_25",
      between(feb_jul_diff_pc, 26,50) ~ "neg_26_50",
      feb_jul_diff_pc >= 51 ~  "neg_51_100",
      TRUE  ~ "check") )  %>% 
  mutate (feb_jul_diff_pc = feb_jul_diff_pc *-1) #back to -ve values

feb_jul_positives <-feb_differences %>% 
 # filter( feb_jul_diff_pc!=0) %>% 
  filter( feb_jul_diff >= 1) %>% 
  mutate( feb_jul_pc_rank   = case_when(
      feb_jul_diff_pc  == 0 ~  "No diff",
      between(feb_jul_diff_pc, 1, 5)  ~ "pos_1_5",
      between(feb_jul_diff_pc, 6, 25) ~ "pos_6_25",
      between(feb_jul_diff_pc, 26,50) ~ "pos_26_50",
      feb_jul_diff_pc >= 51 ~  "pos_51_100",
      TRUE  ~ "check") )  


feb_comp_difference <- rbind(feb_jul_zeros, feb_jul_negatives, feb_jul_positives)  %>% 
  mutate(pc_ranking = case_when(feb_jul_pc_rank  ==  "neg_1_5" ~ "Between 0 and 5", 
                                feb_jul_pc_rank  ==  "pos_1_5" ~ "Between 0 and 5", 
                                feb_jul_pc_rank  ==  "No diff" ~ "Between 0 and 5", 
                                feb_jul_pc_rank  ==  "pos_6_25" ~ "Between 6 and 25", 
                                feb_jul_pc_rank  ==  "neg_6_25" ~ "Between 6 and 25", 
                                feb_jul_pc_rank  ==  "pos_26_50" ~ "Between 26 and 50", 
                                feb_jul_pc_rank ==  "neg_26_50" ~ "Between 26 and 50", 
                                feb_jul_pc_rank  ==  "pos_51_100" ~ "Between 51 and 100", 
                                feb_jul_pc_rank ==  "neg_51_100" ~ "Between 51 and 100", 
                                TRUE~"check") ) %>% 
  arrange(year, datazone2011)

fwrite( feb_ca_sum, glue({SAPE_filepath},"/ feb_ca_sum.csv", na = ""))  




feb_sum <- feb_comp_difference %>% 
  group_by(year, pc_ranking) %>% 
  summarise(count = n()  )  %>% 
  ungroup() %>% 
  pivot_wider(names_from = pc_ranking,
              values_from = count)  

fwrite( feb_ca_sum, glue({SAPE_filepath},"/ feb_ca_sum.csv", na = ""))

  
  feb_ca_sum <- feb_comp_difference%>% 
    filter(pc_ranking !="Between 0 and 5") %>% 
    group_by(year,ca2019name, pc_ranking) %>% 
    summarise(count = n()  )  %>% 
    ungroup() %>% 
    pivot_wider(names_from = pc_ranking,
                values_from = count)  
  
  
feb_big_diff <- feb_comp_difference%>% 
  filter(pc_ranking !="Between 0 and 5") %>% 
  arrange (ca2019name,datazone2011name,  year )

fwrite( feb_big_diff, glue({SAPE_filepath},"/feb_big_diff.csv", na = ""))


feb_changes <- com_pop %>% 
  filter(datazone2011 %in% feb_jul_big_diff$datazone2011) %>% 
  mutate(feb_orig_diff = feb_pop - orig_pop)

feb_dz_by_ca <- feb_changes %>% 
  distinct() %>% 
  group_by (ca2019name ) %>% 
  summarise(dz_count= n()/12)
