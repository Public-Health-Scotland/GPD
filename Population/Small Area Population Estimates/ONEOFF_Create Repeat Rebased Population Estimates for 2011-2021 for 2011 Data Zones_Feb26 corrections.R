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



#### Feb-26 revisions

# feb_col_names <- c("datazone2011","datazonename", "ca","caname","sex", "feb_pop")
# 
# fm_revised <- vector(mode = "list", length = 11)
# 
# for (i in 2011:2021){
#   year_file <- glue("{data_filepath}/Rebased SAPE {i} (2011 data zones).xlsx")
#   fm_revised[[i - 2010]] <- read_excel(year_file, sheet = as.character(i), 
#                                        range = "A4:F20931", 
#                                        col_names = FALSE) %>% 
#     #select(-(2:3)) %>% 
#     set_names(., feb_col_names) %>% 
#     mutate(year = i)    
# }  
# 
# new_2011_2021 <- do.call(bind_rows, fm_revised)

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



com_pop <- g_orig_file %>% 
  left_join(g_july_file, by = c("year", "datazone2011")) %>% 
  left_join(new_2011_2022,  by = c("year", "datazone2011")) %>% 
  mutate(
    # feb_orig_diff    = feb_pop - orig_pop, # diff in pop in Feb dataset to orig pre rebase dataset
    # feb_orig_diff_pc = round(feb_orig_diff / orig_pop * 100, digits = 0),
    july_orig_diff   = july_pop - orig_pop, # rebase difference
    july_orig_diff_pc = round(july_orig_diff / orig_pop * 100, digits = 0),
       ) # difference in 2nd rebase
# 
# %>% 
#   filter(datazone2011name != "Sighthill - 03")
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
    #july_orig_diff_pc == 0  ~ "No diff",
    between(july_orig_diff_pc, 1, 5)  ~ "-ve 1-5",
    between(july_orig_diff_pc, 6, 25) ~ "-ve 6–25",
    between(july_orig_diff_pc, 26,50) ~ "-ve 26–50",
    july_orig_diff_pc >= 51 ~  "-ve 50-100",
                                       TRUE  ~ "check") ) %>% 
  mutate (july_orig_diff_pc= july_orig_diff_pc*-1)



jul_positives <-com_pop %>% 
#  filter( july_orig_diff_pc !=0) %>% 
    filter( july_orig_diff_pc >=1) %>% 
  mutate(  july_pc_rank   = case_when( 
  #  july_orig_diff_pc == 0  ~ "No diff",
    between(july_orig_diff_pc, 1, 5)~ "+ve 1-5",
    between(july_orig_diff_pc, 6, 25) ~ "+ve 6–25",
    between(july_orig_diff_pc, 26,50) ~ "+ve 26–50",
    july_orig_diff_pc >= 51 ~  "+ve 50-100",
    TRUE  ~ "check") )  


pc_rank_order <- (c( "No diff", "+ve 1-5","-ve 1-5", "+ve 6–25","-ve 6–25",
                     "+ve 26–50","-ve 26–50","+ve 50-100","-ve 50-100"))


jul_comp_difference <- rbind(jul_zeros, jul_positives, jul_negatives) %>% 
  group_by(year, july_pc_rank) %>% 
  summarise(count = n()  )  %>% 
  ungroup() %>% 
  mutate(july_pc_rank = factor(july_pc_rank, levels = pc_rank_order)   ) %>% 
  arrange((year), july_pc_rank) %>% 
  pivot_wider(names_from = july_pc_rank,
            values_from = count) %>% 
  select(year, "No diff", "+ve 1-5","-ve 1-5", "+ve 6–25","-ve 6–25",
         "+ve 26–50","-ve 26–50","+ve 50-100","-ve 50-100") 


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
    mutate( feb_jul_pc_rank   = case_when(
      feb_jul_diff_pc  == 0 ~  "No diff",
      between(feb_jul_diff_pc, 1, 5)  ~ "-ve 1-5",
      between(feb_jul_diff_pc, 6, 25) ~ "-ve 6–25",
      between(feb_jul_diff_pc, 26,50) ~ "-ve 26–50",
      feb_jul_diff_pc >= 51 ~  "-ve 50-100",
      TRUE  ~ "check") )  %>% 
  mutate (feb_jul_diff_pc = feb_jul_diff_pc *-1) #back to -ve values

feb_jul_positives <-feb_differences %>% 
 # filter( feb_jul_diff_pc!=0) %>% 
  filter( feb_jul_diff >= 1) %>% 
  mutate( feb_jul_pc_rank   = case_when(
      feb_jul_diff_pc  == 0 ~  "No diff",
      between(feb_jul_diff_pc, 1, 5)  ~ "-ve 1-5",
      between(feb_jul_diff_pc, 6, 25) ~ "-ve 6–25",
      between(feb_jul_diff_pc, 26,50) ~ "-ve 26–50",
      feb_jul_diff_pc >= 51 ~  "-ve 50-100",
      TRUE  ~ "check") )  


feb_comp_difference <- rbind(feb_jul_zeros, feb_jul_negatives, feb_jul_positives)  %>% 
  group_by(year,feb_jul_pc_rank) %>% 
  summarise(count = n()  )  %>% 
  mutate(feb_jul_pc_rank = factor(feb_jul_pc_rank, levels = pc_rank_order),
         #pc_diff = round(count/6975*100, digits =1)
  ) %>% 
  arrange((year), feb_jul_pc_rank) %>% 
  pivot_wider(names_from = feb_jul_pc_rank,
              values_from = count) 
  select(year,  "No diff", "+ve 1-5","-ve 1-5", "+ve 6–25","-ve 6–25",
         "+ve 26–50","-ve 26–50","+ve 50-100","-ve 50-100") 


feb_jul_big_diff <- feb_differences %>% 
  filter(feb_jul_diff_pc>=5 | feb_jul_diff_pc <=-5) %>% 
  filter(datazone2011 != "S01010227")

feb_changes <- com_pop %>% 
  filter(datazone2011 %in% feb_jul_big_diff$datazone2011) %>% 
  mutate(feb_orig_diff = feb_pop - orig_pop)

feb_dz_by_ca <- feb_changes %>% 
  distinct() %>% 
  group_by (ca2019name ) %>% 
  summarise(dz_count= n()/12)
