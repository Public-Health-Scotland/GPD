### 1 - Housekeeping ----

#rm(list = ls())

# Read in packages from library

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
if (!pacman::p_isinstalled("friendlyloader")){pacman::p_install_gh("RosalynLP/friendlyloader")}
# Read in packages from library

pacman::p_load( glue, janitor, tidylog, tidyr, readxl, data.table, magrittr,
                dplyr, stringr, arrow)


# Set file paths
SAPE_filepath   <- glue("//data/geography/Population/Small Area Population estimates")
data_filepath   <- glue("{SAPE_filepath}/Source Data")
lookup_filepath <- glue("{SAPE_filepath}/Lookup Files/R Files")






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
i_feb_file <- dz2011_pop_final 


g_feb_file <- i_feb_file %>% 
  group_by(year, datazone2011) %>% 
  summarise(feb_pop = sum (total_pop))

com_pop <- g_orig_file %>% 
  left_join(g_july_file, by = c("year", "datazone2011")) %>% 
  left_join(g_feb_file,  by = c("year", "datazone2011")) %>% 
  mutate(
    feb_orig_diff    = feb_pop - orig_pop, 
    feb_orig_diff_pc = round(feb_orig_diff / orig_pop * 100, 0),
    july_orig_diff   = july_pop - orig_pop, 
    july_orig_diff_pc = round(july_orig_diff / orig_pop * 100, 0),
    july_feb_diff    = feb_orig_diff - july_orig_diff    ) %>% 
  filter(datazone2011name != "Sighthill - 03")
#%>% 
 # filter (july_orig_diff==1)

jul_negatives <- com_pop %>% 
  filter( july_orig_diff_pc <=-1) %>% 
  mutate (july_orig_diff_pc= july_orig_diff_pc*-1) %>% 
  mutate(  july_pc_rank   = case_when( july_orig_diff_pc == 0  ~ "No diff",
                                         between(july_orig_diff_pc, 1, 5) | between(july_orig_diff_pc, -1, -5) ~ "-ve 1-5",
                                         between(july_orig_diff_pc, 6, 25) ~ "-ve 6–25",
                                         between(july_orig_diff_pc, 26,50) ~ "-ve 26–50",
                                         july_orig_diff_pc >= 51 ~  "-ve 50-100",
                                         TRUE  ~ "check") ) %>% 
  mutate (july_orig_diff_pc= july_orig_diff_pc*-1)



jul_positives <-com_pop %>% 
  filter( july_orig_diff_pc >=0) %>% 
  mutate(  july_pc_rank   = case_when( july_orig_diff_pc == 0  ~ "No diff",
                                       between(july_orig_diff_pc, 1, 5) | between(july_orig_diff_pc, -1, -5) ~ "+ve 1-5",
                                       between(july_orig_diff_pc, 6, 25) ~ "+ve 6–25",
                                       between(july_orig_diff_pc, 26,50) ~ "+ve 26–50",
                                       july_orig_diff_pc >= 51 ~  "+ve 50-100",
                                       TRUE  ~ "check") )  
 

pc_rank_order <- (c( "No diff", "+ve 1-5","-ve 1-5", "+ve 6–25","-ve 6–25",
                   "+ve 26–50","-ve 26–50","-ve 50-100","+ve 50-100"))


jul_comp_difference <- rbind(jul_positives, jul_negatives) %>% 
  group_by(july_pc_rank) %>% 
  summarise(n_matches = n()  )  %>% 
  mutate(july_pc_rank = factor(july_pc_rank, levels = pc_rank_order)) %>% 
  arrange(july_pc_rank)

feb_comp_differences <-com_pop %>% 
  filter(july_feb_diff!=0)

