##########################################################
# Compare SIMD 2020 and SIMD 2020v2
# Calum Purdie
# Original date 29/05/2020
# Latest update author - Calum Purdie
# Latest update date - 29/05/2020
# Latest update description 
# Type of script - Creation
# Written/run on RStudio Desktop
# Version of R that the script was most recently run on - 3.5.1
# Code for comparing differences in population-weighted SIMD2020 and SIMD2020 v2
# Approximate run time - 6 minutes
##########################################################

##########################################################
### 1 Housekeeping ----
##########################################################

library(magrittr)
library(tidyr)
library(dplyr)
library(tidylog)
library(glue)
library(janitor)

# set filepaths

simd_lookup <- glue("//Freddy/DEPT/PHIBCS/PHI/Referencing & Standards/GPD/", 
                    "3_Deprivation/SIMD/Lookup Files/SIMD 2020")



##############################################################
# 2. Read in SIMD Data
##############################################################

original_simd <- readRDS(glue("{simd_lookup}/DataZone2011_simd2020.rds"))
revised_simd <- readRDS(glue("{simd_lookup}/DataZone2011_simd2020v2.rds"))

##############################################################
# 3. Check SIMD differences
##############################################################

simd_check <- function(simd2020_col, simd2020v2_col){
  
  df_simd2020 <- original_simd %>% count(!!as.name(simd2020_col), 
                                         name = "n_simd2020")
  
  df_simd2020v2 <- revised_simd %>% count(!!as.name(simd2020v2_col), 
                                          name = "n_simd2020v2")
  
  bind_cols(df_simd2020, df_simd2020v2) %>% 
    mutate(diff = n_simd2020v2 - n_simd2020)
  
}

bt15 <- simd_check("simd2020bt15", "simd2020v2bt15")

tp15 <- simd_check("simd2020tp15", "simd2020v2tp15")

dz_bt15_old <- original_simd %>% 
  select(datazone2011, simd2020bt15) %>% 
  left_join(select(revised_simd, datazone2011, simd2020v2bt15)) %>% 
  filter(simd2020bt15 == 1 & simd2020v2bt15 == 0)

dz_bt15_old %>% count(name = "dz no longer in bt15")

dz_bt15_new <- original_simd %>% 
  select(datazone2011, simd2020bt15) %>% 
  left_join(select(revised_simd, datazone2011, simd2020v2bt15)) %>% 
  filter(simd2020bt15 == 0 & simd2020v2bt15 == 1)

dz_tp15_old <- original_simd %>% 
  select(datazone2011, simd2020tp15) %>% 
  left_join(select(revised_simd, datazone2011, simd2020v2tp15)) %>% 
  filter(simd2020tp15 == 1 & simd2020v2tp15 == 0)

dz_tp15_new <- original_simd %>% 
  select(datazone2011, simd2020tp15) %>% 
  left_join(select(revised_simd, datazone2011, simd2020v2tp15)) %>% 
  filter(simd2020tp15 == 0 & simd2020v2tp15 == 1)

sc_decile <- simd_check("simd2020_sc_decile", "simd2020v2_sc_decile")

sc_quintile <- simd_check("simd2020_sc_quintile", "simd2020v2_sc_quintile")

simd_rank <- original_simd %>% 
  select(datazone2011, simd2020_rank) %>% 
  left_join(select(revised_simd, datazone2011, simd2020v2_rank)) %>% 
  mutate(diff = simd2020v2_rank - simd2020_rank)

sc_dec <- original_simd %>% 
  select(datazone2011, simd2020_sc_decile) %>% 
  left_join(select(revised_simd, datazone2011, simd2020v2_sc_decile)) %>% 
  mutate(diff = simd2020v2_sc_decile - simd2020_sc_decile)

sc_quin <- original_simd %>% 
  select(datazone2011, simd2020_sc_quintile) %>% 
  left_join(select(revised_simd, datazone2011, simd2020v2_sc_quintile)) %>% 
  mutate(diff = simd2020v2_sc_quintile - simd2020_sc_quintile)

hb_dec <- original_simd %>% 
  select(datazone2011, simd2020_hb2019_decile) %>% 
  left_join(select(revised_simd, datazone2011, simd2020v2_hb2019_decile)) %>% 
  mutate(diff = simd2020v2_hb2019_decile - simd2020_hb2019_decile)

hscp_dec <- original_simd %>% 
  select(datazone2011, simd2020_hscp2019_decile) %>% 
  left_join(select(revised_simd, datazone2011, simd2020v2_hscp2019_decile)) %>% 
  mutate(diff = simd2020v2_hscp2019_decile - simd2020_hscp2019_decile)

ca_dec <- original_simd %>% 
  select(datazone2011, simd2020_ca2019_decile) %>% 
  left_join(select(revised_simd, datazone2011, simd2020v2_ca2019_decile)) %>% 
  mutate(diff = simd2020v2_ca2019_decile - simd2020_ca2019_decile)
