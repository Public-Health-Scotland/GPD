### 1 - Housekeeping ----

rm(list = ls())

# Read in packages from library
library(glue)
library(magrittr)
library(dplyr)
library(tidyr)
library(readxl)
library(data.table)

# Set file paths
SAPE_filepath   <- glue("//data/geography/Population/Small Area Population estimates")
data_filepath   <- glue("{SAPE_filepath}/Source Data")
lookup_filepath <- glue("{SAPE_filepath}/Lookup Files/R Files")

# Read in previous years file
prev_dz2011_pop    <- readRDS(glue("{lookup_filepath}/DataZone2011_pop_est_2011_2022.rds"))
prev_dz2011_pop_5y <- readRDS(glue("{lookup_filepath}/DataZone2011_pop_est_5year_agegroups_2011_2022.rds"))

# read in column names
# source_col_names <- as.character(
#   read_excel(glue("{data_filepath}/Re-based SAPE 2011 (2011 data zones).xlsx"), 
#              skip = 2, sheet = "2011", n_max = 1, col_names = FALSE))
final_col_names <- c("datazone2011","sex", "total_pop",
                     paste0("age", 0:89), "age90plus")

# Read in Source Data for Females
# Define a list
fm_revised <- vector(mode = "list", length = 11)

for (i in 2011:2021){
  year_file <- glue("{data_filepath}/Rebased SAPE {i} (2011 data zones).xlsx")
  fm_revised[[i - 2010]] <- read_excel(year_file, sheet = as.character(i), 
                                       range = "A6980:CS20931", 
                                       col_names = FALSE) %>% 
    select(-(2:4)) %>% 
    set_names(., final_col_names) %>% 
    mutate(year = i) 
}  
  
fm_df <- do.call(bind_rows, fm_revised) %>% 
  mutate(sex = case_when(sex == "Females" ~ "F",
                         sex == "Males" ~ "M")) %>% 
  arrange(year, datazone2011, desc(sex)) %>% 
  select(year, datazone2011, sex, age0:age90plus, total_pop)

prev_dz2011_pop_2011_2021 <- prev_dz2011_pop %>% 
  filter(year %in% 2011:2021)
prev_dz2011_pop_2022 <- prev_dz2011_pop %>% 
  filter(year == 2022)

prev_dz2011_pop_2011_2021_rev <- prev_dz2011_pop_2011_2021 %>% 
  left_join(fm_df, by = c("year", "datazone2011", "sex"), 
            suffix = c(".x", "")) %>% 
  select(-ends_with(".x")) %>% 
  select(year, datazone2011, datazone2011name, sex, age0:age90plus, 
         total_pop, everything())

dz2011_pop_final <- bind_rows(prev_dz2011_pop_2011_2021_rev,
                              prev_dz2011_pop_2022)

# Save data zone yearly estimates
saveRDS(dz2011_pop_final, glue("{lookup_filepath}/DataZone2011_pop_est_2011_2022_REBASED_v2.rds"))
fwrite(dz2011_pop_final, glue("{lookup_filepath}/DataZone2011_pop_est_2011_2022_REBASED_v2.csv", na = ""))


# Create 5y age groups
dz2011_pop_final_5y <- dz2011_pop_final %>%
  mutate(ageg04   = rowSums(select(., age0:age4)),
         ageg59   = rowSums(select(., age5:age9)),
         ageg1014 = rowSums(select(., age10:age14)),
         ageg1519 = rowSums(select(., age15:age19)),
         ageg2024 = rowSums(select(., age20:age24)),
         ageg2529 = rowSums(select(., age25:age29)),
         ageg3034 = rowSums(select(., age30:age34)),
         ageg3539 = rowSums(select(., age35:age39)),
         ageg4044 = rowSums(select(., age40:age44)),
         ageg4549 = rowSums(select(., age45:age49)),
         ageg5054 = rowSums(select(., age50:age54)),
         ageg5559 = rowSums(select(., age55:age59)),
         ageg6064 = rowSums(select(., age60:age64)),
         ageg6569 = rowSums(select(., age65:age69)), 
         ageg7074 = rowSums(select(., age70:age74)),
         ageg7579 = rowSums(select(., age75:age79)),
         ageg8084 = rowSums(select(., age80:age84)),
         ageg8589 = rowSums(select(., age85:age89))) %>% 
  select(-c(age0:age89)) %>% 
  rename(ageg90plus = age90plus) %>% 
  relocate(ageg04:ageg8589, .before = ageg90plus)

# Data zone 5 yearly estimates
saveRDS(dz2011_pop_final_5y, glue("{lookup_filepath}/DataZone2011_pop_est_5year_agegroups_2011_2022_REBASED_v2.rds"))
fwrite(dz2011_pop_final_5y, glue("{lookup_filepath}/DataZone2011_pop_est_5year_agegroups_2011_2022_REBASED_v2.csv", na = ""))


### Create SAPE Files for 2011 Intermediate Zones ----

### IntZone Single Year File ----

# Group by year, intzone2011 and sex
# Sum age columns and total_pop across grouped variables
# Ungroup to turn the grouped data back to a normal dataframe
# Sort data by year, intzone2011 and sex

iz2011_pop_final <- dz2011_pop_final %>%
  group_by(year, intzone2011, intzone2011name, sex) %>%
  summarise_at(vars(age0:total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(year, intzone2011, desc(sex))

saveRDS(iz2011_pop_final, glue("{lookup_filepath}/IntZone2011_pop_est_2011_2022_REBASED_v2.rds"))
fwrite(iz2011_pop_final, glue("{lookup_filepath}/IntZone2011_pop_est_2011_2022_REBASED_v2.csv", na = ""))


### Create IntZone2011_pop_est_5year_agegroups File ----

# Group by year, intzone2011 and sex
# Sum age columns and total_pop across grouped variables
# Ungroup to turn the grouped data back to a normal dataframe
# Sort data by year, intzone2011 and sex

iz2011_pop_final_5y <- dz2011_pop_final_5y %>%
  group_by(year, intzone2011, intzone2011name, sex) %>%
  summarise_at(vars(ageg04:total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(year, intzone2011, desc(sex))

# IntZone2011_pop_est_5year File 
saveRDS(iz2011_pop_final_5y, glue("{lookup_filepath}/IntZone2011_pop_est_5year_agegroups_2011_2022_REBASED_v2.rds"))
fwrite(iz2011_pop_final_5y, glue("{lookup_filepath}/IntZone2011_pop_est_5year_agegroups_2011_2022_REBASED_v2.csv", na = ""))
