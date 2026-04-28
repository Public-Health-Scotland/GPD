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

# Read in previous years file
prev_dz2011_pop    <- readRDS(glue("{lookup_filepath}/DataZone2011_pop_est_2011_2022.rds"))
prev_dz2011_pop_5y <- readRDS(glue("{lookup_filepath}/DataZone2011_pop_est_5year_agegroups_2011_2022.rds"))

# read in column names

final_col_names <- c("datazone2011","sex", "total_pop",
                     paste0("age", 0:89), "age90plus")

# Read in Source Data 
i_orig <-readRDS(glue(lookup_filepath, "/DataZone2011_pop_est_2011_2022.rds"))
i_july_rebase <- readRDS(glue(lookup_filepath, "/DataZone2011_pop_est_2011_2022_REBASED.rds"))
i_feb_rebase <- readRDS(glue(lookup_filepath, "/DataZone2011_pop_est_2011_2022_REBASED_v3.rds"))


g_orig <- i_orig %>% 
 # select (year, datazone2011, datazone2011name, orig_pop = total_pop) %>% 
  group_by(year, datazone2011, datazone2011name) %>% 
  summarise(orig_dz_pop = sum(total_pop))

g_july_rebase <- i_july_rebase %>% 
  select (year, datazone2011, july_pop = total_pop)

g_feb_rebase <- i_july_rebase %>% 
  select (year, datazone2011, feb_pop = total_pop)

pop_compare <- g_orig %>% 
  left_join(g_july_rebase, by = c("year", "datazone2011"))


rev_dz2011_pop_2022 <- i_rev_dz2011_pop_2022 %>% 
  select(-(2:4)) %>% 
  filter(Sex!="Persons") %>%
  rename(sex=Sex) %>% 
  relocate(sex, .after = `Data zone code`) %>% 
  set_names(., final_col_names) %>% 
  # set_names(., c("datazone2011","sex", "total_pop","age0", "age1", "age2", 
  #                "age3", "age4", "age5", "age6", "age7", "age8", "age9", 
  #                "age10", "age11", "age12", "age13", "age14", "age15", 
  #                "age16", "age17", "age18", "age19", "age20", "age21", 
  #                "age22", "age23", "age24", "age25", "age26", "age27", 
  #                "age28", "age29", "age30", "age31", "age32", "age33", 
  #                "age34", "age35", "age36", "age37", "age38", "age39", 
  #                "age40", "age41", "age42", "age43", "age44", "age45", 
  #                "age46", "age47", "age48", "age49", "age50", "age51", 
  #                "age52", "age53", "age54", "age55", "age56", "age57", 
  #                "age58", "age59", "age60", "age61", "age62", "age63", 
  #                "age64", "age65", "age66", "age67", "age68", "age69", 
  #                "age70", "age71", "age72", "age73", "age74", "age75", 
  #                "age76", "age77", "age78", "age79", "age80", "age81", 
  #                "age82", "age83", "age84", "age85", "age86", "age87", 
  #                "age88", "age89", "age90plus"))    %>% 
  mutate(sex = case_when(sex == "Females" ~ "F",
                       sex == "Males" ~ "M"),
         year=2022) %>% 
  arrange(year, datazone2011, desc(sex)) %>% 
  select(year, datazone2011, sex, age0:age90plus, total_pop)


# Define a list
fm_revised <- vector(mode = "list", length = 11)

for (i in 2011:2021){
  year_file <- glue("{data_filepath}/Rebased SAPE {i} (11DZ) - corrected.xlsx")
  fm_revised[[i - 2010]] <- read_excel(year_file, sheet = as.character(i), 
                                       range = "A6980:CS20931", 
                                       col_names = FALSE) %>% 
    select(-(2:4)) %>% 
    set_names(., final_col_names) %>% 
    mutate(year = i) 
}



fm_df <- do.call(bind_rows, fm_revised) %>% 
  mutate(sex = case_when(sex == "Females" ~ "F",
                         sex == "Males" ~ "M",
                         TRUE~"check")) %>% 
  arrange(year, datazone2011, desc(sex)) %>% 
  select(year, datazone2011, sex, age0:age90plus, total_pop) %>% 
  rbind(rev_dz2011_pop_2022)

# prev_dz2011_pop_2011_2021 <- prev_dz2011_pop %>% 
#   filter(year %in% 2011:2021)
# prev_dz2011_pop_2022 <- prev_dz2011_pop %>% 
#   filter(year == 2022)

dz2011_pop_final <- prev_dz2011_pop %>% 
  left_join(fm_df, by = c("year", "datazone2011", "sex"), 
            suffix = c(".x", "")) %>% 
  select(-ends_with(".x")) %>% 
  select(year, datazone2011, datazone2011name, sex, age0:age90plus, 
         total_pop, everything())

# dz2011_pop_final <- bind_rows(prev_dz2011_pop_2011_2021_rev,
#                               prev_dz2011_pop_2022)

# Save data zone yearly estimates
saveRDS(dz2011_pop_final, 
        glue({lookup_filepath},
             "/DataZone2011_pop_est_2011_2022_REBASED_v3.rds"))

fwrite(dz2011_pop_final, 
       glue({lookup_filepath},
            "/DataZone2011_pop_est_2011_2022_REBASED_v3.csv", 
            na = ""))

write_parquet(dz2011_pop_final,
              sink = glue({lookup_filepath},
                          "/DataZone2011_pop_est_2011_2022_REBASED_v3.parquet"),
              compression = "zstd")

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
saveRDS(dz2011_pop_final_5y, 
        glue("{lookup_filepath}/DataZone2011_pop_est_5year_agegroups_2011_2022_REBASED_v3.rds"))

fwrite(dz2011_pop_final_5y, 
       glue("{lookup_filepath}/DataZone2011_pop_est_5year_agegroups_2011_2022_REBASED_v3.csv", na = ""))

write_parquet(dz2011_pop_final_5y, 
              sink = glue("{lookup_filepath}",
                          "/DataZone2011_pop_est_5year_agegroups_2011_2022_REBASED_v3.parquet"),
              compression = "zstd")


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

saveRDS(iz2011_pop_final, glue("{lookup_filepath}/IntZone2011_pop_est_2011_2022_REBASED_v3.rds"))
fwrite(iz2011_pop_final, glue("{lookup_filepath}/IntZone2011_pop_est_2011_2022_REBASED_v3.csv", na = ""))

write_parquet(iz2011_pop_final, 
              sink = glue("{lookup_filepath}",
                          "/IntZone2011_pop_est_2011_2022_REBASED_v3.parquet"),
              compression = "zstd")


### Create IntZone2011_pop_est_5year_agegroups File ----

# Group by year, intzone2011 and sex
# Sum age columns and total_pop across grouped variables
# Ungroup to turn the grouped data back to a normal dataframe
# Sort data by year, intzone2011 and sex

iz2011_pop_final_5y <- dz2011_pop_final_5y %>%
  group_by(year, intzone2011, intzone2011name, sex) %>%
  summarise_at(vars(ageg04:total_pop), list(sum)) %>%
  ungroup() %>%
  arrange(year, intzone2011, desc(