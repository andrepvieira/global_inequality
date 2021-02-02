# clean up workspace

rm(list=ls())

# close all figure windows created with x11()

graphics.off()

# LOAD PACKAGES ----

library(tidyverse)

# LOAD DATA ----

# code for countries ----

code <- read_csv("D:/global_inequality/data/cnt-code.csv")

# human rights (table 2) ----

hum_right <- read_csv("D:/global_inequality/data/human-rights-scores.csv")

# political power (table 3) ----

v_dem <- readRDS("D:/global_inequality/data/V-DEM/V-Dem-CY-Full+Others-v10.rds")

# gender equality index (table 4) ----

wbl <- readxl::read_excel("D:/global_inequality/data/WBL50YearPanelDetailsWeb01Jun2020.xlsx", sheet = 2, skip = 1)

# power distribution by sex orientation (table 5) ----

v_dem_gender <- readRDS("D:/global_inequality/data/V-DEM/V-Dem-CY-Full+Others-v10.rds")

# lifexpec <- read_csv("./tabelas/life-expectancy.csv")
# yrs_school <- read_csv("./tabelas/mean-years-of-schooling-1.csv")

# CLEAN DATA ----

# code for countries ----

code <- code %>% 
      janitor::clean_names() %>% 
      select(code = alpha_3, region, sub_region)

# human rights ----

# group_by region


# group_by sub_region + brazil

hum_right_clean_tbl <- hum_right %>% 
      janitor::clean_names() %>% 
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2017)) %>% 
      left_join(code) %>% 
      mutate(sub_region = case_when(entity == "Yemen Arab Republic" | entity == "Yemen People's Republic" ~ "Western Asia", 
                                    entity == "Kosovo" ~ "Southern Europe",
                                    entity == "West Germany" ~ "Western Europe",
                                    entity == "East Germany" ~ "Western Europe",
                                    entity == "Yugoslavia" ~ "Southern Europe",
                                    entity == "Republic of Vietnam" ~ "South-eastern Asia",
                                    TRUE ~ as.character(sub_region)),
             region = ifelse(entity == "World", "World", region),
             sub_region2 = ifelse(entity == "Brazil", entity, sub_region),
             sub_region2 = case_when(sub_region %in% c("Western Asia",
                                                       "Northern Africa") ~ "Middle East and North Africa",
                                     sub_region %in% c("Central Asia",
                                                       "Eastern Europe",
                                                       "Northern Europe",
                                                       "Southern Europe",
                                                       "Western Europe") ~ "Europe and Central Asia",
                                     sub_region %in% c("South-eastern Asia",
                                                       "Western Asia",
                                                       "Australia and New Zealand",
                                                       "Melanesia",
                                                       "Micronesia",
                                                       "Polynesia") ~ "East Asia and Pacific",
                                     sub_region %in% c("Southern Asia") ~ "South Asia",
                                     TRUE ~ as.character(sub_region2)))

saveRDS(hum_right_clean_tbl, "./data/hum_right_clean_tbl.rds")

# political power ----

v_dem_clean_tbl <- v_dem %>% 
      tibble() %>% 
      filter(v2pepwrses_nr > 3 & v2clacjust_nr > 3) %>% 
      select(country_name, code = country_text_id, year, historical_date, codingstart,
             v2pepwrses, v2clacjust) %>%
      mutate(v2pepwrses_std = scale(v2pepwrses),
             v2clacjust_std = scale(v2clacjust)) %>% 
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2019)) %>% 
      left_join(code) %>% 
      mutate(region = ifelse(country_name == "World", "World", region)) %>%
      rename(entity = country_name) %>% 
      mutate(sub_region = case_when(entity == "Yemen Arab Republic" | entity == "Yemen People's Republic" ~ "Western Asia", 
                                    entity == "Kosovo" ~ "Southern Europe",
                                    entity == "West Germany" ~ "Western Europe",
                                    entity == "East Germany" ~ "Western Europe",
                                    entity == "Yugoslavia" ~ "Southern Europe",
                                    entity == "Republic of Vietnam" ~ "South-eastern Asia",
                                    entity == "Palestine/Gaza" ~ "Middle East and North Africa",
                                    entity == "Somaliland" ~ "Middle East and North Africa",
                                    entity == "South Yemen" ~ "Middle East and North Africa",
                                    entity == "Zanzibar" ~ "Sub-Saharan Africa",
                                    TRUE ~ as.character(sub_region)),
             region = ifelse(entity == "World", "World", region),
             sub_region2 = ifelse(entity == "Brazil", entity, sub_region),
             sub_region2 = case_when(sub_region %in% c("Western Asia",
                                                       "Northern Africa") ~ "Middle East and North Africa",
                                     sub_region %in% c("Central Asia",
                                                       "Eastern Europe",
                                                       "Northern Europe",
                                                       "Southern Europe",
                                                       "Western Europe") ~ "Europe and Central Asia",
                                     sub_region %in% c("South-eastern Asia",
                                                       "Western Asia",
                                                       "Australia and New Zealand",
                                                       "Melanesia",
                                                       "Micronesia",
                                                       "Polynesia") ~ "East Asia and Pacific",
                                     sub_region %in% c("Southern Asia") ~ "South Asia",
                                     TRUE ~ as.character(sub_region2)))

saveRDS(v_dem_clean_tbl, "./data/political_power_clean_tbl.rds")

# gender equality index ----

wbl_clean_tbl <- wbl %>% 
      tibble() %>% 
      janitor::clean_names() %>% 
      select(code, economy, wbl_report_year, wbl_index) %>%
      filter(wbl_report_year %in% c(1971, 1980, 1990, 2000, 2010, 2020)) %>% 
      left_join(code) %>% 
      mutate(
            sub_region = case_when(economy == "Congo, Dem. Rep." ~ "Sub-Saharan Africa",
                                   economy == "Kosovo" ~ "Europe and Central Asia",
                                   economy == "Romania" ~ "Europe and Central Asia",
                                   economy == "Timor-Leste" ~ "East Asia and Pacific",
                                   economy == "West Bank and Gaza" ~ "Middle East and North Africa",
                                   TRUE ~ as.character(sub_region)),
            # region = ifelse(entity == "World", "World", region),
             sub_region2 = ifelse(code == "BRA", "Brazil", sub_region),
             sub_region2 = case_when(sub_region %in% c("Western Asia",
                                                       "Northern Africa") ~ "Middle East and North Africa",
                                     sub_region %in% c("Central Asia",
                                                       "Eastern Europe",
                                                       "Northern Europe",
                                                       "Southern Europe",
                                                       "Western Europe") ~ "Europe and Central Asia",
                                     sub_region %in% c("South-eastern Asia",
                                                       "Western Asia",
                                                       "Australia and New Zealand",
                                                       "Melanesia",
                                                       "Micronesia",
                                                       "Polynesia") ~ "East Asia and Pacific",
                                     sub_region %in% c("Southern Asia") ~ "South Asia",
                                     TRUE ~ as.character(sub_region2)))
      
saveRDS(object = wbl_clean_tbl, file = "D:/global_inequality/data/wbl_clean_tbl.rds")

# power by sexual orientation ----

v_dem_gender_clean_tbl <- v_dem_gender %>% 
      tibble() %>% 
      select(country_name, code = country_text_id, year, historical_date, codingstart,
             v2pepwrort_osp) %>%
      filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2019)) %>% 
      left_join(code) %>% 
      mutate(region = ifelse(country_name == "World", "World", region)) %>%
      rename(entity = country_name) %>% 
      mutate(sub_region = case_when(entity == "Yemen Arab Republic" | entity == "Yemen People's Republic" ~ "Western Asia", 
                                    entity == "Kosovo" ~ "Southern Europe",
                                    entity == "West Germany" ~ "Western Europe",
                                    entity == "East Germany" ~ "Western Europe",
                                    entity == "Yugoslavia" ~ "Southern Europe",
                                    entity == "Republic of Vietnam" ~ "South-eastern Asia",
                                    entity == "Palestine/Gaza" ~ "Middle East and North Africa",
                                    entity == "Somaliland" ~ "Middle East and North Africa",
                                    entity == "South Yemen" ~ "Middle East and North Africa",
                                    entity == "Zanzibar" ~ "Sub-Saharan Africa",
                                    entity == "German Democratic Republic" ~ "Western Europe",
                                    TRUE ~ as.character(sub_region)),
             region = ifelse(entity == "World", "World", region),
             sub_region2 = ifelse(entity == "Brazil", entity, sub_region),
             sub_region2 = case_when(sub_region %in% c("Western Asia",
                                                       "Northern Africa") ~ "Middle East and North Africa",
                                     sub_region %in% c("Central Asia",
                                                       "Eastern Europe",
                                                       "Northern Europe",
                                                       "Southern Europe",
                                                       "Western Europe") ~ "Europe and Central Asia",
                                     sub_region %in% c("South-eastern Asia",
                                                       "Western Asia",
                                                       "Australia and New Zealand",
                                                       "Melanesia",
                                                       "Micronesia",
                                                       "Polynesia") ~ "East Asia and Pacific",
                                     sub_region %in% c("Southern Asia") ~ "South Asia",
                                     TRUE ~ as.character(sub_region2)))

saveRDS(object = v_dem_gender_clean_tbl, file = "D:/global_inequality/data/v_dem_gender_clean_tbl.rds")









