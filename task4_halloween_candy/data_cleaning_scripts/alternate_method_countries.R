

library(tidyverse)
library(janitor)
library(stringr)
library(readxl)

source(here::here("data_cleaning_scripts/extra_countries_different_names.R"))

#bind tables

countries <- bind_rows(candy_2015_piv, candy_2016_piv, candy_2017_piv) %>% 
  mutate(country = str_to_lower(country)) %>% 
  count(country)

countries_name_list <- countries %>%
  mutate(country_2 = case_when(
    str_detect(country, "^usa") ~ "usa",
    str_detect(country, "^u[ ]*s") ~ "usa",
    str_detect(country, "^u\\.s\\.") ~ "usa",
    str_detect(country, "^united[ ]+s") ~ "usa",
    str_detect(country, "^u[a-z ]+ states") ~ "usa",
    str_detect(country, "^the united states") ~ "usa",
    str_detect(country, "^america") ~ "usa")) %>% 
  mutate(country_2 = case_when(
    country %in% other_valid_countries ~ country,
    TRUE ~ country_2)) %>%
  mutate(country_2 = case_when(
    country %in% spain_group ~ "spain",
    TRUE ~ country_2)) %>%
  mutate(country_2 = case_when(
    country %in% south_korea_group ~ "south korea",
    TRUE ~ country_2)) %>%
  mutate(country_2 = case_when(
    country %in% netherlands_group ~ "netherlands",
    TRUE ~ country_2)) %>%
  mutate(country_2 = case_when(
    str_detect(country, "united k") ~ "uk",
    str_detect(country, "^u[\\.]*k") ~ "uk",
    TRUE ~ country_2)) %>%
  mutate(country_2 = case_when(
    str_detect(country, "canada") ~ "canada",
    str_detect(country,"^can$") ~ "canada",
    TRUE ~ country_2)) %>% 
  select(country, country_2, n) 

usa_group_standard <- countries_name_list %>% filter(country_2 == "usa") %>% pull(country)
usa_group <- c(usa_group_standard, usa_group_extras)

uk_group_standard <- countries_name_list %>% filter(country_2 == "uk") %>% pull(country)
uk_group <- c(uk_group_standard, uk_group_extras)

discarded_countries <- countries_name_list %>% 
  mutate(country_2 = case_when(
    country %in% usa_group ~ "usa",
    country %in% uk_group ~ "uk",
    TRUE ~ country_2)) %>% 
  filter(is.na(country_2))


rm(countries_name_list, countries, usa_group_standard, usa_group_extras, uk_group_extras, uk_group_standard)