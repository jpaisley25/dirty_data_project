
# cleaning script for task 4 - halloween candy

# load libraries 

library(tidyverse)
library(janitor)
library(stringr)
library(readxl)


# load data and asign variable names 

candy_2015 <- read_excel(here::here("raw_data/boing-boing-candy-2015.xlsx"))

candy_2016 <- read_excel(here::here("raw_data/boing-boing-candy-2016.xlsx"))

candy_2017 <- read_excel(here::here("raw_data/boing-boing-candy-2017.xlsx"))


## clean all three tables to get them in to a suitable format to bind
## clean column names so that where columns are present in both tables the names are the same
## make sure all tables have gender, age, country columns
## add an id column so that i can group by when doing analysis
## pivot so that tables have the same number of columns


# candy 2015
candy_2015_piv <- candy_2015 %>%
  select(c(2:96)) %>%
  mutate(
    year = 2015,
    id = c(paste0(1:(nrow(
      candy_2015
    )), "_", 2015)),
    country = as.character(NA),
    gender = as.character(NA)
  ) %>%
  rename(
    age = "How old are you?",
    going_out = "Are you going actually going trick or treating yourself?",
    "[JoyJoy (Mit Iodine!)]" = "[JoyJoy (Mit Iodine)]"
  ) %>%
  select(id, year, going_out, gender, age, country, c(3:95)) %>%
  rename_all( ~ str_to_lower(.)) %>%
  pivot_longer(
    cols = c("[butterfinger]":"[york peppermint patties]"),
    names_to = "candy",
    values_to = "rating"
  ) %>%
  mutate(candy = str_sub(candy, 2, -2))

# candy 2016
candy_2016_piv <- candy_2016 %>%
  select(c(2:106)) %>%
  mutate(year = 2016,
         id = c(paste0(1:(nrow(
           candy_2016
         )), "_", 2016))) %>%
  select(id, year, c(1:4, 6:105)) %>%
  rename(
    going_out = "Are you going actually going trick or treating yourself?",
    gender = "Your gender:",
    age = "How old are you?",
    country = "Which country do you live in?",
    "[box’o’ raisins]" = "[Box'o'Raisins]"
  ) %>%
  rename_all( ~ str_to_lower(.)) %>%
  pivot_longer(
    cols = c("[100 grand bar]":"[york peppermint patties]"),
    names_to = "candy",
    values_to = "rating"
  ) %>%
  mutate(candy = str_sub(candy, 2, -2)) 

# candy 2017 
candy_2017_piv <- candy_2017 %>%
  select(c(2:109)) %>%
  mutate(year = 2017,
         id = c(paste0(1:(nrow(
           candy_2017
         )), "_", 2017))) %>%
  select(id, year, c(1:4, 6:108)) %>%
  rename_all( ~ str_replace(., "^Q[0-9]: ", "")) %>%
  rename_all( ~ str_to_lower(.)) %>%
  rename(
    "q6 | anonymous brown globs that come in black and orange wrappers" =
      "q6 | anonymous brown globs that come in black and orange wrappers	(a.k.a. mary janes)",
    "q6 | box’o’ raisins" = "q6 | box'o'raisins"
  )   %>%
  pivot_longer(
    cols = c("q6 | 100 grand bar":"q6 | york peppermint patties"),
    names_to = "candy",
    values_to = "rating"
  ) %>%
  mutate(candy = str_sub(candy, 6)) %>%
  clean_names()

#country groups - all entries in the country column that can be reasonably 
# be interpreted to be referring to the same country have been grouped
# country script is sourced here. Discarded_countries is an output to check no 
# valid countries are discarded.

## source(here::here("data_cleaning_scripts/countries_lists.R"))

source(here::here("data_cleaning_scripts/alternate_method_countries.R"))



# candy bind - binding the three tables 
# fixing the country and age values
# assumed lower limit of > 5 and upper limit > 101 on age 

candy_all_years <-
  bind_rows(candy_2015_piv, candy_2016_piv, candy_2017_piv) %>%
  mutate(
    country = tolower(country),
    country =
      case_when(
        country %in% usa_group ~ "usa",
        country %in% uk_group ~ "uk",
        country %in% spain_group ~ "spain",
        country %in% south_korea_group ~ "south korea",
        country %in% netherlands_group ~ "netherlands",
        country %in% canada_group ~ "canada",
        country %in% other_valid_countries ~ country)
  ) %>%
  mutate(
    age = case_when(str_detect(age, "[^0-9\\.]") ~ "0",
                    TRUE ~ age),
    age = round(as.numeric(age)),
    age = case_when(age < 101 & age > 5 ~ age)
  )

discarded_countries

write_csv(candy_all_years, here::here("clean_data/candys_data_clean.csv"))

