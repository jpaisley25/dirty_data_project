# cleaning script for seabirds

library(tidyverse)
library(readxl)
library(janitor)



# load data

bird_data <- read_excel(path = here::here("raw_data/seabirds.xls"), sheet = 2)
ship_data <- read_excel(path = here::here("raw_data/seabirds.xls"), sheet = 1)

# clean birds data

bird_data_clean <- bird_data %>%
  select(
    RECORD,
    `RECORD ID`,
    `Species common name (taxon [AGE / SEX / PLUMAGE PHASE])`,
    `Species abbreviation`,
    `Species  scientific name (taxon [AGE /SEX /  PLUMAGE PHASE])`,
    COUNT
  ) %>%
  clean_names() %>%
  mutate(
    species_abbreviation = str_extract(species_abbreviation, "^[A-Z]+"),
    species_common_name_taxon_age_sex_plumage_phase = str_remove(
      species_common_name_taxon_age_sex_plumage_phase,
      "[A-Z0-9 ]+$"
    ),
    species_common_name_taxon_age_sex_plumage_phase = str_remove(
      species_common_name_taxon_age_sex_plumage_phase,
      "(?i)\\(unidentified\\)+$"
    ),
    species_scientific_name_taxon_age_sex_plumage_phase = str_remove(
      species_scientific_name_taxon_age_sex_plumage_phase,
      "[A-Z0-9 ]+$"
    )
  ) %>%
  rename(species_common_name = "species_common_name_taxon_age_sex_plumage_phase",
         species_scientific_name = "species_scientific_name_taxon_age_sex_plumage_phase")


# clean ship data

ship_data_clean <- ship_data %>%
  select(`RECORD ID`, LAT) %>%
  clean_names()


# join tables 

bird_sighting_data <- bird_data_clean %>% 
  left_join(ship_data_clean, by = c("record_id"))

# write cleaned data

write_csv(bird_sighting_data, here::here("clean_data/bird_data_clean.csv"))












