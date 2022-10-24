
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
  clean_names() %>%
  mutate(year = 2015, 
         id = c(paste0(1:(nrow(candy_2015)), "_", 2015)),
         country = as.character(NA),
         gender = as.character(NA)) %>% 
  rename(age = "how_old_are_you",
         going_out = "are_you_going_actually_going_trick_or_treating_yourself") %>% 
  select(id, year, going_out, gender, age, country, c(3:95)) %>% 
  rename_all(~ str_to_lower(.)) %>%
  pivot_longer(cols = c("butterfinger":"york_peppermint_patties"),
               names_to = "candy",
               values_to = "rating") 

# candy 2016
candy_2016_piv <- candy_2016 %>% 
  select(c(2:106)) %>% 
  clean_names() %>% 
  mutate(year = 2016,
         id = c(paste0(1:(nrow(candy_2016)), "_", 2016))) %>% 
  select(id, year, c(1:4, 6:105)) %>% 
  rename(going_out = "are_you_going_actually_going_trick_or_treating_yourself",
         gender = "your_gender",
         age = "how_old_are_you",
         country = "which_country_do_you_live_in",
         box_o_raisins = "boxo_raisins") %>% 
  rename_all(~ str_to_lower(.)) %>%
  pivot_longer(cols = c("x100_grand_bar":"york_peppermint_patties"),
               names_to = "candy",
               values_to = "rating") 

# candy 2017 
candy_2017_piv <- candy_2017 %>% 
  select(c(2:109)) %>% 
  clean_names() %>%
  mutate(year = 2017,
         id = c(paste0(1:(nrow(candy_2017)), "_", 2017))) %>%
  select(id, year, c(1:4, 6:108)) %>%
  rename_all(~ str_replace(., "^q[0-9]_", ""),
             ~ str_to_lower(.)) %>%
  rename("x100_grand_bar" = "100_grand_bar",
         anonymous_brown_globs_that_come_in_black_and_orange_wrappers = 
           "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes",
         box_o_raisins = "boxo_raisins")   %>% 
  pivot_longer(cols = c("x100_grand_bar":"york_peppermint_patties"),
               names_to = "candy",
               values_to = "rating") 

#country groups - all entries in the country column that can be reasonably 
# be interpreted to be referring to the same country have been grouped

usa_group <- c("usa", "us", "united states of america", "united states", 
               "ussa", "u.s.a.", "murica", "usa!", 
               "usa (i think but it's an election year so who can really tell)", 
               "u.s.", "america", "units states", "usa usa usa", 
               "the best one - usa", "usa! usa! usa!", 
               "the yoo ess of aaayyyyyy", "usa!!!!!!", "usa! usa!", 
               "united sates", "sub-canadian north america... 'merica", 
               "trumpistan", "merica", "united stetes", "usa usa usa usa", 
               "united states of america", "united state", "united staes", 
               "usausausa", "us of a", "unites states", "the united states", 
               "north carolina", "unied states", "u s", 
               "the united states of america", "unite states", "'merica", 
               "usas", "pittsburgh", "new york", "california", 
               "i pretend to be from canada, but i am really from the united states.", 
               "united stated", "ahem....amerca", "new jersey", "united ststes", 
               "united statss", "murrika", "usaa", "alaska", "u s a", 
               "united statea", "usa usa usa!!!!", "united states of america")
uk_group <- c("uk", "england", "united kingdom", "united kindom", "u.k.", 
              "scotland")
spain_group <- c("españa", "spain")
south_korea_group <- c("south korea", "korea")
netherlands_group <- c("the netherlands", "netherlands")
canada_group <- c("canada", "can", "canada`")
other_valid_countries <- c("japan", "france", "switzerland", "belgium", 
                           "croatia", "portugal", "españa", "spain", "panama", 
                           "australia", "hungary", "austria", "new zealand", 
                           "germany", "mexico", "brasil", "philippines", 
                           "sweden", "finland", "china", "kenya", "uae", 
                           "costa rica", "greece", "ireland", "south africa", 
                           "iceland", "denmark", "singapore", "taiwan", 
                           "hong kong")



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
        country %in% other_valid_countries ~ country,
        TRUE ~ "unknown"
      ),
    country = na_if(country, "unknown")
  ) %>%
  mutate(
    age = case_when(str_detect(age, "[^0-9\\.]") ~ "0",
                    TRUE ~ age),
    age = round(as.numeric(age)),
    age = case_when(age < 101 & age > 5 ~ age,
                    TRUE ~ 0),
    age = na_if(age, 0)
  )


write_csv(candy_all_years, here::here("clean_data/candys_data_clean.csv"))

