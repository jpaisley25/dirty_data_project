
library(tidyverse)
library(janitor)
library(stringr)

#read in data

raw_cake_data <- read_csv(here::here("raw_data/cake-ingredients-1961.csv"))
cake_code <-  read_csv(here::here("raw_data/cake_ingredient_code.csv"))

#create list of ingredients and their measures

ingredients_and_measurements <- cake_code %>% 
  unite("i_and_m", ingredient:measure, sep = ",") %>% 
  pull(i_and_m)


# cleaning script
## rename incredient columns with the vector of ingredients and measures
## pivot so data is tidy
## seperate the ingredients and measures into different columns
## recode sour cream to removal the measurement from the ingredient column
## clean names

clean_cake_data <- raw_cake_data %>%
  rename_at(vars(AE:ZH), ~ ingredients_and_measurements) %>%
  pivot_longer(cols = 2:35,
               names_to = "temp",
               values_to = "amount") %>%
  separate(
    col = "temp",
    into = c("ingredient", "measure"),
    sep = ","
  ) %>%
  mutate(
    ingredient = recode(ingredient, "Sour cream cup" = "Sour cream"),
    measure = recode(measure, "NA" = "cup")
  ) %>% 
  clean_names()


# write clean data

write_csv(clean_cake_data, here::here("clean_data/cake_data_clean.csv"))

?rename_at
