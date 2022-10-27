

library(tidyverse)

#reading in data
raw_dog_data <- read_csv(here::here("raw_data/dog_survey.csv"))

#non-email value for later
non_email <- raw_dog_data %>% 
  filter(!str_detect(email, "@")) %>% 
  pull(email)


#cleaning script
clean_dog_data <- raw_dog_data %>%
  distinct() %>%  #remove duplicates
  select(id:dog_age) %>% #remove unimportant columns
  filter(!str_detect(dog_age, "and") &
           !str_detect(dog_age, ",")) %>% #removing rows with multi dogs
  mutate(
    amount_spent_on_dog_food = case_when( #amount spent cleaning module
      !str_detect(amount_spent_on_dog_food, "^£[0-9]+") ~ "0",
      TRUE ~ str_extract(amount_spent_on_dog_food, "[0-9]+\\.[0-9]+")
    ),
    amount_spent_on_dog_food = na_if(amount_spent_on_dog_food, "0"),
    amount_spent_on_dog_food = as.numeric(amount_spent_on_dog_food)
  ) %>%
  mutate(#dog sex cleaning module
    dog_gender = case_when(
      str_detect(dog_gender, "(?i)^fem") ~ "F",
      str_detect(dog_gender, "(?i)^male") ~ "M",
      dog_gender == "F" | dog_gender == "M" ~ dog_gender,
      TRUE ~ "unknown"
    ),
    dog_gender = na_if(dog_gender, "unknown")
  ) %>%
  mutate(# dog size cleaning module
    dog_size = case_when(
      str_detect(dog_size, "(?i)large") ~ "L",
      str_detect(dog_size, "(?i)medium") ~ "M",
      str_detect(dog_size, "(?i)small") ~ "S",
      dog_size %in% c("XS", "S", "M", "L", "XL") ~ dog_size,
      TRUE ~ "unknown"
    ),
    dog_size = na_if(dog_size, "unknown")
  ) %>%
  mutate(#dog age cleaning module
    dog_age = case_when(str_detect(dog_age, "[^0-9]") ~ "NA",
                        TRUE ~ dog_age),
    dog_age = na_if(dog_age, "NA"),
    dog_age = as.numeric(dog_age)
  ) %>%
  mutate(email = na_if(email, non_email)) #email fix




#write clean data to csv 

write_csv(clean_dog_data, here::here("clean_data/clean_dog_data.csv"))
