
library(tidyverse)
library(stringr)

# read data

rwa_data <- read_csv(here::here("raw_data/rwa.csv")) 

# only select data needed to answer the questions 

rwa_needed_data <- rwa_data %>%
  select(
    c(
      "Q3":"Q22",
      "testelapse",
      "education",
      "urban",
      "gender",
      "age",
      "hand",
      "familysize"
    )
  ) %>%
  mutate(
    id = c(1:nrow(rwa_data)),
    across(
      .cols = c("Q3":"Q22"),
      .fns = na_if,
      0
    ),
    across(
      .cols = c("Q4", "Q6", "Q8", "Q9",
                "Q11", "Q13", "Q15", "Q18",
                "Q20", "Q21"),
      .fns = ~ 10 - .x,
      na.rm = TRUE
    )
  )


# get the rwa scores (avg of Q3 to Q22 with unreversed scores)

rwa_scores <- rwa_needed_data %>%
  select(c("id", "Q3":"Q22")) %>%
  pivot_longer(cols = c("Q3":"Q22"),
               names_to = "question",
               values_to = "answer") %>%
  group_by(id) %>%
  summarise(rwa_score = mean(answer))

# join scores back on

rwa_needed_data <- rwa_needed_data %>% 
  select(id, testelapse, education, urban, gender, age, hand, familysize) %>% 
  left_join(
    y = rwa_scores,
    by = c("id" = "id") )

# recoding

rwa_cleaned_data <- rwa_needed_data %>%
  rename(
    education_level = "education",
    childhood_area = "urban",
    handedness = "hand",
    test_duration_s = "testelapse"
  ) %>%
  mutate(
    education_level = case_when(
      education_level == 1 ~ "less than high school",
      education_level == 2 ~ "high school",
      education_level == 3 ~ "university degree",
      education_level == 4 ~ "graduate degree"
    ),
    childhood_area = case_when(
      childhood_area == 1 ~ "rural",
      childhood_area == 2 ~ "suburban",
      childhood_area == 3 ~ "urban"
    ),
    gender = case_when(
      gender == 1 ~ "male",
      gender == 2 ~ "female",
      gender == 3 ~ "other"
    ),
    handedness = case_when(
      handedness == 1 ~ "right",
      handedness == 2 ~ "left",
      handedness == 3 ~ "both",
    ),
    age = case_when(
      age > 101 ~ as.numeric(NA),
      TRUE ~ age
    )
  ) %>% 
  drop_na()


write_csv(rwa_cleaned_data, here::here("clean_data/clean_data_rwa.csv"))

