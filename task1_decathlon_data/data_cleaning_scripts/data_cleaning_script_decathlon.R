#cleaning for task 1 - decathlon data

library(tidyverse)
library(stringr)
library(janitor)

decathlon_results <- readRDS(here::here("raw_data/decathlon.rds"))

clean_decathlon_results <- decathlon_results %>% 
  tibble::rownames_to_column("name") %>% 
  pivot_longer(cols = c("100m":"1500m"), 
               names_to = "event",
               values_to = "event_performance"
  ) %>% 
  clean_names() %>% 
  mutate(name = str_to_lower(name)) %>% 
  mutate(event = str_to_lower(event),
         event = str_replace(event, "\\.", "_"))

write_csv(clean_decathlon_results, here::here("clean_data/decatlon_clean_data.csv"))