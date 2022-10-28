
# cleaning function

tidy_candy <- function(data, x = 2015){
  temp <- data %>% 
  rename_at(vars(everything(.)), ~ str_extract(names(data), "[^\\[].*[^\\]]"))
  
  temp %>% 
    rename_at(vars(everything(.)), ~ str_remove(names(temp), "Q[0-9]+:?.?\\|? ")) %>% 
    rename_all(~ str_to_lower(.)) %>% 
    select(2:"york peppermint patties") %>% 
    mutate(
      year = x,
      id = c(paste0(1:(nrow(
        temp
      )), "_", x))) %>% 
    pivot_longer(
      cols = c(6:"york peppermint patties"),
      names_to = "candy",
      values_to = "rating"
    ) %>%
    clean_names %>% 
    select(id, year, going_out, gender, age, country, candy, rating) 
}