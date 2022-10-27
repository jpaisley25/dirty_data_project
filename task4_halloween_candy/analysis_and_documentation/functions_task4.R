#functions


find_the_mean_age <- function(data){
  data %>% 
    group_by(id) %>% 
    summarise(avg_age = mean(age)) %>% 
    summarise(avg_age = mean(avg_age))
}

find_the_median_age <- function(data){
  data %>% 
    group_by(id) %>% 
    summarise(med_age = median(age)) %>% 
    summarise(med_age = median(med_age))
}