library(dplyr)

df <- mtcars

df %>%
  # select_if(is.numeric) %>%
  mutate_all(function(x) log((x - min(x))/(max(x) - min(x)) + 1 ))

df %>%
  mutate_at(vars(names(.[sapply(., is.numeric)])), 
            function(x) log((x - min(x))/(max(x) - min(x)) + 1 ))

df %>%
  mutate_at(.vars = names(.)[sapply(., is.numeric)], 
            function(x) log((x - min(x))/(max(x) - min(x)) + 1 ))
