library(dplyr)

to_factors <- function(test_data, factors){
  
}

to_factors(mtcars[1:4], factors = c(1, 3))

factors = 4
df <- mtcars[,1:4]

df %>%
  mutate_at(.vars = factors, 
      function(x) factor(ifelse(x > mean(x, na.rm=T), 1, 0)))



