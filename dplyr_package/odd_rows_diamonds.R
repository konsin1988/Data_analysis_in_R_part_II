library(dplyr)
library(ggplot2)

df <- diamonds

df %>%
  slice(seq(1,nrow(.), 2))

# mtcars task 
mtcars %>%
  select(mpg, hp, am, vs) %>%
  filter(mpg > 14, hp > 100) %>%
  arrange(desc(mpg)) %>%
  slice(1:10) %>%
  rename('Miles per gallon' = mpg, 'Gross horsepower' = hp)
