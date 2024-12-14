library(dplyr)
library(ggplot2)

d <- mtcars %>%
  select(am, vs) %>%
  mutate_all(as.factor) %>%
  mutate(vs = recode(vs, '1' = 'V', '0' = 'S'))
str(d)
