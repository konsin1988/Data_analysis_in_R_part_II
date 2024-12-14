library(dplyr)
library(ggplot2)

df <- diamonds

df %>%
  group_by(color) %>%
  arrange(desc(price)) %>%
  slice(1:10) %>%
  select(color, price)
