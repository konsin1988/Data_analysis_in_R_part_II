library(dplyr)
library(ggplot2)

df <- diamonds
df %>% 
  filter(color == c('E', 'H')) %>%
  group_by(cut, color) %>%
  select_if(is.numeric) %>%
  summarise_all(mean)

df %>%
  filter(color == 'D', price > 5000, cut == 'Fair') %>%
  summarize(n()) 

names <- df %>%
  select(matches('^c.{4,8}')) %>%
  names()
  
df %>% 
  select(all_of(names))

df %>% 
  group_by(color) %>%
  summarize_if(is.numeric, mean)

df %>%
  select(names) %>% 
  group_by(color) %>%
  summarize_at(names[1], mean)





