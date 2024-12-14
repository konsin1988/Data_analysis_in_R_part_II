library(dplyr)
library(ggplot2)

df <- data.frame(x = rnorm(10000), y = rnorm(10000))

df <- df %>%
  mutate(z = factor(as.numeric(x > y), labels = c('Less', 'More')))

df <- diamonds
df %>%
  select(cut:price, -clarity) %>%
  slice(20:nrow(.))

df %>%
  select(starts_with(c('d', 'c')))

df %>%
  arrange(price, depth) %>% 
  select(matches('^c.{4}$')) %>%
  filter(carat > 2 | color == 'J') #%>%
  # summarize_at('carat', mean)

# sort 
df %>%
  arrange(price, depth)

# rename
df %>%
  rename(table_t = table) %>%
  select(table_t, clarity, color) %>%
  arrange(desc(table_t)) %>%
  filter(color == 'F') %>%
  slice(20:40)

df %>%
  mutate(sqrt_price = sqrt(price)) %>%
  select(carat, sqrt_price) %>%
  filter(sqrt_price > 65) %>%
  arrange(carat)

  
  
  
  
  
  
  
  
  