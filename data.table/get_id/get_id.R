all_data

df_id <- data.frame(id=seq(1,20))

df <- data.frame(lapply(all_data, function(x) merge(x, df_id, by = 'id', all=T)))
names <- names(df)
df <- cbind(data.frame(id = seq(1, 20), df[, grepl('temp.*', names)]))

df <- df[apply(df, 1, function(x) all(!is.na(x))),]
df$mean_temp <- rowMeans(df[,2:ncol(df)])
df[,c('id', 'mean_temp')]

# solution without dplyr ----------------------------
get_id <- function(data_list){
  df <- data.frame(id=seq(1,20))
  df <- data.frame(lapply(all_data, function(x) merge(x, df, by = 'id', all=T)))
  names <- names(df)
  df <- cbind(data.frame(id = seq(1, 20), df[, grepl('temp.*', names)]))
  df <- df[apply(df, 1, function(x) all(!is.na(x))),]
  df$mean_temp <- rowMeans(df[,2:ncol(df)])
  return(df[,c('id', 'mean_temp')])
}

get_id(all_data)

# solution with dplyr 

library(dplyr)
all_data %>%
  lapply(function(x) merge(x, data.frame(id=seq(1,20)), by = 'id', all=T)) %>%
  as.data.frame() %>%
  na.exclude() %>% 
  select_at(vars(id, contains('temp'))) %>%
  mutate(mean_temp = rowMeans(.[,2:ncol(.)])) %>%
  select(id, mean_temp)


# solution with data.table ------------------------
suppressMessages(library(data.table))

do.call()

merge_two(all_data[3], all_data[7])




