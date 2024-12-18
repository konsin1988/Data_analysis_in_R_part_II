library(data.table)
library(stringr)

df <- fread("test_data_01.csv")
df <- read.csv('test_data_01.csv')


str(df)
head(df, 10)


# with data.table -----------------------------
fix_data <- function(d) {
  data_without_space <- 
    sapply(d[, !grep('[A-Z]', d), with=F], function(x) gsub(' ', '', x))
  names <- colnames(d[, ! grep('[A-Z]', d), with=F])
  lapply(data.table(data_without_space), as.numeric)
  d[, c(names) := lapply(data.table(data_without_space), as.numeric)]
  return(d)
}

# without libraries ------------------------
fix_data_without_dt <- function(df){
  cols_to_fix = !grepl('[A-Z]', df)
  df[cols_to_fix] <- sapply(df[cols_to_fix], 
          function(x) as.numeric(gsub(' ', '', x)))
  return(df)
}
fix_data_without_dt(df)

# dplyr --------------------------------------
fix_data_dplyr <- function(df){
  library(dplyr)
  df %>%
    mutate_if(!grepl('[A-Z]', .), 
              function(x) sapply(x, function(y) as.numeric(gsub(' ', '', y))))
}
temp <- fix_data_dplyr(df)
str(temp)


test_data <- as.data.frame(list(V1 = 
      c("-0.20 31", "0.7542", "0.70 34", "0.289", "-1. 2196"), 
      V2 = c("-0.3985", "0.4293", "-1.8307", "-1.3018", "3 .0653"), 
      V3 = c("1. 7192", "0.048 5", "0.177 4", "1.026", "0.6123"), 
      V4 = c("-1.1281", "0.06 45", "0.7 883", "0.2146", "1.3485"), 
      V5 = c("LD 34", "HP 129", "II 2", "LD 34", "HP 129")))

fix_data_without_dt(test_data)





   