library(dplyr)

descriptive_stats <- function (dataset){
  
}

test_data <- read.csv("https://stepik.org/media/attachments/course/724/salary.csv")
str(test_data)

factors <- names(test_data[!sapply(test_data, is.numeric)])
numeric_vars <- names(test_data[sapply(test_data, is.numeric)])

test_data %>%
  mutate_at(vars(all_of(factors)), as.factor) %>%
  group_by(.[factors]) %>%
  summarize(n = n(), 
    mean=mean(salary, na.rm = TRUE),
    sd = sd(salary, na.rm = T),
    median = median(salary, na.rm = T),
    first_quartile = quantile(salary, 0.25, na.rm=T),
    third_quartile = quantile(salary, 0.75, na.rm=T),
    na_values = sum(is.na(salary))
    )

descriptive_stats <- function (dataset){
  dataset %>%
    mutate_at(vars(gender, country), as.factor) %>%
    group_by(.[factors]) %>%
    summarize(n = n(), 
              mean=mean(salary, na.rm = TRUE),
              sd = sd(salary, na.rm = T),
              median = median(salary, na.rm = T),
              first_quartile = quantile(salary, 0.25, na.rm=T),
              third_quartile = quantile(salary, 0.75, na.rm=T),
              na_values = sum(is.na(salary))
    )
}

descriptive_stats(test_data)
