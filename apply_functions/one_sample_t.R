library(dplyr)

one_sample_t <- function(test_data, general_mean){
  test_data %>%
    select_if(is.numeric) %>%
    lapply(., function(x) {
      t_test <- t.test(x, mu=general_mean)}) %>%
    lapply(., function(x) c(x$statistic, x$parameter, x$p.value))  
}

one_sample_t(iris, 4)

list_my <- iris %>%
  select_if(is.numeric) %>%
  lapply(., function(x) {
    t_test <- t.test(x, mu=general_mean)}) %>%
  lapply(., function(x) c(x$statistic, x$parameter, x$p.value))


       