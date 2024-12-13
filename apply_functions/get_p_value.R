library(dplyr)

get_p_value <- function(test_list){
  lapply(test_list, function(x) x$p.value) 
}
library(data.table)

