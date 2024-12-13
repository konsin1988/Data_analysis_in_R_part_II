suppressMessages(library(dplyr))
# find_outliers <- function(t){
#   num_var_name <- names(t[sapply(t, is.numeric)])
#   is_outliers <- function(x) {
#     as.numeric(abs(x  - mean(x)) > 2 * sd(x))
#   }
#   df_with_outliers <- do.call(rbind, by(t, t[!sapply(df, is.numeric)], 
#         function(x) { x$is_outlier <- is_outliers(x[[num_var_name]])
#                       return(x)
#         }))
#   x_names <- row.names(df_with_outliers)
#   t$is_outlier <- 
#     df_with_outliers[order(as.numeric(x_names)),,drop=F]$is_outlier
#   t
# }
ToothGrowth$dose <- factor(ToothGrowth$dose)
find_outliers(ToothGrowth)
df <- ToothGrowth
find_outliers(df)


is_outliers <- function(x) {
  as.numeric(abs(x  - mean(x)) > 2 * sd(x))
}
factors <- colnames((df[!sapply(df, is.numeric)]))
num_var <- colnames(df[sapply(df, is.numeric)])
df_sum <- df %>%
  group_by(.[factors]) %>%
  summarize_all(list(mean = mean, sd = sd))
df_sum
df_merged <- merge(df, df_sum, by=c(factors))
df_merged$is_outlier <- 
  as.numeric(abs(df_merged[[num_var]] - df_merged$mean) > df_merged$sd * 2)
df_result <- subset(df_merged, select=-c(mean, sd))

# The best way))
df_sum <- df %>%
  group_by(.[factors]) %>%
  summarize_all(list(mean = mean, sd = sd)) %>%
  merge(df, by=c(factors)) %>%
  mutate(is_outlier = 
  as.numeric(abs(.[[num_var]] - .$mean) > .$sd * 2)) %>%
  subset(select=-c(mean, sd))










