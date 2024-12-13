library(dplyr)

smart_lm <- function(x){
  x_IV <- x %>% 
    subset(select = 2:length(.)) %>%
    select_if(sapply(., function(x) shapiro.test(x)$p.value > 0.05)) 
  if(length(x_IV) == 0){
    return("There are no normal variables in the data")
  } else {
  fit <- lm(cbind(x[,1], x_IV))    
  summary(fit)$coef[,1]
  }
}
smart_lm(swiss)

test_data <- data.frame(x = 1:100, y = 1:100, z = 1:100)
smart_lm(test_data)


df <- swiss
df_IV <- df[,2:length(df)]

df_normal <- df_IV[sapply(df_IV, function(x) shapiro.test(x)$p.value > 0.05)]
fit <- lm(cbind(df[,1], df_normal))

df[,2:length(df)][sapply(df_IV, function(x) shapiro.test(x)$p.value > 0.05)]
fit <- lm(cbind(test_data[,1], 
        test_data[,2:length(test_data)][sapply(df_IV, function(x) shapiro.test(x)$p.value > 0.05)]))    
summary(fit)$coef[1:4, 1]

df_IV <- df %>% 
  subset(select = 2:length(.)) %>%
  select_if(sapply(., function(x) shapiro.test(x)$p.value > 0.05)) 

  