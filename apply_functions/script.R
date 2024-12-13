library(ggplot2)
df <- diamonds
str(df)


min_size <- apply(diamonds[,8:10], 1, min) 
min_size

apply(mtcars, 2, sd)
df_cars <- mtcars
rbind(df_cars, data.frame(apply(df_cars, 2, range), row.names = c('min_value', 'max_value')))

# -----------------------------------------
set.seed(42)
d <- data.frame(matrix(rnorm(30), nrow=5))
apply(d, 2, function(x) x[!is.na(x) & x < 0])

# ------------------------------------------
get_negative_values <- function(test_data){    
  negative_col <- apply(test_data, 2, function(x) any(x[!is.na(x)] < 0))    
  return(apply(test_data[negative_col], 2, function(x) x[!is.na(x) & x <0]))}


test_data <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), 
                                V2 = c(NA, -10.2, -10.1, -9.3, -12.2), 
                                V3 = c(NA, NA, -9.3, -10.9, -9.8)))
test_data <- as.data.frame(list(V1 = c(NA, 0.5, 0.7, 8), 
                                V2 = c(-0.3, NA, 2, 1.2), 
                                V3 = c(2, -1, -5, -1.2)))
test_data <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), 
                                V2 = c(-0.3, NA, -2, -1.2), 
                                V3 = c(1, 2, 3, NA)))
test_data <- as.data.frame(list(V1 = c(-8.9, -10.9, -7.1, -10.9, -11.1, NA), 
                                V2 = c(-9.5, -9.8, -6.8, -8.9, -11.1, NA), 
                                V3 = c(-10.2, -8.7, -9.1, -10.1, -8.8, NA), 
                                V4 = c(-10, -10, -10.3, -10.1, -8.2, NA), 
                                V5 = c(-9.1, -9.5, -9.6, -9.1, -10.9, NA), 
                                V6 = c(11.2, 8.8, NA, 9.6, 9.9, NA), 
                                V7 = c(-10.2, -9.8, -9.1, -9.4, -10.4, NA), 
                                V8 = c(-10.7, -9.3, -9.3, -10.5, -10.5, NA)))
test_data <- as.data.frame(list(V1 = c(-10.7, -10.2, -8.7, -10.2, -10.6, NA, NA), 
                                V2 = c(-10.3, -10.2, -9.9, -9.6, -12, NA, NA), 
                                V3 = c(-8.7, -10.8, -9.7, -9.8, -11.2, NA, NA), 
                                V4 = c(-7.7, -9.6, -9.5, -10.7, -9.4, NA, NA), 
                                V5 = c(-9.6, -8.6, -10.4, -10, -9.9, NA, NA), 
                                V6 = c(9.7, 8.6, 10.5, NA, 10.9, NA, NA), 
                                V7 = c(-11.2, -10, -12.9, -10.8, -9.3, NA, NA)))


get_negative_values(test_data)

# -------------------------------------------------------
apply(iris[,1:4], 2, function(x) aov(x ~ iris$Species)$coefficients)
fit <- aov(iris$Sepal.Length ~ iris$Species) 
fit$coefficients

head(iris[,apply(iris[,1:4], 2, function(x) shapiro.test(x)$p.value < 0.05)])

head(iris)

# -----------------------------------------------------
set_na_mean <- function(vect){
  mean_v <- mean(vect, na.rm=T)
  vect[is.na(vect)] <- mean_v
  return(vect)
}
na_rm  <- function(x){
  sapply(x, set_na_mean)
}

test_data <- as.data.frame(
  list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), 
      V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), 
      V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), 
      V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))
na_rm(test_data)


# -----------------------------------------------------
positive_sum <-  function(test_data){
  lapply(test_data, function(x) sum(x[x>0], na.rm=T))  
}
d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))
positive_sum(d)

# ------------------------------------------------------
cars <- c('Mazda', 'Volga', 'Volvo')
car <- 'Mazda RX4'
library(stringr)
sapply(cars, function(x) grepl(x, car))

iris[sapply(iris, is.numeric)]

df <- mtcars
df$am <- factor(df$am, labels = c('Auto', 'Manual'))
tapply(df$mpg, df$am, sd)

# --------------------------------------------
by(iris[sapply(iris, is.numeric)], iris$Species, colMeans)

str(df)
df$vs <- factor(df$vs, labels = c('V', 'S'))
by(df[sapply(df, is.numeric)], 
   df$am, function(x) sapply(x, function(col) shapiro.test(col)$p.value))

aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)

aggregate(. ~ am, df[,5:10], function(x) shapiro.test(x)$p.value)

aggregate(. ~ df$vs, df[sapply(df, is.numeric)], function(x) shapiro.test(x)$p.value)

mapply(min, df[1:16,'mpg'], df[17:32,'mpg'])

data.frame(min = apply(df[,3:4], 1, min))

# --------------------------------------------
data.frame(mapply(paste, list('row', 'col'), list(1:100, 1:100), sep='_'))

# ------------------------------------------------------
test_data <- as.data.frame(list(name = 
          c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", 
            "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", 
            "p15@HPS9"), 
          expression = c(118.84, 90.04, 106.6, 104.99, 
                      93.2, 66.84, 90.02, 108.03, 111.83)))

names = c("HPS5", "HPS6", "HPS9", "HPS2", 
          "HPS3", "HPS7", "HPS4", "HPS8")

test_data[sapply(test_data$name, function(x) any(sapply(names, 
                      function(name) grepl(name, x)))),]
test_data[grepl(paste(names, collapse='|'), test_data$name),]


grepl(names[7])




