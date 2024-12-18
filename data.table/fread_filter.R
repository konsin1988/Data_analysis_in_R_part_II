library(data.table)
library(dplyr)

products <- fread('./dataset_5/products.csv')

products[price > 10000, , by=brand] 

products[price > 10000, list(price.mean = mean(price/1000)), by = brand]

head(products[order(-price), .(name, price.1k = price / 1000)], 5)

# first 5 most expensive products
products[order(-price), .(head(name, 5), head(price, 5))]

products[
  ,.(price_factor = mean(price)),
  by = brand
  ]

products[order(-price),.(price = head(price, 3)), by=brand]







