library(data.table)

products <- fread('./dataset_5/products.csv')

filter.expensive.available <- function(products, brands) {

}

brands <- products[,.(brand),by=brand]$brand[1:50]

products[
    (brand %in% brands) &
    (price >= 50000) &
    (available == T)
  ]
