library(data.table)

products <- fread('./dataset_5/products.csv')

# .SD - subset of data
products[
  order(-price),
  head(.SD, 3),
  by = brand
][,
  .(brand, product_id, price)
]

# .N - number of rows = nrow(.SD)
products[
  price > 10000,
  .(expensive.items = .N),
  by = brand
][
  order(-expensive.items)
]

# := 
df <- as.data.table(iris)
df[, c('sum.petals', 'sub.petals') := 
     list(Petal.Length + Petal.Width, Petal.Length - Petal.Width)]

df[, `:=`(sum.petals = Petal.Length + Petal.Width, sub.petals = Petal.Length - Petal.Width)]

# merge
purchases <- fread('./dataset_5/purchases.csv')

merge(purchases, products, by = 'product_id')

setkey(products, product_id, price)
setkey(purchases, product_id, ordernumber)

merge(products, purchases)

products[J(c(158, 208, 10001, 826355))]

