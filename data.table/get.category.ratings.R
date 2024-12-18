library(data.table)

# data
purchases <- fread('./dataset_5/purchases.csv')
categories <- fread('./dataset_5/categories.csv')
product.category <- fread('./dataset_5/product-categories.csv')
products <- fread('./dataset_5/products.csv')

# str
str(categories)
str(product_categories)
str(purchases)

# set keys
setkey(purchases, product_id)
setkey(categories, category_id)
setkey(product.category, category_id, product_id)

# solution
get.category.ratings <- function(purchases, product.category) {
  merge(purchases, product.category, by = 'product_id')[
    ,.(product_id, totalcents, quantity, category_id)
  ][
    ,.(totalcents = sum(totalcents), quantity = sum(quantity)),by = category_id
  ][
    # order(category_id)
  ]
}

# test data
product.category <- data.table(product_id = c(1,1,2,2,3),
                               category_id = c(1,2,1,3,3))
purchases <- data.table(product_id = c(1, 2, 3),
                        totalcents = c(100, 200, 300),
                        quantity = c(1, 1, 3))
get.category.ratings(purchases, product.category)



