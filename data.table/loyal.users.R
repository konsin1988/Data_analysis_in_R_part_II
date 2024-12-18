library(data.table)

purchases <- fread('./dataset_5/purchases.csv')
products <- fread('./dataset_5/products.csv')

# keys to faster connections
setkey(purchases, product_id, externalsessionid)
setkey(products, product_id, brand)

purchases.with_brands <- merge(
  purchases,
  products[,.(product_id, brand)],
  by = 'product_id'
)

top.20.brands <- head(purchases.with_brands[,
  list(total.brands.users = length(unique(externalsessionid))),
  by = brand
  ][
    order(-total.brands.users) 
  ], 20)

users <- purchases.with_brands[
  , list(unique_brands = length(unique(brand)),
      items = .N,
      brand = brand[1]
         ),
  by = externalsessionid
][order(-externalsessionid)]

brand.loyal.users <- users[items > 1][unique_brands == 1][
  ,.(total.loyal.orders = .N), 
  by = brand
]  
  
brand.stats <- merge(
  top.20.brands, 
  brand.loyal.users,
  by='brand'
)
  
brand.stats[, `loyal` := round(total.loyal.orders / total.brands.users * 100, 2)
            ][
              order(-loyal)
            ]



