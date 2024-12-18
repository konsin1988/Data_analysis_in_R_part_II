library(data.table)

purchases <- fread('./dataset_5/purchases.csv')

head(purchases)
purchases[
   (quantity >= 0)
  ][
    order(-pricecents)
  ][
   ,.(ordernumber, product_id)
  ]
