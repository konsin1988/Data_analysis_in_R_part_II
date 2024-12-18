library(data.table)

purchases <- fread('./dataset_5/purchases.csv')

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

sample.purchases[
  quantity > 0
  ][
    ,.(order_price = sum(price * quantity)), by=ordernumber
  ][,median(order_price)]

names(purchases)
