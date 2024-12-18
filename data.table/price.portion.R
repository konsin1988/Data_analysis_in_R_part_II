library(data.table)

# data
purchases <- fread('./dataset_5/purchases.csv')
str(purchases)

# solution
mark.position.portion <- function(purchases){
purchases <- purchases[quantity > 0]
purchases[purchases[
  ,.(sum_order = sum(price * quantity)), by = ordernumber
], on = 'ordernumber', 
price.portion := 
  as.character(format(round(price * quantity /sum_order * 100, 2), 
                      nsmall = 2))]
  return(purchases)
}

# test
sample.purchases <- data.table(price = c(100, 300, 50, 700, 30),
                               ordernumber = c(1,1,1,2,3),
                               quantity = c(1,1,2,1,-1),
                               product_id = 1:5)
mark.position.portion(sample.purchases)


# test
purchases <- fread('./dataset_5/purchases.csv')
str(purchases)
products <- fread('./dataset_5/products.csv')
str(products)

purchases[products[,.(product_id, price_sum = sum(price * 2)), by=brand], 
          on='product_id', product_price := 
            pricecents + price_sum]
