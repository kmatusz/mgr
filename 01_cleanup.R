library(readr)
library(tidyverse)
library("leaflet")
library(psych)


orders <- read_csv("data/olist_orders_dataset.csv")
customers <- read_csv("data/olist_customers_dataset.csv")
geolocation <- read_csv("data/olist_geolocation_dataset.csv")
order_items <- read_csv("data/olist_order_items_dataset.csv")
order_payments <- read_csv("data/olist_order_payments_dataset.csv")
order_reviews <- read_csv("data/olist_order_reviews_dataset.csv")
products <- read_csv("data/olist_products_dataset.csv")
sellers <- read_csv("data/olist_sellers_dataset.csv")
product_translation <- read_csv("data/product_category_name_translation.csv")


customers 

# Basic data preparation

# Number of orders by customer
# Mean value of orders by customer 
# Number of items in order

order_items %>%
  group_by(order_id) %>%
  tally(name = 'no_items') -> no_items_per_order
# znowu - bardzo duża część tylko 1 item w koszyku

order_payments %>%
  group_by(order_id) %>%
  summarise(total_payment = sum(payment_value, na.rm = T)) -> order_value


orders %>%
  select(order_id, customer_id) %>%
  left_join(order_value) %>%
  left_join(no_items_per_order) %>%
  select(customer_id, total_payment, no_items) -> order_value2


customers %>%
  left_join(order_value2) %>%
  group_by(customer_unique_id) %>%
  summarise(no_of_orders = n(),
            value_sum = sum(total_payment, na.rm = T),
            value_mean = mean(total_payment, na.rm = T),
            no_of_items_mean = mean(no_items)) -> customers_order_values


customers_order_values

customers_order_values %>%
  summary()

table(customers_order_values$no_of_orders)

# 93/96 kupiło tylko raz

customers_order_values %>%
  filter(no_of_orders == 1)


customers_order_values %>%
  mutate(if_one_timer = ifelse(no_of_orders == 1, 1, 0)) -> customers_order_values

psych::describeBy(customers_order_values$value_mean, group = customers_order_values$if_one_timer)




customers_order_values




