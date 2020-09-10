library(readr)
library(tidyverse)
library("leaflet")
library(psych)
library(lubridate)
library(cluster)
library(factoextra)


orders <- read_csv("data/olist_orders_dataset.csv")
customers <- read_csv("data/olist_customers_dataset.csv")
geolocation <- read_csv("data/olist_geolocation_dataset.csv")
order_items <- read_csv("data/olist_order_items_dataset.csv")
order_payments <- read_csv("data/olist_order_payments_dataset.csv")
order_reviews <- read_csv("data/olist_order_reviews_dataset.csv")
products <- read_csv("data/olist_products_dataset.csv")
sellers <- read_csv("data/olist_sellers_dataset.csv")
product_translation <- read_csv("data/product_category_name_translation.csv")


# customers 

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
  select(customer_id, order_id, total_payment, no_items) -> order_value2

order_value2 %>%
  left_join(orders %>% 
              select(order_id, order_purchase_timestamp) ) -> order_value3

customers %>%
  left_join(order_value3) -> customers2

customers2 %>%
  select(customer_zip_code_prefix) %>%
  mutate(a = str_sub(customer_zip_code_prefix,end = 4)) %>%
  group_by(a) %>%
  tally()

customers2 %>% 
  mutate(geo = str_sub(customer_zip_code_prefix, end = 4)) %>%
  group_by(geo) %>%
  summarise(no_of_orders = n(),
            value_sum = sum(total_payment, na.rm = T),
            value_mean = mean(total_payment, na.rm = T),
            no_of_items_mean = mean(no_items, na.rm = T),
            tstp_first_order = min(order_purchase_timestamp, na.rm = T),
            tstp_last_order = max(order_purchase_timestamp, na.rm = T),
  ) -> geo_order_values


geo_order_values

as_of_date <- max(orders$order_purchase_timestamp)
first_purchase <- min(orders$order_purchase_timestamp)

geo_order_values %>%
  mutate(
    days_since_last_order = as.numeric(make_difftime(as_of_date - tstp_last_order, units = 'days')),
    days_since_first_order = as.numeric(make_difftime(as_of_date - tstp_first_order, units = 'days')),
    shopping_time_span = days_since_first_order-days_since_last_order
  ) -> geo_order_values2

# Select only frequent customers
# customers_order_values2 %>%
#   filter(no_of_orders > 1) %>% 
#   select(
#     customer_unique_id,
#     no_of_orders, 
#     value_sum, 
#     value_mean, 
#     no_of_items_mean,
#     days_since_first_order, 
#     days_since_last_order,
#     shopping_time_span
#   ) -> frequent_customers


geo_order_values2 %>%
  select(no_of_orders, value_mean) -> a
a %>% mutate_all(scale) %>% pam(k = 2) -> p3

ggplot(a, aes(x = no_of_orders, y= value_mean)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

fviz_nbclust(a%>%mutate_all(scale), FUNcluster = pam,k.max = 3) -> nbclust

nbclust

a %>%
  # mutate_all(scale) %>%
  mutate_all(log) %>%
  get_clust_tendency(100)

silhouette(p3)[,3] %>% mean

