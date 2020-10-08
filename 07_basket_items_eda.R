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

order_items

# categories of products are in portuguese - translating to english

products %>%
  left_join(product_translation) %>%
  select(-product_category_name) %>%
  select(1,9, everything()) -> products2
# Except category name there are also descriptive measures of the product, 
# like height of the product etc. Nothing interesting.

products2 %>%nrow()

products2 %>%
  group_by(product_category_name_english) %>%
  tally(sort = T)

products2 %>%
  select(1,2) -> products3

#' There are 33k of products, but only 72 categories.
#' 
#' 
#' Now checking products in relation to actual orders - and not the avaliable ones

order_items %>%
  select(order_id, product_id, price) %>%
  left_join(products3) -> order_items2

order_items2 %>%
  distinct()

#' Number of orders:
order_items2 %>%
  select(1) %>%
  distinct() %>%
  nrow()

#' Number of items per order
order_items2 %>%
  group_by(order_id) %>%
  tally(sort=T) %>%
  group_by(n) %>%
  tally() %>%
  mutate(nn = nn/sum(nn)) -> no_items_per_order

no_items_per_order
#' Great majority of items has less than 4 items in basket. 
#' Checking the price of the order by number of items bought:

order_items2 %>%
  group_by(order_id) %>%
  summarise(
    no_items = n(),
    price = sum(price)
  ) -> a

ggplot(a, aes(x = no_items, group=no_items, y = price)) +
  geom_boxplot() ->  pl
  plotly::ggplotly(pl)
# After 10 items there are only couple of orders present in the dataset - very noisy
  
a %>%
  group_by(no_items) %>%
  summarise(median_price = median(price)) %>%
  left_join(no_items_per_order, by = c('no_items' = 'n'))
#' up to some point the value of the order rises with number of items 
#' - so although these are very non_typical values and could not be accounted for, 
#' it would be unwise as these have the biggest potential for revenue.  


#' What item categories are the most popular?

order_items2 %>%
  group_by(product_category_name_english) %>%
  tally(sort = T) %>%
  View()


order_items %>%
  group_by(order_id) %>%
  filter(order_item_id == max(order_item_id)) %>%
  ungroup() %>%
  group_by(order_item_id) %>%
  tally(sort = T) %>%
  mutate(nn = n/sum(n))

order_items %>%
  filter(order_id == '1b15974a0141d54e36626dca3fdc731a')
  group_by(order_id, product_id) %>%
  tally(sort = T) 
