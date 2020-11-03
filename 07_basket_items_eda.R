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

# If particular item is present in the basket more than one time the observation
# Is doubled - adding variable number of items and total price

order_items2 %>%
  group_by(order_id, product_id, product_category_name_english) %>%
  summarise(
    qty = n(),
    unit_price = min(price),
    total_price =  qty*unit_price
  ) %>%
  ungroup() -> order_items3

#' Number of orders:
order_items3 %>%
  select(1) %>%
  distinct() %>%
  nrow()

#' Number of items per order
order_items3 %>%
  group_by(order_id) %>%
  summarise(n = sum(qty)) %>%
  arrange(-n) %>%
  group_by(n) %>%
  tally() %>%
  mutate(nnn = nn/sum(nn)) -> no_items_per_order

no_items_per_order
#' Great majority of items has less than 4 items in basket. 
#' Checking the price of the order by number of items bought:

order_items3 %>%
  group_by(order_id) %>%
  summarise(
    no_items = sum(qty),
    price = sum(total_price)
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

order_items3 %>%
  group_by(product_category_name_english) %>%
  tally(sort = T) %>%
  View()



order_items3


order_items3 %>%
  select(1,3) %>%
  distinct() %>%
  mutate(a=1) %>%
  pivot_wider(names_from = product_category_name_english, 
              values_from = a, values_fill = 0) -> order_matrix

options(scipen = 999)
order_matrix %>%
  summarise_if(is.numeric, sum) %>%
  pivot_longer(everything()) %>%
  arrange(-value) %>%
  View()

#' Clustering
#' 
#' Options:
#' - clustering without dim. reduction - k-modes or other for categorical data
#' - first dim. reduction then standard clustering with euc. metric
#' Dim reduction:
#' - UMAP - it has categorical metrics: https://umap-learn.readthedocs.io/en/latest/parameters.html#metric
#'   Implementation in R is worse - no distance metrics. Better than t-sne and MDS as it is way faster
#' - first correlation matrix, then SVD (the same as PCA but correlation matrix insteado of covariance)
#' - standard PCA - don't care about categorical vars
#' 
#'   
#'   