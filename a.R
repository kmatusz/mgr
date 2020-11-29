library(readr)
library(tidyverse)
library("leaflet")


orders <- read_csv("data/olist_orders_dataset.csv")
customers <- read_csv("data/olist_customers_dataset.csv")
geolocation <- read_csv("data/olist_geolocation_dataset.csv")
order_items <- read_csv("data/olist_order_items_dataset.csv")
order_payments <- read_csv("data/olist_order_payments_dataset.csv")
order_reviews <- read_csv("data/olist_order_reviews_dataset.csv")
products <- read_csv("data/olist_products_dataset.csv")
sellers <- read_csv("data/olist_sellers_dataset.csv")
product_translation <- read_csv("data/product_category_name_translation.csv")


#' ### Orders 
orders %>% head(5)

length(unique(orders$order_id))
length(unique(orders$customer_id)) 

#' Customer_id unique

#' Order status
orders %>% 
  group_by(order_status) %>%
  tally() %>%
  arrange(-n)

#' Najwięcej delivered, ale jest trochę innych 


orders %>%
  mutate(purchased_date = lubridate::date(order_purchase_timestamp),
         delivered_date = lubridate::date(order_delivered_customer_date),
         time_to_purchase = delivered_date - purchased_date
         ) -> a

#' Nierówny rozkład daty

ggplot(a, aes(x = purchased_date)) + 
  geom_histogram(bins = 50)

a$purchased_date %>% summary

#' Dane od 2016.09 do 2018.10


#' Pytania:
#' - Ile średnio itemów w koszyku
#' - Jaki rozkład kosztu zamówienia, jaki koszt dostawy
#' - Jaki rozkład ilości zamówień patrząc po kliencie
#' - Analiza koszyka - jakie produkty najczęściej ze sobą
#' - Analiza geograficzna - mapy, rozkład w przestrzeni
#' - Do geo wymiaru dodać inne wymiary - średnie zamówienie, typy zamawianych towarów
#' - Zrobić joina na wszystkim jak tylko się da

# Spatial

geolocation %>% group_by(geolocation_zip_code_prefix) %>%
  tally() %>%
  arrange(-n)


geolocation %>% 
  filter(geolocation_zip_code_prefix == "24220") %>% 
  View()

#' W geolocation jednemu postal code odpowiada wiele koordynatów. 
#' Coś z tym trzeba zrobić w przyszłości, 
#' na przykład zagregować biorąc średnią z wymiarów

geolocation %>%
  group_by(geolocation_zip_code_prefix) %>%
  nest() %>%
  mutate(sample_row = map(data, sample_n, 1)) -> b


geolocation %>%
  sample_n(10000) %>% 
  rename(lat = geolocation_lat,
         lng = geolocation_lng) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(radius = 0.5)
  
# Optymalizacja customer review
# Wyznaczenie odpowiedzialnych czynników
table(order_reviews$review_score)


order_reviews %>%
  group_by(review_score) %>%
  summarise(a = sum(!is.na(review_comment_message))/n())
