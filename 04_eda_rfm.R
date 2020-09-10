library(readr)
library(tidyverse)
library("leaflet")
library(psych)
library(lubridate)
library(cluster)
library(factoextra)
library(fpc)

orders <- read_csv("data/olist_orders_dataset.csv")
customers <- read_csv("data/olist_customers_dataset.csv")
geolocation <- read_csv("data/olist_geolocation_dataset.csv")
order_items <- read_csv("data/olist_order_items_dataset.csv")
order_payments <- read_csv("data/olist_order_payments_dataset.csv")
order_reviews <- read_csv("data/olist_order_reviews_dataset.csv")
products <- read_csv("data/olist_products_dataset.csv")
sellers <- read_csv("data/olist_sellers_dataset.csv")
product_translation <- read_csv("data/product_category_name_translation.csv")

# definition of RFM:
# Recency - how many days ago the last order by the particular customer was placed?
# Frequency - how many orders did particular customer placed in all his history?
# monetary value - how much money did particular customer pay in all his history?

# after log-transforming the 



# todo:
# distribution of orders by year/month (for rolling RFM) ok
# get measures:
# date of each order ok
# payment of each order ok
# customer id of each order ok


order_payments %>%
  group_by(order_id) %>%
  summarise(payment_value = sum(payment_value)) -> order_payments_agg

orders %>%
  select(order_id,
         customer_id,
         order_purchase_timestamp) %>%
  left_join(order_payments_agg, by = c('order_id'='order_id')) %>%
  left_join(customers %>% select(customer_id, customer_unique_id)) %>%
  select(5,1,3,4) -> df

ggplot(df, aes(x = order_purchase_timestamp)) +
  geom_histogram()
# mostly from 01.2017 to 10.2018


# calculate RFM measures for particular date - 01-06-2017
df %>%
  filter(order_purchase_timestamp < dmy('01-06-2017')) -> df_to_date

df_to_date %>%
  group_by(customer_unique_id) %>%
  summarise(
    most_recent = max(order_purchase_timestamp),
    rec = difftime(dmy('01-06-2017'), most_recent, units = 'days'),
    fre = n(),
    mon = sum(payment_value, na.rm = T)
    ) -> a

a %>%
  mutate(rec = as.numeric(rec)) -> df_rfm

df_rfm %>%
  summary()

df_rfm %>%
  mutate(one_timer = ifelse(fre==1, 1,0))-> df_rfm_a

df_rfm_a %>%
  ggplot(aes(x=mon/fre, fill=as.character(one_timer))) +
  geom_density(alpha=0.5)

df_rfm %>% 
  select(-customer_unique_id) %>%
  GGally::ggpairs()

df_rfm %>%
  mutate(
    # fre = ifelse(fre>0, log(fre), 0),
         mon = ifelse(mon>0, log(mon), 0)
         ) %>%
  # mutate(rec = as.numeric(cut_number(rec, n=100)),
  #        mon = as.numeric(cut_number(mon, n=100))
  #        ) %>%
  select(-customer_unique_id, -most_recent) %>%
  filter(fre>0)  -> df_rfm_bins


df_rfm_bins %>%
  sample_n(100) %>%
  get_clust_tendency(99)

factoextra::fviz_nbclust(df_rfm_bins %>% sample_n(1000) %>% mutate_if(is.numeric, scale), FUNcluster = kmeans)

fviz_cluster(kmeans(df_rfm_bins%>%select(mon, rec), centers =5), 
             data = df_rfm_bins%>%select(mon, rec),geom = 'point')

kmeans(df_rfm_bins%>%select(mon, fre), centers =5)

df_rfm_bins %>%
  select(mon, rec)%>%
  mutate_all(scale) %>%
  # sample_n(2000)%>%
  kmeans(centers=5)-> pm

pm %>%
  fviz_silhouette()

silhouette(pm$cluster, dist(df_rfm_bins %>% select(mon, rec) %>%mutate_all(scale)))[,3] %>% mean() %>% print()

kmeans(df_rfm_bins,centers = 3)

df_rfm %>%
  ggplot(aes(x = rec, y = mon))+
  geom_jitter()

df_rfm %>%
  filter(mon<2000) %>%
  ggplot(aes(x=log(mon))) + 
  geom_density()

df_rfm_bins


df_rfm_bins %>%
  mutate(score = 0.3*rec + 0.3*fre + 0.4*mon) %>%
  ggplot()+
  geom_density(aes(x=score))

as.numeric(a$rec)

b <- difftime(as.POSIXct(dmy('01-07-2017')), a$rec[1], units = 'days')
b
# Basic data preparation

# Number of orders by customer
# Mean value of orders by customer 
# Number of items in order


customers %>%
  left_join(geolocation %>% 
              group_by(geolocation_zip_code_prefix) %>%
              filter(row_number()==1) %>%
              ungroup() %>% select(1,2,3) %>% distinct(),
            by = c('customer_zip_code_prefix' = 'geolocation_zip_code_prefix')) %>%
  select(geolocation_lng, geolocation_lat) %>%
  filter(geolocation_lat<20, geolocation_lng< -20) %>%
  sample_n(1000) -> a
  # factoextra::fviz_nbclust(FUNcluster = pam)
  kmeans(a, centers = 2) %>%
  fviz_cluster(a)
  ggplot(aes(x = geolocation_lng, y = geolocation_lat)) +
  geom_jitter()

  
  geolocation %>% 
    group_by(geolocation_zip_code_prefix) %>%
    filter(row_number()==1) %>%
    ungroup()
    
    filter(geolocation_zip_code_prefix=='14409') %>% View()
    
    
    
    
    km1<-kmeans(xxx, 2) # stats::
    fpc::calinhara(xxx,km1$cluster)
          
