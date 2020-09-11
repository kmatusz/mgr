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

#' Definition of RFM:
#' Recency - how many days ago the last order by the particular customer was placed?
#' Frequency - how many orders did particular customer placed in all his history?
#' monetary value - how much money did particular customer pay in all his history?



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
#' Orders in the dataset are mostly from 01.2017 to 10.2018 - 
#' there is a small amount at the end of 2016, but it looks like it shouldn't be there. 
#' We can drop it as it won't affect the count largely.


#' This code is prepared to move the window of the analysis - to change the date of the clustering.
df %>%
  filter(dmy('01-01-2017') < order_purchase_timestamp, 
         order_purchase_timestamp < dmy('18-10-2018')) -> df_to_date


#' TODO:
# - Pokazać wykresy dla różnych rodzajów normalizacji - log, scale
# - Dla całych danych - wybrać lojalnych - 1 zakup
# - Dla całych danych, wszystkich
# Klastrowanie - kmeans, pam, dbscan



#' Calculating RFM measures (due to the date computation it can take a while):
df_to_date %>%
  group_by(customer_unique_id) %>%
  summarise(
    most_recent = max(order_purchase_timestamp),
    rec = as.numeric(difftime(dmy('18-10-2018'), most_recent, units = 'days')),
    fre = n(),
    mon = sum(payment_value, na.rm = T)
    ) -> df_rfm
# save(df_rfm, file='data/04_df_rfm.Rdata')

df_rfm %>%
  summary()
#' very large skew visible in fre and mon.

df_rfm %>% 
  select(-customer_unique_id, -most_recent) %>%
  GGally::ggpairs()
#' Here the skew is even more visible. K-means can't accept such non-spherical data. 
#' Monetary value can be log-transformed, and frequency encoded as categorical variable - 
#' if more than one purchase.

df_rfm %>%
  mutate(fre = ifelse(fre==1, 0,1)) %>%
  mutate(
    mon = ifelse(mon>0, log(mon), 0)
  ) %>%
  select(rec, fre, mon) %>%
  mutate_at(c('rec', 'mon'), ~(scale(.) %>% as.vector))-> df_rfm_scaled

df_rfm_scaled

df_rfm_scaled %>% 
  GGally::ggpairs()

#' As freqency is now categorical variable with 2 levels, we can split the dataset based on it: 

df_rfm_scaled %>%
  ggplot(aes(x = rec, y = mon)) +
  geom_point(alpha=0.5) +
  facet_grid(rows = vars(fre))

#' Nothing particularly interesting is visible here. Recency has a distribution close to uniform,
#' and monetary value after logarithming got close to normal distribution. 
#' Now I am going to test clustering tendency using Hopkins statistic:

df_rfm_scaled %>%
  sample_n(200) %>%
  get_clust_tendency(n=199)
#' Hopkins statistic is 0.75, meaning that the dataset is clusterable. 


#'
#'Same statistic, but applied only on frequent customers
df_rfm_scaled %>%
  filter(fre==1) %>%
  sample_n(400) %>%
  get_clust_tendency(n=399)
#' Hopkins statistic is 0.67, meaning that the dataset is clusterable.


#' Testing correct number of clusters for loyal customers 
factoextra::fviz_nbclust(df_rfm_scaled %>%
                           filter(fre==1) %>%select(-fre), FUNcluster = kmeans)

#' 2 clusters are optimal - but silhouette value is not very good
#' 

km <- kmeans(df_rfm_scaled %>%
               filter(fre==1) %>%
               select(-fre), 
             centers = 2
             )

fviz_cluster(km, 
             data = df_rfm_scaled %>%
               filter(fre==1) %>%
               select(-fre),
             geom = 'point')


silhouette(km$cluster, 
           dist(df_rfm_scaled %>%
                  filter(fre==1) %>%
                  select(-fre)))-> sil

fviz_silhouette(sil)

#' Average silhouette is 0.37. This result is not so great. 
#' 
#' Now, I am trying to fit dbscan algorithm - as a more flexible alternative to kmeans.
library(dbscan)
dbscan::kNNdistplot(df_rfm_scaled %>%
                      filter(fre==1) %>%
                      select(-fre), k =  5)
abline(h=0.25)

#'
#' For 5 customers as minimum amount in the cluster, 
#' distance chosen by the elbow method is 0.25 

res.db <- dbscan::dbscan(df_rfm_scaled %>%
                           filter(fre==1) %>%
                           select(-fre), 0.25, 5)

fviz_cluster(res.db, df_rfm_scaled %>%
               filter(fre==1) %>%
               select(-fre), geom = "point")
#' Almost all points were assigned to one big cluster - nothing interesting is visible here.
#' 
#' 

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

df_rfm %>%
  mutate(one_timer = ifelse(fre==1, 1,0))-> df_rfm_a

df_rfm_a %>%
  ggplot(aes(x=mon/fre, fill=as.character(one_timer))) +
  geom_density(alpha=0.5)


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

#' clustering using RFM variables did not give any promising results. However,
#'  there are also other variables in the dataset, that could give better results.
#'  Below, I have analyzed geolocation data in search for good clustering. 
#'  First, data preparation:

customers %>%
  left_join(geolocation %>% 
              group_by(geolocation_zip_code_prefix) %>%
              filter(row_number()==1) %>%
              ungroup() %>% select(1,2,3) %>% distinct(),
            by = c('customer_zip_code_prefix' = 'geolocation_zip_code_prefix')) %>%
  select(geolocation_lng, geolocation_lat) %>%
  filter(geolocation_lat<20, geolocation_lng< -20) %>%
  sample_n(1000) %>%
  scale-> a

#' Now, checking the best number of clusters
#' 
factoextra::fviz_nbclust(a, FUNcluster = kmeans)

#' Best number of clusters judging by silhouette is 2. 
#' However, clustering with more centers also looks promising 

kmeans(a, centers = 2) %>%
  fviz_cluster(a, geom = 'point')

#' Now, checking DBSCAN:
#'   
    
library(dbscan)
dbscan::kNNdistplot(a, k =  3)
abline(h=0.45)

#'
#' For 5 customers as minimum amount in the cluster, 
#' distance chosen by the elbow method is 0.45 

res.db <- dbscan::dbscan(a, 0.24, 3)

res.db

fviz_cluster(res.db, a, geom = "point")

#' To summarise, RFM variables have not shown a potential for clustering. 
#' However, as a proof of concept, geolocation variables did, and because of that the idea
#' witch clustering is still worth exploring.

