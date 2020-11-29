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
  select(customer_id, order_id, total_payment, no_items) -> order_value2

order_value2 %>%
  left_join(orders %>% 
              select(order_id, order_purchase_timestamp) ) -> order_value3

customers %>%
  left_join(order_value3) -> customers2

customers2 %>% 
  group_by(customer_unique_id) %>%
  summarise(no_of_orders = n(),
            value_sum = sum(total_payment, na.rm = T),
            value_mean = mean(total_payment, na.rm = T),
            no_of_items_mean = mean(no_items),
            tstp_first_order = min(order_purchase_timestamp),
            tstp_last_order = max(order_purchase_timestamp),
  ) -> customers_order_values


customers_order_values

as_of_date <- max(orders$order_purchase_timestamp)
first_purchase <- min(orders$order_purchase_timestamp)

customers_order_values %>%
  mutate(
    days_since_last_order = as.numeric(make_difftime(as_of_date - tstp_last_order, units = 'days')),
    days_since_first_order = as.numeric(make_difftime(as_of_date - tstp_first_order, units = 'days')),
    shopping_time_span = days_since_first_order-days_since_last_order
  ) -> customers_order_values2

# Select only frequent customers
customers_order_values2 %>%
  filter(no_of_orders > 1) %>% 
  select(
    customer_unique_id,
    no_of_orders, 
    value_sum, 
    value_mean, 
    no_of_items_mean,
    days_since_first_order, 
    days_since_last_order,
    shopping_time_span
  ) -> frequent_customers

# EDA ----

frequent_customers %>%
  select(-customer_unique_id) %>%
  GGally::ggpairs()

frequent_customers

table(frequent_customers$no_of_orders)

# first clustering ----


# prepare data - remove customer id, impute 1 to number of items, scale to unit variance
frequent_customers %>%
  select(-customer_unique_id) %>%
  mutate(no_of_items_mean = ifelse(is.na(no_of_items_mean), 1, no_of_items_mean)) %>%
  mutate_all(scale) -> to_model

cor(to_model)
# almost perfect correlation between value_sum and value_mean 
# dropping value_mean
to_model <- select(to_model, -value_mean)

# check clustering tendency for data
to_model %>%
  sample_n(100) %>%
  get_clust_tendency(99)
# it's clusterable

# check silhouette for full data (with pam to interpret results better)

# fviz_nbclust(to_model, FUNcluster = pam) -> nbclust

nbclust

# 2 clusters, after that steady shit witk kmeans and pam, pam slow

to_model

# Run pam for full datasets with differing number of clusters
pams <- list()

for (k in 2:7){
  print(k)
  pams[[k]] <- pam(to_model, k = k)
}

# print silhouette for different no of clusters
for (k in 2:7){
  print(k)
silhouette(pams[[k]])[,3] %>% mean() %>% print()
}

# assign clustering to data
after_model <- to_model
after_model$cluster <- as.character(pams[[3]]$clustering)
after_model

# general ggpairs plot 
after_model %>% 
  select(1,2, cluster) %>%
  GGally::ggpairs(aes(color = cluster, alpha = 0.5))

# monetary value plot
after_model %>%
  ggplot(aes(x = value_sum, fill = cluster), alpha = 0.5) +
  geom_density(alpha = 0.5)
# all clusters are the same 

# Show exemplary protopyte for each cluster
pams[[3]]$medoids


# run without days_since_first_order - no new info here ----
pams <- list()

cols_to_model <- c("no_of_orders", 
                   "value_sum", 
                   # "value_mean",
                   "no_of_items_mean", 
                   # "days_since_first_order", 
                   "days_since_last_order", 
                   "shopping_time_span"
)

to_model <- to_model[,cols_to_model]
# to_model$value_sum <- to_model$value_sum *10000

for (k in 2:7){
  print(k)
  pams[[k]] <- pam(to_model, k = k)
}

# print silhouette for different no of clusters
for (k in 2:7){
  print(k)
  silhouette(pams[[k]])[,3] %>% mean() %>% print()
}

# assign clustering to data
after_model <- to_model
after_model$cluster <- as.character(pams[[4]]$clustering)
after_model

# general ggpairs plot 
after_model %>% 
  select(1,2, cluster) %>%
  GGally::ggpairs(aes(color = cluster, alpha = 0.5))

# monetary value plot
after_model %>%
  ggplot(aes(x = value_sum, fill = cluster), alpha = 0.5) +
  geom_density(alpha = 0.5)
# all clusters are the same 

# Show exemplary protopyte for each cluster
pams[[4]]$medoids




frequent_customers %>%
  select(-customer_unique_id, -value_mean) %>%
  mutate(no_of_items_mean = ifelse(is.na(no_of_items_mean), 1, no_of_items_mean)) %>%
  pam(k=3) -> p3 


frequent_customers %>%
  select(-customer_unique_id, -value_mean) %>%
  mutate(no_of_items_mean = ifelse(is.na(no_of_items_mean), 1, no_of_items_mean)) -> a

a$cluster = p3$clustering

a %>% 
  GGally::ggpairs(aes(color = as.factor(cluster), alpha = 0.5))

a %>%
  ggplot(aes(x = value_sum, fill = as.factor(cluster)), alpha = 0.5) +
  geom_density(alpha = 0.5)

a %>%
  group_by(cluster) %>%
  tally()
# 3. cluster jest 3 razy mniejszy ale więcej zakupów

a %>%
  ggplot(aes(x = days_since_first_order, fill = as.factor(cluster)), alpha = 0.5) +
  geom_density(alpha = 0.5)

a %>%
  ggplot(aes(x = days_since_first_order, y = days_since_last_order, color = as.factor(cluster)), alpha = 0.5) +
  geom_point()

a %>%
  mutate(
    shopping_time_span = days_since_first_order-days_since_last_order
  ) %>%
  ggplot(aes(x = shopping_time_span, y = days_since_last_order, color = as.factor(cluster)), alpha = 0.5) +
  geom_point()

fviz_silhouette(p3)





frequent_customers %>%
  select(-customer_unique_id, -value_mean) %>%
  mutate(no_of_items_mean = ifelse(is.na(no_of_items_mean), 1, no_of_items_mean)) %>%
  pam(k=4) -> p2





frequent_customers %>%
  select(-customer_unique_id, -value_mean) %>%
  mutate(no_of_items_mean = ifelse(is.na(no_of_items_mean), 1, no_of_items_mean)) %>%
  mutate_all(scale) %>%
  pam(k=2) -> p2

fviz_silhouette(p2)

frequent_customers %>%
  select(-customer_unique_id, -value_mean) %>%
  mutate(no_of_items_mean = ifelse(is.na(no_of_items_mean), 1, no_of_items_mean)) %>%
  mutate_all(scale) %>%
  fviz_nbclust(FUNcluster = pam) -> nbclust

nbclust


frequent_customers %>%
  select(-customer_unique_id, -value_mean) %>%
  mutate(no_of_items_mean = ifelse(is.na(no_of_items_mean), 1, no_of_items_mean)) %>%
  mutate_all(scale) %>%
  pam(k=3) -> p2

fviz_silhouette(p2)

a %>%
  mutate_all(scale) %>%
  sample_n(100) %>%
  get_clust_tendency(99)




a %>% mutate_all(scale) %>% 
  select(-cluster) %>%
  cor()
prcomp() -> pr
pr
summary(pr)



# check 2 dimensions

ggplot(frequent_customers, aes(x = days_since_last_order, y= value_sum)) +
  geom_point()+
  scale_y_log10()


to_model2 <- frequent_customers %>%
  select(value_sum, days_since_last_order) %>%
  mutate(value_sum = log(value_sum)) 

to_model3 <- to_model2 %>%
  mutate_all(scale)



pams <- list()
for (k in 2:7){
  print(k)
  pams[[k]] <- pam(to_model3, k = k)
}

# print silhouette for different no of clusters
for (k in 2:7){
  print(k)
  silhouette(pams[[k]])[,3] %>% mean() %>% print()
}

# assign clustering to data
after_model <- to_model2
after_model$cluster <- as.character(pams[[3]]$clustering)
after_model

to_model3 %>%
  select(value_sum) %>%
  sample_n(500) %>%
  get_clust_tendency(499)

ggplot(after_model, aes(x= value_sum, fill =cluster)) +
  geom_histogram(alpha=0.5)

ggplot(after_model, aes(x = days_since_last_order, y= value_sum, color = cluster)) +
  geom_point()+
  scale_y_log10()


after_model %>%
  ggplot()
