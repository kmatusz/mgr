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


orders
customers %>% select(2,3) %>% distinct() %>%
  mutate(a=str_sub(customer_zip_code_prefix, 1, 1)) %>%
  select(3) %>% distinct()

geolocation
order_items
order_payments
products
sellers
product_translation


order_payments %>%
  group_by(order_id) %>%
  summarise(payment_value = sum(payment_value)) -> order_payments_agg

order_reviews %>%
  group_by(order_id) %>%
  filter(review_creation_date == max(review_creation_date)) %>%
  select(order_id, review_score) -> ratings


orders %>%
  select(order_id,
         customer_id,
         order_purchase_timestamp) %>%
  left_join(order_payments_agg, by = c('order_id'='order_id')) %>%
  left_join(customers %>% mutate(zip=str_sub(customer_zip_code_prefix, 1, 1)) %>%select(customer_id, customer_unique_id,zip)) %>%
  select(5,1,3,4,6) %>%
  arrange(customer_unique_id, order_purchase_timestamp) %>%
  group_by(customer_unique_id) %>%
  mutate(order_no=row_number()) %>%
  ungroup() %>%
  left_join(ratings) -> orders_master

orders_master %>%
  select(order_no) %>%
  group_by(order_no)%>%
  tally()
# majority has bought one time. We can check the behaviour of customers that bought at least
# one time

# percentage of people that bought x times that also bought x+1 times
orders_master %>%
  select(order_no) %>%
  group_by(order_no)%>%
  tally() %>%
  mutate(n2 = n/lag(n))
# there is 3% chance that people who bought one time will also buy second time.
# But for choosing from 2nd to 3rd it's 8.5%
# So after making second buy the chances of longer loyalty grow.

# select first order, with indication if particular customer bought also next times
orders_master %>%
  filter(order_no>1) %>%
  distinct(customer_unique_id) %>%
  .$customer_unique_id -> loyal_customers_ids

orders_master %>%
  filter(order_no==1) %>%
  select(-order_no) %>%
  mutate(if_second_order = ifelse(customer_unique_id %in% loyal_customers_ids,
                                  '1',
                                  '0'
                                  )) -> first_orders



# descriptive statistics for first order

# distribution of first order value
first_orders %>%
  ggplot(aes(x = log(payment_value), fill=if_second_order)) +
  geom_density(alpha=0.5)
  
# no visible difference between these

# distribution of review score - hypothesis is that low values of review don't
# lead to future buys
first_orders %>%
  ggplot(aes(fill = as.character(review_score), x=if_second_order)) +
  geom_bar(stat='count', position='fill')
# no difference - this is somehow strange. If the reception was bad for the first time,
# why did the customer decide to buy again?
# This can also mean that customer loyalty is not so important in this sector!


# Test average time between buys
orders_master %>%
  filter(customer_unique_id %in% loyal_customers_ids) %>%
  arrange(customer_unique_id) %>%
  group_by(customer_unique_id) %>%
  arrange(order_no) %>%
  mutate(previous_order_time = lag(order_purchase_timestamp)) %>%
  filter(!is.na(previous_order_time))%>%
  ungroup() %>%
  arrange(customer_unique_id) %>%
  mutate(a = order_purchase_timestamp-previous_order_time) -> time_since_last
  
units(time_since_last$a) <- 'days'
  
time_since_last %>%
  filter(order_no == 2) %>%
  ggplot(aes(x=a)) +
  geom_boxplot()


options(scipen = 11111111)
time_since_last %>%
  filter(order_no == 2) %>%
  select(a) %>%
  .$a %>%
  quantile(c(0.25, 0.5, 0.63, 0.7, 0.75))
# 50% of second orders are placed within 25 days from first buy
# 63% are placed within 2 months
# 70% are placed within 3 months
# 75% are place within 4 months 

# This proves that 'leads get colder' - the longer the time from first order, 
# the less likely it is to make customer place second order


# check if percentage of loyal customers is changing throughout the period of analysis
# expected is drop at the end of period (as we don't have the data about future orders)
# also maybe before december (buying in for christmas) this changes?
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


first_orders %>%
  group_by(zip, if_second_order) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(values_from = n,names_from=if_second_order, names_prefix='a') %>%
  mutate(perc_second= a1/(a0+a1))

# There is very small difference in percentages between regions

first_orders

