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
  left_join(customers %>% mutate(zip=str_sub(customer_zip_code_prefix, 1, 1)) %>%
              select(customer_id, customer_unique_id,zip, customer_zip_code_prefix)) %>%
  select(5,1,3,4,6,7) %>%
  arrange(customer_unique_id, order_purchase_timestamp) %>%
  group_by(customer_unique_id) %>%
  mutate(order_no=row_number()) %>%
  ungroup() %>%
  left_join(ratings) -> orders_master

orders_master %>%
  select(order_no) %>%
  group_by(order_no)%>%
  tally()
#' majority of the customers have bought only one time. 
#' Let's check general profitability of the customer depending on number of his orders

orders_master %>%
  group_by(customer_unique_id) %>%
  summarise(
    no_orders = max(order_no),
    profit = sum(payment_value)
  ) %>%
  group_by(no_orders) %>%
  summarise(avg_profit=mean(profit, na.rm = T))


orders_master %>%
  # filter(order_no <5) %>%
  mutate(order_no = ifelse(order_no<5, order_no, 50))%>%
  group_by(order_no) %>%
  summarise(
    no_orders = n(),
    mean_payment = mean(payment_value, na.rm=T)) %>%
  mutate(
    percent = ifelse(order_no == 50, NA, no_orders/lag(no_orders))
  ) %>%
  mutate(order_no = ifelse(order_no != 50, order_no, '5 or more')) -> dependent_var_stats

save(dependent_var_stats, file = 'data/05_dependent_var_stats.Rdata')
  

#' This is quite obvious, but customers who have bought more times also are more profitable. 
#' This means that customer retention is potentially very rewarding for the company.
#' We can check the behaviour of customers that bought at least one time, to see if it is possible to
#' predict, which of the one-timers may be potentially interested in buying second time.
#' 
#' In other words, for we can check the behaviour of loyal customer in the past, that is during
#' their first purchase 
#' 


#' Percentage of people that bought x times that also bought x+1 times
orders_master %>%
  select(order_no) %>%
  group_by(order_no)%>%
  tally() %>%
  mutate(n2 = n/lag(n))
#' There is 3% chance that people who bought one time will also buy second time.
#' But for choosing from 2nd to 3rd it's 8.5%.
#' So after making second buy the chances of longer loyalty grow. This proves that making the 
#' customer place second order is the hardest part in retention of him.

#' Select first order, with indication if particular customer bought also next time.
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



#' Descriptive statistics for first order:

#' Distribution of first order value with indication if the customer has also placed second order:
first_orders %>%
  ggplot(aes(x = log(payment_value), fill=if_second_order)) +
  geom_density(alpha=0.5)
  
#' no visible difference between these

#' Distribution of review score placed after purchase - hypothesis is that low values of review don't
#' lead to future buys - customer is discouraged to the shop.
#'
first_orders %>%
  ggplot(aes(fill = as.character(review_score), x=if_second_order)) +
  geom_bar(stat='count', position='fill')

#' no difference - this is somehow strange. If the reception was bad for the first time,
#' why did the customer decide to buy again?
#' This can also mean that customer loyalty is not so important in e-commerce sector!


#' Test average time between buys:
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
#' - 50% of second orders are placed within 25 days from first buy
#' - 63% are placed within 2 months
#' - 70% are placed within 3 months
#' - 75% are placed within 4 months 
#' 
#' This proves that 'leads get colder' - the longer the time from first order, 
#' the less likely it is to make customer place second order



write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#' Now let's check if location of the customer influences if he will become loyal customer.
#' zip code present in the dataset has 5 digits, and each consecutive digit gives 
#' another level of deaggregation. First, check the biggest level of aggregation - 10 'macroregions'
#' 
#' 
first_orders %>%
  group_by(zip, if_second_order) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(values_from = n,names_from=if_second_order, names_prefix='if_loyal') %>%
  mutate(perc_second_purchase= if_loyal1/(if_loyal0+if_loyal1))

#' There is very small difference in percentages between regions. Now, let's try to dig deeper
#' into second level of aggregation

first_orders %>%
  mutate(zip = str_sub(customer_zip_code_prefix, 1,2)) %>%
  group_by(zip, if_second_order) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(values_from = n,names_from=if_second_order, names_prefix='if_loyal') %>%
  mutate(perc_second_purchase= if_loyal1/(if_loyal0+if_loyal1)) %>% 
  summary()

#' The difference between 1st and 3rd quartiles of the percentage is ~1 p.p. However,
#' operating on such low level of aggregation is tricky, 
#' as there is probably quite big randomness in the outcome.
#' 

save(orders_master, first_orders, file = 'data/05_orders_enhanced.Rdata')
