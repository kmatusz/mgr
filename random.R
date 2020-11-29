orders %>%
  group_by(order_status) %>%
  tally()


orders <- orders %>%
  filter(order_status == "delivered") %>%
  mutate(delivery_time_days = as.numeric(difftime(order_delivered_customer_date, order_approved_at, units = "days")),
         delay_days = as.numeric(difftime(order_delivered_customer_date, order_estimated_delivery_date, units = "days"))) 


summary(orders)

orders %>%
  filter(delay_days > 0)
# ~7% delayed




order_items <- read_csv("data/olist_order_items_dataset.csv")



order_items %>% 
  group_by(order_id) %>% 
  summarise_if(is.numeric, sum, na.rm = T) %>%
  mutate(total = price + freight_value) -> a

a %>% 
  filter(order_id == "b81ef226f3fe1789b1e8b2acac839d17")



# Number of items in an order

order_items %>% 
  group_by(order_id) %>% 
  tally() %>%
  arrange(-n) %>%
  summary()

customers %>%
  group_by(customer_unique_id) %>%
  tally() %>%
  arrange(-n) %>% 
  summary()


# Number of times particular prduct was sold:

order_items %>%
  group_by(product_id) %>%
  tally() %>%
  arrange(-n)



order_items %>% 
  filter(product_id == "99a4788cb24856965c36a24e339b6058") %>%
  View()


order_items %>%
  group_by(order_id) %>%
  summarise(total = sum(price, na.rm = T)) -> f

orders %>%
  select(1,2) %>%
  left_join(f) %>%
  select(2, 3)-> g

customers %>%
  left_join(g) %>%
  left_join(sampled_zipcodes[c("geolocation_zip_code_prefix", "geolocation_lat", "geolocation_lng")], 
            by = c("customer_zip_code_prefix" = "geolocation_zip_code_prefix")) %>%
  select(1, 6, 7, 8)-> h


h
library(factoextra)

res <- get_clust_tendency(h[2:4] %>% sample_n(500), n = 499,
                          graph = T)
res$hopkins_stat
res

