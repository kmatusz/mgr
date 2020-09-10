library(readr)
library(tidyverse)
library("leaflet")
library(sf)
library(tmap)

geolocation <- read_csv("data/olist_geolocation_dataset.csv")

# Cleaning outside of bounding box

#Brazils most Northern spot is at 5 deg 16′ 27.8″ N latitude.;
geolocation = geolocation[geolocation$geolocation_lat <= 5.27438888,]
#it’s most Western spot is at 73 deg, 58′ 58.19″W Long.
geolocation = geolocation[geolocation$geolocation_lng >= -73.98283055,]
#It’s most southern spot is at 33 deg, 45′ 04.21″ S Latitude.
geolocation = geolocation[geolocation$geolocation_lat >= -33.75116944,]
#It’s most Eastern spot is 34 deg, 47′ 35.33″ W Long.
geolocation = geolocation[geolocation$geolocation_lng <=  -34.79314722,]

# Only a few removed

# Get levels of aggregation
geolocation$zip_code_1 <- str_sub(geolocation$geolocation_zip_code_prefix, 1, 1)
geolocation$zip_code_2 <- str_sub(geolocation$geolocation_zip_code_prefix, 1, 2)
geolocation$zip_code_3 <- str_sub(geolocation$geolocation_zip_code_prefix, 1, 3)

geolocation %>%
  group_by(geolocation_zip_code_prefix) %>%
  tally() %>%
  arrange(-n) -> no_of_coords_per_prefix

no_of_coords_per_prefix %>% summary()
# Most of the prefixes have more than one geolocation assigned. 
# Using pam with 1 center to replace
no_of_coords_per_prefix %>% 
  filter(n != 1) %>%
  .$geolocation_zip_code_prefix -> prefixes_with_more_coords

library(cluster)

robust_pam <- function(initial_df) {
  
  message(initial_df$geolocation_zip_code_prefix[1])
  
  if (nrow(initial_df) == 1){
    return(initial_df)
  }
  
  coords <- initial_df %>%
    select(geolocation_lat, geolocation_lng)
  
  pam1 <- pam(coords, 1)
  
  out <- initial_df[pam1$id.med, ]
  
  out
}

geolocation %>%
  mutate(to_group = geolocation_zip_code_prefix) %>%
  group_by(to_group) %>%
  nest() -> grouped

grouped %>%
  mutate(sampled = map(data, robust_pam)) -> res
# 13.28 13.33 - nie ma tragedii

sampled_zipcodes <- bind_rows(res$sampled)

# Get map for brazil (also less aggregation is avaliable)
# this package is avaliable only from cran archive to install locally
brazil_map <- brazilmaps::get_brmap("Brazil")
# m_2 <- brazilmaps::get_brmap("MicroRegion")

df <- sf::st_as_sf(sampled_zipcodes, coords = c(3,2))
# WGS84 is ok

# WARNING! Downsampling temporarly to get workflow
df_small <- df %>% sample_frac(0.1)


# tmap_mode("view")
tmap_mode("plot")

tm_shape(brazil_map) +
  tm_polygons(col = "white") +
  tm_shape(df_small) +
  tm_symbols(size = 0.1, 
             col = "zip_code_1",
             border.lwd = NA,
             alpha = 0.5)



# Adding information about orders 
orders <- read_csv("data/olist_orders_dataset.csv")
customers <- read_csv("data/olist_customers_dataset.csv")


# Number of orders per zip code
orders %>%
  left_join(
customers %>% 
  select(1, 3)
) -> orders_with_zip

orders_with_zip %>%
  group_by(customer_zip_code_prefix) %>%
  tally() %>%
  rename(no_orders = n) -> no_orders_per_zip

df %>%
  left_join(no_orders_per_zip, 
            by = c("geolocation_zip_code_prefix" = "customer_zip_code_prefix")) -> temp


# Number of orders per zip code

tm_shape(brazil_map) +
  tm_polygons(col = "white") +
  tm_shape(temp) +
  tm_symbols(
    # size = "no_orders", 
             col = "no_orders",
             n = 5,
             border.lwd = NA,
             size = 0.1,
             alpha = 0.9)

# Zastanowić się nad wizualizacjami - słabe kolory itd

no_orders_per_zip_2 <- no_orders_per_zip %>%
  mutate(zip_code_2 = str_sub(customer_zip_code_prefix, 1, 2)) %>%
  group_by(zip_code_2) %>%
  summarise(no_orders = sum(no_orders, na.rm = T))

df %>%
  left_join(no_orders_per_zip_2) -> temp1


# Number of orders per zip code (aggregated to 1st digit)

tm_shape(brazil_map) +
  tm_polygons(col = "black") +
  tm_shape(temp1) +
  tm_symbols(
    # size = "no_orders", 
    col = "no_orders",
    # n = 5,
    border.lwd = NA,
    palette = "Reds",
    size = 0.1,
    alpha = 0.51)




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


