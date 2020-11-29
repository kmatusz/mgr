library(readr)
library(tidyverse)
library("leaflet")
library(sf)
library(tmap)

geolocation <- read_csv("data/olist_geolocation_dataset.csv")

#' Cleaning outside of bounding box

#Brazils most Northern spot is at 5 deg 16′ 27.8″ N latitude.;
geolocation = geolocation[geolocation$geolocation_lat <= 5.27438888,]
#it’s most Western spot is at 73 deg, 58′ 58.19″W Long.
geolocation = geolocation[geolocation$geolocation_lng >= -73.98283055,]
#It’s most southern spot is at 33 deg, 45′ 04.21″ S Latitude.
geolocation = geolocation[geolocation$geolocation_lat >= -33.75116944,]
#It’s most Eastern spot is 34 deg, 47′ 35.33″ W Long.
geolocation = geolocation[geolocation$geolocation_lng <=  -34.79314722,]

#' Only a few removed

#' Get levels of aggregation
geolocation$zip_code_1 <- str_sub(geolocation$geolocation_zip_code_prefix, 1, 1)
geolocation$zip_code_2 <- str_sub(geolocation$geolocation_zip_code_prefix, 1, 2)
geolocation$zip_code_3 <- str_sub(geolocation$geolocation_zip_code_prefix, 1, 3)

geolocation %>%
  group_by(geolocation_zip_code_prefix) %>%
  tally() %>%
  arrange(-n) -> no_of_coords_per_prefix

no_of_coords_per_prefix %>% summary()
#' Most of the prefixes have more than one geolocation assigned. 
#' Using pam with 1 center to replace
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

# results cached - very resource expensive
# grouped %>%
#   mutate(sampled = map(data, robust_pam)) -> res
# sampled_zipcodes <- bind_rows(res$sampled)
# save(sampled_zipcodes, file = 'data/map_sampled_zipcodes.Rdata')


load('data/map_sampled_zipcodes.Rdata')
#' Get map for brazil (also less aggregation is avaliable)
#' this package is avaliable only from cran archive to install locally
brazil_map <- brazilmaps::get_brmap("Brazil")
m2 <- brazilmaps::get_brmap("City")

df <- sf::st_as_sf(sampled_zipcodes, coords = c(3,2))
#' WGS84 is ok
#' WARNING! Downsampling temporarly to get workflow going - drawing maps is very resource expensive
df_small <- df %>% sample_frac(1)

tm_shape(m2) +
  tm_polygons(col = "white")

#' Basic map of zipcodes - places in which customers placed orders 
# tmap_mode("view")
tmap_mode("plot")

tm_shape(brazil_map) +
  tm_polygons(col = "white") +
  tm_shape(df_small) +
  tm_symbols(size = 0.1, 
             col = "zip_code_1",
             border.lwd = NA,
             alpha = 0.5)



#' Adding information about orders 
orders <- read_csv("data/olist_orders_dataset.csv")
customers <- read_csv("data/olist_customers_dataset.csv")


#' Number of orders per zip code
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

#' For some of the zipcodes there is not even one order placed. 
#' This database has full range of zipcodes 
#' - even if there are no customers in particular area



#' Number of orders per zip code (second level of aggregation)
no_orders_per_zip_2 <- no_orders_per_zip %>%
  mutate(zip_code_2 = str_sub(customer_zip_code_prefix, 1, 2)) %>%
  group_by(zip_code_2) %>%
  summarise(no_orders = sum(no_orders, na.rm = T))

df %>%
  left_join(no_orders_per_zip_2) %>%
  group_by(zip_code_2) %>%
  filter(row_number()==1) %>%
  ungroup()-> temp1


tm_shape(brazil_map) +
  tm_polygons(col = "black") +
  tm_shape(temp1) +
  tm_symbols(
    # size = "no_orders", 
    col = "no_orders",
    # n = 5,
    border.lwd = NA,
    palette = "Reds",
    size = 1,
    alpha = 0.7)


#' Number of orders per zip code (third level of aggregation)
no_orders_per_zip_3 <- no_orders_per_zip %>%
  mutate(zip_code_3 = str_sub(customer_zip_code_prefix, 1, 3)) %>%
  group_by(zip_code_3) %>%
  summarise(no_orders = sum(no_orders, na.rm = T))

df %>%
  left_join(no_orders_per_zip_3) %>%
  group_by(zip_code_3) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  mutate(no_orders_log = log(no_orders))-> temp2

temp2

# use log to make more color palette more visible
tm_shape(brazil_map) +
  tm_polygons() +
  tm_shape(temp2) +
  tm_symbols(
    # size = "no_orders", 
    col = "no_orders_log",
    # n = 5,
    border.lwd = NA,
    palette = "viridis",
    size = 0.3,
    alpha = 0.7)

#' Bigger concentration of orders is visible in southern part of 
#' Brazil - this is more urban area. 


#' Joinig information about loyal customers and  
orders_loyal <- new.env()
load(file ='data/05_orders_enhanced.Rdata', env = orders_loyal)
orders_master <- orders_loyal$orders_master
first_orders <- orders_loyal$first_orders
orders_master
first_orders

first_orders %>%
  mutate(zip_code_3=str_sub(customer_zip_code_prefix, 1,3)) %>%
  right_join(df)

df %>%
  left_join(first_orders, 
            by =c('geolocation_zip_code_prefix'='customer_zip_code_prefix')) -> temp_customer

pal <- colorFactor(c("navy", "red"), domain = c("0", "1"))

leaflet(temp_customer %>% filter(if_second_order=='1')) %>% 
  addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(if_second_order == "0", 0.1, 2),
    color = ~pal(if_second_order),
    stroke = FALSE, fillOpacity = 0.5
  )

#' it looks like the spatial distribution of customers placing second order 
#' is more or less the same as other customers


