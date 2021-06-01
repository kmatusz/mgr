#' Libraries, loading the dataset etc
#' 

library(readr)
library(tidyverse)
library("leaflet")
library(psych)
library(lubridate)
library(cluster)
library(factoextra)
library(caret)
library(rpart)
library(DALEX)
library(stringr)

load('run_all_models_cache/to_model_train.Rdata')
load('run_all_models_cache/to_model_test.Rdata')
  
vars_basic <- c("payment_value", "review_score", "geolocation_lat", 
                "geolocation_lng",
                "no_items", "sum_freight")

df <- to_model_train[c(vars_basic, 'if_second_order')]


#' Payment value - density plot
#'
df %>% 
  ggplot(aes(x =payment_value, fill= if_second_order)) +
  geom_density(alpha=0.5) +
  scale_x_log10()



#' Review score - barplot and percentage per each review score
ggplot(df, aes(x = review_score)) +
  geom_bar()


df %>%
  filter(if_second_order=='yes') %>%
  group_by(review_score) %>%
  tally(name = 'cnt_second_order') -> tmp1

df %>% 
  group_by(review_score) %>%
  tally() %>%
  left_join(tmp1) %>%
  mutate(percent_second_order = cnt_second_order/n) %>%
  ggplot(aes(x = review_score, y = percent_second_order)) +
  geom_col()

#' No items - numbers and percentage
ggplot(df, aes(x = no_items)) +
  geom_bar() 

df %>%
  filter(if_second_order=='yes') %>%
  group_by(no_items) %>%
  tally(name = 'cnt_second_order') -> tmp2

df %>% 
  group_by(no_items) %>%
  tally() %>%
  left_join(tmp2) %>%
  mutate(percent_second_order = cnt_second_order/n) %>%
  ggplot(aes(x = no_items, y = percent_second_order)) +
  geom_col()

#' sum_freigt  
df %>% 
  ggplot(aes(x = sum_freight, fill=if_second_order)) +
  geom_density(alpha=0.5) +
  scale_x_log10()


#' Map
#' 
#' Load libraries, process the dataset
#' 

library(sf)
library(tmap)
tmap_mode("plot")
brazil_map <- brazilmaps::get_brmap("Brazil")

df = df[df$geolocation_lat <= 5.27438888,]
df = df[df$geolocation_lng >= -73.98283055,]
df = df[df$geolocation_lat >= -33.75116944,]
df = df[df$geolocation_lng <=  -34.79314722,]


df_map <- sf::st_as_sf(df %>% sample_n(10000), coords = c(4,3))
df_map <- sf::st_as_sf(df, coords = c(4,3))
st_crs(df_map) <- st_crs(brazil_map)

df_map %>%
  mutate(size=ifelse(if_second_order=='yes', 1, 0.4)) -> df_map


#' Draw map
tm_shape(brazil_map) +
  tm_polygons(col = "white") +
  tm_shape(df_map %>% filter(if_second_order!='yes')) +
  tm_symbols(size = 0.1,
             col = "if_second_order",
             palette = RColorBrewer::brewer.pal(3, 'Dark2')[1:2],
             # palette = c('red', ''),
             # style = "fixed",
             # breaks = c(45, 60, 75, 90),
             border.lwd = NA,
             alpha = 0.5)+
  tm_shape(df_map %>% filter(if_second_order=='yes')) +
  tm_symbols(size = 0.2,
             col = "if_second_order",
             palette = RColorBrewer::brewer.pal(3, 'Dark2')[2],
             # palette = c('red', ''),
             # style = "fixed",
             # breaks = c(45, 60, 75, 90),
             border.lwd = NA,
             alpha = 0.5)
  
