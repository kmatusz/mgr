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




# Load data from sidra website - population
sidra <- read_csv("data/sidra/test_microregions.csv", na = c('-', '...'))

# set names to english
sidra %>%
  select(6,7,8) %>%
  setNames(c('microregion_code', 'microregion', 'value')) -> sidra2

sidra2

# Tables worth checking:
# tabela 200 - basic
# 2098 - aktywność zawodowa wg rasy
# 3548 - dochód względem płacy minimalnej
# 3741 - wskaźnik analfabetyzmu
# 2094 - religia i rasa
# 631 - liczba imigrantów

# change full geolocation data to sf, set projections for both zipcodes and microregions
geolocation %>% 
  sf::st_as_sf(coords = c(3,2)) -> zips

st_crs(zips) = 4326
m2 = st_transform(m2, 4326)

# To each zipcode, add information in which region is it
zips_with_join <- as_tibble(st_join(zips, m2 %>% select(MicroRegion), join = st_within))

# To each zipcode, add information from sidra
sidra3 <- sidra2 %>%
  # select(1,3) %>%
  rename(population = value)

zips_with_join %>%
  select(-geometry) %>%
  left_join(sidra3, by = c('MicroRegion' = 'microregion_code')) %>%
  rename(microregion_code = MicroRegion) -> zips_with_join2


# Aggregate - if one zipcode spans for more than 2 microregions - weigh the value by count of points
zips_with_join2 %>%
  # filter(geolocation_zip_code_prefix == '28450') %>%
  select(1, 7,9) %>%
  group_by(geolocation_zip_code_prefix, microregion_code) %>%
  summarise(cnt = n(),
            population = min(population),
            a = cnt*population) %>%
  summarise(b = sum(a)/sum(cnt)) -> values_weighted


# Tests of correctness
zips %>%
  sample_frac(0.1) %>%
  left_join(values_weighted) -> b

zips_with_join2 %>%
  select(1,7) %>%
  group_by(geolocation_zip_code_prefix) %>%
  distinct() %>%
  summarise(a = n()) %>% 
  arrange(a)

zips_with_join2 %>%
  filter(geolocation_zip_code_prefix=='01001')


# Show points on the map - option view for leaflet
# tmap_mode("view")
tmap_mode("plot")
tm_shape(m2) +
  tm_polygons(col = "white") +
  tm_shape(b) +
  tm_symbols(size = 1, 
             col = "b",
             border.lwd = NA,
             alpha = 0.5)
