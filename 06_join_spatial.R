library(readr)
library(tidyverse)
library("leaflet")
library(sf)
library(tmap)

geolocation <- read_csv("data/olist_geolocation_dataset.csv")

# Cleaning outside of bounding box ----

#Brazils most Northern spot is at 5 deg 16′ 27.8″ N latitude.;
geolocation = geolocation[geolocation$geolocation_lat <= 5.27438888,]
#it’s most Western spot is at 73 deg, 58′ 58.19″W Long.
geolocation = geolocation[geolocation$geolocation_lng >= -73.98283055,]
#It’s most southern spot is at 33 deg, 45′ 04.21″ S Latitude.
geolocation = geolocation[geolocation$geolocation_lat >= -33.75116944,]
#It’s most Eastern spot is 34 deg, 47′ 35.33″ W Long.
geolocation = geolocation[geolocation$geolocation_lng <=  -34.79314722,]

#' Only a few removed

# Get levels of aggregation -----
geolocation$zip_code_1 <- str_sub(geolocation$geolocation_zip_code_prefix, 1, 1)
geolocation$zip_code_2 <- str_sub(geolocation$geolocation_zip_code_prefix, 1, 2)
geolocation$zip_code_3 <- str_sub(geolocation$geolocation_zip_code_prefix, 1, 3)

geolocation %>%
  group_by(geolocation_zip_code_prefix) %>%
  tally() %>%
  arrange(-n) -> no_of_coords_per_prefix

no_of_coords_per_prefix %>% summary()

# Get most central geolocation for each zip code ----
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


# Get microregions map, change formats and projections ----
#' this package is avaliable only from cran archive to install locally
microregion_map <- brazilmaps::get_brmap("City")

df <- sf::st_as_sf(sampled_zipcodes, coords = c(3,2))
# WGS84 is proper

# change full geolocation data to sf, set projections for both zipcodes and microregions
geolocation %>% 
  sf::st_as_sf(coords = c(3,2)) -> zips

st_crs(zips) = 4326
microregion_map = st_transform(microregion_map, 4326)

# Load data from sidra website ----
sidra <- read_csv("data/sidra/test_microregions.csv", na = c('-', '...'))

# set names to english
sidra %>%
  select(6,7,8) %>%
  setNames(c('microregion_code', 'microregion', 'value')) -> sidra2

sidra2

load('data/preprocessed/spatial_all.Rdata')
spatial_all
# Tables worth checking:
# tabela 200 - basic
# 2098 - aktywność zawodowa wg rasy
# 3548 - dochód względem płacy minimalnej
# 3741 - wskaźnik analfabetyzmu
# 2094 - religia i rasa
# 631 - liczba imigrantów

# To each zipcode from olist, add information in which microregion is it
zips_with_join <- as_tibble(st_join(zips, microregion_map %>% select(MicroRegion), join = st_within))

# To each zipcode, add information from sidra
sidra3 <- sidra2 %>%
  # select(1,3) %>%
  rename(population = value)

zips_with_join %>%
  select(-geometry) %>%
  left_join(sidra3, by = c('MicroRegion' = 'microregion_code')) %>%
  rename(microregion_code = MicroRegion) -> zips_with_join2

zips_with_join %>%
  select(-geometry) %>%
  left_join(spatial_all, by = c('MicroRegion' = 'microregion_code')) %>%
  rename(microregion_code = MicroRegion) -> zips_with_join2


# Aggregate - if one zipcode spans for more than 2 microregions - weigh the value by count of points. 
zips_with_join2 %>%
  # filter(geolocation_zip_code_prefix == '28450') %>%
  select(1, 7,9) %>%
  group_by(geolocation_zip_code_prefix, microregion_code) %>%
  summarise(cnt = n(),
            population = min(population),
            a = cnt*population) %>%
  summarise(b = sum(a)/sum(cnt)) -> values_weighted

zips_with_join2 %>%
  mutate(microregion_code = as.character(microregion_code)) %>% # to correct summarise
  filter(geolocation_zip_code_prefix == '28450') %>%
  # select(1, 7,9) %>%
  group_by(geolocation_zip_code_prefix, microregion_code) %>%
  summarise(cnt = n(),
            population = min(population),
            a = cnt*population) #%>%
  summarise(b = sum(a)/sum(cnt)) -> values_weighted

  zips_with_join2 %>%
    mutate(microregion_code = as.character(microregion_code)) %>% # to correct summarise
    # filter(geolocation_zip_code_prefix == '28450') %>%
    group_by(geolocation_zip_code_prefix) %>%
    summarise_if(is.numeric, mean, na.rm=T) -> values_weighted

  
# Show points on the map - option view for leaflet
# tmap_mode("view")
tmap_mode("plot")
tm_shape(microregion_map) +
  tm_polygons(col = "white") +
  tm_shape(b) +
  tm_symbols(size = 1, 
             col = "b",
             border.lwd = NA,
             alpha = 0.5)
