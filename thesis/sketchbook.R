plot_map <- function(variable, title){
  print(1)
  tm_shape(brazil_map) +
    tm_borders()+
    tm_shape(stats_per_microregion3) +
    tm_polygons(col = variable,border.alpha = 0) +
    tm_shape(brasil_cities_coords %>% arrange(-population) %>% head(10)) +
    tm_symbols(size = 0.2,
               col = "black",
               border.lwd = NA,
               alpha = 0.8) +
    tm_text(text='city', just='top',size = 0.8) +
    tm_layout(title= title, title.size = 0.9)
}

w1 <- plot_map('no_customers_per_10000_pop', 'No. customers per 10 thousand inhabitants')

tm_shape(brazil_map) +
  tm_borders()+
  tm_shape(stats_per_microregion3) +
  tm_polygons(col = variable,border.alpha = 0,title=' ') +
  tm_shape(brasil_cities_coords %>% arrange(-population) %>% head(10)) +
  tm_symbols(size = 0.2,
             col = "black",
             border.lwd = NA,
             alpha = 0.8) +
  tm_text(text='city', just='top',size = 0.8) +
  tm_layout(title= title, title.size = 0.9) 

### DUMP ----

# jak zrobić grid
no_cells = 20
grid_geom=st_make_grid(df_map, n=c(no_cells,no_cells)) #Final number of cells
df_map_grid_0 = st_sf(grid_id=1:length(grid_geom), grid_geom)
# plot(df_map_grid_0)


intersection <- st_intersection(y = df_map_grid_0 %>% select(grid_id) , x = df_map)

intersection %>%
  group_by(grid_id) %>%
  count() %>%
  rename(no_customers = n) %>%
  mutate(no_customers = coalesce(log(no_customers), 0)) %>%
  st_set_geometry(NULL) -> grid_points_counts

