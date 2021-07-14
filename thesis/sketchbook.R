tm_shape(brazil_map) +
  tm_polygons(col = "white") +
  tm_shape(df[which(lengths(st_within(df, brazil_map)) != 0), ]%>%rename(`Prediction quantile`=a)) +
  tm_symbols(size = 0.4,
             col = "yhat",
             palette = RColorBrewer::brewer.pal(9, 'Greens'),
             # style = "fixed",
             # breaks = c(45, 60, 75, 90),
             border.lwd = NA,
             n = 10,
             alpha = 0.8) +
  tm_shape(brasil_cities_coords %>% arrange(-population) %>% head(10)) +
  tm_symbols(size = 0.4,
             col = "red",
             # style = "fixed",
             # breaks = c(45, 60, 75, 90),
             border.lwd = NA,
             alpha = 0.8) +
  tm_text(text='city', just='top')


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

