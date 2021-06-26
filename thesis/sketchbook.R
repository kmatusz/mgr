st_coordinates(df_map) %>% 
  as_tibble() %>%
  # sample_frac(0.1) %>%
  ggplot(aes(x=X, y=Y)) +
  geom_density2d_filled(alpha = 0.7) +
  geom_point(size=0.05) +
  theme_minimal()


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

