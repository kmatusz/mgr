temp_df_imp %>%
  ggplot(aes(x = var2, y = Overall, label = round(Overall))) +
  geom_point(stat='identity', fill="black", size=7) +
  geom_segment(aes(y = 0,
                   x = var2,
                   yend = Overall,
                   xend = var2),
               color = "black") +
  geom_text(color="white", size=3) +
  # ylim(-2.5, 2.5) +
  coord_flip() +
  theme_minimal() +
  labs(
    x = 'Variable',
    y = 'Importance'
  )
  

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

