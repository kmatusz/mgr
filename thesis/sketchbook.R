product_categories_temp1 %>%
  left_join(product_categories_temp2) %>%
  arrange(-percent_second_order) %>%
  mutate_if(is.numeric, round,3) %>%
  mutate_at(vars(starts_with('perc')), ~sprintf('%.1f%%', .x*100)) %>%
  rename(
    `Product category` = category,
    `No. items` = no_items,
    `Percentage` = percentage,
    `Percentage of\n second order` = percent_second_order
  ) %>%
  flextable::flextable() %>%
  flextable::set_caption("Product categories") %>%
  flextable::font(fontname = 'Times New Roman', part = 'all') %>%
  flextable::fontsize(size = 12) %>%
  flextable::autofit()


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

