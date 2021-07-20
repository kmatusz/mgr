auc_test_table %>%
  left_join(auc_train_table) %>%
  arrange(-AUC_test) %>%
  mutate(AUC_perc_performance_drop = (AUC_test-max(AUC_test))/max(AUC_test)) %>%
  mutate_at(vars(AUC_test, AUC_train), function(x) sprintf('%.4f', x)) %>%
  mutate_at(vars(AUC_perc_performance_drop), function(x) sprintf('%.2f%%', x*100)) %>%
  rename(
    `Variable` = name,
    `AUC score - test set` = AUC_test,
    `AUC score - train set` = AUC_train,
    `Performance drop vs. the best model` = AUC_perc_performance_drop
  ) %>%
flextable::flextable() %>%
  flextable::set_caption("AUC values for XGBoost model") %>%
  flextable_format
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

