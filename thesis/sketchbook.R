
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

plot_varimp <- function(df){
  df %>% 
    ggplot(aes(x = reorder(var2,Overall), y = Overall, label = round(Overall,2))) +
    geom_point(stat='identity', fill="black", size=3) +
    geom_segment(aes(y = 0,
                     x = var2,
                     yend = Overall,
                     xend = var2),
                 color = "black") +
    # geom_text(color="white", size=3) +
    # ylim(-2.5, 2.5) +
    coord_flip() +
    theme_minimal() +
    labs(
      x = 'Variable',
      y = 'Importance'
    ) +
    scale_y_continuous(minor_breaks = NULL)
}

# binned by category and geolocation
tibble(a = models_list$product_categories$model$coefnames) %>%
  mutate(a2 = ifelse(str_starts(a, 'prod_cat_'), 'prod_categories', a)) %>%
  mutate(a2 = ifelse(str_starts(a, 'geolocation'), 'geolocation', a2)) %>%
  group_by(a2) %>%
  nest() -> tmp_binned

features_groups_all_binned <- map(tmp_binned$data, function(x) x$a) %>% setNames(tmp_binned$a2)

permutation_var_imp_cat_binned <- calc_varimp_groups(models_list$product_categories$model, to_model_test, features_groups_all_binned) %>% rename(var2 = feature_group, Overall = score)
permutation_var_imp_cat <- var_imp_cat_p
save('data/permutation_var_imp_cat')

permutation_var_imp_cat_binned %>%
  plot_varimp() -> pl_varimp_binned

gridExtra::grid.arrange(pl_varimp_raw, pl_varimp_binned, ncol=2, widths=c(1.4,1))

#### big bin

tibble(a = models_list$all_with_pca$model$coefnames) %>%
  mutate(var2 = case_when(
    str_starts(a, 'spatial_') ~ 'geo - demographic',
    str_starts(a, 'geolocation_') ~ 'geo - raw location',
    str_starts(a, 'agglomeration') ~ 'geo - density',
    str_starts(a, 'topic_') ~ 'perception',
    str_starts(a, 'review_') ~ 'perception',
    str_starts(a, 'prod_cat_') ~ 'behavioural (first transaction)',
    a %in% c('payment_value', 'sum_freight', 'no_items', 'prod_categories') ~ 'behavioural (first transaction)',
    TRUE ~ a
  )) %>%
  group_by(var2) %>%
  nest() -> tmp_binned

features_groups_all_binned <- map(tmp_binned$data, function(x) x$a) %>% setNames(tmp_binned$var2)


permutation_var_imp_all_binned <- calc_varimp_groups(models_list$all_with_pca$model, to_model_test, features_groups_all_binned) %>% rename(Overall = score, var2 = feature_group)

save(permutation_var_imp_all_binned, file = here('data/permutation_var_imp_all_binned.Rdata'))
# load(file = here('data/permutation_var_imp_all_binned.Rdata'))


permutation_var_imp_all_binned %>%
  rename(var2 = feature_group) %>%
  plot_varimp()



range01 <- function(x){(x-min(x))/(max(x)-min(x))}


plot_varimp(temp_df_imp)


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

