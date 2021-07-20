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
    scale_x_cont
}

var_imp_cat <- varImp(models_list$product_categories$model, scale=F)


# Not binned at all
var_imp_cat$importance %>%
  as_tibble(rownames = 'var2') %>%
  # mutate(Overall = range01(Overall)*100) %>%
  plot_varimp() -> pl_varimp_raw

# binned by category and geolocation

var_imp_cat$importance %>%
  as_tibble(rownames = 'var') %>%
  mutate(var2 = ifelse(str_starts(var, 'prod_cat_'), 'prod_categories', var)) %>%
  mutate(var2 = ifelse(str_starts(var, 'geolocation'), 'geolocation', var2)) %>%
  group_by(var2) %>%
  summarise(Overall = sum(Overall)) %>%
  # mutate(Overall = range01(Overall)*100) %>%
  as.data.frame() -> temp_df_imp_cat

pl_varimp_binned <- plot_varimp(temp_df_imp_cat)

gridExtra::grid.arrange(pl_varimp_raw, pl_varimp_binned, ncol=2, widths=c(1.4,1))

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

