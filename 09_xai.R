library(readr)
library(tidyverse)
library("leaflet")
library(psych)
library(lubridate)
library(cluster)
library(factoextra)
library(caret)
library(rpart)
library(DALEX)



# Create functions -----

#' Create functions for calculating AUC etc.

bootstrap_auc <- function(model, test_set, no_resamples){
  out_roc <- vector('numeric', no_resamples)
  
  len_test <- nrow(test_set)
  for (i in 1:no_resamples){
    idxes <- sample(1:len_test, size = len_test, replace = T)
    temp_test <- test_set[idxes,]
    
    predictions_temp = predict(model, temp_test,type = 'prob')
    roc_temp  <- pROC::roc(as.numeric(temp_test$if_second_order == "yes"), 
                           predictions_temp[, 1])
    out_roc[i] <- roc_temp$auc
    i <- i+1
    
  }
  out_roc
}

bootstrap_summary<- function(out_roc){
  
  tibble(auc =out_roc) %>%
    summary %>%
    print
  
  mean_auc <- mean(out_roc)
  tibble(auc =out_roc, y=0) %>%
    ggplot(aes(x=auc)) +
    geom_density() +
    geom_jitter(aes(y=y), alpha=0.5) +
    geom_vline(xintercept = mean_auc, color = 'red')
  
}


calc_metrics <- function(model, to_model_test, run_confusion_matrix = F){
  predictions1 <- predict(model, to_model_test,type = 'prob')
  roc_test1  <- pROC::roc(as.numeric(to_model_test$if_second_order == "yes"), 
                          predictions1[, 1])
  
  plot(roc_test1) 
  title('ROC curve on test set \n')
  print('Calc AUC on test set:')
  print(roc_test1)
  
  if(run_confusion_matrix){
    confusionMatrix(data = as.factor(as.numeric(predictions1[, 2] > 0.5)), # probability of yes was more than
                    reference = as.factor(as.numeric(to_model_test$if_second_order == "yes")),
                    # definitions of the "success" label
                    positive = '1') 
  }
  # print('Quantiles of predicted responses:')
  # print('-----')
  # predictions1[,2] %>%
  #   quantile(seq(0.8,1, 0.01))
}

calc_roc <- function(model, to_model_test, run_confusion_matrix = F){
  predictions1 <- predict(model, to_model_test,type = 'prob')
  roc_test1  <- pROC::roc(as.numeric(to_model_test$if_second_order == "yes"), 
                          predictions1[, 1])
  return(roc_test1)
}


#' Load the models and training data - without logistic regression -----

library(stringr)
files_cache <- list.files('run_all_models_cache/')
files_names_models <- files_cache[str_starts(files_cache, 'saved_model')]
files_names_models <- files_names_models[!str_starts(files_names_models, 'saved_model_lr')]
files_names_models


load('run_all_models_cache/to_model_train.Rdata')
load('run_all_models_cache/to_model_test.Rdata')

models_list <- vector('list', length(files_names_models))

for (i in 1:length(files_names_models)){
  print(i)
  name <- files_names_models[i]
  temp_env <- new.env()
  load(paste0('run_all_models_cache/',name),envir = temp_env)
  models_list[[i]] <- temp_env$model_spec
}

models_names <- c()
for (i in models_list){
  models_names <- c(models_names, i$model_name)
}

names(models_list) <- models_names

#' Create auc-roc tables ----
for (i in models_names){
  print(i)
  models_list[[i]]$auc_test <- calc_roc(models_list[[i]]$model, to_model_test)$auc
  models_list[[i]]$auc_train <- calc_roc(models_list[[i]]$model, to_model_train)$auc
}

models_list %>% 
  purrr::map_df(~as.numeric(.x$auc_test)) %>%
  pivot_longer(cols=everything(),values_to = 'AUC_test') -> auc_test_table

models_list %>% 
  purrr::map_df(~as.numeric(.x$auc_train)) %>%
  pivot_longer(cols=everything(),values_to = 'AUC_train') -> auc_train_table

#' AUC table for Random Forest
auc_test_table %>%
  left_join(auc_train_table) %>%
  arrange(-AUC_test) %>%
  mutate(AUC_perc_performance_drop = (AUC_test-max(AUC_test))/max(AUC_test))

# Zapis LR tabeli do pliku
# auc_test_table %>%
#   left_join(auc_train_table) %>%
#   arrange(-AUC_test) %>%
#   mutate(AUC_perc_performance_drop = (AUC_test-max(AUC_test))/max(AUC_test)) -> auc_table_for_lr
#   
# save(auc_table_for_lr, file='run_all_models_cache/auc_table_for_lr.Rdata')

#' AUC table for Logistic regression
#' 
load(file='run_all_models_cache/auc_table_for_lr.Rdata')

auc_table_for_lr

#' product_categories is the best

plot(varImp(models_list$basic_info$model))
plot(varImp(models_list$product_categories$model))
#' Importance of categories is not big, however the model is still the best - 2% gain in AUC test compared to non-categories models


#' DALEX explainer ----

explain <- DALEX::explain(model = models_list$product_categories$model,  
                          data = to_model_test %>% select(-if_second_order),
                          y = to_model_test$if_second_order == "yes", 
                          label = "Product categories")


to_model_test %>%
  select_if(is.numeric) %>%
  summarise_all(mean) -> mean_values

#' Break down of average prediction
bd_rf <- predict_parts(explainer = explain,
                       new_observation = mean_values,
                       type = "break_down")
bd_rf 
plot(bd_rf)


#' Create grid of predictions for lng-lat ----
predict_profile(explainer = explain, 
                new_observation = mean_values,variables = c('geolocation_lng', 'geolocation_lat')) -> profile_geo

profile_geo %>%
  as_tibble() %>%
  select(geolocation_lat) %>%
  distinct() %>%
  crossing(
    profile_geo %>% 
      select(geolocation_lng) %>%
      distinct()) %>%
  crossing(mean_values %>% select(-geolocation_lat, -geolocation_lng)) -> profile_geo

profile_geo$yhat <- predict(models_list$all_with_pca$model, profile_geo,type='prob')[,2]

#' Leaflet map
#' 

profile_geo %>%
  select(geolocation_lat, geolocation_lng, yhat) %>%
  sample_frac(0.7) %>%
  mutate(a = as.factor(as.numeric(cut(yhat, breaks=9)))) -> profile_geo_to_plot

# Bounding box
profile_geo_to_plot = profile_geo_to_plot[profile_geo_to_plot$geolocation_lat <= 5.27438888,]
profile_geo_to_plot = profile_geo_to_plot[profile_geo_to_plot$geolocation_lng >= -73.98283055,]
profile_geo_to_plot = profile_geo_to_plot[profile_geo_to_plot$geolocation_lat >= -33.75116944,]
profile_geo_to_plot = profile_geo_to_plot[profile_geo_to_plot$geolocation_lng <=  -34.79314722,]


pal <- colorFactor(RColorBrewer::brewer.pal(9, 'Greens'), domain = levels(profile_geo_to_plot$a))

profile_geo_to_plot %>%
  rename(lat = geolocation_lat, lng = geolocation_lng) %>%
  leaflet() %>%  
  # addTiles() %>%
  addProviderTiles('Stamen.Toner') %>%
  addCircleMarkers(
    color = ~pal(a),
    stroke = FALSE, fillOpacity = 0.5,
    radius = 4
  )
# TODO: Bounding box z kraju - żeby nie wychodziło poza Brazylię


# ggplot(profile_geo_to_plot, aes(y = geolocation_lat, x = geolocation_lng, color=a)) +
#   geom_point(size = 1) +
#   scale_color_brewer(palette='Greens')

# library(tmap)
# tmap_mode("plot")
# brazil_map <- brazilmaps::get_brmap("Brazil")
# brazil_map_city <- brazilmaps::get_brmap("City")
# 
# df <- sf::st_as_sf(sampled_zipcodes, coords = c(3,2))
# tm_shape(brazil_map_city) +
#   tm_polygons(col = "white") +
#   # tm_shape(df_small) +
#   tm_symbols(size = 0.01, 
#              # col = "zip_code_1",
#              border.lwd = NA,
#              alpha = 0.5)


#' Create univariate cateris paribus plots ----
#' 
plot(varImp(models_list$product_categories$model))
predict_profile(explainer = explain, 
                new_observation = mean_values) -> profile_all


#' a
plot(profile_all, variables = c("payment_value")) + ggtitle("Ceteris-paribus profile", "")
#' a
plot(profile_all, variables = c("sum_freight")) + ggtitle("Ceteris-paribus profile", "")
#' a
plot(profile_all, variables = c("review_score")) + ggtitle("Ceteris-paribus profile", "")
#' a
plot(profile_all, variables = c("no_items")) + ggtitle("Ceteris-paribus profile", "")



#' Analysis of potential revenue ----
# preds <- to_model_topic2
# preds['prob'] <- predict(model_topic1, to_model_topic2%>% select(-if_second_order), type='prob')['yes']

preds <- to_model_test
preds['prob'] <- predict(models_list$product_categories$model, to_model_test%>% select(-if_second_order), type='prob')['yes']

preds %>% 
  mutate(if_second_order = ifelse(if_second_order=='yes', 1, 0)) %>%
  select(if_second_order, prob) -> preds2

ggplot(preds2, aes(x=as.character(if_second_order), y = prob)) +
  geom_violin()


score_randomly <- mean(preds2$if_second_order)
score_randomly
#' 0.033 of people have second order
#' 
#' This means that if we would send out the encouragement randomly, 
#' we would hit 0.033 percent of people that truly would do second order

#' Now we can utilize the model output to do predictions:
preds2 %>%
  filter(prob>quantile(prob, 0.5)) %>%
  summarise(score=mean(if_second_order),
            score_randomly = score_randomly,
            lift_over_randomly = (score-score_randomly)/score_randomly
  )
#' jeżeli problem jest "przy limitowanym budżecie na reklamę, wyślij reklamę do ludzi którzy chcą coś
#' kupić jeszcze raz" to mój system osiąga o 35% lepsze wyniki niż randomowo, jeżeli chcemy wysłać coś do 50% klientów

preds2 %>%
  filter(prob>quantile(prob, 0.99)) %>%
  summarise(score=mean(if_second_order),
            score_randomly = score_randomly,
            lift_over_randomly = (score-score_randomly)/score_randomly
  )

output_lifts <- data.frame()

for (quant in seq(0.99, 0.01, -0.01)){
  # print(1-quant)
  
  preds2 %>%
    filter(prob>quantile(prob, quant)) %>%
    summarise(
      fraction_of_customers = 1-quant,
      no_customers_in_bin = n(),
      score=mean(if_second_order),
      score_randomly = score_randomly,
      performance_gain_over_randomly = (score-score_randomly)/score_randomly,
      lift_over_randomly = score/score_randomly
    ) -> tmp
  
  output_lifts <- rbind(output_lifts, tmp)
  
}

#' Table with selected quantiles
quantiles_to_show <- c(0.99, 0.98, 0.97, 0.96, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5)
output_lifts %>% 
  filter(fraction_of_customers %in% (1-quantiles_to_show))

#' Lift curve
output_lifts %>%
  ggplot(aes(x = fraction_of_customers, y= lift_over_randomly)) +
  geom_line() +
  geom_hline(yintercept = 1, color = 'red') +
  labs(title = 'Lift curve') +
  theme_minimal()




#' Top best customers according to the model ---
preds[c(models_list$product_categories$vars_used, 'prob')] %>%
  arrange(-prob) %>%
  select(prob, everything()) %>%
  distinct() %>%
  head(10)



#' random ----
# break
# preds2 %>%
#   arrange(-prob) %>%
#   mutate(bin = floor(row_number()/1000)) %>%
#   group_by(bin) %>%
#   summarise(perc_second = mean(if_second_order)) %>%
#   View()
# 
# 
# preds2 %>%
#   filter(prob>quantile(prob, 0.7)) %>%
#   summarise(no_people_second=mean(if_second_order),
#             perc_second = (mean(if_second_order-0.0344)/0.0344)
#   )
# # przy "wyślij do 30% klientów" o 65% lepsze wyniki
# 
# preds2 %>%
#   filter(prob>quantile(prob, 0.9)) %>%
#   summarise(no_people_second=mean(if_second_order),
#             perc_second = (mean(if_second_order-0.0344)/0.0344)
#   )
# # a przy "wyślij do 10% klientów" o 144% 
# 
# 
# cp2d <- ingredients::ceteris_paribus_2d(explainer = explain_topic,observation = mean_values,
#                                         variables = c('geolocation_lng', 'geolocation_lat'))
# 
# cp2d %>%
#   ggplot(aes(x=geolocation_lat, y = geolocation_lng, fill=y_hat)) +
#   geom_raster()
# 
# 
# fifa_mp_gbm_deep <- model_parts(explain)
# plot(fifa_mp_gbm_deep, max_vars = 20, 
#      bar_width = 4, show_boxplots = FALSE) 
# 
