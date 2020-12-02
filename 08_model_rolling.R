# Script for modeling whether the customer bought second time in the shop
# Data from first purchase is used 

library(readr)
library(tidyverse)
library("leaflet")
library(psych)
library(lubridate)
library(cluster)
library(factoextra)
library(caret)
library(rpart)

# Data preparation ----
orders <- read_csv("data/olist_orders_dataset.csv")
customers <- read_csv("data/olist_customers_dataset.csv")
geolocation <- read_csv("data/olist_geolocation_dataset.csv")
order_items <- read_csv("data/olist_order_items_dataset.csv")
order_payments <- read_csv("data/olist_order_payments_dataset.csv")
order_reviews <- read_csv("data/olist_order_reviews_dataset.csv")
products <- read_csv("data/olist_products_dataset.csv")
sellers <- read_csv("data/olist_sellers_dataset.csv")
product_translation <- read_csv("data/product_category_name_translation.csv")


load(file = 'data/05_orders_enhanced.Rdata')

first_orders


geolocation %>%
  select(1,2,3) %>%
  group_by(geolocation_zip_code_prefix) %>%
  filter(row_number() == 1) %>%
  ungroup() -> geolocation2



geolocation2 %>% head(5)

order_items %>%
  group_by(order_id) %>%
  summarise(no_items = max(order_item_id),
            sum_freight = sum(freight_value, na.rm=T)
  ) -> order_items2

order_items %>%
  select(order_id, product_id, price) %>%
  group_by(order_id) %>%
  filter(price == max(price)) %>%
  left_join(products %>% select(1,2)) %>%
  select(1,4) -> order_items3

first_orders %>%
  left_join(geolocation2, by = c('customer_zip_code_prefix' = 
                                   'geolocation_zip_code_prefix')) %>%
  left_join(order_items2) %>%
  left_join(order_items3) %>%
  select(
    payment_value,
    review_score,
    if_second_order,
    geolocation_lat,
    geolocation_lng,
    no_items,
    sum_freight
    # product_category_name
  ) %>%
  mutate(if_second_order = as.factor(if_second_order))  -> to_model

to_model %>% head()


# Prepare data for rolling predictions ----

# Distribution of purchases (plot)
ggplot(first_orders, aes(x=order_purchase_timestamp)) +
  geom_histogram()

first_orders %>% select(order_purchase_timestamp) %>% summary()

first_orders %>%
  mutate(cohort = (year(order_purchase_timestamp)-2000)*100 + month(order_purchase_timestamp)) %>%
  group_by(cohort) %>%
  summarise(n()) %>% View()

# all 16 - very small number, maybe bin? 
# 1809, 1810 - last observations, few samples


first_orders %>%
  # Create bins from month
  mutate(cohort = (year(order_purchase_timestamp)-2000)*100 + month(order_purchase_timestamp)) %>%
  # Bin all from 2016 to one bin - 1612
  mutate(cohort = ifelse(cohort<1613, 1612, cohort)) %>%
  # Remove 1809, 1810
  filter(!(cohort %in% c(1809,1810))) %>%
  left_join(geolocation2, by = c('customer_zip_code_prefix' = 
                                   'geolocation_zip_code_prefix')) %>%
  left_join(order_items2) %>%
  left_join(order_items3) %>%
  select(
    cohort,
    payment_value,
    review_score,
    if_second_order,
    geolocation_lat,
    geolocation_lng,
    no_items,
    sum_freight
    # product_category_name
  ) %>%
  mutate(if_second_order = as.factor(if_second_order))  -> to_model_cohort


to_model_cohort %>%
  mutate(no_items = ifelse(is.na(no_items), 1, no_items),
         if_second_order = ifelse(as.character(if_second_order) == '1', 'yes', 'no')
  ) %>%
  drop_na() -> to_model_cohort2

to_model_cohort2


to_model_cohort2

# Create model from rolling months (1, then 1,2 then 1,2,3 and so on) - LR ----

to_model_cohort2 %>%
  distinct(cohort) %>%
  arrange(cohort) %>%
  .$cohort -> cohorts_ordered

lr_cohorts_list2 <- vector('list', length(cohorts_ordered))
names(lr_cohorts_list2) <- cohorts_ordered

set.seed(825)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  repeats = 1)

# Training in loop

coh_rolling <- c()
for (coh in cohorts_ordered){
  coh_rolling <- c(coh_rolling, coh)
  
  print(paste0('Running cohort: ', coh_rolling))
  to_model_cohort2_current <- to_model_cohort2 %>%
    filter(cohort %in% coh_rolling) %>%
    select(-cohort)
  
  lr_cohorts_list2[as.character(coh)] <- list(train(if_second_order ~., to_model_cohort2_current, 
                                                    # method = "gbm",
                                                    method="glm",
                                                    metric="ROC",
                                                    family=binomial(),
                                                    trControl = fitControl
  ))
}

# Create roc for next months splitted ----
roc_list2 <- vector('list', length(cohorts_ordered)-1)
names(roc_list2) <- cohorts_ordered[-21]


roc_list_temp <-  tibble(model_name = '', test_on = '', auc = 0)
roc_list_all <-  tibble(model_name = '', test_on = '', auc = 0)

# For each model
for (i in 1:(length(cohorts_ordered)-1)){
  # For each month
  for (cohort_to_test_id in cohorts_ordered){
    print('a')
    print(cohorts_ordered[i])
    print(cohort_to_test_id)
    current_coh <- as.character(cohorts_ordered[i])
    next_coh <- as.character(cohort_to_test_id)
    
    next_cohort_test <- to_model_cohort2 %>%
      filter(cohort == cohort_to_test_id) %>%
      select(-cohort)
    
    predictions = predict(lr_cohorts_list2[current_coh][1], next_cohort_test,type = 'prob')[[1]]
    
    roc5  <- pROC::roc(as.numeric(next_cohort_test$if_second_order == "yes"), 
                       predictions[, 1])$auc
    
    roc_list_temp <- rbind(roc_list_temp, tibble(model_name = current_coh, test_on = next_coh, auc = roc5))
    
  }
  roc_list_all <- rbind(roc_list_all, roc_list_temp)
  roc_list_temp <-  tibble(model_name = '', test_on = '', auc = 0)
  
}

roc_list_all %>%
  filter(model_name != '') %>%
  mutate_if(is.character, as.numeric) %>%
  filter(model_name<test_on) %>%
  # filter(model_name == 1612) %>%
  mutate(test_on = as.Date(as.character(test_on*100+1+20000000), format = '%Y%m%d')) %>%
  ggplot(aes(x = test_on, y = auc, color = as.character(model_name), group  = as.character(model_name))) +
  geom_line() -> p1

plotly::ggplotly(p1)

roc_list_all %>%
  filter(model_name != '') %>%
  mutate_if(is.character, as.numeric) %>%
  filter(model_name<test_on) %>%
  group_by(test_on) %>%
  filter(auc == max(auc)) %>% 
  arrange(test_on)


# Create model from rolling months (1, then 1,2 then 1,2,3 and so on) - XGB ----

to_model_cohort2 %>%
  distinct(cohort) %>%
  arrange(cohort) %>%
  .$cohort -> cohorts_ordered

xgb_cohorts_list2 <- vector('list', length(cohorts_ordered))
names(xgb_cohorts_list2) <- cohorts_ordered

set.seed(825)

tune_grid_xgb <- expand.grid(
  nrounds = seq(from = 50, to = 150, by = 50),
  eta = c(0.025, 0.1),
  max_depth = c(2, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

fitControl_xgb <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 2,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  repeats = 1)

# Training in loop

coh_rolling <- c()
for (coh in cohorts_ordered){
  coh_rolling <- c(coh_rolling, coh)
  
  print(paste0('Running cohort: ', coh_rolling))
  to_model_cohort2_current <- to_model_cohort2 %>%
    filter(cohort %in% coh_rolling) %>%
    select(-cohort)
  
  xgb_cohorts_list2[as.character(coh)] <- list(train(if_second_order ~., to_model_cohort2_current, 
                                                    # method = "xgbTree",
                                                    method = "xgbTree",
                                                    # method="glm",
                                                    metric="ROC",
                                                    tuneGrid = tune_grid,
                                                    trControl = fitControl
  ))
}


# Create roc for next months splitted = XGB----
roc_list2 <- vector('list', length(cohorts_ordered)-1)
names(roc_list2) <- cohorts_ordered[-21]


roc_list_temp <-  tibble(model_name = '', test_on = '', auc = 0)
roc_list_all_xgb <-  tibble(model_name = '', test_on = '', auc = 0)

# For each model
for (i in 1:(length(cohorts_ordered)-1)){
  # For each month
  for (cohort_to_test_id in cohorts_ordered){
    print('a')
    print(cohorts_ordered[i])
    print(cohort_to_test_id)
    current_coh <- as.character(cohorts_ordered[i])
    next_coh <- as.character(cohort_to_test_id)
    
    next_cohort_test <- to_model_cohort2 %>%
      filter(cohort == cohort_to_test_id) %>%
      select(-cohort)
    
    predictions = predict(xgb_cohorts_list2[current_coh][1], next_cohort_test,type = 'prob')[[1]]
    
    roc5  <- pROC::roc(as.numeric(next_cohort_test$if_second_order == "yes"), 
                       predictions[, 1])$auc
    
    roc_list_temp <- rbind(roc_list_temp, tibble(model_name = current_coh, test_on = next_coh, auc = roc5))
    
  }
  roc_list_all_xgb <- rbind(roc_list_all_xgb, roc_list_temp)
  roc_list_temp <-  tibble(model_name = '', test_on = '', auc = 0)
  
}

roc_list_all_xgb %>%
  filter(model_name != '') %>%
  mutate_if(is.character, as.numeric) %>%
  filter(model_name<test_on) %>%
  # filter(model_name == 1612) %>%
  mutate(test_on = as.Date(as.character(test_on*100+1+20000000), format = '%Y%m%d')) %>%
  ggplot(aes(x = test_on, y = auc, color = as.character(model_name), group  = as.character(model_name))) +
  geom_line() -> p1

plotly::ggplotly(p1)

roc_list_all_xgb %>%
  filter(model_name != '') %>%
  mutate_if(is.character, as.numeric) %>%
  filter(model_name<test_on) %>%
  group_by(test_on) %>%
  arrange(-auc) %>%
  mutate(rank = row_number()) %>%
  filter(rank %in% c(1)) %>% 
  arrange(test_on, rank) %>% 
  View()
# In lot of cases most recent model is ranked on top 2 places


# Create model for each month - LR ----
to_model_cohort2 %>%
  distinct(cohort) %>%
  arrange(cohort) %>%
  .$cohort -> cohorts_ordered

lr_cohorts_list1 <- vector('list', length(cohorts_ordered))
names(lr_cohorts_list1) <- cohorts_ordered

set.seed(825)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  repeats = 1)
# Training in loop
for (coh in cohorts_ordered){
  print(paste0('Running cohort: ', coh))
  to_model_cohort2_current <- to_model_cohort2 %>%
    filter(cohort == coh) %>%
    select(-cohort)
  
  lr_cohorts_list1[as.character(coh)] <- list(train(if_second_order ~., to_model_cohort2_current, 
                                                    # method = "gbm",
                                                    method="glm",
                                                    metric="ROC",
                                                    family=binomial(),
                                                    trControl = fitControl
  ))
}

# Create roc for next month
roc_list1 <- vector('list', length(cohorts_ordered)-1)
names(lr_cohorts_list1) <- cohorts_ordered[-21]

for (i in 1:(length(cohorts_ordered)-1)){
  print('a')
  print(cohorts_ordered[i])
  print(cohorts_ordered[i+1])
  current_coh <- as.character(cohorts_ordered[i])
  next_coh <- as.character(cohorts_ordered[i+1])
  
  next_cohort_test <- to_model_cohort2 %>%
    filter(cohort == cohorts_ordered[i+1]) %>%
    select(-cohort)
  
  predictions = predict(lr_cohorts_list1[current_coh][1], next_cohort_test,type = 'prob')[[1]]
  
  
  roc5  <- pROC::roc(as.numeric(next_cohort_test$if_second_order == "yes"), 
                     predictions[, 1])
  
  roc_list1[current_coh] <- list(roc5)
  
}

for (roc in roc_list1){
  print(roc)
}
# Very variable outputs - from 0.49 to 0.67 - not consistent



# First model - gbm with no upsampling on 1701, test on 1702 ----
to_model_cohort2_1701 <- to_model_cohort2 %>%
  filter(cohort == 1701) %>%
  select(-cohort)

next_cohort_test <- to_model_cohort2 %>%
  filter(cohort == 1702) %>%
  select(-cohort)


set.seed(825)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  repeats = 1)


xgb_cohort1 <- train(if_second_order ~., to_model_cohort2_1701, 
                     method = "gbm",
                     # method="xgbTree",
                     metric="ROC",
                     # family=binomial(),
                     trControl = fitControl
)
xgb_cohort1


predictions5 = predict(xgb_cohort1, next_cohort_test,type = 'prob')


roc5  <- pROC::roc(as.numeric(next_cohort_test$if_second_order == "yes"), 
                   predictions5[, 1])

plot(roc5)
roc5
# AUC 0.53 - very low


# Second model - gbm with upsampling on 1701, test on 1702 ----
to_model_cohort2_1701 <- to_model_cohort2 %>%
  filter(cohort == 1701) %>%
  select(-cohort)

up_train <- upSample(x = to_model_cohort2_1701 %>% select(-if_second_order),
                     y = as.factor(to_model_cohort2_1701$if_second_order)) %>%
  as_tibble() %>%
  mutate(if_second_order = as.character(Class)) %>%
  select(-Class)

next_cohort_test <- to_model_cohort2 %>%
  filter(cohort == 1702) %>%
  select(-cohort)


set.seed(825)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  repeats = 1)


xgb_cohort2 <- train(if_second_order ~., up_train, 
                     method = "gbm",
                     # method="xgbTree",
                     metric="ROC",
                     # family=binomial(),
                     trControl = fitControl
)
xgb_cohort2


predictions5 = predict(xgb_cohort2, next_cohort_test,type = 'prob')


roc5  <- pROC::roc(as.numeric(next_cohort_test$if_second_order == "yes"), 
                   predictions5[, 1])

plot(roc5)
roc5
# AUC 0.51 - even lower than without upsampling


# model 3 - Logistic Regression with no upsampling on 1701, test on 1702 ----
to_model_cohort2_1701 <- to_model_cohort2 %>%
  filter(cohort == 1701) %>%
  select(-cohort)

next_cohort_test <- to_model_cohort2 %>%
  filter(cohort == 1702) %>%
  select(-cohort)


set.seed(825)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  repeats = 1)


glm_cohort1 <- train(if_second_order ~., to_model_cohort2_1701, 
                     # method = "gbm",
                     method="glm",
                     metric="ROC",
                     family=binomial(),
                     trControl = fitControl
)
glm_cohort1


predictions5 = predict(glm_cohort1, next_cohort_test,type = 'prob')


roc5  <- pROC::roc(as.numeric(next_cohort_test$if_second_order == "yes"), 
                   predictions5[, 1])

plot(roc5)
roc5
# AUC 0.58 - way better than with GBM, probably overfitting poorly



