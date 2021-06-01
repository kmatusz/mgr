# Script for modeling whether the customer bought second time in the shop
# Data from first purchase is used 

TRAIN_MODELS <- TRUE

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


order_reviews_topics <- read_csv("data/reviews_with_topic.csv")

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


# Prepare data for caret ----
to_model %>%
  mutate(no_items = ifelse(is.na(no_items), 1, no_items),
         if_second_order = ifelse(as.character(if_second_order) == '1', 'yes', 'no')
  ) %>%
  drop_na() -> to_model2

save(to_model2, file='08_to_model2.Rdata')

# Train test split, create dataset with upsampling                           -----
set.seed(10)
training_obs <- createDataPartition(to_model2$if_second_order, 
                                    p = 0.7, 
                                    list = FALSE)
to_model_train <- to_model2[training_obs[,1],]
to_model_test  <- to_model2[-training_obs[,1],]

up_train2 <- upSample(x = to_model_train %>% select(-if_second_order),
                      y = as.factor(to_model_train$if_second_order)) %>%
  as_tibble() %>%
  mutate(if_second_order = as.character(Class)) %>%
  select(-Class)


# Functions for bootstraping AUC on test set                          -----
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

# Model 1 - GBM on standard hyperparameters with upsampling (AUC test 0.5921)        ----

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  repeats = 1)


# Train model
set.seed(825)
if (TRAIN_MODELS){
  model1 <- train(if_second_order ~., up_train2, 
                  method = "gbm",
                  # method="glm", 
                  metric="ROC",
                  # family=binomial(), 
                  trControl = fitControl
                  # tuneGrid = gbmGrid
  )
  save(model1, file = 'models_cache/model1.Rdata')
} else {
  load('models_cache/model1.Rdata')
}
model1

# Create metrics - roc and confusion matrix
calc_metrics(model1, to_model_test)

# Bootstrap for test set
out_roc1 <- bootstrap_auc(model1, to_model_test, no_resamples = 100)
bootstrap_summary(out_roc1)

# Model 2 - GBM on standard hyperparameters without upsampling (Auc test 0.5899)      ----

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  repeats = 1)


# Train model
set.seed(825)
if (TRAIN_MODELS){
  model2 <- train(if_second_order ~., to_model_train, 
                  method = "gbm",
                  # method="glm", 
                  metric="ROC",
                  # family=binomial(), 
                  trControl = fitControl
                  # tuneGrid = gbmGrid
  )
  save(model2, file = 'models_cache/model2.Rdata')
} else {
  load('models_cache/model2.Rdata')
}
model2

# Create metrics - roc and confusion matrix
calc_metrics(model2, to_model_test)

# Bootstrap for test set
out_roc2 <- bootstrap_auc(model2, to_model_test, no_resamples = 100)
bootstrap_summary(out_roc2)

# Model 3 - Logistic with upsampling (Auc test 0.5575)                               ----

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  repeats = 1)


# Train model
set.seed(825)
if (TRAIN_MODELS){
  model3 <- train(if_second_order ~., up_train2, 
                  method="glm",
                  metric="ROC",
                  family=binomial(),
                  trControl = fitControl
                  # tuneGrid = gbmGrid
  )
  save(model3, file = 'models_cache/model3.Rdata')
} else {
  load('models_cache/model3.Rdata')
}
model3

# Create metrics - roc and confusion matrix
calc_metrics(model3, to_model_test)


# Bootstrap for test set
out_roc3 <- bootstrap_auc(model3, to_model_test, no_resamples = 100)
bootstrap_summary(out_roc3)

# Model 4 - Logistic without upsampling (Auc test 0.5563)                            ----

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  repeats = 1)


# Train model
set.seed(825)
if (TRAIN_MODELS){
  model4 <- train(if_second_order ~., to_model_train, 
                  method="glm",
                  metric="ROC",
                  family=binomial(),
                  trControl = fitControl
                  # tuneGrid = gbmGrid
  )
  save(model4, file = 'models_cache/model4.Rdata')
} else {
  load('models_cache/model4.Rdata')
}
model4

# Create metrics - roc and confusion matrix
calc_metrics(model4, to_model_test)


# Bootstrap for test set
out_roc4 <- bootstrap_auc(model4, to_model_test, no_resamples = 100)
bootstrap_summary(out_roc4)



# Model 5 - XGB extensive hyperparameters search with upsampling (Auc test 0.6159)           ----

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  repeats = 1)


# Train model
set.seed(825)
if (TRAIN_MODELS){
# if (TRUE){
  model5 <- train(if_second_order ~., up_train2, 
                  method="xgbTree",
                  metric="ROC",
                  trControl = fitControl,
                  verbose = T
                  # tuneGrid = gbmGrid
  )
  save(model5, file = 'models_cache/model5.Rdata')
} else {
  load('models_cache/model5.Rdata')
}
model5

# Create metrics - roc and confusion matrix
calc_metrics(model5, to_model_test)


# Bootstrap for test set
out_roc5 <- bootstrap_auc(model5, to_model_test, no_resamples = 100)
bootstrap_summary(out_roc5)


# Data preparation with geographic info ----
load('data/preprocessed/spatial_all_by_zip.Rdata')
spatial_all_by_zip

first_orders %>%
  left_join(geolocation2, by = c('customer_zip_code_prefix' = 
                                   'geolocation_zip_code_prefix')) %>%
  left_join(spatial_all_by_zip, by = c('customer_zip_code_prefix' = 
                                         'geolocation_zip_code_prefix')) %>%
  left_join(order_items2) %>% 
  left_join(order_items3) %>%
  mutate(if_second_order = as.numeric(if_second_order)) %>%
  select_if(is.numeric) %>%
  mutate(if_second_order = as.factor(if_second_order))%>%
  mutate(no_items = ifelse(is.na(no_items), 1, no_items),
         if_second_order = ifelse(as.character(if_second_order) == '1', 'yes', 'no')
  ) %>% 
  # Replace NA with column mean
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) -> to_model_geo

set.seed(10)
training_obs_geo <- createDataPartition(to_model_geo$if_second_order, 
                                        p = 0.7, 
                                        list = FALSE)
to_model_train_geo <- to_model_geo[training_obs_geo[,1],]
to_model_test_geo  <- to_model_geo[-training_obs_geo[,1],]

up_train_geo <- upSample(x = to_model_train_geo %>% select(-if_second_order),
                         y = as.factor(to_model_train_geo$if_second_order)) %>%
  as_tibble() %>%
  mutate(if_second_order = as.character(Class)) %>%
  select(-Class)

# prcomp(up_train_geo %>% select(-if_second_order),scale. = T) -> pca_model
# pca_model %>% summary()


# Model geo 1- extensive XGB search with upsampling on geo data (Auc test 0.5546) ----

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  repeats = 1)


# Train model
set.seed(825)
if (TRAIN_MODELS){
# if (TRUE){
  model_geo1 <- train(if_second_order ~., up_train_geo, 
                      method="xgbTree",
                      metric="ROC",
                      trControl = fitControl,
                      verbose = T
                      # tuneGrid = gbmGrid
  )
  save(model_geo1, file = 'models_cache/model_geo1.Rdata')
} else {
  load('models_cache/model_geo1.Rdata')
}
model_geo1

# Create metrics - roc and confusion matrix
calc_metrics(model_geo1, to_model_test_geo)


# Bootstrap for test set
out_roc_geo1 <- bootstrap_auc(model_geo1, to_model_test_geo, no_resamples = 100)
bootstrap_summary(out_roc_geo1)

# Data preparation - PCA on geo dataset ----
library(factoextra)
to_model_train
pca_model <- prcomp(to_model_train_geo %>%
                      select(-if_second_order), scale = TRUE,center = TRUE)
summary(pca_model)
fviz_eig(pca_model)
# Setting no of components to 8 - 90% of variability

pca_model_8 <- prcomp(up_train_geo %>%
                         select(-if_second_order), scale = TRUE,center = TRUE,rank. = 10)
summary(pca_model_8)

predict(pca_model_8, up_train_geo) %>%
  as_tibble() %>%
  mutate(if_second_order = up_train_geo$if_second_order) -> up_train_geo_pca


predict(pca_model_8, to_model_test_geo) %>%
  as_tibble() %>%
  mutate(if_second_order = to_model_test_geo$if_second_order) -> to_model_test_geo_pca

# Model geo 2 - XGB search with upsampling on geo data with PCA on 8 components (Auc test 0.5289) ----

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  repeats = 1)


# Train model
set.seed(825)
if (TRAIN_MODELS){
# if (TRUE){
  model_geo2 <- train(if_second_order ~., up_train_geo_pca, 
                      method="xgbTree",
                      metric="ROC",
                      trControl = fitControl,
                      verbose = T
                      # tuneGrid = gbmGrid
  )
  save(model_geo2, file = 'models_cache/model_geo2.Rdata')
  #AAAAAA
} else {
  load('models_cache/model_geo2.Rdata')
}
model_geo2

# Create metrics - roc and confusion matrix
calc_metrics(model_geo2, to_model_test_geo_pca)


# Bootstrap for test set
out_roc_geo2 <- bootstrap_auc(model_geo2, to_model_test_geo_pca, no_resamples = 100)
bootstrap_summary(out_roc_geo2)


# Data preparation - dbscan on geo data ----

library(dbscan)

to_model2 %>%
  select(geolocation_lat, geolocation_lng) %>%
  sample_n(10000) %>%
  ggplot(aes(geolocation_lng, geolocation_lat)) +
  geom_point(alpha = 0.5) ->p

plotly::ggplotly(p)
to_model2 %>%
  select(geolocation_lat, geolocation_lng) -> geo

dbscan::kNNdistplot(geo, k =1000)
abline(h = 5)
# 2 is ok 

# dbscan_geo <- dbscan::dbscan(geo, 2, 100)
dbscan_geo <- dbscan::dbscan(geo, 0.2, 100)

# 23.3S
# 23.75S - 50 km

geo %>%
  mutate(cluster = dbscan_geo$cluster) %>%
  mutate(agglomeration = ifelse(cluster==0, 0,1)) %>%
  ggplot(aes(geolocation_lng, geolocation_lat, color = agglomeration)) +
  geom_point(alpha=0.5)
  

library("factoextra")
fviz_cluster(dbscan_geo, geo, stand = FALSE, frame = FALSE, geom = "point")

to_model2 %>%
  mutate(cluster = dbscan_geo$cluster) %>%
  mutate(if_agglomeration = ifelse(cluster==0, 0,1)) %>%
  select(-cluster) -> to_model3

# Train test split, create dataset with upsampling
set.seed(10)
training_obs <- createDataPartition(to_model3$if_second_order, 
                                    p = 0.7, 
                                    list = FALSE)
to_model_train <- to_model3[training_obs[,1],]
to_model_test  <- to_model3[-training_obs[,1],]

up_train3 <- upSample(x = to_model_train %>% select(-if_second_order),
                      y = as.factor(to_model_train$if_second_order)) %>%
  as_tibble() %>%
  mutate(if_second_order = as.character(Class)) %>%
  select(-Class)


# prepare data with topic info -----
order_reviews_topics %>%
  select(order_id, review_score, starts_with('topic_'))
first_orders %>%
  left_join(geolocation2, by = c('customer_zip_code_prefix' = 
                                   'geolocation_zip_code_prefix')) %>%
  left_join(order_items2) %>%
  left_join(order_items3) %>%
  left_join(order_reviews_topics %>%
              select(order_id, review_score, starts_with('topic_'))) %>%
  select(
    payment_value,
    review_score,
    if_second_order,
    geolocation_lat,
    geolocation_lng,
    no_items,
    sum_freight,
    starts_with('topic_')
    # product_category_name
  ) %>%
  mutate(if_second_order = as.factor(if_second_order))  -> to_model_topic

to_model_topic %>% head()


# Prepare data for caret ----
to_model_topic %>%
  mutate(no_items = ifelse(is.na(no_items), 1, no_items),
         if_second_order = ifelse(as.character(if_second_order) == '1', 'yes', 'no')
  ) %>%
  drop_na() -> to_model_topic2

save(to_model_topic2, file='08_to_model_topic2.Rdata')
# load('08_to_model_topic2.Rdata')

# Train test split, create dataset with upsampling                           -----
set.seed(10)
training_obs <- createDataPartition(to_model_topic2$if_second_order, 
                                    p = 0.7, 
                                    list = FALSE)
to_model_train <- to_model_topic2[training_obs[,1],]
to_model_test  <- to_model_topic2[-training_obs[,1],]

up_train_topic <- upSample(x = to_model_train %>% select(-if_second_order),
                      y = as.factor(to_model_train$if_second_order)) %>%
  as_tibble() %>%
  mutate(if_second_order = as.character(Class)) %>%
  select(-Class)

# Model reviews topics 1 - XGB with upsampling on data from topics and the rest (Auc test 0.6431) ----
# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,returnResamp = 'all',
  repeats = 1)


# Train model
set.seed(825)
if (TRAIN_MODELS){
  # if (TRUE){
  model_topic1 <- train(if_second_order ~., up_train_topic, 
                  method="xgbTree",
                  metric="ROC",
                  trControl = fitControl,
                  verbose = T
                  # tuneGrid = gbmGrid
  )
  save(model_topic1, file = 'models_cache/model_topic1.Rdata')
} else {
  load('models_cache/model_topic1.Rdata')
}

# Create metrics - roc and confusion matrix
calc_metrics(model_topic1, to_model_test)
calc_metrics(model_topic1, to_model_train)


# Bootstrap for test set
out_roc_topic1 <- bootstrap_auc(model_topic1, to_model_test, no_resamples = 100)
bootstrap_summary(out_roc_topic1)

# Variable importance
plot(varImp(model_topic1))


# cluster not valuable at all

to_model3 %>%
  group_by(cluster) %>%
  summarise(how_many_second = sum(ifelse(if_second_order=='yes', 1, 0)),
            a = n()
            ) %>%
  mutate(b = how_many_second/a)

# Join all types of information -----
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
  left_join(spatial_all_by_zip, by = c('customer_zip_code_prefix' = 
                                         'geolocation_zip_code_prefix')) %>%
  left_join(order_items2) %>% 
  left_join(order_items3) %>%
  left_join(order_reviews_topics %>%
              select(order_id, starts_with('topic_'))%>%group_by(order_id)%>%head(1)) %>%
  mutate(if_second_order = as.numeric(if_second_order)) %>%
  select_if(is.numeric) %>%
  mutate(no_items = ifelse(is.na(no_items), 1, no_items),
         if_second_order = ifelse(as.character(if_second_order) == '1', 'yes', 'no')
  ) %>% 
  # Replace NA with column mean, no. NAs around 300 per 100 000 obs
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) -> to_model_all_0

dbscan_geo <- dbscan::dbscan(to_model_all_0%>%select(geolocation_lat,geolocation_lng), 0.2, 100)

to_model_all_0 %>%
  mutate(cluster = dbscan_geo$cluster) %>%
  mutate(agglomeration = ifelse(cluster==0, 0,1)) %>%
  select(-cluster) -> to_model_all_1

to_model_all_1 %>% head()

to_model_all_1 %>% summary()
# Prepare data for caret ----
to_model_all_1 %>%
  drop_na() -> to_model_all

save(to_model_all, file='08_to_model_all.Rdata')

# Train test split, create dataset with upsampling                           -----
set.seed(10)
training_obs <- createDataPartition(to_model_all$if_second_order, 
                                    p = 0.7,
                                    list = FALSE)
to_model_train <- to_model_all[training_obs[,1],]
to_model_test  <- to_model_all[-training_obs[,1],]

up_train <- upSample(x = to_model_train %>% select(-if_second_order),
                      y = as.factor(to_model_train$if_second_order)) %>%
  as_tibble() %>%
  mutate(if_second_order = as.character(Class)) %>%
  select(-Class)
up_train$if_second_order %>% table

# Model all1- extensive XGB search with upsampling on full dataset (Auc test 0.62, train 0.9999) ----

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  verboseIter = TRUE,
  repeats = 1)

tune_grid <- expand.grid(nrounds=c(100,200,400),
                        max_depth = c(3:7),
                        eta = c(0.01, 0.1,0.2),
                        gamma = c(0.01),
                        colsample_bytree = c(0.75),
                        subsample = c(0.50),
                        min_child_weight = c(0,1))

# tune_grid <- expand.grid(nrounds = 200,
#                          max_depth = 5,
#                          eta = 0.05,
#                          gamma = 0.01,
#                          colsample_bytree = 0.75,
#                          min_child_weight = 0,
#                          subsample = 0.5)

# Train model
set.seed(825)
if (TRAIN_MODELS){
  # if (TRUE){
  model_all1 <- train(if_second_order ~., up_train, 
                      method="xgbTree",
                      metric="ROC",
                      trControl = fitControl,
                      verbose = T,
                      tuneGrid = tune_grid,
  )
  save(model_all1, file = 'models_cache/model_all1.Rdata')
} else {
  load('models_cache/model_all1.Rdata')
}
model_all1
plot(varImp(model_all1))
plot(model_all1$finalModel)
# 9:40

# Create metrics - roc and confusion matrix
calc_metrics(model_all1, to_model_test)
calc_metrics(model_all1, to_model_train)
# way too much overfitting - auc train 0.9999 (!!!), while test 0.62

# Bootstrap for test set
out_roc_all1 <- bootstrap_auc(model_all1, to_model_test, no_resamples = 100)
bootstrap_summary(out_roc_all1)

# Model all2- extensive XGB search with upsampling on full dataset, (Auc test ) ----

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  verboseIter = TRUE,
  repeats = 1)

tune_grid <- expand.grid(nrounds=c(400),
                         max_depth = c(3,5),
                         eta = c(0.001, 0.01, 0.1),
                         gamma = c(0.01),
                         colsample_bytree = c(0.1, 0.2, 0.5),
                         subsample = c(0.50),
                         min_child_weight = c(0,1))

# tune_grid <- expand.grid(nrounds=c(400),
#                          max_depth = c(7),
#                          eta = c(0.2),
#                          gamma = c(0.01),
#                          colsample_bytree = c(0.75),
#                          subsample = c(0.50),
#                          min_child_weight = c(0,1))

# Train model
set.seed(825)
if (TRAIN_MODELS){
  # if (TRUE){
  model_all2 <- train(if_second_order ~., up_train, 
                      method="xgbTree",
                      metric="ROC",
                      trControl = fitControl,
                      verbose = T,
                      tuneGrid = tune_grid,
  )
  save(model_all2, file = 'models_cache/model_all2.Rdata')
} else {
  load('models_cache/model_all2.Rdata')
}
model_all2
plot(model_all2$finalModel)

# Create metrics - roc and confusion matrix
calc_metrics(model_all2, to_model_test)
calc_metrics(model_all2, to_model_train)

# Bootstrap for test set
out_roc_all2 <- bootstrap_auc(model_all2, to_model_test, no_resamples = 100)
bootstrap_summary(out_roc_all2)

# Model all3- extensive XGB search with upsampling on full dataset, fighting overfitting (Auc test ) ----

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,returnResamp = 'all',
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = F,
  verboseIter = TRUE,
  repeats = 1)

tune_grid <- expand.grid(nrounds=c(1:800),
                         max_depth = c(5),
                         eta = c(0.01),
                         gamma = c(0.01),
                         colsample_bytree = c(0.01),
                         subsample = c(0.10),
                         min_child_weight = c(0))

# tune_grid <- expand.grid(nrounds=c(400),
#                          max_depth = c(7),
#                          eta = c(0.2),
#                          gamma = c(0.01),
#                          colsample_bytree = c(0.75),
#                          subsample = c(0.50),
#                          min_child_weight = c(0,1))

# Train model
set.seed(825)
if (TRAIN_MODELS){
  # if (TRUE){
  model_all3 <- train(if_second_order ~., up_train, 
                      method="xgbTree",
                      metric="ROC",
                      trControl = fitControl,
                      verbose = 1,
                      tuneGrid = tune_grid,
  )
  save(model_all3, file = 'models_cache/model_all3.Rdata')
} else {
  load('models_cache/model_all3.Rdata')
}
model_all3$results %>% select(nrounds, ROC) %>%
  plot
plot(model_all3$finalModel)

# Create metrics - roc and confusion matrix
calc_metrics(model_all3, to_model_test)
calc_metrics(model_all3, to_model_train)

# Bootstrap for test set
out_roc_all2 <- bootstrap_auc(model_all2, to_model_test, no_resamples = 100)
bootstrap_summary(out_roc_all2)


# Save models for markdown viz ----

save(
  to_model_test, 
  model1,	out_roc1,
  model2,	out_roc2,
  model3,	out_roc3,
  model4,	out_roc4,
  model5,	out_roc5,
  
  to_model_test_geo,
  to_model_test_geo_pca,
  model_geo1,	out_roc_geo1,
  model_geo2,	out_roc_geo2,
  model_topic1, out_roc_topic1,
  file = 'models_cache/08_all_models.Rdata'
  
)

break

# Logistic regression  with caret and without upsampling----

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,allowParallel = T,
  
  ## repeated ten times
  repeats = 3)


set.seed(825)
model_glm <- train(if_second_order ~., to_model_train, 
                   # method = "gbm",
                   method="glm",
                   metric="ROC",
                   family=binomial(),
                   trControl = fitControl
                   # tuneGrid = gbmGrid
                   ## This last option is actually one
                   ## for gbm() that passes through
)
model_glm


predictions_glm = predict(model_glm, to_model_test,type = 'prob')


ROC_glm  <- pROC::roc(as.numeric(to_model_test$if_second_order == "yes"), 
                      predictions_glm[, 1])

plot(ROC_glm)
ROC_glm
# 0.54 

# Logistic regression  with caret and with upsampling----

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,allowParallel = T,
  
  ## repeated ten times
  repeats = 3)


set.seed(825)
model_glm2 <- train(if_second_order ~., up_train2, 
                    # method = "gbm",
                    method="glm",
                    metric="ROC",
                    family=binomial(),
                    trControl = fitControl
                    # tuneGrid = gbmGrid
                    ## This last option is actually one
                    ## for gbm() that passes through
)
model_glm2


predictions_glm2= predict(model_glm2, to_model_test,type = 'prob')


ROC_glm2  <- pROC::roc(as.numeric(to_model_test$if_second_order == "yes"), 
                       predictions_glm2[, 1])

plot(ROC_glm2)
ROC_glm2
# 0.54 


# XGB ----


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,allowParallel = T,
  verboseIter = TRUE,
  repeats = 2)

# gbmGrid_long <-  expand.grid(interaction.depth = c(1, 5, 10, 20),
#                         n.trees = (1:6)*50,
#                         shrinkage = c(0.01, 0.1, 0.5, 0.9),
#                         n.minobsinnode = c(1,5,10,20, 50, 100))

set.seed(825)
gbmFit4 <- train(if_second_order ~., up_train2, 
                 # method = "gbm",
                 method="xgbTree",
                 metric="ROC",
                 # family=binomial(), 
                 trControl = fitControl
                 # tuneGrid = gbmGrid_long
                 ## This last option is actually one
                 ## for gbm() that passes through
)
# stopCluster(cl)
gbmFit4


predictions4 = predict(gbmFit4, to_model_test,type = 'prob')


roc4  <- pROC::roc(as.numeric(to_model_test$if_second_order == "yes"), 
                   predictions4[, 1])

plot(roc4)
roc4
# 0.61 - not good, not bad

varImp(gbmFit4)
confusionMatrix(data = as.factor(as.numeric(predictions3[, 2] > 0.5)), # probability of yes was more than
                reference = as.factor(as.numeric(to_model_test$if_second_order == "yes")),
                # definitions of the "success" label
                positive = '1') 

predictions3[,2] %>%
  quantile(seq(0.8,1, 0.01))

# save.image()


# Geo Model ----
set.seed(825)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  repeats = 1)

gbmGrid_long <-  expand.grid(interaction.depth = c(3),
                             n.trees = (3)*50,
                             shrinkage = c(0.1),
                             n.minobsinnode = c(20))


xgb_geo1 <- train(if_second_order ~., to_model_train, 
                  method = "gbm",
                  # method="xgbTree",
                  metric="ROC",
                  # family=binomial(),
                  trControl = fitControl,
                  tuneGrid = gbmGrid_long
)
# stopCluster(cl)
xgb_geo1


predictions4 = predict(xgb_geo1, to_model_test,type = 'prob')


roc4  <- pROC::roc(as.numeric(to_model_test$if_second_order == "yes"), 
                   predictions4[, 1])

plot(roc4)
roc4


# run PCA and scale

prcomp(to_model_geo %>% select(-if_second_order),scale. = T) -> pca_model
pca_model %>% summary() # first 8 components


prcomp(to_model_geo %>% select(-if_second_order),scale. = T,rank. = 8) -> pca_model2
pca_model2$x %>%  cbind(to_model_geo%>% select(if_second_order)) %>% as_tibble() -> to_model_pca

to_model_pca$if_second_order %>% table()
set.seed(10)
training_obs <- createDataPartition(to_model_pca$if_second_order, 
                                    p = 0.7, 
                                    list = FALSE)
to_model_train <- to_model_pca[training_obs[,1],]
to_model_test  <- to_model_pca[-training_obs[,1],]

up_train4 <- upSample(x = to_model_train %>% select(-if_second_order),
                      y = as.factor(to_model_train$if_second_order)) %>%
  as_tibble() %>%
  mutate(if_second_order = as.character(Class)) %>%
  select(-Class)




# Geo Model ----
set.seed(825)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  repeats = 1)

gbmGrid_long <-  expand.grid(interaction.depth = c(3),
                             n.trees = (3)*50,
                             shrinkage = c(0.1),
                             n.minobsinnode = c(20))


xgb_geo2 <- train(if_second_order ~., up_train4, 
                  method = "gbm",
                  # method="xgbTree",
                  metric="ROC",
                  # family=binomial(),
                  trControl = fitControl,
                  tuneGrid = gbmGrid_long
)
xgb_geo2


predictions5 = predict(xgb_geo2, to_model_test,type = 'prob')


roc5  <- pROC::roc(as.numeric(to_model_test$if_second_order == "yes"), 
                   predictions5[, 1])

plot(roc5)
roc5
