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
  # left_join(order_items3) %>% 
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

# Train test split, create dataset with upsampling -----
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


# Functions for bootstraping AUC on test set -----
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

# Model 1 - GBM on standard params with upsampling (AUC test 0.5921)----

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

# Model 2 - GBM on standard params without upsampling (Auc test 0.5899) ----

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

# Model 3 - Logistic with upsampling (Auc test 0.5575) ----

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

# Model 4 - Logistic without upsampling (Auc test 0.5563) ----

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



# Model 5 - extensive XGB search with upsampling (Auc test 0.6159) ----

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
  left_join(spatial_all_by_zip, by = c('customer_zip_code_prefix' = 
                                         'geolocation_zip_code_prefix')) %>%
  left_join(order_items2) %>% 
  # left_join(order_items3) %>%
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
  load('models_cache/model5.Rdata')
}
model_geo1

# Create metrics - roc and confusion matrix
calc_metrics(model_geo1, to_model_test_geo)


# Bootstrap for test set
out_roc_geo1 <- bootstrap_auc(model_geo1, to_model_test_geo, no_resamples = 100)
bootstrap_summary(out_roc_geo1)

# Data preparation - PCA on geo dataset ----
library(factoextra)

pca_model <- prcomp(up_train_geo %>%
                      select(-if_second_order), scale = TRUE,center = TRUE)
summary(pca_model)
fviz_eig(pca_model)
# Setting no of components to 10 - 94% of variability

pca_model_10 <- prcomp(up_train_geo %>%
                         select(-if_second_order), scale = TRUE,center = TRUE,rank. = 10)
summary(pca_model_10)

predict(pca_model_10, up_train_geo) %>%
  as_tibble() %>%
  mutate(if_second_order = up_train_geo$if_second_order) -> up_train_geo_pca


predict(pca_model_10, to_model_test_geo) %>%
  as_tibble() %>%
  mutate(if_second_order = to_model_test_geo$if_second_order) -> to_model_test_geo_pca

# Model geo 2 - XGB search with upsampling on geo data with PCA on 10 components (Auc test 0.5289) ----

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
} else {
  load('models_cache/model_geo2.Rdata')
}
model_geo2

# Create metrics - roc and confusion matrix
calc_metrics(model_geo2, to_model_test_geo_pca)


# Bootstrap for test set
out_roc_geo2 <- bootstrap_auc(model_geo2, to_model_test_geo_pca, no_resamples = 100)
bootstrap_summary(out_roc_geo2)


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
