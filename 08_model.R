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


# Modeling ----

# First model (glm) - variables are significant-----
a <- glm(if_second_order ~., to_model, family = 'binomial')
a %>% summary()


# Tree model - only one node produced due to imbalanced class ----
library(rpart)
model_tree1 <- 
  rpart(if_second_order ~., to_model,
        method = "class")

pred_model_tree1 <- predict(model_tree1, to_model, type = 'class')


confusionMatrix(data = pred_model_tree1, # predictions
                # actual values
                reference = as.factor(to_model$if_second_order),
                # definitions of the "success" label
                positive = '1') 
# Only one node due to imbalanced class

summary(datausa.tree4)

# Prepare data for caret ----
to_model %>%
  mutate(no_items = ifelse(is.na(no_items), 1, no_items),
         if_second_order = ifelse(as.character(if_second_order) == '1', 'yes', 'no')
  ) %>%
  drop_na() -> to_model2
# GBM model from caret without  upsampling  ----
# Great performance, but on overfitted train set


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  ## repeated ten times
  repeats = 3)


set.seed(825)
gbmFit1 <- train(if_second_order ~., to_model2, 
                 method = "gbm",
                 # method="glm", 
                 metric="ROC",
                 # family=binomial(), 
                 trControl = fitControl
                 ## This last option is actually one
                 ## for gbm() that passes through
                 )
gbmFit1
# Best model has interaction depth = 3, ntrees = 150 - probably even more would be good

to_model2

# Upsampling ----
up_train <- upSample(x = to_model2 %>% select(-if_second_order),
                     y = as.factor(to_model2$if_second_order)) %>%
  as_tibble() %>%
  mutate(if_second_order = as.character(Class)) %>%
  select(-Class)


to_model2$if_second_order %>% table()
up_train$if_second_order %>% table()


library(doParallel)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,allowParallel = T,
  
  ## repeated ten times
  repeats = 3)

gbmGrid <-  expand.grid(interaction.depth = c(3,5,7), 
                        n.trees = (1:6)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

set.seed(825)
gbmFit2 <- train(if_second_order ~., up_train, 
                 method = "gbm",
                 # method="glm", 
                 metric="ROC",
                 # family=binomial(), 
                 trControl = fitControl,
                 tuneGrid = gbmGrid
                 ## This last option is actually one
                 ## for gbm() that passes through
)
stopCluster(cl)
gbmFit2

predictions = predict(gbmFit2, to_model2,type = 'prob')


ROC.train.tree5  <- pROC::roc(as.numeric(to_model2$if_second_order == "yes"), 
                        predictions[, 1])

plot(ROC.train.tree5)
# looks good, but probable overfitting - running on train set


# !!!! Model with proper validation -----
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


cl <- makePSOCKcluster(5)
registerDoParallel(cl)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,allowParallel = T,
  
  ## repeated ten times
  repeats = 3)

# gbmGrid <-  expand.grid(interaction.depth = c(1:5)*2-1,
#                         n.trees = (1:6)*50,
#                         shrinkage = 0.1,
#                         n.minobsinnode = 20)

set.seed(825)
gbmFit3 <- train(if_second_order ~., up_train2, 
                 method = "gbm",
                 # method="glm", 
                 metric="ROC",
                 # family=binomial(), 
                 trControl = fitControl
                 # tuneGrid = gbmGrid
                 ## This last option is actually one
                 ## for gbm() that passes through
)
stopCluster(cl)
gbmFit3


predictions3 = predict(gbmFit3, to_model_test[1000:2000,],type = 'prob')
ROC.train.tree5  <- pROC::roc(as.numeric(to_model_test$if_second_order == "yes")[1000:2000], 
                              predictions3[, 1])

plot(ROC.train.tree5)
ROC.train.tree5
# 0.63 - not good, not bad


confusionMatrix(data = as.factor(as.numeric(predictions3[, 2] > 0.5)), # probability of yes was more than
                reference = as.factor(as.numeric(to_model_test$if_second_order == "yes")),
                # definitions of the "success" label
                positive = '1') 

predictions3[,2] %>%
  quantile(seq(0.8,1, 0.01))

# Bootstrap for test set
no_resamples = 100
out_roc <- vector('numeric', no_resamples)


len_test <- nrow(to_model_test)
for (i in 1:no_resamples){
  idxes <- sample(1:len_test, size = len_test, replace = T)
  temp_test <- to_model_test[idxes,]
  
  predictions_temp = predict(gbmFit3, temp_test,type = 'prob')
  roc_temp  <- pROC::roc(as.numeric(temp_test$if_second_order == "yes"), 
                                predictions_temp[, 1])
  out_roc[i] <- roc_temp$auc
  i <- i+1
  
}
out_roc

tibble(auc =out_roc, y=0) %>%
  ggplot(aes(x=auc)) +
  geom_density() +
  geom_jitter(aes(y=y), alpha=0.5)
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

# Data preparation with geographic info ----
load('data/preprocessed/spatial_all_by_zip.Rdata')
spatial_all_by_zip

first_orders %>%
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
  drop_na() -> to_model_geo


training_obs <- createDataPartition(to_model_geo$if_second_order, 
                                    p = 0.7, 
                                    list = FALSE)
to_model_train <- to_model_geo[training_obs[,1],]
to_model_test  <- to_model_geo[-training_obs[,1],]

up_train3 <- upSample(x = to_model_train %>% select(-if_second_order),
                      y = as.factor(to_model_train$if_second_order)) %>%
  as_tibble() %>%
  mutate(if_second_order = as.character(Class)) %>%
  select(-Class)

prcomp(up_train3 %>% select(-if_second_order),scale. = T) -> pca_model
pca_model %>% summary()


# Model ----
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




# Model ----
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
# stopCluster(cl)
xgb_geo2


predictions5 = predict(xgb_geo2, to_model_test,type = 'prob')


roc5  <- pROC::roc(as.numeric(to_model_test$if_second_order == "yes"), 
                   predictions5[, 1])

plot(roc5)
roc5



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


to_model %>%
  mutate(no_items = ifelse(is.na(no_items), 1, no_items),
         if_second_order = ifelse(as.character(if_second_order) == '1', 'yes', 'no')
  ) %>%
  drop_na() -> to_model_cohort2

to_model_cohort2


# How to do modeling?
# 2. Run each month, predict next month
# 3. Run each month, predict all data

to_model_cohort2

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

# Create roc for next month
roc_list2 <- vector('list', length(cohorts_ordered)-1)
names(roc_list2) <- cohorts_ordered[-21]

for (i in 1:(length(cohorts_ordered)-1)){
  print('a')
  print(cohorts_ordered[i])
  print(cohorts_ordered[i+1])
  current_coh <- as.character(cohorts_ordered[i])
  next_coh <- as.character(cohorts_ordered[i+1])
  
  next_cohort_test <- to_model_cohort2 %>%
    filter(cohort >= cohorts_ordered[i+1]) %>%
    select(-cohort)
  
  predictions = predict(lr_cohorts_list2[current_coh][1], next_cohort_test,type = 'prob')[[1]]
  
  
  roc5  <- pROC::roc(as.numeric(next_cohort_test$if_second_order == "yes"), 
                     predictions[, 1])
  
  roc_list2[current_coh] <- list(roc5)
  
}

for (roc in roc_list1){
  print(roc)
}
# Very variable outputs - from 0.5 to 0.67 - not consistent




