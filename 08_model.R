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


predictions3 = predict(gbmFit3, to_model_test,type = 'prob')


ROC.train.tree5  <- pROC::roc(as.numeric(to_model_test$if_second_order == "yes"), 
                              predictions3[, 1])

plot(ROC.train.tree5)
ROC.train.tree5
# 0.6 - not good, not bad


confusionMatrix(data = as.factor(as.numeric(predictions3[, 2] > 0.5)), # probability of yes was more than
                reference = as.factor(as.numeric(to_model_test$if_second_order == "yes")),
                # definitions of the "success" label
                positive = '1') 

predictions3[,2] %>%
  quantile(seq(0.8,1, 0.01))


# XGB


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

save.image()
