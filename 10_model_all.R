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

load('data/preprocessed/spatial_all_by_zip.Rdata')
names(spatial_all_by_zip) <- paste0('demographic_', names(spatial_all_by_zip))


# first 15 - 80% of all categories
order_items3 %>% group_by(product_category_name) %>% 
  tally(sort=T) %>%
  mutate(a=cumsum(n)/sum(n)) %>% 
  mutate(prod_cat_ = ifelse(row_number()>15, 'other', product_category_name)) %>%
  select(product_category_name, prod_cat_) -> product_category_mapping


order_items3 %>%
  ungroup() %>%
  left_join(product_category_mapping) %>%
  left_join(product_translation, by = c('prod_cat_'='product_category_name')) %>%
  mutate(prod_cat_ = coalesce(product_category_name_english, prod_cat_)) -> order_items4

order_items_for_stats_table <- order_items4
save(order_items_for_stats_table, file = 'run_all_models_cache/order_items_for_stats_table.Rdata')

dummy <- dummyVars(" ~ prod_cat_", data=order_items4)
order_items4 %>% 
  cbind(tibble(data.frame(predict(dummy, newdata = order_items4)))) %>%
  tibble() %>% 
  select(-product_category_name, -prod_cat_, -prod_cat_other) %>%
  group_by(order_id) %>%
  summarise_if(is.numeric, max) -> order_items5


# geo
library(factoextra)

pca_model <- prcomp(spatial_all_by_zip %>% 
                      mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>%
                      select(-demographic_geolocation_zip_code_prefix), scale = TRUE,center = TRUE)

summary(pca_model)
summary(pca_model) -> pca_importances

pca_importances$importance %>%
  as_tibble(rownames = 'a') %>%
  filter(row_number()==3) %>%
  pivot_longer(2:36) %>%
  select(name, value) %>%
  mutate(PC = row_number()) -> pca_to_plot

save(pca_to_plot, file='run_all_models_cache/pca_to_plot.Rdata')
  
ggplot(pca_to_plot, aes(x=PC, y=value)) +
  geom_line() +
  ggrepel::geom_text_repel(data = tibble(value = 0.973, PC=10), aes(label=value))+
  geom_point(data = tibble(value = 0.973, PC=10), color='red')
  

tibble(var = pca_model$sdev)  %>%
  mutate(no_components=row_number(),
         cum_variance = cumsum(var)/sum(var)
         )

fviz_eig(pca_model)
# Setting no of components to 8 - 90% of variability
pca_model_8 <- prcomp(spatial_all_by_zip %>% 
                        mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>%
                        select(-demographic_geolocation_zip_code_prefix), scale = TRUE,center = TRUE,rank. = 10)
summary(pca_model_8)

predict(pca_model_8, spatial_all_by_zip %>% 
          mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>%
          select(-demographic_geolocation_zip_code_prefix)) %>%
  as_tibble() -> spatial_pca_0

names(spatial_pca_0) <- paste0('spatial_', names(spatial_pca_0))

spatial_pca_0 %>%
  cbind(spatial_all_by_zip %>% select(demographic_geolocation_zip_code_prefix)) %>%
  as_tibble() -> spatial_pca_1

spatial_pca_1



first_orders %>%
  left_join(geolocation2, by = c('customer_zip_code_prefix' = 
                                   'geolocation_zip_code_prefix')) %>%
  left_join(spatial_all_by_zip, by = c('customer_zip_code_prefix' = 
                                         'demographic_geolocation_zip_code_prefix')) %>%
  left_join(spatial_pca_1, by = c('customer_zip_code_prefix' = 
                                    'demographic_geolocation_zip_code_prefix')) %>%
  
  left_join(order_items2) %>% 
  left_join(order_items5) %>%
  left_join(order_reviews_topics %>%
              select(order_id, starts_with('topic_'))%>%group_by(order_id)%>%top_n(1)) %>%
  mutate(if_second_order = as.numeric(if_second_order)) %>%
  
  select_if(is.numeric) %>%
  mutate(no_items = ifelse(is.na(no_items), 1, no_items),
         if_second_order = ifelse(as.character(if_second_order) == '1', 'yes', 'no')
  ) %>% 
  # Replace NA with column mean, no. NAs around 300 per 100 000 obs
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) -> to_model_all_0

dbscan_geo <- dbscan::dbscan(to_model_all_0%>%select(geolocation_lat,geolocation_lng), 0.2, 100)
plot(dbscan_geo)

RUN_DBSCAN_TESTING = TRUE

if (RUN_DBSCAN_TESTING){
  
}
dbscan::kNNdistplot(to_model_all_0%>%select(geolocation_lat,geolocation_lng), k =100)
abline(h=2.3)


dbscan_geo_test <- dbscan::dbscan(to_model_all_0%>%select(geolocation_lat,geolocation_lng) %>% sample_frac(0.5), 2, 100)
dbscan_geo_test
plot(dbscan_geo)

dbscan::hullplot(to_model_all_0%>%select(geolocation_lat,geolocation_lng), dbscan_geo, solid=TRUE, alpha=0.7)



to_model_all_0 %>%
  mutate(cluster = dbscan_geo$cluster) %>%
  mutate(agglomeration = ifelse(cluster==0, 0,1)) %>%
  select(-cluster) -> to_model_all_1

to_model_all_1 %>% head()
to_model_all_1 %>% ncol
to_model_all_1 %>% summary()


# Prepare data for caret ----
to_model_all_1 %>%
  drop_na() -> to_model_all

save(to_model_all, file='08_to_model_all.Rdata')


vars_all <- c("payment_value", "review_score", "geolocation_lat", 
              "geolocation_lng", "demographic_total_pop", "demographic_age_0_4", 
              "demographic_age_5_9", "demographic_age_10_14", "demographic_age_15_19", 
              "demographic_age_20_24", "demographic_age_25_29", "demographic_age_30_34", 
              "demographic_age_35_39", "demographic_age_40_44", "demographic_age_45_49", 
              "demographic_age_50_54", "demographic_age_55_59", "demographic_age_60_64", 
              "demographic_age_65_69", "demographic_age_70_74", "demographic_age_75_79", 
              "demographic_age_80_84", "demographic_age_85_89", "demographic_age_90_94", 
              "demographic_age_95_99", "demographic_perc_rural", "demographic_perc_urban", 
              "demographic_inc0", "demographic_inc0.25", "demographic_inc0.5", 
              "demographic_inc1", "demographic_inc2", "demographic_inc3", "demographic_inc5", 
              "demographic_inc10", "demographic_inc15", "demographic_inc20", 
              "demographic_inc30", "demographic_no_immigrants", "spatial_PC1", 
              "spatial_PC2", "spatial_PC3", "spatial_PC4", "spatial_PC5", "spatial_PC6", 
              "spatial_PC7", "spatial_PC8", "spatial_PC9", "spatial_PC10", 
              "no_items", "sum_freight", "prod_cat_auto", "prod_cat_baby", 
              "prod_cat_bed_bath_table", "prod_cat_computers_accessories", 
              "prod_cat_cool_stuff", "prod_cat_electronics", "prod_cat_furniture_decor", 
              "prod_cat_garden_tools", "prod_cat_health_beauty", "prod_cat_housewares", 
              "prod_cat_perfumery", "prod_cat_sports_leisure", "prod_cat_telephony", 
              "prod_cat_toys", "prod_cat_watches_gifts", "topic_0.0", "topic_1.0", 
              "topic_10.0", "topic_11.0", "topic_12.0", "topic_13.0", "topic_2.0", 
              "topic_3.0", "topic_4.0", "topic_5.0", "topic_6.0", "topic_7.0", 
              "topic_8.0", "topic_9.0", "topic_nan", "agglomeration")

vars_y <- 'if_second_order'

vars_basic <- c("payment_value", "review_score", "geolocation_lat", 
                "geolocation_lng",
                "no_items", "sum_freight")

vars_topics <- c("topic_0.0", "topic_1.0", 
                 "topic_10.0", "topic_11.0", "topic_12.0", "topic_13.0", "topic_2.0", 
                 "topic_3.0", "topic_4.0", "topic_5.0", "topic_6.0", "topic_7.0", 
                 "topic_8.0", "topic_9.0", "topic_nan")


vars_demographic <- c("demographic_total_pop", "demographic_age_0_4", 
                      "demographic_age_5_9", "demographic_age_10_14", "demographic_age_15_19", 
                      "demographic_age_20_24", "demographic_age_25_29", "demographic_age_30_34", 
                      "demographic_age_35_39", "demographic_age_40_44", "demographic_age_45_49", 
                      "demographic_age_50_54", "demographic_age_55_59", "demographic_age_60_64", 
                      "demographic_age_65_69", "demographic_age_70_74", "demographic_age_75_79", 
                      "demographic_age_80_84", "demographic_age_85_89", "demographic_age_90_94", 
                      "demographic_age_95_99", "demographic_perc_rural", "demographic_perc_urban", 
                      "demographic_inc0", "demographic_inc0.25", "demographic_inc0.5", 
                      "demographic_inc1", "demographic_inc2", "demographic_inc3", "demographic_inc5", 
                      "demographic_inc10", "demographic_inc15", "demographic_inc20", 
                      "demographic_inc30", "demographic_no_immigrants")

vars_demographic_pca <- c("spatial_PC1", 
                          "spatial_PC2", "spatial_PC3", "spatial_PC4", "spatial_PC5", "spatial_PC6", 
                          "spatial_PC7", "spatial_PC8", "spatial_PC9", "spatial_PC10"
)

vars_prod_categories <- c("prod_cat_auto", "prod_cat_baby", 
                          "prod_cat_bed_bath_table", "prod_cat_computers_accessories", 
                          "prod_cat_cool_stuff", "prod_cat_electronics", "prod_cat_furniture_decor", 
                          "prod_cat_garden_tools", "prod_cat_health_beauty", "prod_cat_housewares", 
                          "prod_cat_perfumery", "prod_cat_sports_leisure", "prod_cat_telephony", 
                          "prod_cat_toys", "prod_cat_watches_gifts")
vars_agglomeration <- c("agglomeration")

model_struct_stub <- list(
  model_name = NULL,
  vars_used = NULL,
  if_upsampling = NULL,
  model = NULL
)


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

save(to_model_train, file = 'run_all_models_cache/to_model_train.Rdata')
save(to_model_test, file = 'run_all_models_cache/to_model_test.Rdata')
save(up_train, file = 'run_all_models_cache/up_train.Rdata')
# break
load('run_all_models_cache/to_model_train.Rdata')
load('run_all_models_cache/to_model_test.Rdata')
load('run_all_models_cache/up_train.Rdata')

create_formula <- function(cols_names){
  paste0('if_second_order ~ ', paste(cols_names, collapse = ' + '))
}

# XGB creation ----
# 1. initialize list
model_definitions <- list()

# 2. create specifications

# Basic info
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'basic_info'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- vars_basic
model_definitions[[1]] <- model_def_temp

# basic info and topics
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'topics'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_topics)
model_definitions[[2]] <- model_def_temp

# basic info and demographic info
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'demographics'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_demographic)
model_definitions[[3]] <- model_def_temp

# basic info and demographic info, but transformed with PCA
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'demographics_pca'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_demographic_pca)
model_definitions[[4]] <- model_def_temp

# basic info and product categories
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'product_categories'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_prod_categories)
model_definitions[[5]] <- model_def_temp

# basic info and if agglomeration
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'agglomeration'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_agglomeration)
model_definitions[[6]] <- model_def_temp


# all variables - included demographic with PCA transformation
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'all_with_pca'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_topics, vars_demographic_pca, vars_prod_categories, vars_agglomeration)
model_definitions[[7]] <- model_def_temp

models_names <- c()
for (i in model_definitions) {
  models_names <- c(models_names, i$model_name)
}
names(model_definitions) <- models_names

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  verboseIter = TRUE,
  repeats = 1)

tune_grid <- expand.grid(nrounds=c(10,100,200,400),
                         max_depth = c(3:7),
                         eta = c(0.01, 0.1,0.2),
                         gamma = c(0.01),
                         colsample_bytree = c(0.75),
                         subsample = c(0.50),
                         min_child_weight = c(0,1))

for (model_name in models_names){
  # if (model_name %in% c('basic_info', 'topics')) next
  model_spec <- model_definitions[[model_name]]
  time_start <- Sys.time()
  print(paste0(time_start, " Starting running model ", model_name))
  
  if (model_spec$if_upsampling) {
    temp_train <- up_train
  } else {
    temp_train <- to_model_train
  }
  
  model_temp <- train(as.formula(create_formula(model_spec$vars_used)), temp_train,
                      method="xgbTree",
                      metric="ROC",
                      trControl = fitControl,
                      verbose = T,
                      tuneGrid = tune_grid
  )
  
  print(paste0(Sys.time(), " Saving model to file"))
  
  model_spec$model <- model_temp
  save(model_spec, file=paste0('run_all_models_cache/saved_model_', model_name, '.Rdata'))
  
  print('Model training finished. Time:')
  print(Sys.time()-time_start)
  
  print('---------------------------------')
}

# Logistic regression ----
# 1. initialize list
model_definitions <- list()

# 2. create specifications

# Basic info
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'lr_basic_info'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- vars_basic
model_definitions[[1]] <- model_def_temp

# basic info and topics
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'lr_topics'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_topics)
model_definitions[[2]] <- model_def_temp

# basic info and demographic info
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'lr_demographics'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_demographic)
model_definitions[[3]] <- model_def_temp

# basic info and demographic info, but transformed with PCA
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'lr_demographics_pca'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_demographic_pca)
model_definitions[[4]] <- model_def_temp

# basic info and product categories
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'lr_product_categories'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_prod_categories)
model_definitions[[5]] <- model_def_temp

# basic info and if agglomeration
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'lr_agglomeration'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_agglomeration)
model_definitions[[6]] <- model_def_temp


# all variables - included demographic with PCA transformation
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'lr_all_with_pca'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_topics, vars_demographic_pca, vars_prod_categories, vars_agglomeration)
model_definitions[[7]] <- model_def_temp

models_names <- c()
for (i in model_definitions) {
  models_names <- c(models_names, i$model_name)
}
names(model_definitions) <- models_names

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  verboseIter = TRUE,
  repeats = 1)

for (model_name in models_names){
  # if (model_name %in% 'basic_info') next
  model_spec <- model_definitions[[model_name]]
  time_start <- Sys.time()
  print(paste0(time_start, " Starting running model ", model_name))
  
  if (model_spec$if_upsampling) {
    temp_train <- up_train
  } else {
    temp_train <- to_model_train
  }
  
  model_temp <- train(as.formula(create_formula(model_spec$vars_used)), temp_train,
                      method="glm",
                      family='binomial',
                      metric="ROC",
                      trControl = fitControl
  )
  
  print(paste0(Sys.time(), " Saving model to file"))
  
  model_spec$model <- model_temp
  save(model_spec, file=paste0('run_all_models_cache/saved_model_lr_', model_name, '.Rdata'))
  
  print('Model training finished. Time:')
  print(Sys.time()-time_start)
  
  print('---------------------------------')
}
break

# Boruta ----
# 1. initialize list
model_definitions <- list()

# 2. create specifications

# all variables - included demographic with PCA transformation
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'boruta_all_with_pca'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_topics, vars_demographic_pca, vars_prod_categories, vars_agglomeration)
model_definitions[[1]] <- model_def_temp

models_names <- c()
for (i in model_definitions) {
  models_names <- c(models_names, i$model_name)
}
names(model_definitions) <- models_names

library(Boruta)


# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  verboseIter = TRUE,
  repeats = 1)

# for (model_name in models_names){
#   break
#   # if (model_name %in% 'basic_info') next
#   model_spec <- model_definitions[[model_name]]
#   time_start <- Sys.time()
#   print(paste0(time_start, " Starting running model ", model_name))
#   
#   if (model_spec$if_upsampling) {
#     temp_train <- up_train
#   } else {
#     temp_train <- to_model_train
#   }
#   
#   boruta.train <- Boruta(as.formula(create_formula(model_spec$vars_used)), data = temp_train %>% mutate(if_second_order = ifelse(if_second_order=='yes', 1, 0)), doTrace = 2,)
#   
#   print(paste0(Sys.time(), " Saving model to file"))
#   
#   model_spec$model <- model_temp
#   save(model_spec, file=paste0('run_all_models_cache/saved_model_', 'boruta_all_with_pca', '.Rdata'))
#   
#   boruta.train$finalDecision %>% as_tibble(rownames = 'var') %>% View()
#   print('Model training finished. Time:')
#   print(Sys.time()-time_start)
#   
#   print('---------------------------------')
# }


# Boruta - XGB
# All variables besides topic of the review are important

# Boruta XGB creation ----
# 1. initialize list
model_definitions <- list()

# 2. create specifications

# all variables - included demographic with PCA transformation
model_def_temp <- model_struct_stub
model_def_temp$model_name <- 'boruta_no_topics'
model_def_temp$if_upsampling <- T
model_def_temp$vars_used <- c(vars_basic, vars_demographic_pca, vars_prod_categories, vars_agglomeration)
model_definitions[[1]] <- model_def_temp

models_names <- c()
for (i in model_definitions) {
  models_names <- c(models_names, i$model_name)
}
names(model_definitions) <- models_names

# Define train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = T,
  verboseIter = TRUE,
  repeats = 1)

tune_grid <- expand.grid(nrounds=c(10,100,200,400),
                         max_depth = c(3:7),
                         eta = c(0.01, 0.1,0.2),
                         gamma = c(0.01),
                         colsample_bytree = c(0.75),
                         subsample = c(0.50),
                         min_child_weight = c(0,1))

for (model_name in models_names){
  model_spec <- model_definitions[[model_name]]
  time_start <- Sys.time()
  print(paste0(time_start, " Starting running model ", model_name))
  
  if (model_spec$if_upsampling) {
    temp_train <- up_train
  } else {
    temp_train <- to_model_train
  }
  
  model_temp <- train(as.formula(create_formula(model_spec$vars_used)), temp_train,
                      method="xgbTree",
                      metric="ROC",
                      trControl = fitControl,
                      verbose = T,
                      tuneGrid = tune_grid
  )
  
  print(paste0(Sys.time(), " Saving model to file"))
  
  model_spec$model <- model_temp
  save(model_spec, file=paste0('run_all_models_cache/saved_model_', model_name, '.Rdata'))
  
  print('Model training finished. Time:')
  print(Sys.time()-time_start)
  
  print('---------------------------------')
}

break
model_temp <- train(as.formula(create_formula(vars_basic)), up_train, 
                    method="xgbTree",
                    metric="ROC",
                    trControl = fitControl,
                    verbose = T,
                    tuneGrid = tune_grid,
)

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
