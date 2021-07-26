
permute_variables <- function(df, vars_to_permute){
  out <- df
  if (is.null(vars_to_permute)) out
  for (x in vars_to_permute){
    out[,x] <- sample(out[[x]])
  }
  out
}

permute_auc <- function(model, test_set, no_resamples, vars_to_permute){
  out_roc <- vector('numeric', no_resamples)
  
  len_test <- nrow(test_set)
  for (i in 1:no_resamples){
    temp_test <- permute_variables(test_set, vars_to_permute)
    
    predictions_temp = predict(model, temp_test,type = 'prob')
    suppressMessages({
      roc_temp  <- pROC::roc(as.numeric(temp_test$if_second_order == "yes"), 
                             predictions_temp[, 1])
    })
    out_roc[i] <- roc_temp$auc
    i <- i+1
    
  }
  mean(out_roc)
}



# list(
#   'a' = c('payment_value'),
#   'b' = c('geolocation_lng')
# ) -> b

calc_varimp_groups <- function(model, df, vars_list, scale = T, no_resamples = 100){
  auc_full <- permute_auc(model, df,no_resamples = 10, vars_to_permute = NULL)
  auc_none <- 0.5
  out <- tibble(
    feature_group = c('full', 'none'),
    score = c(auc_full, auc_none)
  )
  
  for (feature_group in names(vars_list)){
    message(paste0('Feature ', feature_group))
    auc_tmp <- permute_auc(model, df,no_resamples = no_resamples, vars_to_permute = vars_list[[feature_group]])
    
    temp_out <- tibble(
      feature_group = feature_group,
      score = auc_tmp
    )
    out <- rbind(out, temp_out)
  }
  
  if(scale){
    out %>%
      mutate(score = range01(1-score)) %>%
      filter(!(feature_group %in% c('none', 'full')))
    
  } else{
    out %>%
      mutate(score = 1-score) %>%
      filter(!(feature_group %in% c('none', 'full')))
    
  }
  
}


# g <- calc_varimp_groups(models_list$basic_info$model, to_model_test, b)
# g %>%
#   mutate(score = range01(score)) %>%
#   filter(!(feature_group %in% c('none', 'full')))
