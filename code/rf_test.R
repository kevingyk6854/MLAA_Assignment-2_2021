library(here)
library(tidyverse)
library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(glmnet)
library(plotly)
library(caret)
library(randomForest)
library(pROC)

source(here("code", "data_trans.R"))

# Extract & Transform data
credict_train_cleaned <- transforming_data(read_csv(here::here("data/AT2_credit_train.csv")), 
                                           is_train = TRUE)

credict_train_cleaned <- credict_train_cleaned %>% relocate('default')

formula_rf <- as.formula(paste("default~", ".", sep = ""))

# Random Forest Model Generation
rf_model_generation <- function (df, formula_rf, target_col, n_trees=100, only_pred=FALSE){
    
  # First step is to set a random seed to ensurre we get the same result each time
  set.seed(42) 
  
  # Split training & test sets
  # trainset_size <- floor(0.80 * nrow(df))
  # trainset_indices <- sample(seq_len(nrow(df)), size = trainset_size)
  indices <- createDataPartition(df$default, p = .7, list = FALSE) 
  trainset <- df[indices, ]
  testset <- df[-indices, ]
  
  # Random Forest Model
  rf_model <- randomForest( formula_rf,
                             data = trainset, 
                             importance=TRUE, 
                             ntree=n_trees,
                             norm.votes = TRUE) 
  
  default_pred <- predict(rf_model, testset) # prediction
  # default_pred
  
  default_prob <- predict(object=rf_model, newdata=testset, type="prob")  # prob
  # default_prob[, 2] # prob for 1
  
  # names(rf_model)
  if (!only_pred) {
    taget_col_values <- testset[[target_col]]
    
    acc <- mean(default_pred == taget_col_values) # accuracy
    
    conf_tab <- table(default_pred, taget_col_values) # confusion table
    
    var_import <- importance(rf_model) # variable importance
    
    # partial dependency plots
    # varImpPlot(rf_model)
    
    #apply roc function
    rf_analysis <- roc(response=taget_col_values, default_prob[, 2])
    #calculate auc
    auc_val <- auc(rf_analysis)
    
    return (list(rf_model=rf_model, pred=default_pred, prob1=default_prob[, 2], 
                 confusion_table=conf_tab, accuracy=acc, 
                 variable_importance=var_import, rf_analysis=rf_analysis, auc_value=auc_val))
  }

  
  return (list(rf_model=rf_model, prob1=default_prob[, 2]))
}

##############################################################################################################

rf_mod <- rf_model_generation(credict_train_cleaned, formula_rf, target_col="default", n_trees=300, only_pred=FALSE)
rf_mod$auc_value # 0.7903

rf_mod2 <- rf_model_generation(credict_train_cleaned, formula_rf, target_col="default", n_trees=500, only_pred=FALSE)
rf_mod2$auc_value # 0.7905

##############################################################################################################
# Use RFE to choose most important variables
test_set = credict_train_cleaned

# RFE
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(test_set %>% select(-default), 
               test_set$default, 
               sizes=c(1:10), 
               rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

##############################################################################################################

# "AGE"       "PAY_1"     "EDUCATION" "LIMIT_BAL" "AGE_BIN"   "SQRT_PAYX" "BILL_AMT1"
features_by_rfe <- predictors(results)
formula_rf2 <- as.formula(paste("default~", paste(features_by_rfe, collapse="+")))
  
rf_mod3 <- rf_model_generation(credict_train_cleaned, formula_rf2, target_col="default", n_trees=300, only_pred=FALSE)
rf_mod3$auc_value # 0.7811
rf_mod3$rf_model$confusion
#       0    1 class.error
# 0 11004  866  0.07295703
# 1  2232 2036  0.52296157

# plot AUC-ROC curve
plot(rf_mod3$rf_analysis, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

########################################################

# credict_test_cleaned <- transforming_data(read_csv(here::here("data/AT2_credit_test.csv")),
#                              is_train = FALSE,
#                              cat_var_to_fact = TRUE) # Noted: gbm does not accept factor variables
# 
# levels(credict_test_cleaned$SQRT_PAYX) <- levels(credict_train_cleaned$SQRT_PAYX)
# levels(credict_test_cleaned$PAY_3) <- levels(credict_train_cleaned$PAY_3)
# levels(credict_test_cleaned$PAY_4) <- levels(credict_train_cleaned$PAY_4)
# credict_test_cleaned$default <- 0
# 
# features_by_rfe <- predictors(results)
# formula_rf <- as.formula(paste("default~", paste(features_by_rfe, collapse="+")))
# 
# probs <- predict(rf_mod3$rf_model, 
#                  newdata = credict_train_cleaned,
#                  type="prob")
# 
# df_t <- read_csv(here::here("data/AT2_credit_test.csv"))
# 
# output_df <- data.frame(
#   ID = df_t %>% select(ID),
#   default = probs
# )
# 
# write_csv(output_df, 'data/rf ver 1.3.1.csv')
