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
rf_model_generation <- function (df, formula_rf, target_col, only_pred=FALSE){
    
  # First step is to set a random seed to ensurre we get the same result each time
  set.seed(42) 
  
  # Split training & test sets
  trainset_size <- floor(0.80 * nrow(df))
  trainset_indices <- sample(seq_len(nrow(df)), size = trainset_size)
  trainset <- df[trainset_indices, ]
  testset <- df[-trainset_indices, ]
  
  # Random Forest Model
  rf_model <- randomForest( formula_rf,
                             data = trainset, 
                             importance=TRUE, 
                             ntree=100,
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
                 variable_importance=var_import, auc_value=auc_val))
  }

  
  return (list(rf_model=rf_model, prob1=default_prob[, 2]))
}

rf_mod <- rf_model_generation(credict_train_cleaned, formula_rf, target_col="default", only_pred=FALSE)
rf_mod$auc_value # 0.7731

##############################################################################################################

test_set = credict_train_cleaned

# RFE
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
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

# "AGE"       "PAY_1"     "EDUCATION" "LIMIT_BAL" "AGE_BIN"   "SQRT_PAYX" "BILL_AMT1" "BILL_AMT4"
features_by_rfe <- predictors(results)
formula_rf2 <- as.formula(paste("default~", paste(features_by_rfe, collapse="+")))
  
rf_mod2 <- rf_model_generation(credict_train_cleaned, formula_rf2, target_col="default", only_pred=FALSE)
rf_mod2$auc_value # 0.7719
rf_mod2$rf_model$confusion
#       0    1 class.error
# 0 12524 1031  0.07606049
# 1  2531 2355  0.51801064

rf_mod$rf_model$confusion
#       0    1 class.error
# 0 12476 1079  0.07960162
# 1  2589 2297  0.52988129