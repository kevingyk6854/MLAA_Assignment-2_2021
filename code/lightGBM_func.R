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
library(lightgbm)
library(methods)

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('rlang') # data manipulation

# modelling
library('xgboost') # modelling
library('caret') # modelling
library('MLmetrics') # gini metric

source(here("code", "data_trans.R"))

# Extract & Transform data
credict_train_cleaned <- transforming_data(read_csv(here::here("data/AT2_credit_train.csv")), 
                                           is_train = TRUE)
                                           
# First step is to set a random seed to ensurre we get the same result each time
set.seed(42) 

# Split training & test sets
trainset_size <- floor(0.80 * nrow(credict_train_cleaned))
trainset_indices <- sample(seq_len(nrow(credict_train_cleaned)), size = trainset_size)

train <- credict_train_cleaned[trainset_indices, ]
val <- credict_train_cleaned[-trainset_indices, ]

#prepare training and validation data
trainm = sparse.model.matrix(default ~., data = train)
train_label = train[,"default"]

valm = sparse.model.matrix(default~., data= val)
val_label = val[,"default"]

train_matrix = lgb.Dataset(data = as.matrix(trainm), label = train_label)
val_matrix = lgb.Dataset(data = as.matrix(valm), label = val_label)

valid = list(test = val_matrix)

# model parameters
params = list(max_bin = 5,
              learning_rate = 0.001,
              objective = "binary",
              metric = 'binary_logloss')

#model training
bst = lightgbm(params = params, train_matrix, valid, nrounds = 1000)

#prediction & confusion matrix
p = predict(bst, valm)
# val$predicted = ifelse(p > 0.3,1,0)
# confusionMatrix(factor(val$predicted), factor(val$default))

#apply roc function
rf_analysis <- roc(response=val[['default']], predict(bst, valm))
#calculate auc
auc_val <- auc(rf_analysis)
auc_val

##############################

# First step is to set a random seed to ensurre we get the same result each time
set.seed(42) 

# Split training & test sets
trainset_size <- floor(0.80 * nrow(credict_train_cleaned))
trainset_indices <- sample(seq_len(nrow(credict_train_cleaned)), size = trainset_size)
train <- credict_train_cleaned[trainset_indices, ]
test <- credict_train_cleaned[-trainset_indices, ]

varnames = setdiff(colnames(credict_train_cleaned), c("default"))

train_sparse = Matrix(as.matrix(train[!is.na('default'), varnames, with=F]), sparse=TRUE)
test_sparse  = Matrix(as.matrix(test[!is.na('default') , varnames, with=F]), sparse=TRUE)

y_train  = train[!is.na('default'), 'default']
y_test  = test[!is.na('default'), 'default']
# test_ids = credict_train_cleaned[is.na(default) ,id]

lgb.trainset = lgb.Dataset(data=train_sparse, label=y_train)
lgb.testset  = lgb.Dataset(data=test_sparse, label=y_test)

format_cols <- function(){
  
  prefix_payx <- 'PAY_'
  prefix_payamtx <- 'PAY_AMT'
  
  payx_cols <- c()
  payamtx_cols <- c()
  
  for (i in 1:6){
    payx_col <- paste(prefix_payx, i, sep="", collapse = NULL)
    payx_cols <- c(payx_cols, payx_col)
    
    payamtx_col <- paste(prefix_payamtx, i, sep="", collapse = NULL)
    payamtx_cols <- c(payamtx_cols, payamtx_col)
  }
  
  return (c(c('SEX', 'EDUCATION', 'MARRIAGE'), payx_cols, payamtx_cols, c('SQRT_PAYX', 'AGE_BIN', 'BAL_BIN', 'PAYX_BIN')))
}

categoricals.vec = format_cols()

# Setting up LGBM Parameters
lgb.grid = list(objective = "binary",
                metric = "auc",
                min_sum_hessian_in_leaf = 1,
                feature_fraction = 0.7,
                bagging_fraction = 0.7,
                bagging_freq = 5,
                min_data = 100,
                max_bin = 50,
                lambda_l1 = 8,
                lambda_l2 = 1.3,
                min_data_in_bin=100,
                min_gain_to_split = 10,
                min_data_in_leaf = 30,
                is_unbalance = TRUE)

# Setting up Gini Eval Function
lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}

# Cross Validation 
#lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.02, num_leaves = 25,
#                   num_threads = 2 , nrounds = 7000, early_stopping_rounds = 50,
#                   eval_freq = 20, eval = lgb.normalizedgini,
#                   categorical_feature = categoricals.vec, nfold = 5, stratified = TRUE)

#best.iter = lgb.model.cv$best_iter
best.iter = 525

# Train final model
lgb.model = lgb.train(params = lgb.grid, data = lgb.trainset, learning_rate = 0.02,
                      num_leaves = 25, num_threads = 2 , nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini,
                      categorical_feature = categoricals.vec)

p2 <- predict(lgb.model,test_sparse)
# p2

#apply roc function
rf_analysis2 <- roc(response=credict_train_cleaned[['default']], p2)
#calculate auc
auc_val2 <- auc(rf_analysis2)
auc_val2