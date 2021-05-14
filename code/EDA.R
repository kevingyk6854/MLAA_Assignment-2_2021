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

# 1. Data ETL
credict_train <- read_csv(here::here("data/AT2_credit_train.csv"))
# credict_test <- read_csv(here::here("data/AT2_credit_test.csv"))
# credict_test

# Three error rows not loaded

# Clean the dataset
credict_train_cleaned <- credict_train %>%
  select(-ID) %>% 
  dplyr::filter(LIMIT_BAL > 0 &
                  SEX %in% c(0, 1, 2) &
                  EDUCATION %in% c(0, 1, 2, 3, 4, 5, 6) &
                  MARRIAGE %in% c(0, 1, 2, 3) &
                  AGE < 120 &
                  default %in% c('Y', 'N')
                ) %>%
  dplyr::rename(
    PAY_1 = PAY_0
  )

# prefix <- c('PAY_AMT', 'PAY', 'BILL_AMT')
# Convert payment amt as categorical variable (0: no payment; 1: yes payment)
prefix <- 'PAY_AMT'
for (i in 1:6){
  col <- paste(prefix, i, sep="", collapse = NULL)
  credict_train_cleaned[col][credict_train_cleaned[col] > 0] <- 1
}

credict_train_cleaned['default'][credict_train_cleaned['default'] == 'Y'] <- '1'
credict_train_cleaned['default'][credict_train_cleaned['default'] == 'N'] <- '0'
credict_train_cleaned$default = as.numeric(credict_train_cleaned$default)

# batch processing: categorical variables convert to factor type
names <- c(2:4, 6:11, 18:24)
credict_train_cleaned[,names] <- lapply(credict_train_cleaned[,names] , factor)

#################################################################################

# First step is to set a random seed to ensurre we get the same result each time
set.seed(42) 

# Split training & test sets
trainset_size <- floor(0.80 * nrow(credict_train_cleaned))
trainset_indices <- sample(seq_len(nrow(credict_train_cleaned)), size = trainset_size)
trainset <- credict_train_cleaned[trainset_indices, ]
testset <- credict_train_cleaned[-trainset_indices, ]

# Random Forest Model
# repurchase_rf <- randomForest( default~.,
#                                data = trainset, 
#                                importance=TRUE, 
#                                ntree=100,
#                                norm.votes = TRUE) 

repurchase_rf <- randomForest( default~.,
                               data = trainset, 
                               importance=TRUE, 
                               xtest=testset[,-24],
                               ntree=100,
                               norm.votes = TRUE) 

names(repurchase_rf)

test_predictions_rf <- data.frame(testset,repurchase_rf$test$predicted)
test_predictions_rf$probability_1 <- repurchase_rf$test$votes[, 2]

mean(repurchase_rf$test$predicted==testset$default)

table(repurchase_rf$test$predicted,testset$default)

importance(repurchase_rf)

# partial dependency plots
varImpPlot(repurchase_rf)

#apply roc function
rf_analysis <- roc(response=testset$default, test_predictions_rf$probability_1)
#calculate auc
auc(rf_analysis)

# # ======================================================================
# # RFE
# control <- rfeControl(functions=rfFuncs, method="cv", number=3)
# # run the RFE algorithm
# # TODO: using dummy variables to factor features
# results <- rfe(testset[,2:ncol(testset)], 
#                testset$Target, 
#                sizes=c(1:8), 
#                rfeControl=control)
# # summarize the results
# print(results)
# # list the chosen features
# predictors(results)
# # plot the results
# plot(results, type=c("g", "o"))


# # ======================================================================

credict_test <- read_csv(here::here("data/AT2_credit_test.csv"))

# Clean the dataset
credict_credit_cleaned <- credict_test %>%
  select(-ID) %>% 
  dplyr::filter(LIMIT_BAL > 0 &
                  SEX %in% c(0, 1, 2) &
                  EDUCATION %in% c(0, 1, 2, 3, 4, 5, 6) &
                  MARRIAGE %in% c(0, 1, 2, 3) &
                  AGE < 120
  ) %>%
  dplyr::rename(
    PAY_1 = PAY_0
  )

# prefix <- c('PAY_AMT', 'PAY', 'BILL_AMT')
# Convert payment amt as categorical variable (0: no payment; 1: yes payment)
prefix <- 'PAY_AMT'
for (i in 1:6){
  col <- paste(prefix, i, sep="", collapse = NULL)
  credict_credit_cleaned[col][credict_credit_cleaned[col] > 0] <- 1
}

# batch processing: categorical variables convert to factor type
names <- c(2:4, 6:11, 18:23)
credict_credit_cleaned[,names] <- lapply(credict_credit_cleaned[,names] , factor)


repurchase_rf_t <- randomForest( default~.,
                                 data = credict_train_cleaned, 
                                 importance=TRUE, 
                                 xtest=credict_credit_cleaned,
                                 ntree=100,
                                 norm.votes = TRUE) 

names(repurchase_rf_t)

test_predictions_rf_t <- data.frame(credict_credit_cleaned,repurchase_rf_t$test$predicted)
test_predictions_rf_t$probability_1 <- repurchase_rf_t$test$votes[, 2]

output_df <- data.frame(
  ID = credict_test %>% select(ID), 
  default = test_predictions_rf_t$probability_1
)

write_csv(output_df, 'data/rf ver 1.0.1.csv')
