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

########################################################################################
# Pure RF
# First step is to set a random seed to ensurre we get the same result each time
set.seed(42) 

credict_train_cleaned <- credict_train_cleaned %>% relocate('default')


formula_rf <- as.formula(paste("default~", ".", sep = ""))

# Split training & test sets
trainset_size <- floor(0.80 * nrow(credict_train_cleaned))
trainset_indices <- sample(seq_len(nrow(credict_train_cleaned)), size = trainset_size)
trainset <- credict_train_cleaned[trainset_indices, ]
testset <- credict_train_cleaned[-trainset_indices, ]

# Random Forest Model
credit_rf <- randomForest( formula_rf,
                           data = trainset, 
                           importance=TRUE, 
                           xtest=testset[,-1],
                           ntree=100,
                           norm.votes = TRUE) 

names(credit_rf)

test_predictions_rf <- data.frame(testset,credit_rf$test$predicted)
test_predictions_rf$probability_1 <- credit_rf$test$votes[, 2]

mean(credit_rf$test$predicted==testset$default)

table(credit_rf$test$predicted,testset$default)

importance(credit_rf)

# partial dependency plots
varImpPlot(credit_rf)

#apply roc function
rf_analysis <- roc(response=testset$default, test_predictions_rf$probability_1)
#calculate auc
auc(rf_analysis)

########################################################################################
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


########################################################################################

# features_by_rfe <- c("PAYX_BIN", "AGE_BIN", "BAL_BIN", "EDUCATION", "BILL_BAL_PER1", "PAY_1")
features_by_rfe <- predictors(results)
formula_rf <- as.formula(paste("default~", paste(features_by_rfe, collapse="+")))

# Split training & test sets
# trainset_size <- floor(0.80 * nrow(credict_train_cleaned))
# trainset_indices <- sample(seq_len(nrow(credict_train_cleaned)), size = trainset_size)
trainset <- credict_train_cleaned[trainset_indices, ] %>% select(default, all_of(features_by_rfe))
testset <- credict_train_cleaned[-trainset_indices, ] %>% select(default, all_of(features_by_rfe))

# Random Forest Model
credit_rf2 <- randomForest( formula_rf,
                           data = trainset, 
                           importance=TRUE, 
                           xtest=testset[,-1],
                           ntree=100,
                           norm.votes = TRUE) 

names(credit_rf2)

test_predictions_rf <- data.frame(testset,credit_rf2$test$predicted)
test_predictions_rf$probability_1 <- credit_rf2$test$votes[, 2]

mean(credit_rf2$test$predicted==testset$default)

table(credit_rf2$test$predicted,testset$default)

importance(credit_rf2)

# partial dependency plots
varImpPlot(credit_rf2)

#apply roc function
rf_analysis <- roc(response=testset$default, test_predictions_rf$probability_1)
#calculate auc
auc(rf_analysis)

########################################################################################################

credict_test <- read_csv(here::here("data/AT2_credit_test.csv"))

# Clean the dataset
credict_test_cleaned <- credict_test %>%
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
  credict_test_cleaned[col][credict_test_cleaned[col] > 0] <- 1
}

# Convert EDUCATION 0, 4, 5, 6 to 4 as others/unknown
# Convert MARRIAGE 0, 3 to 3 as others/unknown
credict_test_cleaned <- credict_test_cleaned %>% 
  mutate(EDUCATION = ifelse(EDUCATION %in% c(0, 4, 5, 6), 4, EDUCATION)) %>%
  mutate(MARRIAGE = ifelse(MARRIAGE %in% c(0, 3), 3, MARRIAGE))

credict_test_cleaned <- credict_test_cleaned %>% 
  # Generate BILL / BAL percentage
  mutate(BILL_BAL_PER1 = round((BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6)/ LIMIT_BAL, 1)) %>%
  # Generate SQRT SQUARED PAY_X
  mutate(SQRT_PAYX = ceiling(sqrt((PAY_1+2)^2 + (PAY_2+2)^2 + (PAY_3+2)^2 + (PAY_4+2)^2 + (PAY_5+2)^2 + (PAY_6+2)^2)))

# Generate Categorical Variables (Bins)
credict_test_cleaned <- credict_test_cleaned %>% 
  # Generate Age Bin
  mutate(AGE_BIN = ifelse(AGE < 30, 1, 
                          ifelse(AGE >= 30 & AGE < 50, 2, 
                                 ifelse(AGE >= 50 & AGE < 70, 3,
                                        ifelse(AGE > 70, 4, 0))))) %>%
  # Generate Balance Bin
  mutate(BAL_BIN = ifelse(LIMIT_BAL < 130000, 1, 
                          ifelse(LIMIT_BAL >= 130000 & LIMIT_BAL < 3600000, 2, 
                                 ifelse(LIMIT_BAL >= 360000 & LIMIT_BAL < 520000, 3,
                                        ifelse(LIMIT_BAL > 520000, 4, 0))))) %>%
  # Generate PAY_X BIN
  mutate(PAYX_BIN = ifelse(SQRT_PAYX <= 5 , 1, 
                           ifelse(LIMIT_BAL >= 6 & LIMIT_BAL <= 9, 2, 
                                  ifelse(LIMIT_BAL >= 10 & LIMIT_BAL <= 13, 3,
                                         ifelse(LIMIT_BAL >= 14, 4, 0)))))

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

to_fac_cols <- c(c('SEX', 'EDUCATION', 'MARRIAGE'), payx_cols, payamtx_cols, c('SQRT_PAYX', 'AGE_BIN', 'BAL_BIN', 'PAYX_BIN'))
# batch processing: categorical variables convert to factor type
# names <- c(2:4, 6:11, 18:23)
credict_test_cleaned[, to_fac_cols] <- lapply(credict_test_cleaned[, to_fac_cols] , factor)


features_by_rfe <- predictors(results)
formula_rf <- as.formula(paste("default~", paste(features_by_rfe, collapse="+")))

levels(credict_train_cleaned$SQRT_PAYX) <- levels(credict_test_cleaned$SQRT_PAYX)
credict_test_cleaned$default <- 0

repurchase_rf_t <- randomForest( formula_rf,
                                 data = credict_train_cleaned, 
                                 importance=TRUE, 
                                 xtest=credict_test_cleaned %>% select(all_of(features_by_rfe)),
                                 ntree=100,
                                 norm.votes = TRUE) 

names(repurchase_rf_t)

test_predictions_rf_t <- data.frame(credict_test_cleaned,repurchase_rf_t$test$predicted)
test_predictions_rf_t$probability_1 <- repurchase_rf_t$test$votes[, 2]

output_df <- data.frame(
  ID = credict_test %>% select(ID), 
  default = test_predictions_rf_t$probability_1
)

write_csv(output_df, 'data/rf ver 1.2.2.csv')
