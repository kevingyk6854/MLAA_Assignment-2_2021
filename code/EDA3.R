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

# Set default as number: 0: No, 1: Yes
credict_train_cleaned['default'][credict_train_cleaned['default'] == 'Y'] <- '1'
credict_train_cleaned['default'][credict_train_cleaned['default'] == 'N'] <- '0'
credict_train_cleaned$default = as.numeric(credict_train_cleaned$default)

# Convert EDUCATION 0, 4, 5, 6 to 4 as others/unknown
# Convert MARRIAGE 0, 3 to 3 as others/unknown
credict_train_cleaned <- credict_train_cleaned %>% 
  mutate(EDUCATION = ifelse(EDUCATION %in% c(0, 4, 5, 6), 4, EDUCATION)) %>%
  mutate(MARRIAGE = ifelse(EDUCATION %in% c(0, 3), 3, MARRIAGE))

# batch processing: categorical variables convert to factor type
# names <- c(2:4, 6:11, 18:24)
# credict_train_cleaned[,names] <- lapply(credict_train_cleaned[,names] , factor)

# Generate Age Bin
credict_train_cleaned$AGE_BIN <- 0
credict_train_cleaned$BAL_BIN <- 0

credict_train_cleaned <- credict_train_cleaned %>% 
  mutate(AGE_BIN = ifelse(AGE < 30, 1, 
                   ifelse(AGE >= 30 & AGE < 50, 2, 
                   ifelse(AGE >= 50 & AGE < 70, 3,
                   ifelse(AGE > 70, 4, 0))))) %>%
  mutate(BAL_BIN = ifelse(LIMIT_BAL < 130000, 1, 
                   ifelse(LIMIT_BAL >= 130000 & LIMIT_BAL < 3600000, 2, 
                   ifelse(LIMIT_BAL >= 360000 & LIMIT_BAL < 520000, 3,
                   ifelse(LIMIT_BAL > 520000, 4, 0)))))

# names_bin <- c(25:26) # 'AGE_BIN', 'BAL_BIN'
# credict_train_cleaned[,names_bin] <- lapply(credict_train_cleaned[,names_bin], factor) 

#####################################################################################################################

credict_tot <- credict_train_cleaned
credict_tot <- credict_tot %>%
  mutate(TOTAL_BILL = BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6) %>%
  mutate(SQRT_PAYX = ceiling(sqrt((PAY_1+2)^2 + (PAY_2+2)^2 + (PAY_3+2)^2 + (PAY_4+2)^2 + (PAY_5+2)^2 + (PAY_6+2)^2)))

payx_df <- credict_tot %>%
  dplyr::group_by(SQRT_PAYX) %>%
  dplyr::summarise(
    default1_cnt = n(),
    default1_percent = sum(default == 1) / n() * 100
  ) %>%
  arrange(default1_percent)
View(payx_df)

fig <- plot_ly(
  x = payx_df$SQRT_PAYX,
  y = payx_df$default1_percent,
  name = "marriage",
  type = "bar"
)

fig

# 0-5, 6-9, 10-13, 14-18
# 5开始 大于0的PAY_X开始占据主要地位

# credict_tot <- credict_train_cleaned
# credict_tot <- credict_tot %>%
#   select(LIMIT_BAL, total_bill, default, starts_with('PAY_'), starts_with('BILL_'), AGE_BIN, BAL_BIN)

# write_csv(credict_tot, 'data/Amt Summary.csv')














# ########################################################################################
# test_set = credict_train_cleaned
# 
# # RFE
# control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# # run the RFE algorithm
# results <- rfe(test_set %>% select(-default), 
#                test_set$default, 
#                sizes=c(1:8), 
#                rfeControl=control)
# # summarize the results
# print(results)
# # list the chosen features
# predictors(results)
# # plot the results
# plot(results, type=c("g", "o"))
# 
# 
# ########################################################################################
# 
# # First step is to set a random seed to ensurre we get the same result each time
# set.seed(42) 
# 
# # features <- c("EDUCATION", "PAY_1", "PAY_1", "BILL_AMT1", "PAY_AMT1", "AGE_BIN", "BAL_BIN")
# features_by_rfe <-c("PAY_1", "AGE", "LIMIT_BAL", "EDUCATION", "BILL_AMT1", "BILL_AMT4", "PAY_3", "BILL_AMT5")
# formula_rf <- as.formula(paste("default~", paste(features_by_rfe, collapse="+")))
# 
# # Split training & test sets
# trainset_size <- floor(0.80 * nrow(credict_train_cleaned))
# trainset_indices <- sample(seq_len(nrow(credict_train_cleaned)), size = trainset_size)
# trainset <- credict_train_cleaned[trainset_indices, ] %>% select(all_of(features_by_rfe), default)
# testset <- credict_train_cleaned[-trainset_indices, ] %>% select(all_of(features_by_rfe), default)
# 
# # Random Forest Model
# credit_rf <- randomForest( formula_rf,
#                            data = trainset, 
#                            importance=TRUE, 
#                            xtest=testset[,-length(features_by_rfe)-1],
#                            ntree=100,
#                            norm.votes = TRUE) 
# 
# names(credit_rf)
# 
# test_predictions_rf <- data.frame(testset,credit_rf$test$predicted)
# test_predictions_rf$probability_1 <- credit_rf$test$votes[, 2]
# 
# mean(credit_rf$test$predicted==testset$default)
# 
# table(credit_rf$test$predicted,testset$default)
# 
# importance(credit_rf)
# 
# # partial dependency plots
# varImpPlot(credit_rf)
# 
# #apply roc function
# rf_analysis <- roc(response=testset$default, test_predictions_rf$probability_1)
# #calculate auc
# auc(rf_analysis)
# 
# ########################################################################################################
# 
# credict_test <- read_csv(here::here("data/AT2_credit_test.csv"))
# 
# # Clean the dataset
# credict_test_cleaned <- credict_test %>%
#   select(-ID) %>% 
#   dplyr::filter(LIMIT_BAL > 0 &
#                   SEX %in% c(0, 1, 2) &
#                   EDUCATION %in% c(0, 1, 2, 3, 4, 5, 6) &
#                   MARRIAGE %in% c(0, 1, 2, 3) &
#                   AGE < 120
#   ) %>%
#   dplyr::rename(
#     PAY_1 = PAY_0
#   )
# 
# 
# # prefix <- c('PAY_AMT', 'PAY', 'BILL_AMT')
# # Convert payment amt as categorical variable (0: no payment; 1: yes payment)
# prefix <- 'PAY_AMT'
# for (i in 1:6){
#   col <- paste(prefix, i, sep="", collapse = NULL)
#   credict_test_cleaned[col][credict_test_cleaned[col] > 0] <- 1
# }
# 
# # Convert EDUCATION 0, 4, 5, 6 to 4 as others/unknown
# # Convert MARRIAGE 0, 3 to 3 as others/unknown
# credict_test_cleaned <- credict_test_cleaned %>% 
#   mutate(EDUCATION = ifelse(EDUCATION %in% c(0, 4, 5, 6), 4, EDUCATION)) %>%
#   mutate(MARRIAGE = ifelse(EDUCATION %in% c(0, 3), 3, MARRIAGE))
# 
# # batch processing: categorical variables convert to factor type
# names <- c(2:4, 6:11, 18:23)
# credict_test_cleaned[,names] <- lapply(credict_test_cleaned[,names] , factor)
# 
# features_by_rfe <-c("PAY_1", "AGE", "LIMIT_BAL", "EDUCATION", "BILL_AMT1", "BILL_AMT4", "PAY_3", "BILL_AMT5")
# formula_rf <- as.formula(paste("default~", paste(features_by_rfe, collapse="+")))
# 
# repurchase_rf_t <- randomForest( formula_rf,
#                                  data = credict_train_cleaned, 
#                                  importance=TRUE, 
#                                  xtest=credict_test_cleaned%>% select(all_of(features_by_rfe)),
#                                  ntree=100,
#                                  norm.votes = TRUE) 
# 
# names(repurchase_rf_t)
# 
# test_predictions_rf_t <- data.frame(credict_test_cleaned,repurchase_rf_t$test$predicted)
# test_predictions_rf_t$probability_1 <- repurchase_rf_t$test$votes[, 2]
# 
# output_df <- data.frame(
#   ID = credict_test %>% select(ID), 
#   default = test_predictions_rf_t$probability_1
# )
# 
# write_csv(output_df, 'data/rf ver 1.1.1.csv')






