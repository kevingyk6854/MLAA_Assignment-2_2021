library(here)
library(tidyverse)
library(magrittr)
library(readr)
library(dplyr)
library(rsample)      # data splitting 
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization
library(corrplot)

source(here("code", "data_trans.R"))

# Extract & Transform data
df <- transforming_data(read_csv(here::here("data/AT2_credit_train.csv")), 
                        is_train = TRUE,
                        cat_var_to_fact = FALSE) # Noted: gbm does not accept factor variables
df <- df %>% relocate('default')

# First step is to set a random seed to ensurre we get the same result each time
set.seed(1234) 

# Split training & test sets
# trainset_size <- floor(0.70 * nrow(df))
# trainset_indices <- sample(seq_len(nrow(df)), size = trainset_size)
indices <- createDataPartition(df$default, p = .7, list = FALSE) 
trainset <- df[indices, ]
testset <- df[-indices, ]


set.seed(1234)

formula_gbm <- as.formula(paste("default~", ".", sep = ""))

# train GBM model
gbm.fit <- gbm(
  formula =  formula_gbm,
  distribution = "bernoulli",
  data = trainset,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001, # learning rate
  cv.folds = 5,
  # train.fraction=.75,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 

# print results
print(gbm.fit)

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")
# 10000

#################### Second TRY - TUNE[1] ##################################

set.seed(1234)
formula_gbm <- as.formula(paste("default~", ".", sep = ""))

# train GBM model
gbm.fit <- gbm(
  formula =  formula_gbm,
  distribution = "bernoulli",
  data = trainset,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1, # learning rate
  cv.folds = 5,
  # train.fraction=.75,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 

# print results
print(gbm.fit)

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")
# 289

################### Create Hyperparameter Grid[2] ############################

hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1),
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)
## [1] 81

# randomize data
random_index <- sample(1:nrow(trainset), nrow(trainset))
random_credit_train <- trainset[random_index, ]

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(1234)
  
  # train model
  gbm.tune <- gbm(
    formula = formula_gbm,
    distribution = "bernoulli",
    data = random_credit_train,
    n.trees = 1000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    # cv.folds = 5,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$cv.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

##################

set.seed(1234)

# train GBM model
gbm.fit.final <- gbm(
  formula = formula_gbm,
  distribution = "bernoulli",
  data = trainset,
  n.trees = 300,
  interaction.depth = 5,
  shrinkage = 0.1,
  n.minobsinnode = 10,
  bag.fraction = 1.0, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# Variable Importance with rel.inf
par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

###########################

features_by_gbm <- summary(
  gbm.fit.final, 
  cBars = 15,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)$var[1:9]
formula_gbm2 <- as.formula(paste("default~", paste(features_by_gbm, collapse="+")))

# train GBM model
gbm.fit.final2 <- gbm(
  formula = formula_gbm2,
  distribution = "bernoulli",
  data = trainset,
  n.trees = 300,
  interaction.depth = 5,
  shrinkage = 0.1,
  n.minobsinnode = 10,
  bag.fraction = 1.0, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

###########################

# predict values for test data
pred <- predict(gbm.fit.final2, n.trees = gbm.fit.final2$n.trees, testset)

library(ROCR)
GBM_pred_testing <- prediction(pred, testset$default)
GBM_ROC_testing <- performance(GBM_pred_testing, "tpr", "fpr")
plot(GBM_ROC_testing)
plot(GBM_ROC_testing, add=TRUE, col='blue')
legend("right", legend = c("GBM"), col = c("blue"), lty = 1:2, cex = 0.6)

prob <- predict(object = gbm.fit.final2,
                n.trees = gbm.fit.final2$n.trees,
                newdata = testset,
                type = "response")

gbm_analysis <- roc(response=testset$default, prob)
plot(gbm_analysis, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

# AUC
auc.tmp <- performance(GBM_pred_testing, "auc")
gbm_auc_testing <- as.numeric(auc.tmp@y.values)
gbm_auc_testing # 0.8083886 0.8099489

###########################

# partial dependence plots (PDPs)
gbm.fit.final2 %>%
  partial(pred.var = "SQRT_PAYX", n.trees = gbm.fit.final2$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = trainset)

###########################

# df_test <- transforming_data(read_csv(here::here("data/AT2_credit_test.csv")), 
#                              is_train = FALSE,
#                              cat_var_to_fact = FALSE) # Noted: gbm does not accept factor variables
# 
# pred <- predict(object = gbm.fit.final, 
#                 n.trees = gbm.fit.final$n.trees, 
#                 newdata = df_test, 
#                 type = "response")
# 
# df_t <- read_csv(here::here("data/AT2_credit_test.csv"))
# 
# output_df <- data.frame(
#   ID = df_t %>% select(ID), 
#   default = pred
# )
# 
# write_csv(output_df, 'data/gbm ver 1.1.2.csv')



