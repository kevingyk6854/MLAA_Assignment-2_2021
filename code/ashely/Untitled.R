library(here)
library(tidyverse)
library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(glmnet)
library(plotly)
library(dummies)
library(lubridate)
library(varImp)

# 1. Data ETL
AT2_db <- read_csv(here::here("data", "AT2_credit_train.csv"))
str(AT2_db)


# Check null values
mean(is.na(AT2_db))

# test missing value
sex_na = summary(is.na(SEX))
edu_na = summary(is.na(EDUCATION))
mrg_na = summary(is.na(MARRIAGE))

sex_na
edu_na
mrg_na

#EDUCATION has category 5 and 6 that are unlabelled, moreover the category 0 is undocumented.
#MARRIAGE has a label 0 that is undocumented

#remove missing values in SEX
AT2_db <- AT2_db %>%
  drop_na(SEX)

# check 
AT2_db %>% distinct(SEX)
AT2_db %>% distinct(EDUCATION)
AT2_db %>% distinct(MARRIAGE)
AT2_db %>% distinct(LIMIT_BAL)

# remove not applicable figure in sex, limit_balance and age
AT2_db <- AT2_db[AT2_db$SEX !="2113" & AT2_db$LIMIT_BAL != "-99" & AT2_db$AGE != "157", ]


# drop the ID & age column
AT2_db$ID <- NULL

# Create dummy variable
sex_dummy <- dummy(AT2_db$SEX)
edu_dummy <- dummy(AT2_db$EDUCATION)
mrg_dummy <- dummy(AT2_db$MARRIAGE)
pay0_dummy <- dummy(AT2_db$PAY_0)
pay2_dummy <- dummy(AT2_db$PAY_2)
pay3_dummy <- dummy(AT2_db$PAY_3)
pay4_dummy <- dummy(AT2_db$PAY_4)
pay5_dummy <- dummy(AT2_db$PAY_5)
pay6_dummy <- dummy(AT2_db$PAY_6)
payamt1_dummy <- dummy(AT2_db$PAY_AMT1)
payamt2_dummy <- dummy(AT2_db$PAY_AMT2)
payamt3_dummy <- dummy(AT2_db$PAY_AMT3)
payamt4_dummy <- dummy(AT2_db$PAY_AMT4)
payamt5_dummy <- dummy(AT2_db$PAY_AMT5)
payamt6_dummy <- dummy(AT2_db$PAY_AMT6)

df_dummy <- cbind(AT2_db,sex_dummy,edu_dummy,mrg_dummy,pay0_dummy,pay2_dummy,pay3_dummy,pay4_dummy,pay5_dummy,pay6_dummy,
                  payamt1_dummy,payamt2_dummy,payamt3_dummy,payamt4_dummy,payamt5_dummy,payamt6_dummy)
df_dummy$SEX <- NULL
df_dummy$EDUCATION <- NULL
df_dummy$MARRIAGE <- NULL
df_dummy$PAY_0 <- NULL
df_dummy$PAY_2 <- NULL
df_dummy$PAY_3 <- NULL
df_dummy$PAY_4 <- NULL
df_dummy$PAY_5 <- NULL
df_dummy$PAY_6 <- NULL
df_dummy$PAY_AMT1 <- NULL
df_dummy$PAY_AMT2 <- NULL
df_dummy$PAY_AMT3 <- NULL
df_dummy$PAY_AMT4 <- NULL
df_dummy$PAY_AMT5 <- NULL
df_dummy$PAY_AMT6 <- NULL

# factor the default value
df_dummy$default <- as.factor(df_dummy$default)


# Logistic regression

# create data partition row list, set a random seed, and get indices of observations to be assigned to training set
trainset_size1 <- floor(0.75 * nrow(df_dummy))
set.seed(42)
trainset_indices1 <- sample(seq_len(nrow(df_dummy)), size = trainset_size1)

#assign observations to training and testing sets

trainset1 <- df_dummy[trainset_indices1, ]
testset1 <- df_dummy[-trainset_indices1, ]

#rowcounts to check
nrow(trainset1)
nrow(testset1)
nrow(df_dummy)

#backwards selection - start with throwing all the variables into the logistic regression
df_dummy.glm = glm(formula = default ~ .,
                   data = trainset1,
                   family = "binomial")
summary(df_dummy.glm)

df_dummy.glm$variable.importance


#forward & backward
logit.step<-step(df_dummy.glm,direction = c("both"))






# Decision Tree














# Random Forest