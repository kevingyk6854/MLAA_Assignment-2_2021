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

# Set default as number: 0: No, 1: Yes
credict_train_cleaned['default'][credict_train_cleaned['default'] == 'Y'] <- '1'
credict_train_cleaned['default'][credict_train_cleaned['default'] == 'N'] <- '0'
credict_train_cleaned$default = as.numeric(credict_train_cleaned$default)

# Convert EDUCATION 0, 4, 5, 6 to 4 as others/unknown
# Convert MARRIAGE 0, 3 to 3 as others/unknown
credict_train_cleaned <- credict_train_cleaned %>% 
  mutate(EDUCATION = ifelse(EDUCATION %in% c(0, 4, 5, 6), 4, EDUCATION))%>%
  mutate(MARRIAGE = ifelse(MARRIAGE %in% c(0, 3), 3, MARRIAGE))

# -----------------------------------------------------------

credict_train_cleaned <- credict_train_cleaned %>% 
  # Generate BILL / BAL percentage
  mutate(BILL_BAL_PER1 = round((BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6)/ LIMIT_BAL, 1)) %>%
  # Generate SQRT SQUARED PAY_X
  mutate(SQRT_PAYX = ceiling(sqrt((PAY_1+2)^2 + (PAY_2+2)^2 + (PAY_3+2)^2 + (PAY_4+2)^2 + (PAY_5+2)^2 + (PAY_6+2)^2)))

# Remove Outliers
credict_train_cleaned <- credict_train_cleaned%>% 
  filter(!LIMIT_BAL %in% c(327680, 760000)) %>%
  filter(!SQRT_PAYX %in% c(17))

# Generate Categorical Variables (Bins)
credict_train_cleaned <- credict_train_cleaned %>% 
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

to_fac_cols <- c('default', c('SEX', 'EDUCATION', 'MARRIAGE'), payx_cols, payamtx_cols, c('SQRT_PAYX', 'AGE_BIN', 'BAL_BIN', 'PAYX_BIN'))
# batch processing: categorical variables convert to factor type
# names <- c(2:4, 6:11, 18:23)
credict_train_cleaned[, to_fac_cols] <- lapply(credict_train_cleaned[, to_fac_cols] , factor)

################################################################################################

# Get extra functions
# source(here("code", "data_transformation.R"))

transforming_data <- function(file_name, is_train=TRUE) {
  # Extract data
  credict_train <- read_csv(here::here("data", file_name))
  # Three error rows not loaded
  
  # Clean the dataset
  df_cleaned <- credict_train %>%
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
    df_cleaned[col][df_cleaned[col] > 0] <- 1
  }
  
  # Set default as number: 0: No, 1: Yes
  if (is_train){
    df_cleaned['default'][df_cleaned['default'] == 'Y'] <- '1'
    df_cleaned['default'][df_cleaned['default'] == 'N'] <- '0'
    df_cleaned$default = as.numeric(df_cleaned$default)
  }
  
  # Convert EDUCATION 0, 4, 5, 6 to 4 as others/unknown
  # Convert MARRIAGE 0, 3 to 3 as others/unknown
  df_cleaned <- df_cleaned %>% 
    mutate(EDUCATION = ifelse(EDUCATION %in% c(0, 4, 5, 6), 4, EDUCATION)) %>%
    mutate(MARRIAGE = ifelse(EDUCATION %in% c(0, 3), 3, MARRIAGE))
  
  # batch processing: categorical variables convert to factor type
  if (is_train){
    names <- c(2:4, 6:11, 18:24)  
  } else {
    names <- c(2:4, 6:11, 18:23) 
  }
  df_cleaned[,names] <- lapply(df_cleaned[,names] , factor)
  
  # Generate Age Bin
  df_cleaned$AGE_BIN <- 0
  df_cleaned$BAL_BIN <- 0
  
  df_cleaned <- df_cleaned %>% 
    mutate(AGE_BIN = ifelse(AGE < 30, 1, 
                            ifelse(AGE >= 30 & AGE < 50, 2, 
                                   ifelse(AGE >= 50 & AGE < 70, 3,
                                          ifelse(AGE > 70, 4, 0))))) %>%
    mutate(BAL_BIN = ifelse(LIMIT_BAL < 130000, 1, 
                            ifelse(LIMIT_BAL >= 130000 & LIMIT_BAL < 3600000, 2, 
                                   ifelse(LIMIT_BAL >= 360000 & LIMIT_BAL < 520000, 3,
                                          ifelse(AGE > 520000, 4, 0)))))
  
  names_bin <- c(25:26) # 'AGE_BIN', 'BAL_BIN'
  df_cleaned[,names_bin] <- lapply(df_cleaned[,names_bin], factor) 
  
  return (df_cleaned)
}

















