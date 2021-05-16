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

feature_creating <- function(df){
  
  df <- df %>% 
    # Generate BILL / BAL percentage
    mutate(BILL_BAL_PER1 = round((BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6)/ LIMIT_BAL, 1)) %>%
    # Generate SQRT SQUARED PAY_X
    mutate(SQRT_PAYX = ceiling(sqrt((PAY_1+2)^2 + (PAY_2+2)^2 + (PAY_3+2)^2 + (PAY_4+2)^2 + (PAY_5+2)^2 + (PAY_6+2)^2)))
  
  # Generate Categorical Variables (Bins)
  df <- df %>% 
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
                             ifelse(SQRT_PAYX >= 6 & SQRT_PAYX <= 9, 2, 
                                    ifelse(SQRT_PAYX >= 10 & SQRT_PAYX <= 13, 3,
                                           ifelse(SQRT_PAYX >= 14, 4, 0)))))
  return (df)
}

transforming_train <- function(df) {
  df <- df %>%
    filter(default %in% c('Y', 'N'))
  
  # Set default as number: 0: No, 1: Yes
  df['default'][df['default'] == 'Y'] <- '1'
  df['default'][df['default'] == 'N'] <- '0'
  df$default = as.numeric(df$default)
  
  # Remove Outliers
  df <- df%>% 
    filter(!LIMIT_BAL %in% c(327680, 760000)) %>%
    filter(!SQRT_PAYX %in% c(17))
  
  return (df)
}

transforming_data <- function(df, is_train=TRUE, cat_var_to_fact=TRUE) {
  
  # Clean the dataset
  df_cleaned <- df %>%
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
    df_cleaned[col][df_cleaned[col] > 0] <- 1
  }
  
  # Convert EDUCATION 0, 4, 5, 6 to 4 as others/unknown
  # Convert MARRIAGE 0, 3 to 3 as others/unknown
  df_cleaned <- df_cleaned %>% 
    mutate(EDUCATION = ifelse(EDUCATION %in% c(0, 4, 5, 6), 4, EDUCATION))%>%
    mutate(MARRIAGE = ifelse(MARRIAGE %in% c(0, 3), 3, MARRIAGE))
  
  # Create new features
  df_cleaned <- feature_creating(df_cleaned)
  
  if (is_train) {
    df_cleaned <- transforming_train(df_cleaned)
  }
  
  # Categorial Variable convert to Factor
  if (cat_var_to_fact) {
    # List the columns required to be converted into Factor
    to_fac_cols <- c(format_cols())
    
    if (is_train) {
      to_fac_cols <- c('default', to_fac_cols)
    }
    
    # batch processing: categorical variables convert to factor type
    # names <- c(2:4, 6:11, 18:23)
    df_cleaned[, to_fac_cols] <- lapply(df_cleaned[, to_fac_cols] , factor)
  }
  
  return (df_cleaned)
}