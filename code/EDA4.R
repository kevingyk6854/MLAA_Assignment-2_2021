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

# Set default as number: 0: No, 1: Yes
credict_train_cleaned['default'][credict_train_cleaned['default'] == 'Y'] <- '1'
credict_train_cleaned['default'][credict_train_cleaned['default'] == 'N'] <- '0'
credict_train_cleaned$default = as.numeric(credict_train_cleaned$default)

# Convert EDUCATION 0, 4, 5, 6 to 4 as others/unknown
# Convert MARRIAGE 0, 3 to 3 as others/unknown
credict_train_cleaned <- credict_train_cleaned %>% 
  mutate(EDUCATION = ifelse(EDUCATION %in% c(0, 4, 5, 6), 4, EDUCATION))

# batch processing: categorical variables convert to factor type
# names <- c(2:4, 6:11, 18:24)
# credict_train_cleaned[,names] <- lapply(credict_train_cleaned[,names] , factor)

#################################################################################
credict_tot4 <- credict_train_cleaned

credict_tot4 <- credict_tot4 %>%
  mutate(BILL_BAL_PER1 = round((BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6)/ LIMIT_BAL, 1)) %>%
  mutate(SQRT_PAYX = ceiling(sqrt((PAY_1+2)^2 + (PAY_2+2)^2 + (PAY_3+2)^2 + (PAY_4+2)^2 + (PAY_5+2)^2 + (PAY_6+2)^2)))

credict_tot4_d1 <- credict_tot4 %>% 
  # filter(BILL_BAL_PER1 <= 0.5, BILL_BAL_PER1 >= 0) %>%
  filter(BILL_BAL_PER1 > -1, BILL_BAL_PER1 < 7) %>%
  dplyr::group_by(BILL_BAL_PER1, default) %>%
  dplyr::summarise(
    BILL_BAL_PER_CNT = n(),
    SQRT_PAYX_MEAN = round(mean(SQRT_PAYX), 2)
  ) %>%
  filter(default == 1)

credict_tot4_dall <- credict_tot4 %>% 
  filter(BILL_BAL_PER1 > -1, BILL_BAL_PER1 < 7) %>%
  dplyr::group_by(BILL_BAL_PER1) %>%
  dplyr::summarise(
    BILL_BAL_PER_CNT = n(),
  )

credict_tot4_d1 <- merge(credict_tot4_d1, credict_tot4_dall, 
                         by.x = "BILL_BAL_PER1", 
                         by.y = "BILL_BAL_PER1", 
                         all.x = TRUE, all.y = FALSE)
  
credict_tot4_d1$DEFAULT1_PER <- round(credict_tot4_d1$BILL_BAL_PER_CNT.x / credict_tot4_d1$BILL_BAL_PER_CNT.y, 2)

fig1 <- plot_ly(x = ~credict_tot4_d1$BILL_BAL_PER1, # 0-0.4, 0.4-6.1, others
                y = ~credict_tot4_d1$DEFAULT1_PER, 
                type = 'scatter',
                mode = 'lines',
                fill = 'tozeroy',
                name = 'DEFAULT1_PER'
)

fig2 <- plot_ly(x = ~credict_tot4_d1$BILL_BAL_PER1, # 0-0.4, 0.4-6.1, others
                y = ~credict_tot4_d1$SQRT_PAYX_MEAN, 
                type = 'scatter',
                mode = 'lines',
                fill = 'tozeroy',
                name = 'SQRT_PAYX_MEAN'
)


fig <- subplot(fig1, fig2)
fig <- fig %>% layout(legend = list(x = 0.1, y = 0.9))
# fig <- fig %>% layout(xaxis = list(title = 'BILL_BAL_PER1'),
#                       yaxis = list(title = 'COUNT')
#                       )

fig