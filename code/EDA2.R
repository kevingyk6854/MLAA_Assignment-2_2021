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
names <- c(2:4, 6:11, 18:24)
credict_train_cleaned[,names] <- lapply(credict_train_cleaned[,names] , factor)

####################################################################################

# group by age
age_df <- credict_train_cleaned %>%
  dplyr::group_by(AGE) %>%
  dplyr::summarise(
    default1_cnt = n(),
    default1_percent = sum(default == 1) / n() * 100
  ) %>%
  arrange(AGE)
View(age_df)

fig <- plot_ly(x = ~age_df$AGE, 
               y = ~age_df$default1_percent, 
               type = 'scatter', 
               mode = 'lines', 
               fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Age'),
                      yaxis = list(title = 'default1_percent'))

fig

# TODO: age bin
# >70 too little data
# thus 50-70 is highest 


# group by education
education_df <- credict_train_cleaned %>%
  dplyr::group_by(EDUCATION) %>%
  dplyr::summarise(
    default1_cnt = n(),
    default1_percent = sum(default == 1) / n() * 100
  ) %>%
  arrange(default1_percent)
View(education_df)

fig <- plot_ly(
  x = education_df$EDUCATION,
  y = education_df$default1_percent,
  name = "Education",
  type = "bar"
)

fig
# Education: 1=graduate school, 2=university, 3=high school, 4=others
# 3>2>1>4


# group by marriage
marriage_df <- credict_train_cleaned %>%
  dplyr::group_by(MARRIAGE) %>%
  dplyr::summarise(
    default1_cnt = n(),
    default1_percent = sum(default == 1) / n() * 100
  ) %>%
  arrange(default1_percent)
View(marriage_df)

fig <- plot_ly(
  x = marriage_df$MARRIAGE,
  y = marriage_df$default1_percent,
  name = "marriage",
  type = "bar"
)

fig
# all is higher than 25% except 0, status-'divorce' is the highest about 33%
# but data is in a small scale

# group by limit balance
credict_lb <- credict_train_cleaned
credict_lb$LIMIT_BAL <- as.factor(credict_lb$LIMIT_BAL)

lb_df <- credict_train_cleaned %>%
  dplyr::group_by(LIMIT_BAL) %>%
  dplyr::summarise(
    default1_cnt = n(),
    default1_percent = sum(default == 1) / n() * 100
  ) %>%
  arrange(LIMIT_BAL)
View(lb_df)

fig <- plot_ly(x = ~lb_df$LIMIT_BAL, 
               y = ~lb_df$default1_percent, 
               type = 'scatter', 
               mode = 'lines', 
               fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Limit Balance'),
                      yaxis = list(title = 'default1_percent'))

fig
# 327680, 760000 as outliers
# limit_balance < 130k is more likely to fail to repay the bill



# TODO: BILL总和， Payment总和 和 limit balance做比较？
credict_tot <- credict_train_cleaned
credict_tot <- credict_tot %>%
  mutate(total_bill = BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6) %>%
  select(LIMIT_BAL, total_bill, default, starts_with('PAY_'))
View(credict_tot)

# write_csv(credict_tot, 'data/Amt Summary.csv')

credict_tot2 <- credict_tot %>%
  dplyr::filter(default == 1) %>%
  arrange(total_bill)

fig <- plot_ly(x = ~credict_tot2$total_bill,
               y = ~credict_tot2$LIMIT_BAL,
               type = 'scatter',
               mode = 'lines',
               fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'total_bill'),
                      yaxis = list(title = 'LIMIT_BAL'))

fig

# TODO: change to percentage
# fig <- plot_ly(data = credict_tot2, 
#                x = ~total_bill, 
#                y = ~LIMIT_BAL)
# 
# fig

# negative Bill amt might be a refund or a statement credit


# test the function of filter_at
credict_train_cleaned_t <- credict_train_cleaned
credict_train_cleaned_t %>% 
  filter_at(vars(BILL_AMT1:BILL_AMT6), all_vars(. < 0))


###############################################################################################
credict_tot2 <- credict_train_cleaned

credict_tot2 <- credict_tot2 %>%
  mutate(BILL_BAL_DIFF = BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 - LIMIT_BAL)

nrow(credict_tot2[which((credict_tot2['BILL_BAL_DIFF'] > 0) 
                        | (credict_tot2['default'] == 1)), ]) / nrow(credict_tot2)
# 0.6687752

# 
# fig <- plot_ly(data = credict_tot2, 
#                x = ~BILL_BAL_DIFF, 
#                y = ~default)
# 
# fig

###############################################################################################
credict_tot3 <- credict_train_cleaned %>%
  filter(default == 1)

# provide the most significant prediction
nrow(credict_tot3[which(credict_tot3['PAY_1'] >= 0), ]) / nrow(credict_tot3)
# 0.78

nrow(credict_tot3[which(credict_tot3['PAY_2'] >= 0), ]) / nrow(credict_tot3)
# 0.74

nrow(credict_tot3[which((credict_tot3['PAY_1'] >= 0) |
                          (credict_tot3['PAY_2'] >= 0)), ]) / nrow(credict_tot3)
# 0.82

###############################################################################################
credict_tot4 <- credict_train_cleaned

credict_tot4 <- credict_tot4 %>%
  mutate(BILL_BAL_PER = round((BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6)/ LIMIT_BAL, 2)) %>%
  mutate(SQRT_PAYX = ceiling(sqrt((PAY_1+2)^2 + (PAY_2+2)^2 + (PAY_3+2)^2 + (PAY_4+2)^2 + (PAY_5+2)^2 + (PAY_6+2)^2))) %>%
  filter(default == 1)

nrow(credict_tot4[which(credict_tot4['BILL_BAL_PER'] > 1), ]) / nrow(credict_tot4)
# 0.645

nrow(credict_tot4[which((credict_tot4['BILL_BAL_PER'] > 0.5) &
                          (credict_tot4['BILL_BAL_PER'] <= 1)), ]) / nrow(credict_tot4)
# 0.056

nrow(credict_tot4[which((credict_tot4['BILL_BAL_PER'] > 0.3) &
                          (credict_tot4['BILL_BAL_PER'] <= 0.5)), ]) / nrow(credict_tot4)

nrow(credict_tot4[which(credict_tot4['BILL_BAL_PER'] < 0), ]) / nrow(credict_tot4)

credict_tot4 <- credict_tot4 %>%
  mutate(BILL_BAL_PER = round((BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6)/ LIMIT_BAL, 2)) %>%
  mutate(BILL_BAL_PER1 = round((BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6)/ LIMIT_BAL, 1)) %>%
  mutate(SQRT_PAYX = ceiling(sqrt((PAY_1+2)^2 + (PAY_2+2)^2 + (PAY_3+2)^2 + (PAY_4+2)^2 + (PAY_5+2)^2 + (PAY_6+2)^2))) %>%
  filter(default == 1)

nrow(credict_tot4[which(credict_tot4['BILL_BAL_PER1'] > 1), ]) / nrow(credict_tot4)
# 0.639

nrow(credict_tot4[which((credict_tot4['BILL_BAL_PER1'] > 0.5) &
                          (credict_tot4['BILL_BAL_PER1'] <= 1)), ]) / nrow(credict_tot4)
# 0.053

nrow(credict_tot4[which((credict_tot4['BILL_BAL_PER1'] > 0.3) &
                          (credict_tot4['BILL_BAL_PER1'] <= 0.5)), ]) / nrow(credict_tot4)
# 0.030

nrow(credict_tot4[which((credict_tot4['BILL_BAL_PER1'] >= 0) &
                          (credict_tot4['BILL_BAL_PER1'] <= 0.3)), ]) / nrow(credict_tot4)
#  0.27

nrow(credict_tot4[which(credict_tot4['BILL_BAL_PER1'] < 0), ]) / nrow(credict_tot4)
# 0.002

credict_tot4_BK <- credict_tot4 %>% 
  # filter(BILL_BAL_PER1 <= 0.5, BILL_BAL_PER1 >= 0) %>%
  filter(BILL_BAL_PER1 > -1, BILL_BAL_PER1 < 7) %>%
  dplyr::group_by(BILL_BAL_PER1) %>%
  dplyr::summarise(
    BILL_BAL_PER_CNT = n(),
    SQRT_PAYX_MEAN = mean(SQRT_PAYX)
  )

fig1 <- plot_ly(x = ~credict_tot4_BK$BILL_BAL_PER1, # 0-0.4, 0.4-6.1, others
               y = ~credict_tot4_BK$BILL_BAL_PER_CNT, 
               type = 'scatter',
               mode = 'lines',
               fill = 'tozeroy',
               )

fig2 <- plot_ly(x = ~credict_tot4_BK$BILL_BAL_PER1, # 0-0.4, 0.4-6.1, others
                y = ~credict_tot4_BK$SQRT_PAYX_MEAN, 
                type = 'scatter',
                mode = 'lines',
                fill = 'tozeroy',
)


fig <- subplot(fig1, fig2)
fig <- fig %>% layout(xaxis = list(title = 'BILL_BAL_PER1'),
                      yaxis = list(title = 'COUNT'))

fig

# fig1 <- plot_ly(economics, x = ~date, y = ~unemploy)
# fig1 <- fig1 %>% add_lines(name = ~"unemploy")
# fig2 <- plot_ly(economics, x = ~date, y = ~uempmed)
# fig2 <- fig2 %>% add_lines(name = ~"uempmed")
# fig <- subplot(fig1, fig2)
# 
# fig



credict_tot5 <- credict_train_cleaned
credict_tot5 <- credict_tot5 %>%
  mutate(BILL_BAL_PER = round((BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6)/ LIMIT_BAL, 2)) %>%
  mutate(SQRT_PAYX = ceiling(sqrt((PAY_1+2)^2 + (PAY_2+2)^2 + (PAY_3+2)^2 + (PAY_4+2)^2 + (PAY_5+2)^2 + (PAY_6+2)^2))) %>%
  filter(default == 1)

credict_tot5 <- credict_tot5 %>% 
  # filter(BILL_BAL_PER <= 0.5, BILL_BAL_PER > 0) %>%
  dplyr::group_by(SQRT_PAYX) %>%
  dplyr::summarise(
    BILL_BAL_PER_MEAN = mean(BILL_BAL_PER),
  )

fig <- plot_ly(credict_tot5, x = ~BILL_BAL_PER_MEAN)
fig <- fig %>% add_trace(y = ~SQRT_PAYX, name = 'trace 0', type='scatter', mode = 'markers')
fig
# 进一步说明了PAY_X^2和越高 相对其开销总和大于BAL的概率也越高


credict_tot5_BK <- credict_train_cleaned
credict_tot5_BK <- credict_tot5_BK %>%
  mutate(BILL_BAL_PER = round((BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6)/ LIMIT_BAL, 2)) %>%
  mutate(SQRT_PAYX = ceiling(sqrt((PAY_1+2)^2 + (PAY_2+2)^2 + (PAY_3+2)^2 + (PAY_4+2)^2 + (PAY_5+2)^2 + (PAY_6+2)^2)))

credict_tot5_BK <- credict_tot5_BK %>% 
  dplyr::group_by(SQRT_PAYX) %>%
  dplyr::summarise(
    BILL_BAL_PER_MEAN = mean(BILL_BAL_PER),
  )

fig <- plot_ly(credict_tot5_BK, x = ~SQRT_PAYX)
fig <- fig %>% add_trace(y = ~BILL_BAL_PER_MEAN, name = 'trace 0',mode = 'lines')
fig


#### TODO: 去除outliers mean趋势异常 等

