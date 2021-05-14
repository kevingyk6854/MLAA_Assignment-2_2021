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

library(caret)
library(parallel)
library(doParallel)
library(gbm)
library(corrplot)
library(magrittr)
library(pdp)
library(lattice)
library(Hmisc)

# 1. Data ETL
AT2_db <- read_csv(here::here("data", "AT2_credit_train.csv"))
AT2_db[sample(nrow(AT2_db),10),]

#find missing or anomalous data

describe(AT2_db) 
#3 missing value in SEX & anomalous data: SEX has category 2113(undocumented), 
                                      #   LIMIT_BAL less than 0                       ------------ more investigate
                                      #   EDUCATION category 0 is undocumented
                                      #   MARRIAGE has a label 0 that is undocumented
                                      #   AGE that over 150 is very unlikely               ------- more investigate
                                      #   PAY_X they all present an undocumented label -2 & 0. ----more investigate

# default proportions
round(prop.table(table(AT2_db$default)),4)    #0.7348(N) vs. 0.2652(Y) 




