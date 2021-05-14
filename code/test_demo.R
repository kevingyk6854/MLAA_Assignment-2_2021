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

df <- data.frame(id=rep(1:10, each=14),
                 tp=letters[1:14],
                 value_type=sample(LETTERS[1:3], 140, replace=TRUE),
                 values=runif(140))

df %>%
  group_by(id, tp) %>%
  summarise(all_mean = mean(values), 
            A_mean = mean(values[value_type=="A"]),
            value_count=sum(value_type == 'A'))


df = data.frame(A=1:10, B=sample(c('TT', 'TG', 'GG'), 10, replace=T))
vals=c('5')
col='A'
df %>% filter(!!sym(col) <= !!vals)