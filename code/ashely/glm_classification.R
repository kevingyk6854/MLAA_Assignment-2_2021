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


# 1. Data ETL
AT2_db <- read_csv(here::here("data", "AT2_credit_train.csv"))
str(AT2_db)

# default proportions
prop.table(table(AT2_db$default)) #0.7347734(N) vs. 0.2652266(Y) 

#count NAs
AT2_db %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  t() %>% 
  data.frame(count=.) %>% 
  tibble::rownames_to_column("column")

#remove NAs and unnecceary value
AT2_db <- AT2_db %>%
  drop_na(SEX)
AT2_db <- AT2_db[AT2_db$SEX !="2113" & AT2_db$LIMIT_BAL != "-99" & AT2_db$AGE != "157", ]

#remove non-numeric column
data <- subset(AT2_db, select = -c(ID))


# check the distribution of each variable
for (var in names(data)){
    if (!var %in% c("ID")){
      b <- data %>% 
        ggplot(aes_string(var)) +
        geom_bar(aes(y=..count..), fill="cornflowerblue", color="black", alpha=0.7) +
        ggtitle(paste0("Bar Plot of", "\n", var))
      print(b)
    }
}

# review correlations
D <- data %>% 
    select(-c(default)) %>% 
    select_if(is.numeric) %>% 
    cor()

head(round(D,2))

corrplot(D, method = "circle")