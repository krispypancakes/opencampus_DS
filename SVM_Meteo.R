#check rmse of linear model and svm für eine (kategorielle) variable modellieren

setwd("~/Documents/Data_Science_sesh/opencampus_DS")
rm(list = ls())

library(readr)
library(dplyr)
#install.packages("e1071")
library(e1071)
#install.packages("Metrics")
library(Metrics)

sales <- read_csv2("OpenCampusSales_reform.csv")
weather <- read_csv2("OpenCampusWetter_reform.csv")

#combine both data sets
data <- sales %>%
  full_join(weather, by = c("Year", "Month", "Day")) # a categorial variable would be Day

data$Day <- as.factor(data$Day)

# split data into traning and test data
set.seed(1)

train <- sample(1:nrow(data), nrow(data)/2)

data.train <- data[train,]
data.test <- data[-train,]

# training the svm

simple_svm <- svm(Sales ~ Day, data.train)
summary(model_svm)
pred_simple <- predict(simple_svm, data.test)
rmse_simple <- rmse(data.test$Sales, pred_simple)


mult_svm <- tune(svm, Sales ~ Day, data = data.train,
                 ranges = list(epsilon = seq(0,1,0.2), cost = 2^(2:3)))

pred_mult <- predict(mult_svm$best.model, data.test)
rmse_mult <- rmse(data.test$Sales, pred)


if (rmse_mult < rmse_simple){
  print("it was worth the effort")
}






