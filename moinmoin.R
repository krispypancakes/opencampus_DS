# Load
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
sales <- read.csv("./OpenCampusSales.csv",header = TRUE, sep = ";")
weather <- read.csv2("./OpenCampusWetter.csv",header = TRUE, sep = ";")

data = merge(sales, weather)

data$Sales = as.double((data$Sales))
data$Date = as.Date(data$Date)
data$weekday = as.factor(wday(data$Date))
data$month = as.factor(month(data$Date))
data$year = as.factor(year(data$Date))
data$Date = as.Date(data$Date)
data$Branch = as.factor(data$Branch)
# data$wwav = as.factor(data$wwav)
# model estimation

# beste MOdel bei year + weekday
mod <- lm(Sales ~ year + weekday, data)
summary(mod)


ggplot(data) +
  geom_point(aes(x  = Date, y = Sales),stat = 'Identity', fill = 'orange')
