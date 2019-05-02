
rm(list = ls())

setwd("~/Documents/Data_Science_sesh/opencampus_DS")
library(tidyverse)


sales <- read_csv2("OpenCampusSales.csv")
weather <- read_csv2("OpenCampusWetter.csv")

s_w <- cbind(weekdays.Date(weather$Date, abbreviate = F), weather, sales$Sales)
names(s_w) <- c("weekday", "date", "avg_clouds", "avg_temp", "avg_wind", "code", "sales")
head(s_w)

sales_days <- s_w %>%
                group_by(weekday) %>%
                summarise(avg_sales = mean(sales), sd_sales = sd(sales))

head(sales_days)

sales_days$weekday <- factor(sales_days$weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))

sales_weekday <- ggplot(sales_days) +
  geom_bar(aes(x = weekday, y = avg_sales), stat = "identity", fill = "green", alpha = 0.5 ) +
  geom_errorbar(aes(x = weekday, ymin = avg_sales - sd_sales, ymax = avg_sales + sd_sales), col = "blue", alpha = 0.5) +
  ggtitle("Average sales per weekday")



