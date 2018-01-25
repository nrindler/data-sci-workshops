### Simple Exponential Smoothing ###

#install.packages("forecast")
#install.packages("readxl")
install.packages("ggplot2")

library(forecast)
library(readxl)
library(ggplot2)

# Set working directory
setwd("C:/Users/nadav.rindler/Documents/DataViz/DSPL Sept2017/Course topics/4.0 - Statistics and data modelling")

# Read XLSX file
api = read_xlsx("02.2 API_Klang.xlsx")

# Rename columns
names(api) = c("date","api")

# Set date to date format
class(api$date) #What is POSIXt?
api$date = as.Date(api$date, format="%Y-%m-%d")

# Quick plot
qplot(data=api, x=date, y=api, geom="line", main="Klang Valley API")


### Simple Exponential Smoothing ###

# Use forecast's ses() function:
es1 = ses(api$api, alpha=0.05, h=31) # h == # time units to forecast out

# What type of object is the es1 object?


# Explore the different objects stored within es1
names(es1)
  es1["method"]

# Try changing alpha -- alpha must be between 0 and 1
es2 = ses(api$api, alpha=0.15, h=31)
es3 = ses(api$api, alpha=0.95, h=31)

  # What does alpha do?


# Plot Simple Exponential Smoothing curves
plot(es1)
lines(es1$fitted, col="blue")
lines(es2$fitted, col="green")
lines(es3$fitted, col="red")
legend('topleft', c('Original Data','alpha=0.05', 'alpha=0.15', 'alpha=0.95'),
       col=c('black','blue', 'green', 'red'), lty=c(1,1,1))
