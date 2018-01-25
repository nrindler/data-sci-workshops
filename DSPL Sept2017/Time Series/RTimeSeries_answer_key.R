###################################################
#HANDS-ON EXERCISE: TIME SERIES ANALYSIS
###################################################

#' Time Series + Forecasting Presentation
#' Oct 2017
#' Created by DataViz My 
#' @author Nadav Rindler

# SET working directory
setwd("C:/Users/nadav.rindler/Documents/DataViz/DSPL Sept2017/Time Series")

# LOAD required packages

#install.packages("dplyr")
#install.packages("RSQLite")
#install.packages("dygraphs")
#install.packages("xts")
#install.packages("forecast")
#install.packages("zoo")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("tidyr")

library(dplyr)
library(RSQLite)
library(dygraphs)
library(xts)
library(forecast)
library(zoo)
library(readxl)
library(ggplot2)
library(tidyr)


### DYGRAPH - INDONESIA FIRE DATA ###

# GET DATA FROM SQL DATABASE
db <- dbConnect(SQLite(),dbname="../Advanced R/IDNFire.sqlite")

# Get # fires by day
fire_day <- dbGetQuery(db, "SELECT date, COUNT(*) num_fires FROM fire group by 1 order by 1")
fire_day$date <- as.Date(fire_day$date)

# Create dygraph of time series
?xts
fday <- xts(fire_day[,-1], order.by=fire_day$date) #Exclude first column, which is the date
names(fday) <- "num_fires"

dygraph(fday) 


### ADVANCED - Dygraph by Month and Island ###

# Get # fires by ISLAND and month

# Write a SQL Query to sum the number of fires by year and month
    #Hint: in SQLite you can convert a day to the first day of that month using: date(date,'start of month')
    #Hint: Nested query
    #Make sure to exclude observations where ISLAND equals " "
query <- paste("SELECT ym,",
                "ISLAND,",
                "COUNT(*) as num_fires",
                "from (",
                "SELECT date(date,'start of month') as ym,",
                "ISLAND",
                "FROM fire) a",
                "where ISLAND is not null and ISLAND != ' '",
                "group by 1,2", sep=" ")
query

fire_month <- dbGetQuery(db, query)
fire_month$ym <- as.Date(fire_month$ym)

#Spread data
fire_month <- fire_month %>% 
  spread(key=ISLAND, value=num_fires)

# Create dygraph of fires by island and month
fmonth <- xts(fire_month[,-1], order.by=fire_month$ym)
dygraph(fmonth) 


### MOVING AVERAGE ###
  
?rollapply
mavg10 = rollapply(fire_day$num_fires, width=10, by=1, FUN=mean, partial=T, align = "right")
mavg20 = rollapply(fire_day$num_fires, width=20, by=1, FUN=mean, partial=T, align = "right")
mavg50 = rollapply(fire_day$num_fires, width=50, by=1, FUN=mean, partial=T, align = "right")

fire_day <- cbind(fire_day,
                  mavg10,
                  mavg20,
                  mavg50)

fday <- xts(fire_day[,-1], order.by=fire_day$date)

dygraph(fday)


### Time Series with Linear Regression ###


# Read XLSX file
api = read_xlsx("../Course topics/4.0 - Statistics and data modelling/02.2 API_Klang.xlsx")

# Rename columns
names(api) = c("date","api")

# Set date to date format
class(api$date) #What is POSIXt?
api$date = as.Date(api$date, format="%Y-%m-%d")

# Create many different time factors
api$day_count = as.numeric(api$date - min(api$date))
api$week_count = floor(api$day_count/7.0)
api$month_count = floor(api$day_count/30.5)
api$year_count = as.numeric(format(api$date, format="%Y")) - as.numeric(format(min(api$date), format="%Y"))

api$month = as.numeric(format(api$date, format="%m"))
api$season = floor((api$month-1) / 3)
api$weekday = as.numeric(format(api$date, format = "%w"))
api$week = as.numeric(format(api$date, format = "%W"))
api$year = as.numeric(format(api$date, format="%Y"))

api = api %>% 
  left_join(fire_day, by="date") %>% 
  mutate(idn_fires = ifelse(is.na(num_fires),0,num_fires)) %>% 
  select(-num_fires)

# Only model on api dates starting in 2014 because of missing Indonesia fire data
api_model = api[api$date>=as.Date("2014-01-01"),]

# Two dates have missing moving average data -- fill nulls by pulling down last non-null value
api_model$mavg10 <- na.locf(api_model$mavg10) 
api_model$mavg20 <- na.locf(api_model$mavg20) 
api_model$mavg50 <- na.locf(api_model$mavg50) 


# Create indicator variable to account for Spike in API around March 14, 2014
api_model$event = ifelse(api_model$date==as.Date("2014-03-13")|
                           api_model$date==as.Date("2014-03-14")|
                           api_model$date==as.Date("2014-03-15"),1,0)



#Split data into Training and Test sets to evaluate forecast accuracy
trainData = api_model[api_model$date<as.Date("2015-08-01"),]
testData = api_model[api_model$date>=as.Date("2015-08-01"),]

# Create linear model:

lm.api = lm(api ~ . - date, data = trainData)
summary(lm.api)

step.api = step(lm.api, direction="backward")
summary(step.api)

fcast = predict(step.api, newdata=testData)

# Merge fitted values and prediction onto data set
api_model$model = c(step.api$fitted.values, fcast)

# Plot fitted model
cutoff = as.numeric(as.Date("2015-08-01", origin = as.Date("1970-01-01")))
api_model$date = as.Date(api_model$date, origin = as.Date("1970-01-01"))
ggplot(api_model) +
  geom_rect(aes(xmin=as.Date("2015-08-01"), 
            xmax=max(api_model$date), 
            ymin=-Inf, ymax=Inf), fill="light blue", alpha=0.5) +
  geom_line(aes(x=date, y=api), color="black", size=1) +
  geom_line(aes(x=date, y=model), color="red", size=1) +
  labs(main="Klang Valley API", sub="Sept 2013 - Oct 2015")
  


# How does the forecast perform?

# Metric for evaluating forecast performance: Mean Absolute Percentage Error
# Formula: Absolute value of [ (Actual - Forecast) / Actual ]
api_model$abs_pct_error = 100*abs((api_model$api - api_model$model)/api_model$api)

mean(unlist(api_model[api_model$date>=as.Date("2015-08-01"),"abs_pct_error"]))

# Print out Actuals, Forecast, and APE for forecasted months
View(api_model[api_model$date>=as.Date("2015-08-01"),c("date","api","model","abs_pct_error")])

# Which additional variables do you think might be predictive of Klang Valley API?

#Disconnect from database
dbDisconnect(db)

