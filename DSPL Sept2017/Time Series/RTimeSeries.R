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

# STEP 1 - GET NUMBER OF INDONESIA FIRES BY DAY
fire_day <- # YOUR CODE HERE
            # HINT: ?dbGetQuery()
            # HINT: Use code and query from Advanced R exercise
            # HINT: ORDER BY DATE!
  
# STEP 2 - CONVERT date field to "Date" format
fire_day$date <- #YOUR CODE HERE

# Check that date conversion worked:
class(fire_day$date)

# Convert to Time Series object
?xts
fday <- xts(fire_day[,-1], order.by=fire_day$date) 
      #Exclude first column, which is the date

# Set name of time series field
names(fday) <- "num_fires"

# Create dygraph of time series
dygraph(fday) 


### ADVANCED - Dygraph by Month and Island ###

# Get # fires by ISLAND and month

# Write a SQL Query to sum the number of fires by year and month
    #Hint: in SQLite you can convert a day to the first day of that month using: date(date,'start of month')
    #Hint: Nested query
    #Make sure to exclude observations where ISLAND equals " "


### MOVING AVERAGE ###
  
# STEP 3 - CREATE 10, 20, AND 50-DAY MOVING AVERAGE FOR # FIRES
?rollapply
mavg10 = rollapply(fire_day$num_fires, width=10, by=1, FUN=mean, partial=T, align = "right")
mavg20 = #YOUR CODE HERE
mavg50 = #YOUR CODE HERE

length(mavg10) #What happened to the first 9 observations in the moving average series?

# MERGE MOVING AVERAGES ONTO FIRES DATA SET
fire_day <- cbind(fire_day, mavg10, mavg20, mavg50)

# STEP 4 - CREATE TIME SERIES OBJECT
  #YOUR CODE HERE
  #HINT: COPY CODE FROM ABOVE

# STEP 5 - CREATE DYGRAPH
  #HINT: COPY CODE FROM ABOVE
  #HINT: SEE 'dygraphs.R' SCRIPT
  #Advanced: Add ?dyRangeSelector
  #Advanced: Set series colors to be black, red, blue, and green
  


### Time Series with Linear Regression ###


# Read Klang API XLSX file from Statistics & Data Modeling session
api = read_xlsx("../Course topics/4.0 - Statistics and data modelling/02.2 API_Klang.xlsx")

# Rename columns
names(api) = c("date","api")

# Set date to date format
class(api$date) #What is POSIXt?
api$date = as.Date(api$date)

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

# How else can we represent time series variables as predictors?
    # What about indicators? (1/0)

# Merge number of daily Indonesia fires onto Klang API data set 
api = api %>% 
  left_join(fire_day, by="date") %>%  #Left join -- just like SQL
  mutate(idn_fires = ifelse(is.na(num_fires),0,num_fires)) %>% #Fill NA values with 0
  select(-num_fires)

# STEP 6 - Subset data to start on Jan 1, 2014 because of missing Indonesia fire data
  #Hint: api$date<=as.Date("2014-01-01")
api_model = api#YOUR CODE HERE

# STEP 7 - Two dates have missing moving average data -- fill nulls by pulling down last non-null value
api_model$mavg10 = na.locf(api_model$mavg10) 
api_model$mavg20 = #YOUR CODE HERE
api_model$mavg50 = #YOUR CODE HERE 

# STEP 8 - Split data into Training and Test sets to evaluate forecast accuracy
  #Training Data < Aug 1, 2015
  #Test Data >= Aug 1, 2015
  #HINT: See STEP 6
trainData = api_model#YOUR CODE HERE
testData = api_model#YOUR CODE HERE

  # How is splitting our data into test and training sets *different* for Time Series
    # analysis than for typical statistical models?

# STEP 9 - RUN LINEAR REGRESSION
  # DEPENDENT VARIABLE: api
  # INDEPENDENT VARIABLES: all other variables
  # HINT: ?lm
lm.api = #lm(YOUR INPUT HERE)
summary(lm.api)

# STEP 10 - RUN STEPWISE REGRESSION TO IDENTIFY CONTRIBUTING VARIABLES
  # HINT: ?step
  # SET direction="backward"
step.api = #step(YOUR INPUT HERE)
summary(step.api)

# Predict API for the test data set:
fcast = predict(step.api, newdata=testData)

# Merge fitted values and prediction onto data set
api_model$model = c(step.api$fitted.values, fcast)

# Plot fitted model
ggplot(api_model) +
  labs(main="Klang Valley API", sub="Sept 2013 - Oct 2015") +
  geom_rect(aes(xmin=as.Date("2015-08-01"), 
            xmax=max(api_model$date), 
            ymin=-Inf, ymax=Inf), fill="light blue", alpha=0.5) +
  geom_line(aes(x=date, y=api), color="black", size=1) +
  geom_line(aes(x=date, y=model), color="red", size=1)
  


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

