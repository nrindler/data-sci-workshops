###################################################
#HANDS-ON EXERCISE: INDONESIA FOREST FIRES DATA SET
###################################################

#' Advance Topics in R Presentation
#' Exercise: Data Science workflow in R
#' Sept 2017
#' Created by DataViz My 
#' @author Nadav Rindler

# SET working directory
setwd("C:/Users/nadav.rindler/Documents/DataViz/DSPL Sept2017/Advanced R")

# LOAD required packages

#install.packages("leaflet")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("RSQLite")
library(leaflet)
library(dplyr)
library(ggplot2)
library(RSQLite)


#GET DATA FROM SQL DATABASE
db <- dbConnect(SQLite(),dbname="IDNFire.sqlite")

#LIST tables in database
dbListTables(db)

#QUERY fire data set
sample <- dbGetQuery(db, 'SELECT * FROM fire LIMIT 5')
str(sample)

#Check number of rows in SQLite table
dbGetQuery(db, "SELECT COUNT(*) FROM fire")

#Get # fires by day
fire_day <- dbGetQuery(db, "SELECT date, COUNT(*) num_fires FROM fire group by 1 order by 2 desc")
fire_day$date <- as.Date(fire_day$date)


#CHART - # fires by day
ggplot(data=fire_day, aes(x=date,y=num_fires)) + #Set data set, X, and Y variables
  geom_line(size=1, color="red") + #Specify chart type (line), line color and width
  labs(title="Indonesia Forest Fires", 
       subtitle="2014-2016", x="Date", y="Number of Fires") + #Set chart and axis labels
  scale_x_date(date_labels="%Y-%m", date_breaks="3 months") + #Format X axis labels (Quarterly dates)
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  #Rotate X axis labels 90 degrees (vertical alignment)

  #What patterns do you see in the 3 years of fire data?


#Identify day with greatest # fires
fire_day[1,]

#IMPORT data for day with greatest number of fires
fire <- dbGetQuery(db, "SELECT * FROM fire where date='2015-10-14'")

#View distribution of fire brightness
quantile(fire$BRIGHTNESS)

#Save 25th, 50th and 75th percentiles
pct25 <- quantile(fire$BRIGHTNESS)[2]
pct50 <- quantile(fire$BRIGHTNESS)[3]
pct75 <- quantile(fire$BRIGHTNESS)[4]

#CUSTOMIZE ICON COLOR

#color markers based on fire brightness
fire$color <- ifelse(fire$BRIGHTNESS<pct25,"green",
                     ifelse(fire$BRIGHTNESS<pct50,"yellow",
                            ifelse(fire$BRIGHTNESS<pct75,"orange",
                                   "red")))

#set icons
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = fire$color
)

# MAP
m <- leaflet(data=fire[1:500,]) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addAwesomeMarkers(lng=~LONGITUDE, lat=~LATITUDE, #fire lat/long coordinates
                    popup=~as.character(BRIGHTNESS), #Pop-up displays fire brightness score
                    icon=icons #add customized icons with color based on fire brightness score
  )
m  # Print the map


#MODEL 

#Regression formula
reg <- lm(BRIGHTNESS ~ treecover + lc_ind + primary_fo + wdpa +
            legal + palm_oil + logging + pulpwood + moratorium + 
            ISLAND,
          data = fire)
summary(reg)

#Use stepwise regression with backwards selection to identify contributing variables
step_reg <- step(reg, direction="backward")
summary(step_reg)

#Which variables effect fire brightness?
#What do the coefficients mean?
#How might we represent the features differently?

#Disconnect from database
dbDisconnect(db)

