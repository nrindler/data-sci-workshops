###################################################
#HANDS-ON EXERCISE: INDONESIA FOREST FIRES DATA SET
###################################################

#' Advance Topics in R Presentation
#' Exercise: Data Science workflow in R
#' Sept 2017
#' Created by DataViz My 
#' @author Nadav Rindler


### INSTRUCTIONS ###
  #MOST OF THE CODE HAS BEEN PROVIDED FOR YOU. EACH NUMBERED STEP (1, 2, ETC.)
  #INDICATES WHERE YOU MUST INPUT INFORMATION TO COMPLETE THE SCRIPT


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

#STEP 1 - Check number of rows in SQLite table
dbGetQuery(db, "YOUR QUERY HERE") 
  #Input query to count number of rows in 'fire' table

#STEP 2 - Get the number of fires by day
fire_day <- dbGetQuery(db, "YOUR QUERY HERE")
  #Input query to count number of fires for each 'date' in 'fire' table
  #Hint: remember to 'group by' the date

#STEP 3 - Convert Date field to R's date format
fire_day$date <- #YOUR CODE HERE

#check that your code worked -- this should output "Date":
class(fire_day$date)

#STEP 4 - CHART number of fires by day
ggplot(data=, 
       aes(x=   ,
           y=   )) + #INPUT DATA SET NAME AND X and Y VARIABLE NAMES
  geom_line(size=1, color="red") + #Specify chart type (line), line color and width
  labs(title="INPUT TITLE HERE", 
       subtitle="2014-2016", 
       x="INPUT X AXIS LABEL HERE", 
       y="INPUT Y AXIS LABEL HERE") + #Set chart and axis labels
  scale_x_date(date_labels="%Y-%m", date_breaks="3 months") + #Format X axis labels (Quarterly dates)
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  #Rotate X axis labels 90 degrees (vertical alignment)

  #What patterns do you see in the 3 years of fire data?


#STEP 5 - Identify day with greatest # fires
    #YOUR CODE HERE

#STEP 6 - IMPORT data for day with greatest number of fires
fire <- dbGetQuery(db, "SELECT * FROM fire where date='SET DATE HERE'")
  #INPUT THE DATE WITH THE GREATEST NUMBER OF FIRES INTO THE SQL QUERY

#STEP 7 - View distribution of fire brightness
quantile() #call quantile() function on the BRIGHTNESS column in the fire data frame

#STEP 8 - Save 25th, 50th and 75th percentiles
pct25 <- quantile(fire$BRIGHTNESS)[2]
pct50 <- #YOUR INPUT HERE
pct75 <- #YOUR INPUT HERE

#CUSTOMIZE ICON COLOR

#STEP 9 - Color markers based on fire brightness (fire$BRIGHTNESS)
    #If BRIGHTNESS < 25th percentile value, "green"
    #Else if BRIGHTNESS < 50th percentile value, "yellow"
    #Else if BRIGHTNESS < 75th percentile value, "orange"
    #Else "red"
  ?ifelse()
  fire$color <- ifelse()#YOUR CODE INSIDE IFELSE STATEMENT

# Set icons
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

#STEP 10 - RUN LINEAR REGRESSION
  # DEPENDENT VARIABLE: BRIGHTNESS
  # INDEPENDENT VARIABLES: treecover, lc_ind, primary_fo, wdpa, legal, 
                        #  palm_oil, logging, pulpwood, moratorium, ISLAND
  # HINT: ?lm

reg <- #lm(YOUR INPUT HERE)
summary(reg)

#STEP 11 - RUN STEPWISE REGRESSION TO IDENTIFY CONTRIBUTING VARIABLES
  # HINT: ?step
  # SET direction="backward"
step_reg <- #step(YOUR INPUT HERE)
summary(step_reg)

#Which variables effect fire brightness?
#What do the coefficients mean?
#How might we represent the features differently?

#Disconnect from database
dbDisconnect(db)

