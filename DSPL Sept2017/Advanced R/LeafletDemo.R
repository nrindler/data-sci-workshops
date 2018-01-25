#' Advanced Topics in R Session
#' Mapping in R with Leaflet Package
#' Sept 2017
#' Created by: Nadav Rindler

# SET working directory
setwd("C:/Users/nadav.rindler/Documents/DataViz/DSPL Sept2017/Advanced R")

#GENERATE SAMPLE LOCATION DATA FROM R'S BUILT-IN US STATES DATA SET
data(state)
us_states <- cbind(long=state.center$x,
                           lat=state.center$y,
                           area=state.area)
us_states <- cbind(state=state.abb,as.data.frame(us_states))

#VIEW DATA
head(us_states)

#MAP WITH LEAFLET PACKAGE

#install.packages("leaflet")
library(leaflet)

#CREATE MAP
m <- leaflet(data=us_states) %>% #Call leaflet function and specify data set
  addTiles() %>%  # Set map background -- the default are OpenStreetMap map tiles
  addMarkers(lng=~long, lat=~lat, #Set lat/long coordinates from US States data set
                                    #NOTE: Must put "~" before variable name or else must specify dataset, e.g. us_states$long
                    popup=~as.character(state)) #display state abbreviations when you click the marker
m  # Print map


#SET ICON COLOR BASED ON STATE AREA (SMALL/MEDIUM/LARGE)
  #Using Leaflet's built-in "awesome" plug-in

#View distribution of US states' area
quantile(us_states$area)

#Create variable for icon color based on state area
us_states$color <- ifelse(us_states$area<38000,"green",
                          ifelse(us_states$area<83000,"orange","red"))

#create custom icons
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = us_states$color
)

#RE-GENERATE MAP
m <- leaflet(data=us_states) %>% #Call leaflet function and specify data set
  addTiles() %>%  # Set map background -- the default are OpenStreetMap map tiles
  addAwesomeMarkers(lng=~long, lat=~lat, #Set lat/long coordinates from US States data set
    #NOTE: function is now "addAwesomeMarkers"
                    popup=~as.character(state), #Displays state abbreviations on click
                    icon=icons) #add customized icons with color based on state area
m  # Print map
