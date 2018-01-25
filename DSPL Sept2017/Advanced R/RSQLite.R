#' Advanced Topics in R Session
#' Connecting R to SQL Database
#' Sept 2017
#' Created by: Nadav Rindler

# SET working directory
setwd("C:/Users/nadav.rindler/Documents/DataViz/DSPL Sept2017/Advanced R")

#install.packages("RSQLite")
#install.packages("DBI")
library(RSQLite)
library(DBI)

#Example using R's built-in CO2 dataset

#Connect to datasetsDb, which contains all of R's built-in data sets
db <- datasetsDb()

#View tables in database
dbListTables(db)

#Read in CO2 data set into R environment
co2 <- dbReadTable(db, "CO2")

#Query database -- print rows where CO2 concentration < 100
  #NOTE we're using SQL syntax here, not R's syntax!
dbGetQuery(db, "SELECT * FROM co2 WHERE conc < 100")

#Query R in-memory dataset -- print rows where CO2 concentration < 100
co2[co2$conc<100,]

#Disconnect from database
dbDisconnect(db)


### EXAMPLE - DENGUE DATA SET

#Create NEW database called "test.sqlite"
db <- dbConnect(SQLite(),dbname="dengue.sqlite")

#Import Dengue data set from CSV to R
dengue_data=read.csv("R Shiny App/Dengue_Cases_Malaysia_2011.csv", header = TRUE, sep = ",")

#Check data types for Dengue data columns
str(dengue_data)

#Write Dengue data set from R to SQLite table
dbWriteTable(db, name="dengue_data", value=dengue_data, row.names=F, overwrite=T)

#Sample query to check that the table was written
dbGetQuery(db, "SELECT DISTINCT NEGERI FROM dengue_data")

#Check number of rows in SQLite table
dbGetQuery(db, "SELECT COUNT(*) FROM dengue_data")

#Disconnect from database
dbDisconnect(db)





