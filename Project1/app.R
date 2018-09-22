# Assignment: Project 1
# Class: R Shiny for Operations Management
# Author: Dominic Contreras
# Date: September 21, 2018

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(lubridate)
library(shinyWidgets)

# Three charts: one showing 

# read in data
crime <- read.csv("http://www.sharecsv.com/dl/d4ece4993a52b02efb08a5ac800123f2/chicagoCrime.csv", header = T, sep = ",")
crime <- select(crime, PRIMARY.DESCRIPTION, LOCATION.DESCRIPTION, ARREST, DOMESTIC, DATE..OF.OCCURRENCE)
colnames(crime) <- c("type", "locType", "arrest", "domestic", "date")

# format time and date columns
crime$time <- format(as.POSIXct(strptime(crime$date,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
crime$date <- mdy(format(as.POSIXct(strptime(crime$date,"%m/%d/%Y %H:%M",tz="")) ,format = "%m/%d/%y"))

# create time of day column
crime$timeDay <- as.factor(ifelse(crime$time > "04:59:00" & crime$time <= "11:59:00", "morning",
                                  ifelse(crime$time > "11:59:00" & crime$time <= "16:59:00", "afternoon",
                                         ifelse(crime$time > "16:59:00" & crime$time <= "21:59:00", "evening", "night"))))

# clean data
crime$type <- as.factor(tolower(crime$type))
crime$locType <- as.factor(tolower(crime$locType))
crime$arrest <- ifelse(crime$arrest == "Y", 1, 0)
crime$domestic <- ifelse(crime$domestic == "Y", 1, 0)
pdf(NULL)