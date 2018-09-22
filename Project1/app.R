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
# title + data source notification
header <- dashboardHeader(title = "Chicago Crime Stats",
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "Source: Chicago Data Portal", 
                                                        icon = icon("fa fa-exclamation-triangle"))
                          )
)
# side bar layout 
sidebar <- dashboardSidebar(
  sidebarMenu( # toggle between plots and downloadable table
    id = "tabs",
    menuItem("Plots", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    # Crime select
    selectizeInput("crimeSelect", 
                   "Crimes:", 
                   choices = sort(unique(crime$type)), 
                   multiple = TRUE,
                   options = list(placeholder = 'Select crime(s)')),
    # Domestic incidents (y/n)
    selectizeInput("domSelect", 
                   "Limit to Domestic Incidents?", 
                   choices = c("Yes", "No"), 
                   multiple = FALSE,
                   selected = "No"),
    # Time of day
    radioButtons("timeSelect", 
                 "Time of Day:",
                 choices = c("morning", "afternoon", "evening", "night", "all"),
                 selected = "all"),
    # Date range
    dateRangeInput("dateSelect",
                   "Date Range:", 
                   start = min(crime$date), end = max(crime$date), 
                   min = min(crime$date), max = max(crime$date), 
                   format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                   language = "en", separator = " to ", width = NULL),
    # Action button to reset filters, keeping original icon b/c works well
    actionButton("reset", "Reset Filters", icon = icon("refresh")) 
  )
)
# tab layout for plots
body <- dashboardBody(tabItems(
  tabItem("plot",
          # Name tabs
          fluidRow(
            valueBoxOutput("totalCrimes"),
            valueBoxOutput("pctSolved"),
            valueBoxOutput("mostCommon")
          ),
          fluidRow(
            tabBox(width = 12,
                   # Layout and description of tab 1
                   tabPanel("Crimes by Frequency", 
                            HTML("<p><em>The graph below shows the frequency of a reported crime for the timeframe selected.&nbsp;</em></p>"),
                            plotlyOutput("plot_total")),
                   # Layout and description of tab 2
                   tabPanel("Percent Arrests by Crime",
                            HTML("<p><em>The graph below shows arrest rates for a reported crime for the time period selected. 
                                 The proportion of all crimes that resulted in arrests are shown 
                                 in light blue and non-arrests in dark blue.&nbsp;</em></p>"), 
                            plotlyOutput("plot_line")),
                   # Layout and description of tab 3
                   tabPanel("Location of Crimes",
                            HTML("<p><em>The graph below shows the 10 most frequent locations of the crimes selected (agregated) for the time period selected.&nbsp;</em></p>"),
                            plotlyOutput("plot_loc")))
            )
            ),
  # Layout of table
  tabItem("table",
          inputPanel(
            downloadButton("downloadData","Download Crime Data") # add button to download table as csv
          ),
          fluidPage(
            box(title = "Selected Crime Stats", DT::dataTableOutput("table"), width = 12))
  )
  )
  )
ui <- dashboardPage(header, sidebar, body)