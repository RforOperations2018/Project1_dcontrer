# Assignment: HW4
# Class: R Shiny for Operations Management
# Author: Dominic Contreras
# Date: October 4, 2018

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(lubridate)
library(shinyWidgets)
library(RSocrata)

# read in app token
# AHHH NOOO, please save this as an enviornmental variable or in a separate file
token <- "b8jsLEt63CZq5qAV4bvMXsqLi"
selectDat <- read.socrata("https://data.cityofchicago.org/resource/3uz7-d32j.json?$select=_primary_decsription, date_of_occurrence",
                          app_token = token)

# generate unique list of crimes for use in input selectors
crimes <- as.character(unique(selectDat$X_primary_decsription))

# generate min and max dates for date range selector input
dateMin <- range(selectDat$date_of_occurrence, na.rm = T)[1]
dateMax <- range(selectDat$date_of_occurrence, na.rm = T)[2]
remove(selectDat)

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
    menuItem("Crime Plots", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Location Data", icon = icon("location-arrow"), tabName = "loc"),
    menuItem("Download Data", icon = icon("download"), tabName = "table"),
    # Crime select
    selectizeInput("crimeSelect", 
                   "Crimes:", 
                   choices = sort(crimes), 
                   multiple = TRUE,
                   selected = crimes[1:5],
                   options = list(placeholder = 'Select crime(s)',
                                  maxItems = 4)),
    # Domestic incidents (y/n)
    selectizeInput("domSelect", 
                   "Limit to Domestic Incidents?", 
                   choices = c("Y", "N"), 
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
                   start = dateMin, end = dateMax, 
                   min = dateMin, max = dateMax, 
                   format = "mm-dd-yyyy", startview = "month", weekstart = 0,
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
                            plotlyOutput("plot_line")))
            )
                   ),
  tabItem("loc",
          fluidRow(
            tabBox(width = 12, height = "800px",
                   # Layout and description of tab 3
                   tabPanel("Location of Crimes",
                            HTML("<p><em>The graph below shows the 10 most frequent locations of the crime(s) selected for the time period selected. </p>
                                 <p>*Note that this chart is most informative when only 1 crime is selected.&nbsp;</em></p>"),
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
# Define server logic
server <- function(input, output, session = session) {
  crimeInput <- reactive({
    # No Crimes Selected
    if (length(input$crimeSelect) == 0 ) {
      # I didn't fix this one either
      crime <- read.socrata(paste0("https://data.cityofchicago.org/resource/3uz7-d32j.json?$where=date_of_occurrence between '2017-10-10T12:00:00' and '2018-01-10T14:00:00'&ARREST=Y&DOMESTIC=", input$domSelect), 
                            app_token = token)
    # One Crime Selected
      # I did not fix this one
    } else if (length(input$crimeSelect) == 1) {
      crime <- read.socrata(paste0("https://data.cityofchicago.org/resource/3uz7-d32j.json?$where=date_of_occurrence between '2017-10-10T12:00:00' and '2018-01-10T14:00:00'&_primary_decsription=", input$crimeSelect, "&ARREST=Y&DOMESTIC=", input$domSelect), 
                            app_token = token)
    # Multiple Crimes Selected
    } else {
      # I did get this one to work
      # For teststing: 
      # input$crimeSelect <- crimes[1:5]
      primary_desc_q <- paste0(input$crimeSelect, collapse = "' OR primary_type= '")
      crime <- read.socrata(paste0("https://data.cityofchicago.org/resource/c4ep-ee5m.json?$where=date >= '2017-10-10T12:00:00' AND date <= '2018-01-10T14:00:00' AND (primary_type= '", primary_desc_q, "') AND arrest = TRUE AND domestic = '", input$domSelect, "'"), 
                            app_token = token)
    }
    
    return(crime)
  })
  # Reactive data for plot 2
  # The melting and stuff doesn't really have to be done in a reactive function if you're only using it once.
  mcInput <- reactive({
    crimeInput() %>% 
      count(type, arrest) %>%
      group_by(type) %>%
      mutate(freq = n / sum(n))
  })
  # Reactive data for plot 3
  locInput <- reactive({
    crimeInput() %>% 
      group_by(type, locType) %>%
      tally() %>%
      top_n(10)
  })
  # Plot 1 - Crimes by Frequency
  output$plot_total <- renderPlotly({
    dat <- crimeInput()
    ggplotly(
      ggplot(data = dat, aes(x = type, fill = type,
                             text = paste0("<b>Total Crimes: ", ..count.., "</b>"))) + 
        geom_histogram(stat = "count") +
        labs(y = "Count",
             title = "Number of Reports by Crime Type",
             x = NULL) +
        theme(plot.title = element_text(family = 'Helvetica',  
                                        color = '#181414', 
                                        face = 'bold', 
                                        size = 18, 
                                        hjust = 0)) +
        theme(axis.title.y = element_text(family = 'Helvetica', 
                                          color = '#181414', 
                                          face = 'bold', 
                                          size = 12, 
                                          hjust = 0)) +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) + 
        guides(color = FALSE)
      , tooltip = "text")
  })
  # Plot 2 - Percent arrests by Crime
  output$plot_line <- renderPlotly({
    dat <- mcInput()
    # Melting and everything can go here. No points off, just telling you
    ggplotly(
      ggplot(data = dat, aes(x = type, y = freq*100, fill = reorder(arrest, arrest), 
                             text = paste0("<b>", type, "</b> ",
                                           "<br>Total Crimes: ", n, "</b> ",
                                           ifelse(dat$arrest == 1, "<br>Percent Arrest: ", "<br>Percent No Arrest: "),
                                           round(freq, digits = 2)*100))) +
        geom_bar(stat = "identity") +
        labs(x = NULL,
             y = "Proportion of Total",
             title = "Proportion of Crimes That Led to an Arrest") +
        theme(plot.title = element_text(family = 'Helvetica',  
                                        color = '#181414', 
                                        face = 'bold', 
                                        size = 18, 
                                        hjust = 0)) +
        theme(axis.title.y = element_text(family = 'Helvetica', 
                                          color = '#181414', 
                                          face = 'bold', 
                                          size = 12, 
                                          hjust = 0)) +
        theme(legend.position = "none") + 
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +  # tilts x-axis labels
        guides(color = FALSE)
      , tooltip = "text")
  })
  # Plot 3 - Location of Crimes
  output$plot_loc <- renderPlotly({
    dat <- locInput()
    ggplotly(
      ggplot(data = dat, aes(x = reorder(locType, n), y = as.numeric(n),
                             text = paste0("<b>", locType, "</b> ",
                                           "<br>Crimes: ", n, "</b>"))) + 
        geom_bar(stat = "identity") + 
        coord_flip() +
        facet_wrap(~type, ncol = 1, scales = "free") +
        labs(x = NULL,
             y = "Number of Reports",
             title = "Most Frequent Locations of Crimes") +
        theme(plot.title = element_text(family = 'Helvetica',  
                                        color = '#181414', 
                                        face = 'bold', 
                                        size = 18, 
                                        hjust = 0)) +
        theme(axis.title.x = element_text(family = 'Helvetica', 
                                          color = '#181414', 
                                          face = 'bold', 
                                          size = 12, 
                                          hjust = 0)) +
        theme(legend.title=element_blank()) +  # tilts x-axis labels
        guides(color = FALSE)
      , tooltip = "text")
  })
  # Downloadable crime datatable
  output$table <- DT::renderDataTable({
    subset(crimeInput(), select = colnames(crimeInput()))
  })
  # Total crimes infobox
  output$totalCrimes <- renderValueBox({
    cr <- crimeInput()
    num <- nrow(cr)
    valueBox(subtitle = "Total crimes during this timeframe", value = num, icon = icon("balance-scale"), color = "red")
  })
  # Percent arrests infobox
  output$pctSolved <- renderValueBox({
    cr <- crimeInput()
    num <- round(mean(cr$arrest), digits = 2)*100
    valueBox(subtitle = "Percent resulted in arrests", value = num, icon = icon("lock"), color = "blue")
  })
  # Most common crime infobox
  output$mostCommon <- renderValueBox({
    cr <- crimeInput()
    name <- names(sort(table(cr$type), decreasing = TRUE)[1])
    valueBox(subtitle = "Was the most common crime", value = name, icon = icon("fa fa-user-circle-o"), color = "green")
  })
  # URL bar update
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  # Make data downloadable and set default download name
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("crime-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(crimeInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "crimeSelect", selected = c(""))
    updateSelectInput(session, "timeSelect", selected = "all")
    updateSelectInput(session, "domSelect", selected = "No")
    updateDateRangeInput(session, "dateSelect", start = min(crime$date), end = max(crime$date)
    )
    showNotification("You reset the filters. Gooooood Job", 
                     type = "message", 
                     duration = 3, 
                     closeButton = F)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
