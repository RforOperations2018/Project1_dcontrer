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
token <- "b8jsLEt63CZq5qAV4bvMXsqLi"
selectDat <- read.socrata("https://data.cityofchicago.org/resource/3uz7-d32j.json?$select=_primary_decsription, date_of_occurrence",
                          app_token = token)

# generate unique list of crimes for use in input selectors
crimes <- as.character(unique(selectDat$X_primary_decsription))
remove(selectDat)

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
                   options = list(placeholder = 'Select crime(s)')),
    
    # Domestic incidents (y/n)
    selectizeInput("domSelect", 
                   "Limit to Domestic Incidents?", 
                   choices = c("TRUE", "FALSE"), 
                   multiple = FALSE,
                   selected = "TRUE"),
    
    # Time of day
    radioButtons("timeSelect", 
                 "Time of Day:",
                 choices = c("morning", "afternoon", "evening", "night", "all"),
                 selected = "all"),
    
    # Date range
    dateRangeInput("dateSelect",
                   "Date Range:", 
                   start = Sys.Date()-365, end = Sys.Date()-335, 
                   min = "2001-01-01", max = Sys.Date()-7, 
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
    
    # When no crimes selected
    if (length(input$crimeSelect) == 0 ) {
      crime <- read.socrata(paste0("https://data.cityofchicago.org/resource/c4ep-ee5m.json?$where=date >= '", input$dateSelect[1], "T00:00:00' AND date <= '", input$dateSelect[2], "T23:59:59' AND domestic = '", input$domSelect, "'"), 
                            app_token = token)
      
      # When one crime selected
    } else if (length(input$crimeSelect) == 1) {
      crime <- read.socrata(paste0("https://data.cityofchicago.org/resource/c4ep-ee5m.json?$where=date >= '", input$dateSelect[1], "T00:00:00' AND date <= '", input$dateSelect[2], "T23:59:59' AND primary_type= '", input$crimeSelect, "' AND domestic = '", input$domSelect, "'"), 
                            app_token = token)
      
      # When multiple crimes selected
    } else {
      primary_desc_q <- paste0(input$crimeSelect, collapse = "' OR primary_type= '")
      crime <- read.socrata(paste0("https://data.cityofchicago.org/resource/c4ep-ee5m.json?$where=date >= '", input$dateSelect[1], "T00:00:00' AND date <= '", input$dateSelect[2], "T23:59:59' AND (primary_type= '", primary_desc_q, "') AND domestic = '", input$domSelect, "'"), 
                            app_token = token)
    }
    
    # Subset and rename data
    crime <- select(crime, primary_type, location_description, arrest, date)
    colnames(crime) <- c("type", "locType", "arrest", "date")
    
    # Create separate date and time columns
    crime$date <- as.factor(crime$date)
    crime$time <- format(as.POSIXct(strptime(crime$date,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
    crime$date <- mdy(format(as.POSIXct(strptime(crime$date,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m/%d/%y"))
    
    crime$timeDay <- as.factor(ifelse(crime$time > "04:59:00" & crime$time <= "11:59:00", "morning",
                                      ifelse(crime$time > "11:59:00" & crime$time <= "16:59:00", "afternoon",
                                             ifelse(crime$time > "16:59:00" & crime$time <= "21:59:00", "evening", "night"))))
    # Clean data
    crime$type <- as.factor(tolower(crime$type))
    crime$locType <- as.factor(tolower(crime$locType))
    crime$arrest <- ifelse(crime$arrest == TRUE, 1, 0)
    
    if (input$timeSelect != "all") {
      crime <- subset(crime, timeDay %in% input$timeSelect)
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
    updateSelectInput(session, "domSelect", selected = "FALSE")
    updateDateRangeInput(session, "dateSelect", start = Sys.Date() - 365, end = Sys.Date() - 335
    )
    showNotification("You reset the filters. Gooooood Job", 
                     type = "message", 
                     duration = 3, 
                     closeButton = F)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)