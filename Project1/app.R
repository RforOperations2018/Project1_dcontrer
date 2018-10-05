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
library(jsonlite)

# read in app token
token <- jsonlite::fromJSON("token.json")$token

# generate unique list of crimes for use in input selectors
selectDat <- read.socrata("https://data.cityofchicago.org/resource/3uz7-d32j.json?$select=_primary_decsription, date_of_occurrence",
                          app_token = token)
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
    
    # Domestic incidents (T/F)
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
                   start = Sys.Date()-365, end = Sys.Date()-183, 
                   min = "2001-01-01", max = Sys.Date()-7, 
                   format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                   language = "en", separator = " to ", width = NULL),
    
    # Action button to reset filters, keeping original icon b/c works well
    actionButton("reset", "Reset Filters", icon = icon("refresh")) 
  )
)

# tab layout for plots
body <- dashboardBody(tabItems(
  
  # Create page 1 (crimes)
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
                                 in teal and non-arrests in pink.&nbsp;</em></p>"),
                            plotlyOutput("plot_line")))
            )
                   ),
  
  # Create page 2 (location)
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
  
  # Create page 3 (table)
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
    crime <- read.socrata(paste0("https://data.cityofchicago.org/resource/3uz7-d32j.json?$where=date_of_occurrence between '2017-10-10T12:00:00' and '2018-01-10T14:00:00'&_primary_decsription=", input$crimeSelect, "&ARREST=Y&DOMESTIC=", input$domSelect), 
                          app_token = token)
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
