#
# Shiny Application to Display COVID-19 data 
# Provided by the New York Times within the 
# GitHub repository at https://github.com/nytimes/covid-19-data
#
library(tidyverse)
library(ggplot2)
library(shiny)

rm(list=ls())

ui <- fluidPage(
    tags$head(tags$link(rel = 'stylesheet', 
                       type = 'text/css', 
                       href = 'app.css')),
    titlePanel("United States COVID-19 Data"),
    selectInput(inputId = 'desiredRegion', 
                label = 'Select region to view:', 
                choices = c('Michigan', 'Pennsylvania', 'Ohio'),
                selected = 'Michigan',
                multiple = FALSE,
                width = '50%'),
    plotOutput('plotCases'),
    plotOutput('plotDeaths')
)

server <- function(input, output, session) {
    
    dfCountyData <- read.csv(file = 'data/us-counties.csv')
    dfStateData <- read.csv(file = 'data/us-states.csv')
    dfStates <- as.list(dfCountyData %>% distinct(state) %>% arrange(state))
    txtDataDate <- Sys.Date()
    txtDataSource <- str_c("Data provided by the N.Y. Times [https://github.com/nytimes/covid-19-data] as of ", txtDataDate)
    observe({
        updateSelectInput(session, inputId = 'desiredRegion', choices = dfStates)
    })
    
    observeEvent(input$desiredRegion, {
        try({dfResults <- dfStateData %>%
            filter(state == input$desiredRegion) %>%
            mutate(day = as.numeric(date)) %>%
            select(day, cases, deaths)}, silent = TRUE)
        maxCases = max(dfResults$cases)
        maxDeaths = max(dfResults$deaths)
        maxDay = max(dfResults$day)
        minDay = min(dfResults$day)
        output$plotCases <- renderPlot({
            ggplot(data = dfResults, mapping = aes(x = day)) +
                geom_line(aes(y = cases), colour = 'lightgreen') +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases', 
                     x = 'Day', 
                     title = str_c('Cases in ', input$desiredRegion),
                     subtitle = txtDataSource) + 
                annotate("text", 
                         x = maxDay, 
                         y = maxCases, 
                         label = as.character(maxCases),
                         color = "white",
                         size = 5) + 
                annotate("text", 
                         x = minDay, 
                         y = maxCases / 20, 
                         label = as.character(minDay),
                         color = "white",
                         size = 5)
        })
        output$plotDeaths <- renderPlot({
            ggplot(data = dfResults, mapping = aes(x = day, y = deaths)) +
                geom_line(colour = 'lightblue') +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Deaths', 
                     x = 'Day', 
                     title = str_c('Deaths in ', input$desiredRegion),
                     subtitle = txtDataSource) + 
                annotate("text", 
                         x = maxDay, 
                         y = maxDeaths, 
                         label = as.character(maxDeaths),
                         color = "white",
                         size = 5) + 
                annotate("text", 
                         x = minDay, 
                         y = maxDeaths / 20, 
                         label = as.character(minDay),
                         color = "white",
                         size = 5)
        })
    })
}

shinyApp(ui = ui, server = server)
