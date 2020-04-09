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
    titlePanel("United States COVID-19 Data"),
    tags$head(tags$link(rel = 'stylesheet', 
                       type = 'text/css', 
                       href = 'www/app.css')),
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
    dfStates <- as.list(dfStateData %>% distinct(state) %>% arrange(state))
    txtDataDate <- "08-Apr-2020"
    txtDataSource <- str_c("Data provided by the N.Y. Times [https://github.com/nytimes/covid-19-data] as of ", txtDataDate)
    observe({
        updateSelectInput(session, inputId = 'desiredRegion', choices = dfStates)
    })
    
    observeEvent(input$desiredRegion, {
        try({dfResults <- dfStateData %>%
            filter(state == input$desiredRegion) %>%
            mutate(day = as.numeric(date)) %>%
            select(day, cases, deaths)}, silent = TRUE)
        output$plotCases <- renderPlot({
            ggplot(data = dfResults, mapping = aes(x = day, y = cases)) +
                geom_line(colour = 'lightgreen') +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases', 
                     x = 'Day', 
                     title = str_c('Cases in ', input$desiredRegion),
                     subtitle = txtDataSource)
        })
        output$plotDeaths <- renderPlot({
            ggplot(data = dfResults, mapping = aes(x = day, y = deaths)) +
                geom_line(colour = 'lightblue') +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Deaths', 
                     x = 'Day', 
                     title = str_c('Deaths in ', input$desiredRegion),
                     subtitle = txtDataSource)
        })
    })
}

shinyApp(ui = ui, server = server)
