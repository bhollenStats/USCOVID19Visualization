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
    selectInput(inputId = 'countyOrState',
                label = 'Select county or state:',
                choices = c('County', 'State'),
                selected = 'State',
                multiple = FALSE,
                width = '50%'),
    selectInput(inputId = 'desiredRegion', 
                label = 'Select location to view:', 
                choices = c('Michigan', 'Pennsylvania', 'Ohio'),
                selected = 'Michigan',
                multiple = FALSE,
                width = '50%'),
    plotOutput('plotDailyCases'),
    plotOutput('plotDailyDeaths'),
    plotOutput('plotCases'),
    plotOutput('plotDeaths'),
    textOutput('tableHeader'),
    tableOutput('tableResults')
)

server <- function(input, output, session) {
    
    withProgress({dfCountyData <- read_csv(url('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'), progress = show_progress())},
                 message = 'Loading data from github repository', 
                 detail = 'This could take a few seconds...',
                 value = 0)
    dfStates <- as.list(dfCountyData %>% distinct(state) %>% arrange(state))
    dfCounties <- as.list(dfCountyData %>% mutate(countyState=str_c(county, " County, ", state)) %>% distinct(countyState) %>% arrange(countyState))
    txtDataDate <- Sys.Date()
    txtDataSource <- str_c("Data provided by the N.Y. Times [https://github.com/nytimes/covid-19-data] as of ", txtDataDate)
    
    observeEvent(input$countyOrState, {
        if(input$countyOrState=='County') {
            updateSelectInput(session, inputId = 'desiredRegion', choices = dfCounties)
        }
        else {
            updateSelectInput(session, inputId = 'desiredRegion', choices = dfStates)
        }
    })

    observeEvent(input$desiredRegion, {
        try({
            if(input$countyOrState=='County') {
                dfResults <- dfCountyData %>%
                    mutate(countyState=str_c(county, " County, ", state)) %>%
                    filter(countyState == input$desiredRegion) %>%
                    mutate(day = as.numeric(strftime(date, format = "%j"))) %>%
                    mutate(dailycases = round(cases - lag(cases), digits = 0), dailydeaths = round(deaths - lag(deaths), digits = 0)) %>%
                    select(date, cases, deaths, day, dailycases, dailydeaths)
            }
            else {
                dfResults <- dfCountyData %>% 
                    filter(state == input$desiredRegion) %>% 
                    group_by(date) %>% 
                    summarise(cases=sum(cases), deaths=sum(deaths)) %>% 
                    mutate(day = as.numeric(strftime(date, format = "%j"))) %>%
                    mutate(dailycases = round(cases - lag(cases), digits = 0), dailydeaths = round(deaths - lag(deaths), digits = 0)) %>%
                    select(date, cases, deaths, day, dailycases, dailydeaths)
            }}, silent = TRUE)
        maxCases = max(dfResults$cases)
        maxDailyCases = max(dfResults$dailycases)
        maxDeaths = max(dfResults$deaths)
        maxDailyDeaths = max(dfResults$dailydeaths)
        maxDay = max(dfResults$day)
        minDay = min(dfResults$day)
        output$plotDailyCases <- renderPlot({
            ggplot(data = dfResults, mapping = aes(x = day)) +
                geom_line(aes(y = dailycases), colour = 'lightgreen') +
                geom_smooth(aes(y = dailycases), colour = 'black', show.legend = TRUE) +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases', 
                     x = 'Day', 
                     title = str_c('Daily Cases in ', input$desiredRegion),
                     subtitle = txtDataSource) + 
                annotate("text", 
                         x = maxDay, 
                         y = maxDailyCases, 
                         label = as.character(maxDailyCases),
                         color = "white",
                         size = 5) + 
                annotate("text", 
                         x = minDay, 
                         y = maxDailyCases / 20, 
                         label = as.character(minDay),
                         color = "white",
                         size = 5)
        })
        output$plotDailyDeaths <- renderPlot({
            ggplot(data = dfResults, mapping = aes(x = day)) +
                geom_line(aes(y = dailydeaths), colour = 'lightblue') +
                geom_smooth(aes(y = dailydeaths), color = 'black', show.legend = TRUE) +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Deaths', 
                     x = 'Day', 
                     title = str_c('Daily Deaths in ', input$desiredRegion),
                     subtitle = txtDataSource) + 
                annotate("text", 
                         x = maxDay, 
                         y = maxDailyDeaths, 
                         label = as.character(maxDailyDeaths),
                         color = "white",
                         size = 5) + 
                annotate("text", 
                         x = minDay, 
                         y = maxDailyDeaths / 20, 
                         label = as.character(minDay),
                         color = "white",
                         size = 5)
        })
        output$plotCases <- renderPlot({
            ggplot(data = dfResults, mapping = aes(x = day)) +
                geom_line(aes(y = cases), colour = 'lightgreen') +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases', 
                     x = 'Day', 
                     title = str_c('Accumulated Cases in ', input$desiredRegion),
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
                     title = str_c('Accumulated Deaths in ', input$desiredRegion),
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
        output$tableHeader <- renderText(str_c('Table for ', input$desiredRegion))        
        output$tableResults <- renderTable(dfResults %>%
                                               mutate(Date=as.character(date), Cases=round(cases, digits = 0), Deaths=round(deaths, digits = 0)) %>% 
                                               mutate(NewCases = round(dailycases, digits = 0), NewDeaths = round(dailydeaths, digits = 0)) %>%
                                               select(Date, Cases, NewCases, Deaths, NewDeaths) %>%
                                               arrange(desc(Date)),
                                           align="rrrrr",
                                           digits = 0,
                                           striped = FALSE,
                                           bordered = TRUE)
    })
}

shinyApp(ui = ui, server = server)
