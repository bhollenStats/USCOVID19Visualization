#
# Shiny Application to Display COVID-19 data 
# Provided by the New York Times within the 
# GitHub repository at https://github.com/nytimes/covid-19-data
#
library(tidyverse)
library(ggplot2)
library(shiny)
library(stringr)
library(zoo)

rm(list=ls())

ui <- fluidPage(
    tags$head(tags$link(rel = 'stylesheet', 
                       type = 'text/css', 
                       href = 'app.css')),
    titlePanel("United States COVID-19 Data"),
    radioButtons(inputId = 'calculationType', 
                 label = 'Select display mode for the graph of national cases:', 
                 choices = c('Daily', 'Seven Day Average'),
                 selected = 'Daily', 
                 inline = TRUE, 
                 width = '50%'),
    plotOutput('plotNationalCases', height = '300px'),
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
    plotOutput('plotDailyCases', height = '300px'),
    plotOutput('plotDailyDeaths', height = '300px'),
    plotOutput('plotCases', height = '300px'),
    plotOutput('plotDeaths', height = '300px'),
    tableOutput('headerPerCapitaResults'),
    tableOutput('tablePerCapitaResults'),
    textOutput('tableHeader'),
    tableOutput('tableResults')
)

server <- function(input, output, session) {
    
    withProgress({dfCountyData <- read_csv(url('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'), progress = show_progress())},
                 message = 'Loading data from github repository', 
                 detail = 'This could take a few seconds...',
                 value = 0)
    popDataCounties <- read_csv(file = 'suppdocu/co-est2019-alldata.csv')
    popDataStates <- popDataCounties %>% 
        group_by(STNAME,STATE) %>% 
        select(STNAME, STATE, POPESTIMATE2019) %>%
        summarize_if(is.numeric, sum, na.rm = TRUE)
    dfStates <- as.list(dfCountyData %>% distinct(state) %>% arrange(state))
    dfCounties <- as.list(dfCountyData %>% mutate(countyState=str_c(county, " County, ", state)) %>% distinct(countyState) %>% arrange(countyState))
    txtDataDate <- Sys.Date()
    txtDataSource <- str_c("Data provided by the N.Y. Times [https://github.com/nytimes/covid-19-data] as of ", txtDataDate)
    
    dfNationalResults <- dfCountyData %>% 
        group_by(date) %>% 
        summarise(cases=sum(cases), deaths=sum(deaths)) %>%
        mutate(day = as.numeric(strftime(date, format = "%j"))) %>%
        mutate(dailycases = round(cases - lag(cases), digits = 0), 
               dailydeaths = round(deaths - lag(deaths), digits = 0)) %>%
        mutate(r7daDailyCases = rollmean(dailycases, 7, fill = NA), 
               r7daDailyDeaths = rollmean(dailydeaths, 7, fill = NA))

    output$plotNationalCases <- renderPlot({
        if (input$calculationType == 'Daily') {
            
            maxNationalDailyCases = max(dfNationalResults$dailycases)
            maxNationalDay = max(dfNationalResults$day)
            minNationalDay = min(dfNationalResults$day)
            
            ggplot(data = dfNationalResults, mapping = aes(x = day)) +
                geom_line(aes(y = dailycases), colour = 'lightgreen') +
                geom_smooth(aes(y = dailycases), colour = 'black', show.legend = TRUE) +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases', 
                     x = 'Day', 
                     title = 'Daily National Cases',
                     subtitle = txtDataSource) + 
                annotate("text", 
                         x = maxNationalDay, 
                         y = maxNationalDailyCases, 
                         label = as.character(maxNationalDailyCases),
                         color = "white",
                         size = 5) + 
                annotate("text", 
                         x = minNationalDay, 
                         y = maxNationalDailyCases / 20, 
                         label = as.character(minNationalDay),
                         color = "white",
                         size = 5)       
            } else {
                
            maxNationalDailyCases = max(dfNationalResults$r7daDailyCases)
            maxNationalDay = max(dfNationalResults$r7daDailyCases)
            minNationalDay = min(dfNationalResults$r7daDailyCases)
            
            ggplot(data = dfNationalResults, mapping = aes(x = day)) +
                geom_line(aes(y = r7daDailyCases), colour = 'lightgreen') +
                geom_smooth(aes(y = r7daDailyCases), colour = 'black', show.legend = TRUE) +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases', 
                     x = 'Day', 
                     title = 'Seven Day Average National Cases',
                     subtitle = txtDataSource) + 
                annotate("text", 
                         x = maxNationalDay, 
                         y = maxNationalDailyCases, 
                         label = as.character(maxNationalDailyCases),
                         color = "white",
                         size = 5) + 
                annotate("text", 
                         x = minNationalDay, 
                         y = maxNationalDailyCases / 20, 
                         label = as.character(minNationalDay),
                         color = "white",
                         size = 5)    
        }
    })

    observeEvent(input$calculationType, {
        if (input$calculationType == 'Daily') {
            
            maxNationalDailyCases = max(dfNationalResults$dailycases)
            maxNationalDay = max(dfNationalResults$day)
            minNationalDay = min(dfNationalResults$day)
            
            ggplot(data = dfNationalResults, mapping = aes(x = day)) +
                geom_line(aes(y = dailycases), colour = 'lightgreen') +
                geom_smooth(aes(y = dailycases), colour = 'black', show.legend = TRUE) +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases', 
                     x = 'Day', 
                     title = 'Daily National Cases',
                     subtitle = txtDataSource) + 
                annotate("text", 
                         x = maxNationalDay, 
                         y = maxNationalDailyCases, 
                         label = as.character(maxNationalDailyCases),
                         color = "white",
                         size = 5) + 
                annotate("text", 
                         x = minNationalDay, 
                         y = maxNationalDailyCases / 20, 
                         label = as.character(minNationalDay),
                         color = "white",
                         size = 5)       
        } else {
            
            maxNationalDailyCases = max(dfNationalResults$r7daDailyCases)
            maxNationalDay = max(dfNationalResults$r7daDailyCases)
            minNationalDay = min(dfNationalResults$r7daDailyCases)
            
            ggplot(data = dfNationalResults, mapping = aes(x = day)) +
                geom_line(aes(y = r7daDailyCases), colour = 'lightgreen') +
                geom_smooth(aes(y = r7daDailyCases), colour = 'black', show.legend = TRUE) +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases', 
                     x = 'Day', 
                     title = 'Seven Day Average National Cases',
                     subtitle = txtDataSource) + 
                annotate("text", 
                         x = maxNationalDay, 
                         y = maxNationalDailyCases, 
                         label = as.character(maxNationalDailyCases),
                         color = "white",
                         size = 5) + 
                annotate("text", 
                         x = minNationalDay, 
                         y = maxNationalDailyCases / 20, 
                         label = as.character(minNationalDay),
                         color = "white",
                         size = 5)    
        }
    })  
    
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
                    select(date, cases, deaths, day, dailycases, dailydeaths, fips)
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
        # Per Capita Result Table
        if (input$countyOrState=='State') {
            output$headerPerCapitaResults <- renderText(str_c('Per Capita Data for ', input$desiredRegion, ', per Million Residents [ ', desPop*1000000, ' ]'))
            desiredPopulation <- popDataCounties %>% 
                filter(STNAME == input$desiredRegion) %>%
                filter(COUNTY == 0)
            desPop = desiredPopulation$POPESTIMATE2019/1000000 #desired population in millions
            output$tablePerCapitaResults <- renderTable(dfResults %>%
                                                   mutate(Date=as.character(date), Cases=round(cases/desPop, digits = 5), Deaths=round(deaths/desPop, digits = 5)) %>% 
                                                   mutate(NewCases = round(dailycases/desPop, digits = 5), NewDeaths = round(dailydeaths/desPop, digits = 5)) %>%
                                                   select(Date, Cases, NewCases, Deaths, NewDeaths) %>%
                                                   arrange(desc(Date)),
                                               align="rrrrr",
                                               digits = 0,
                                               striped = FALSE,
                                               bordered = TRUE)
        }
        else {
            ctyStateNames = str_split(input$desiredRegion, ", ", n = 2)
            ctyName = ctyStateNames[[1]][1]
            stateName = ctyStateNames[[1]][2]
            desiredPopulation <- popDataCounties %>% 
                filter(CTYNAME == ctyName) %>%
                filter(STNAME == stateName)
            desPop = desiredPopulation$POPESTIMATE2019/1000 #desired population in thousands
            output$headerPerCapitaResults <- renderText(str_c('Per Capita Data for ', input$desiredRegion, ', per Thousand Residents [ ', desPop*1000, ' ]'))
            output$tablePerCapitaResults <- renderTable(dfResults %>%
                                                            mutate(Date=as.character(date), Cases=round(cases/desPop, digits = 5), Deaths=round(deaths/desPop, digits = 5)) %>% 
                                                            mutate(NewCases = round(dailycases/desPop, digits = 5), NewDeaths = round(dailydeaths/desPop, digits = 5)) %>%
                                                            select(Date, Cases, NewCases, Deaths, NewDeaths) %>%
                                                            arrange(desc(Date)),
                                                        align="rrrrr",
                                                        digits = 0,
                                                        striped = FALSE,
                                                        bordered = TRUE)          
        }
    })
}

shinyApp(ui = ui, server = server)
