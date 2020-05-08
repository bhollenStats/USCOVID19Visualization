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

createPlot <- function(inPlotData, 
                       plotColor = 'lightgreen', 
                       plotTitle = 'PLOT_TITLE', 
                       plotSubtitle = 'PLOT_SUBTITLE',
                       plotMaxCases = -1,
                       plotMaxDay = -1,
                       plotMinDay = -1) {
    if (isTRUE(plotMaxCases > 0) && isTRUE(plotMaxDay > 0) && isTRUE(plotMinDay > 0)) {
        ggplot(data = inPlotData, mapping = aes(x = day)) +
            geom_line(aes(y = cases), colour = plotColor) +
            geom_smooth(aes(y = cases), colour = 'black', show.legend = TRUE) +
            coord_cartesian() +
            theme_dark() +
            labs(y = 'Cases++',
                 x = 'Day',
                 title = plotTitle,
                 subtitle = plotSubtitle) +
            annotate("text", 
                     x = plotMaxDay, 
                     y = plotMaxCases, 
                     label = as.character(plotMaxCases),
                     color = "white",
                     size = 5) + 
            annotate("text", 
                     x = plotMinDay, 
                     y = plotMaxCases / 20, 
                     label = as.character(plotMinDay),
                     color = "white",
                     size = 5)             
    } else {
        ggplot(data = inPlotData, mapping = aes(x = day)) +
            geom_line(aes(y = cases), colour = plotColor) +
            geom_smooth(aes(y = cases), colour = 'black', show.legend = TRUE) +
            coord_cartesian() +
            theme_dark() +
            labs(y = 'Cases--',
                 x = 'Day',
                 title = plotTitle,
                 subtitle = plotSubtitle)         
    }
}

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
    
    # Read the data and build the full set of data for visualization
    # 1. Read the NYTimes GitHub repository of all the COVID-19 cases per state and county
    withProgress({dfCountyData <- read_csv(url('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'), progress = show_progress())},
                 message = 'Loading data from github repository', 
                 detail = 'This could take a few seconds...',
                 value = 0)
    # 2. Read the caches population data to determine count and state populations
    popDataCounties <- read_csv(file = 'suppdocu/co-est2019-alldata.csv')
    popDataStates <- popDataCounties %>% 
        group_by(STNAME,STATE) %>% 
        select(STNAME, STATE, POPESTIMATE2019) %>%
        summarize_if(is.numeric, sum, na.rm = TRUE)
    # 3. Build the list of states that can be visualized
    dfStates <- as.list(dfCountyData %>% distinct(state) %>% arrange(state))
    # 4. Build the list of counties that can be visualized
    dfCounties <- as.list(dfCountyData %>% mutate(countyState=str_c(county, " County, ", state)) %>% distinct(countyState) %>% arrange(countyState))
    # 5. Populate text notes for the plot displays
    txtDataDate <- Sys.Date()
    txtDataSource <- str_c("Data provided by the N.Y. Times [https://github.com/nytimes/covid-19-data] as of ", txtDataDate)
    
    # Massage the data down to the following attributes {dailycases, dailydeaths, r7daDailyCases, r7daDailyDeaths}
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
            df <- dfNationalResults %>%
                mutate(cases = dailycases)
            maxCases = max(dfNationalResults$dailycases, na.rm = TRUE)
            maxDay = max(dfNationalResults$day, na.rm = TRUE)
            minDay = min(dfNationalResults$day, na.rm = TRUE)
            createPlot(df,  
                       plotColor = 'lightblue', 
                       plotTitle = 'Daily National Cases', 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases,
                       plotMaxDay = maxDay,
                       plotMinDay = minDay)
            } else {
               df <- dfNationalResults %>%
                    mutate(cases = r7daDailyCases)
                maxCases = round(max(dfNationalResults$r7daDailyCases, na.rm = TRUE), digits = 0)
                maxDay = max(dfNationalResults$day, na.rm = TRUE)
                minDay = min(dfNationalResults$day, na.rm = TRUE)
                createPlot(df,  
                           plotColor = 'lightgreen', 
                           plotTitle = 'Seven Day Average Cases', 
                           plotSubtitle = txtDataSource,
                           plotMaxCases = maxCases,
                           plotMaxDay = maxDay,
                           plotMinDay = minDay)
        }
    })

    observeEvent(input$calculationType, {
        if (input$calculationType == 'Daily') {
            df <- dfNationalResults %>%
                mutate(cases = dailycases)
            maxCases = max(dfNationalResults$dailycases, na.rm = TRUE)
            maxDay = max(dfNationalResults$day, na.rm = TRUE)
            minDay = min(dfNationalResults$day, na.rm = TRUE)
            createPlot(df,  
                       plotColor = 'lightblue', 
                       plotTitle = 'Daily National Cases', 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases,
                       plotMaxDay = maxDay,
                       plotMinDay = minDay)
        } else {
            df <- dfNationalResults %>%
                mutate(cases = r7daDailyCases)
            maxCases = round(max(dfNationalResults$r7daDailyCases, na.rm = TRUE), digits = 0)
            maxDay = max(dfNationalResults$day, na.rm = TRUE)
            minDay = min(dfNationalResults$day, na.rm = TRUE)
            createPlot(df,  
                       plotColor = 'lightgreen', 
                       plotTitle = 'Seven Day Average Cases', 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases,
                       plotMaxDay = maxDay,
                       plotMinDay = minDay)
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
