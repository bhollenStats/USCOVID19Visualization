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
                       plotMinDay = -1,
                       plotTrend = FALSE) {
    if (isTRUE(plotMaxCases > 0) && isTRUE(plotMaxDay > 0) && isTRUE(plotMinDay > 0)) {
        if (plotTrend) {
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
        }
    } else {
        if (plotTrend) {
            ggplot(data = inPlotData, mapping = aes(x = day)) +
                geom_line(aes(y = cases), colour = plotColor) +
                geom_smooth(aes(y = cases), colour = 'black', show.legend = TRUE) +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases--',
                     x = 'Day',
                     title = plotTitle,
                     subtitle = plotSubtitle)   
        } else {
            ggplot(data = inPlotData, mapping = aes(x = day)) +
                geom_line(aes(y = cases), colour = plotColor) +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases--',
                     x = 'Day',
                     title = plotTitle,
                     subtitle = plotSubtitle)              
        }
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
            maxCases = round(max(df$cases, na.rm = TRUE), digits = 0)
            maxDay = max(df$day, na.rm = TRUE)
            minDay = min(df$day, na.rm = TRUE)
            createPlot(df,  
                       plotColor = 'lightblue', 
                       plotTitle = 'Daily National Cases', 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases,
                       plotMaxDay = maxDay,
                       plotMinDay = minDay,
                       plotTrend = TRUE)
            } else {
               df <- dfNationalResults %>%
                    mutate(cases = r7daDailyCases)
               maxCases = round(max(df$cases, na.rm = TRUE), digits = 0)
               maxDay = max(df$day, na.rm = TRUE)
               minDay = min(df$day, na.rm = TRUE)
                createPlot(df,  
                           plotColor = 'lightgreen', 
                           plotTitle = 'Seven Day Average Cases', 
                           plotSubtitle = txtDataSource,
                           plotMaxCases = maxCases,
                           plotMaxDay = maxDay,
                           plotMinDay = minDay,
                           plotTrend = TRUE)
        }
    })

    observeEvent(input$calculationType, {
        if (input$calculationType == 'Daily') {
            df <- dfNationalResults %>%
                mutate(cases = dailycases)
            maxCases = round(max(df$cases, na.rm = TRUE), digits = 0)
            maxDay = max(df$day, na.rm = TRUE)
            minDay = min(df$day, na.rm = TRUE)
            createPlot(df,  
                       plotColor = 'lightblue', 
                       plotTitle = 'Daily National Cases', 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases,
                       plotMaxDay = maxDay,
                       plotMinDay = minDay,
                       plotTrend = TRUE)
        } else {
            df <- dfNationalResults %>%
                mutate(cases = r7daDailyCases)
            maxCases = round(max(df$cases, na.rm = TRUE), digits = 0)
            maxDay = max(df$day, na.rm = TRUE)
            minDay = min(df$day, na.rm = TRUE)
            createPlot(df,  
                       plotColor = 'lightgreen', 
                       plotTitle = 'Seven Day Average Cases', 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases,
                       plotMaxDay = maxDay,
                       plotMinDay = minDay,
                       plotTrend = TRUE)
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
                    select(date, cases, deaths, day, dailycases, dailydeaths, fips) %>%
                    mutate(r7daDailyCases = rollmean(dailycases, 7, fill = NA), 
                           r7daDailyDeaths = rollmean(dailydeaths, 7, fill = NA))
            }
            else {
                dfResults <- dfCountyData %>% 
                    filter(state == input$desiredRegion) %>% 
                    group_by(date) %>% 
                    summarise(cases=sum(cases), deaths=sum(deaths)) %>% 
                    mutate(day = as.numeric(strftime(date, format = "%j"))) %>%
                    mutate(dailycases = round(cases - lag(cases), digits = 0), dailydeaths = round(deaths - lag(deaths), digits = 0)) %>%
                    select(date, cases, deaths, day, dailycases, dailydeaths) %>%
                    mutate(r7daDailyCases = rollmean(dailycases, 7, fill = NA), 
                           r7daDailyDeaths = rollmean(dailydeaths, 7, fill = NA))
            }}, silent = TRUE)

        # Daily Cases in Region Selected
        df_1 <- dfResults %>%
            mutate(cases = dailycases)
        maxCases_1 = round(max(df_1$cases, na.rm = TRUE), digits = 0)
        maxDay_1 = max(df_1$day, na.rm = TRUE)
        minDay_1 = min(df_1$day, na.rm = TRUE)
        output$plotDailyCases <- renderPlot({
            createPlot(df_1,  
                       plotColor = 'lightblue', 
                       plotTitle = str_c('Daily Cases in ', input$desiredRegion), 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases_1,
                       plotMaxDay = maxDay_1,
                       plotMinDay = minDay_1,
                       plotTrend = TRUE)
        })
    
        # Daily Deaths in Region Selected
        df_2 <- dfResults %>%
            mutate(cases = dailydeaths)
        maxCases_2 = round(max(df_2$cases, na.rm = TRUE), digits = 0)
        maxDay_2 = max(df_2$day, na.rm = TRUE)
        minDay_2 = min(df_2$day, na.rm = TRUE)
        output$plotDailyDeaths <- renderPlot({
            createPlot(df_2,  
                       plotColor = 'lightblue', 
                       plotTitle = str_c('Daily Deaths in ', input$desiredRegion), 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases_2,
                       plotMaxDay = maxDay_2,
                       plotMinDay = minDay_2,
                       plotTrend = TRUE)            
        })

        # Accumulating Cases in Region Selected
        df_3 <- dfResults
        maxCases_3 = round(max(df_3$cases, na.rm = TRUE), digits = 0)
        maxDay_3 = max(df_3$day, na.rm = TRUE)
        minDay_3 = min(df_3$day, na.rm = TRUE)        
        output$plotCases <- renderPlot({
            createPlot(df_3,  
                       plotColor = 'lightblue', 
                       plotTitle = str_c('Accumulated Cases in ', input$desiredRegion), 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases_3,
                       plotMaxDay = maxDay_3,
                       plotMinDay = minDay_3)            
        })

        # Accumulating Deaths in Region Selected
        df_4 <- dfResults %>%
            mutate(cases = deaths)
        maxCases_4 = round(max(df_4$cases, na.rm = TRUE), digits = 0)
        maxDay_4 = max(df_4$day, na.rm = TRUE)
        minDay_4 = min(df_4$day, na.rm = TRUE)          
        output$plotDeaths <- renderPlot({
            createPlot(df_4,  
                       plotColor = 'lightblue', 
                       plotTitle = str_c('Accumulated Deaths in ', input$desiredRegion), 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases_4,
                       plotMaxDay = maxDay_4,
                       plotMinDay = minDay_4)  
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
