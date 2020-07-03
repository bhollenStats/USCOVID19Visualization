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

g_calcType <- 'Daily'
g_desiredRegion <- 'Michigan'
g_countyOrState <- 'State'
g_dfNR <- 'TBD'
g_df_1 <- 'TBD'
g_df_2 <- 'TBD'
g_df_3 <- 'TBD'
g_df_4 <- 'TBD'

# dfNationalResults %>% 
#   ggplot() + 
#   geom_col(mapping=aes(x=day, y=dailycases), color='slateblue1') + 
#   geom_line(mapping=aes(x=day, y=r7daDailyCases), color='#00ff00') + 
#   theme_dark()

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
                geom_col(aes(y = cases), colour = 'slateblue1') +
                geom_line(aes(y = cases7d), colour = plotColor) +
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
                geom_col(aes(y = cases), colour = 'slateblue1') +
                geom_line(aes(y = cases7d), colour = plotColor) +
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
                geom_col(aes(y = cases), colour = 'slateblue1') +
                geom_line(aes(y = cases7d), colour = plotColor) +
                geom_smooth(aes(y = cases), colour = 'black', show.legend = TRUE) +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases--',
                     x = 'Day',
                     title = plotTitle,
                     subtitle = plotSubtitle)   
        } else {
            ggplot(data = inPlotData, mapping = aes(x = day)) +
                geom_col(aes(y = cases), colour = 'slateblue1') +
                geom_line(aes(y = cases7d), colour = plotColor) +
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
    actionButton(inputId = "visualizeIt", 
                 label = 'Visualize Data'),
    plotOutput('plotNationalCases', height = '300px', brush = 'plot_brush'),
    verbatimTextOutput('infoNR'),
    plotOutput('plotDailyCases', height = '300px', brush = 'plot_brush_g_df_1'),
    verbatimTextOutput('info_g_df_1'),
    plotOutput('plotDailyDeaths', height = '300px', brush = 'plot_brush_g_df_2'),
    verbatimTextOutput('info_g_df_2'),
    plotOutput('plotCases', height = '300px', brush = 'plot_brush_g_df_3'),
    verbatimTextOutput('info_g_df_3'),
    plotOutput('plotDeaths', height = '300px', brush = 'plot_brush_g_df_4'),
    verbatimTextOutput('info_g_df_4'),
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
        
    observeEvent(input$calculationType, {
        g_calcType <<- input$calculationType  
    })

    observeEvent(input$desiredRegion, {
        g_desiredRegion <<- input$desiredRegion
    })  

    observeEvent(input$countyOrState, {
        if(input$countyOrState=='County') {
            updateSelectInput(session, inputId = 'desiredRegion', choices = dfCounties)
        }
        else {
            updateSelectInput(session, inputId = 'desiredRegion', choices = dfStates)
        }
        g_countyOrState <<- input$countyOrState
    })    
    
    output$infoNR <- renderPrint({
        try({
        brushedPoints(g_dfNR, input$plot_brush, xvar = "day", yvar = "cases")
        })
    })
    
    output$info_g_df_1 <- renderPrint({
        try({
            brushedPoints(g_df_1, input$plot_brush_g_df_1, xvar = "day", yvar = "cases")
        })
    }) 

    output$info_g_df_2 <- renderPrint({
        try({
            brushedPoints(g_df_2, input$plot_brush_g_df_2, xvar = "day", yvar = "cases")
        })
    }) 

    output$info_g_df_3 <- renderPrint({
        try({
            brushedPoints(g_df_3, input$plot_brush_g_df_3, xvar = "day", yvar = "cases")
        })
    }) 
    
    output$info_g_df_4 <- renderPrint({
        try({
            brushedPoints(g_df_4, input$plot_brush_g_df_4, xvar = "day", yvar = "cases")
        })
    }) 
    # This button will do it all now, especially to help with the performance of the site,
    # and all plots will be rendered based on the selection of daily or seven day averages
    # in the end, but now just move it into this button event
    observeEvent(input$visualizeIt, {
        
        cT <<- g_calcType
        dR <<- g_desiredRegion
        cOS <<- g_countyOrState
        
        # National data
        output$plotNationalCases <- renderPlot({
            
            if (cT == 'Daily') {
                df <- dfNationalResults %>%
                    mutate(cases = dailycases, cases7d = r7daDailyCases)
                g_dfNR <<- df
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
                           plotTrend = FALSE)
            } else {
                df <- dfNationalResults %>%
                    mutate(cases = dailycases, cases7d = r7daDailyCases)
                g_dfNR <<- df
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
                           plotTrend = FALSE)
            }
        })
        
        try({
            if(cOS=='County') {
                dfResults <- dfCountyData %>%
                    mutate(countyState=str_c(county, " County, ", state)) %>%
                    filter(countyState == dR) %>%
                    mutate(day = as.numeric(strftime(date, format = "%j"))) %>%
                    mutate(dailycases = round(cases - lag(cases), digits = 0), dailydeaths = round(deaths - lag(deaths), digits = 0)) %>%
                    select(date, cases, deaths, day, dailycases, dailydeaths, fips) %>%
                    mutate(r7daDailyCases = rollmean(dailycases, 7, fill = NA), 
                           r7daDailyDeaths = rollmean(dailydeaths, 7, fill = NA))
            }
            else {
                dfResults <- dfCountyData %>% 
                    filter(state == dR) %>% 
                    group_by(date) %>% 
                    summarise(cases=sum(cases), deaths=sum(deaths)) %>% 
                    mutate(day = as.numeric(strftime(date, format = "%j"))) %>%
                    mutate(dailycases = round(cases - lag(cases), digits = 0), dailydeaths = round(deaths - lag(deaths), digits = 0)) %>%
                    select(date, cases, deaths, day, dailycases, dailydeaths) %>%
                    mutate(r7daDailyCases = rollmean(dailycases, 7, fill = NA), 
                           r7daDailyDeaths = rollmean(dailydeaths, 7, fill = NA))
            }}, silent = TRUE)
        
        # Daily Cases in Region Selected
        if (cT == 'Daily') {
            g_df_1 <<- dfResults %>%
                mutate(cases = dailycases, cases7d = r7daDailyCases)
            maxCases_1 = round(max(g_df_1$cases, na.rm = TRUE), digits = 0)
            maxDay_1 = max(g_df_1$day, na.rm = TRUE)
            minDay_1 = min(g_df_1$day, na.rm = TRUE)
            output$plotDailyCases <- renderPlot({
                createPlot(g_df_1,  
                           plotColor = 'lightblue', 
                           plotTitle = str_c('Daily Cases in ', dR), 
                           plotSubtitle = txtDataSource,
                           plotMaxCases = maxCases_1,
                           plotMaxDay = maxDay_1,
                           plotMinDay = minDay_1,
                           plotTrend = FALSE)
            })
        } else {
            g_df_1 <<- dfResults %>%
                mutate(cases = dailycases, cases7d = r7daDailyCases)
            maxCases_1 = round(max(g_df_1$cases, na.rm = TRUE), digits = 0)
            maxDay_1 = max(g_df_1$day, na.rm = TRUE)
            minDay_1 = min(g_df_1$day, na.rm = TRUE)
            output$plotDailyCases <- renderPlot({
                createPlot(g_df_1,
                           plotColor = 'lightgreen',
                           plotTitle = str_c('Seven Day Average Cases in ', dR),
                           plotSubtitle = txtDataSource,
                           plotMaxCases = maxCases_1,
                           plotMaxDay = maxDay_1,
                           plotMinDay = minDay_1,
                           plotTrend = FALSE)
            })
        }

        # Daily Deaths in Region Selected
        if (cT == 'Daily') {
            g_df_2 <<- dfResults %>%
                mutate(cases = dailydeaths, cases7d = r7daDailyDeaths)
            maxCases_2 = round(max(g_df_2$cases, na.rm = TRUE), digits = 0)
            maxDay_2 = max(g_df_2$day, na.rm = TRUE)
            minDay_2 = min(g_df_2$day, na.rm = TRUE)
            output$plotDailyDeaths <- renderPlot({
                createPlot(g_df_2,  
                           plotColor = 'lightblue', 
                           plotTitle = str_c('Daily Deaths in ', dR), 
                           plotSubtitle = txtDataSource,
                           plotMaxCases = maxCases_2,
                           plotMaxDay = maxDay_2,
                           plotMinDay = minDay_2,
                           plotTrend = FALSE)            
            })
        } else {
            g_df_2 <<- dfResults %>%
                mutate(cases = dailydeaths, cases7d = r7daDailyDeaths)
            maxCases_2 = round(max(g_df_2$cases, na.rm = TRUE), digits = 0)
            maxDay_2 = max(g_df_2$day, na.rm = TRUE)
            minDay_2 = min(g_df_2$day, na.rm = TRUE)
            output$plotDailyDeaths <- renderPlot({
                createPlot(g_df_2,  
                           plotColor = 'lightgreen', 
                           plotTitle = str_c('Seven Day Average Deaths in ', dR), 
                           plotSubtitle = txtDataSource,
                           plotMaxCases = maxCases_2,
                           plotMaxDay = maxDay_2,
                           plotMinDay = minDay_2,
                           plotTrend = FALSE)            
            })
        }        
        
        # Accumulating Cases in Region Selected
        g_df_3 <<- dfResults
        maxCases_3 = round(max(g_df_3$cases, na.rm = TRUE), digits = 0)
        maxDay_3 = max(g_df_3$day, na.rm = TRUE)
        minDay_3 = min(g_df_3$day, na.rm = TRUE)        
        output$plotCases <- renderPlot({
            createPlot(g_df_3,  
                       plotColor = 'lightblue', 
                       plotTitle = str_c('Accumulated Cases in ', dR), 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases_3,
                       plotMaxDay = maxDay_3,
                       plotMinDay = minDay_3)            
        })
        
        # Accumulating Deaths in Region Selected
        g_df_4 <<- dfResults %>%
            mutate(cases = deaths)
        maxCases_4 = round(max(g_df_4$cases, na.rm = TRUE), digits = 0)
        maxDay_4 = max(g_df_4$day, na.rm = TRUE)
        minDay_4 = min(g_df_4$day, na.rm = TRUE)          
        output$plotDeaths <- renderPlot({
            createPlot(g_df_4,  
                       plotColor = 'lightblue', 
                       plotTitle = str_c('Accumulated Deaths in ', dR), 
                       plotSubtitle = txtDataSource,
                       plotMaxCases = maxCases_4,
                       plotMaxDay = maxDay_4,
                       plotMinDay = minDay_4)  
        })
        
        output$tableHeader <- renderText(str_c('Table for ', dR))   
        
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
        if (cOS=='State') {
            output$headerPerCapitaResults <- renderText(str_c('Per Capita Data for ', dR, ', per Million Residents [ ', desPop*1000000, ' ]'))
            desiredPopulation <- popDataCounties %>% 
                filter(STNAME == dR) %>%
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
            ctyStateNames = str_split(dR, ", ", n = 2)
            ctyName = ctyStateNames[[1]][1]
            stateName = ctyStateNames[[1]][2]
            desiredPopulation <- popDataCounties %>% 
                filter(CTYNAME == ctyName) %>%
                filter(STNAME == stateName)
            desPop = desiredPopulation$POPESTIMATE2019/1000 #desired population in thousands
            output$headerPerCapitaResults <- renderText(str_c('Per Capita Data for ', dR, ', per Thousand Residents [ ', desPop*1000, ' ]'))
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
