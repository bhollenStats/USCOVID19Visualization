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
    plotOutput('plotTopTenHotspots', height = '300px', brush = 'plot_brush'),
    plotOutput('plotDailyCases', height = '300px', brush = 'plot_brush_g_df_1'),
    plotOutput('plotDailyDeaths', height = '300px', brush = 'plot_brush_g_df_2'),
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

    # In order to plot a line graph of the top ten current hotspots I have to scour through the data in order 
    # to build a dataframe with each of the top ten states current seven-day running averages so that they 
    # can be plotted on one graph
    # 1. Determine what the day number is for reference in the data
    today = as.numeric(strftime(Sys.Date(), format = "%j"))
    # 2. Search through the total list to find out which states have the current top ten seven-day running average
    #    and return the data into a dataframe of the top ten state names
    dfTopTenHotspots <- dfCountyData %>%
        group_by(state, date) %>%
        mutate(day = as.numeric(strftime(date, format = "%j"))) %>%
        summarise(cases=sum(cases), deaths=sum(deaths)) %>%
        mutate(dailycases = round(cases - lag(cases), digits = 0), 
               dailydeaths = round(deaths - lag(deaths), digits = 0)) %>%
        mutate(r7daDailyCases = rollmean(dailycases, 7, fill = NA), 
               r7daDailyDeaths = rollmean(dailydeaths, 7, fill = NA)) %>%
        mutate(day = as.numeric(strftime(date, format = "%j"))) %>%
        filter(day == (today - 4)) %>%
        ungroup() %>%
        select(state, r7daDailyCases) %>%
        arrange(-r7daDailyCases) %>%
        top_n(10) %>%
        select(state)
    
    # 3. Now build a dataframe of all the day-based seven-day running averages from each 
    #    of the top ten states.  This data will be not ready for graphing, so I've called
    #    it Untidy for clean up immediately following
    dfTopTenHotspotsDataUntidy <- dfCountyData %>%
        group_by(state, date) %>%
        mutate(day = as.numeric(strftime(date, format = "%j"))) %>%
        summarise(cases=sum(cases)) %>%
        mutate(dailycases = round(cases - lag(cases), digits = 0)) %>%
        mutate(r7daDailyCases = rollmean(dailycases, 7, fill = NA)) %>%
        mutate(day = as.numeric(strftime(date, format = "%j"))) %>%
        filter(state == dfTopTenHotspots$state[1] |
                   state == dfTopTenHotspots$state[2] |
                   state == dfTopTenHotspots$state[3] |
                   state == dfTopTenHotspots$state[4] |
                   state == dfTopTenHotspots$state[5] |
                   state == dfTopTenHotspots$state[6] |
                   state == dfTopTenHotspots$state[7] |
                   state == dfTopTenHotspots$state[8] |
                   state == dfTopTenHotspots$state[9] |
                   state == dfTopTenHotspots$state[10]) %>%
        ungroup() %>%
        select(state, day, r7daDailyCases)
    
    # 4. Now build the tidy dataframe of all the states running seven-day average values
    #    with respect to the day variable so that they can be graphed
    dfTopTenHotspotsData <- ""
    for (i in 1:10) {
        dfSingleState <- dfTopTenHotspotsDataUntidy %>%
            filter(state == dfTopTenHotspots[[1]][i]) %>%
            select(day, r7daDailyCases)
        colnames(dfSingleState)[2] = str_c('State', i)
        if (i == 1) {
            dfTopTenHotspotsData <- dfSingleState
        } else {
            if (min(dfSingleState$day) < min(dfTopTenHotspotsData$day)) {
                dfTopTenHotspotsData <- dfSingleState %>% left_join(dfTopTenHotspotsData, by = "day")
            } else {
                dfTopTenHotspotsData <- dfTopTenHotspotsData %>% left_join(dfSingleState, by = "day")
            }
        }
    }
 
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

        # Plot top ten hotspots
        output$plotTopTenHotspots <- renderPlot({
            my.colors <- c("State1"="chocolate3", 
                           "State2"="deeppink", 
                           "State3"="black", 
                           "State4"="navyblue",
                           "State5"="blueviolet",
                           "State6"="orange3",
                           "State7"="red4",
                           "State8"="lightblue",
                           "State9"="forestgreen",
                           "State10"="darkmagenta")
            my.labels <- c(dfTopTenHotspots[[1]][1], 
                           dfTopTenHotspots[[1]][2], 
                           dfTopTenHotspots[[1]][3], 
                           dfTopTenHotspots[[1]][4],
                           dfTopTenHotspots[[1]][5],
                           dfTopTenHotspots[[1]][6],
                           dfTopTenHotspots[[1]][7],
                           dfTopTenHotspots[[1]][8],
                           dfTopTenHotspots[[1]][9],
                           dfTopTenHotspots[[1]][10])
            ggplot(data = dfTopTenHotspotsData, mapping = aes(x = day)) +
                geom_line(aes(y = State1, colour = "State1")) +
                geom_line(aes(y = State2, colour = "State2")) +
                geom_line(aes(y = State3, colour = "State3")) +
                geom_line(aes(y = State4, colour = "State4")) +
                geom_line(aes(y = State5, colour = "State5")) +
                geom_line(aes(y = State6, colour = "State6")) +
                geom_line(aes(y = State7, colour = "State7")) +
                geom_line(aes(y = State8, colour = "State8")) +
                geom_line(aes(y = State9, colour = "State9")) +
                geom_line(aes(y = State10, colour = "State10")) +
                scale_colour_manual("", values = my.colors, labels = my.labels) +
                coord_cartesian() +
                theme_dark() +
                labs(y = 'Cases++',
                     x = 'Day',
                     title = 'Top Ten Hotspot States',
                     subtitle = 'Seven-Day Running Averages of Cases')
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
