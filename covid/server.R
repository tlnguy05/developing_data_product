


# Load libraries ---------------------------------------------------------------
library(shiny)
library(rvest)


# Data read & manipulation libraries
require(dplyr)
require(lubridate)

# Visualization libraries
require(leaflet)
require(plotly)
require(ggplot2)

# Define server logic required to draw a histogram

fileurl <- "https://www.worldometers.info/coronavirus/country/us/"
data <- fileurl %>% read_html() %>% html_nodes(xpath = '//*[@id="usa_table_countries_today"]') %>% html_table(fill = TRUE)
data <- data[[1]]
covidUSA_temp <- data[,c(-1, -14, -15)]
covidUSA1 <- covidUSA_temp[2:52,]
covidUSA <- covidUSA1[order(covidUSA1$USAState),]
covidUSA <- covidUSA[-9,] 

shinyServer(function(input, output) {
    
    
    plotdata <- reactive({
        analysis <- gsub(" ", "", input$select2)
        temp <- data.frame(State = state.abb, Total = covidUSA[,analysis])
        temp$Total <- as.numeric(gsub(",", "", temp$Total))
        temp$hover <- with(temp, paste(State, '<br>', input$select2, ":" , Total))
        temp
    })        
        
    
    
    # Render text
    output$text1 <- renderText(toupper(input$select2))
    output$text2 <- renderText({
        analysis2 <- gsub(" ", "", input$select2)
        covidUSA_temp[1, analysis2]
    })
    output$text3 <- renderText(as.character(today()))
    
    #Render plot2
    output$plot2 <- renderPlotly({
      analysis1 <- gsub(" ", "", input$select2)
      covidUSA1[, analysis1] <- as.numeric(gsub(",", "", covidUSA1[, analysis1]))
      covidUSA1 <- covidUSA1[order(covidUSA1[, analysis1], decreasing = TRUE),]
      bar <- covidUSA1[1:10,]
      bar$USAState <- factor(bar$USAState, levels = rev(bar$USAState))
      plot_ly(y = bar[, "USAState"] , x = bar[, analysis1], type = "bar") 
      
    })
    
    
    
   
    
    
    
    # Render plot
    output$plot1 <- renderPlotly({
        # give state boundaries a white border
        borders <- list(color = toRGB("black"), width = 1)
        
        # specify some map projection/options
        map_options <- list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          showlakes = TRUE,
          lakecolor = toRGB('white')
        )
        
        #Plot
        plot_ly(data = plotdata(), z = ~Total, text = ~hover, locations = ~State, 
                type = 'choropleth', locationmode = 'USA-states', 
                color = ~Total, colors = 'Reds', marker = list(line = borders)) %>%
            layout(geo = map_options)
        
    })
    
    
})
