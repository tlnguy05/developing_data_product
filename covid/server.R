


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
require(DT)

# Getting data from www.worldometers.info/coronavirus/country/us/ 
fileurl <- "https://www.worldometers.info/coronavirus/country/us/"
data <- fileurl %>% read_html() %>% html_nodes(xpath = '//*[@id="usa_table_countries_today"]') %>% html_table(fill = TRUE)
data <- data[[1]]
covidUSA_temp <- data[,c(-1, -14, -15)]
covidUSA1 <- covidUSA_temp[2:52,]
covidUSA <- covidUSA1[order(covidUSA1$USAState),]
covidUSA <- covidUSA[-9,] 

shinyServer(function(input, output) {
    
    # Processing selected input
    analysis <- reactive(gsub(" ", "", input$select1))
    
    # Render text 1
    output$text1 <- renderText(toupper(input$select1))
    
    # Render text 2
    output$text2 <- renderText(covidUSA_temp[1, analysis()])
    
    # Render text 3
    output$text3 <- renderText(as.character(today(tzone = "America/New_York")))
    
    # Processing data output for plot 1  
    plotdata <- reactive({
        temp <- data.frame(State = state.abb, Total = covidUSA[,analysis()])
        temp$Total <- as.numeric(gsub(",", "", temp$Total))
        temp$hover <- with(temp, paste(State, '<br>', input$select1, ":" , Total))
        temp
    })   
    
    # Render plot 1
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
      
    }) # End of rendering plot 1
    
        
    # Render plot 2
    output$plot2 <- renderPlotly({
      covidUSA1[, analysis()] <- as.numeric(gsub(",", "", covidUSA1[, analysis()]))
      covidUSA1 <- covidUSA1[order(covidUSA1[, analysis()], decreasing = TRUE),]
      bar <- covidUSA1[1:input$slider1,]
      bar$USAState <- factor(bar$USAState, levels = rev(bar$USAState))
      plot_ly(y = bar[, "USAState"] , x = bar[, analysis()], type = "bar") 
      
    }) # End of rendering plot 2
    
    # Data and button on data tab
    output$table <- DT::renderDataTable({covidUSA_temp}, rownames = F,
                                        options = list(bFilter = FALSE, 
                                                       iDisplayLength = 10))
    
    output$downloadData <- downloadHandler(
      filename = "covidUSA.csv",
      content = function(file){
        write.csv(covidUSA_temp, file, row.names = F)
      }
    )
    
    
   
    
    
    
   
    
    
})
