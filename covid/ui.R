#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load library
library(plotly)
library(shiny)
library(shinythemes)
library(rvest)




# Required libraries for output
require(leaflet)
require(plotly)
require(ggplot2)
require(dplyr)
require(lubridate)

# Adjustment
h3.align <- "center"

# Define UI for application
shinyUI(navbarPage(
    # Application title
    title = "Covid-19 USA Dashboard",
    
    
    # Pick Shiny themes
    theme = shinytheme("cerulean"),
    
    #
    
    
    # Analytics tab panel
    tabPanel(
        title = "Analytics",
        
        sidebarPanel(
            width = 3,
            fluidRow(
                column(12, textOutput("text3"), align = "center"),
                tags$head(tags$style("#text3{color: #33AAFF;
                                 font-size: 30px;
                                 align:center
                                 }"
                )
                ),
                
            ),
            
            h3("Interactive Panel", align = h3.align),
            h6("Select options and click the ANALYSIS button", 
               align = "center"),
            selectInput(
                "select1",
                "Please select the data",
                c("Today", "Yesterday")
            ),
            selectInput(
                "select2",
                "Please select the analysis",
                c("Total Cases", "Total Deaths", "New Cases", "New Deaths", 
                  "Total Recovered", "Active Cases", "Total Tests"),
                selected = "Total Cases"),
            fluidRow(
                column(3),
                column(3,submitButton("ANALYSIS"))
            )
        ),
        mainPanel(
            tabsetPanel(
                
                # Analysis Tab -----------------------------------------
                tabPanel(
                    p(icon("area-chart"), "Analysis"),
                    
                    # Sales Over Time By Category
                    
                    fluidRow(
                        column(12, offset = 0, h3("DISTRIBUTION OF COVID CASES ACROSS THE USA", align="center"),
                               h4("Hover over the state to view", align="center"),
                               plotlyOutput("plot1")
                                )
                
                            ),
                    fluidRow(
                        column(4,textOutput("text1"), br(), textOutput("text2"), align = "center"), 
                        tags$head(tags$style("#text1{color: #33AAFF;
                                 font-size: 30px;
                                 align:center
                                 }"
                            )
                        ),
                        tags$head(tags$style("#text2{color: #EC5C81;
                                 font-size: 40px;
                                 font-style: italic;
                                 align:center
                                 }"
                        )
                        ),
                        
                         column(8, h3("TOP 10 STATES", align = "center"),plotlyOutput("plot2", height = 300))
                        
                        )
                    
                         ),
                
                # Today data tab
                tabPanel(
                    p(icon("table"), "Today Data"),
                    
                    fluidRow(
                        column(6, h3("Search, Filter & Download Data", align='left')),
                        column(6, downloadButton('downloadData1', 'Download', class="pull-right"))
                        
                    ),
                    hr(),
                    fluidRow(
                        dataTableOutput(outputId="table")
                        
                    ))
        
    ))), #End of Analytics tab panel
    
    #About tab panel
    tabPanel("Supporting Docs",
             mainPanel("haha"
             )
    ) # End About Tab Panel   
    
    
    
) #End of navbarPage

) #End of Shiny UI



