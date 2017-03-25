#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("stringprediction.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    datasetInput <- reactive(stringprediction(input$inputstring)[1:5,])
    
    output$topwords <- renderDataTable(datasetInput(),
                                       options = list(paging= FALSE,
                                                      searching=FALSE,
                                                      info=FALSE))
    
    output$predictedword <- renderText({
                            pred <- datasetInput()
                            if(is.na(as.character(pred[1,1]))) ""
                            else as.character(pred[1,1])
                            })
    
  })
