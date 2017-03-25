#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predictive Text Model"),
  
  # Sidebar with input and predicted word
  sidebarLayout(
    sidebarPanel(
       p("Please allow approx 30 seconds for the data to load."),
       textInput("inputstring", "Input", value = "",
                 width = NULL, placeholder = "Text to predict"),
       submitButton("Submit"),
       br(),
       h5("Predicted Word:"),
       textOutput("predictedword"),
       br(),
       p("This app will predict the next word following a sequence of
         words typed into the input box. The top 5 words are also shown
         on the right, with their relative scores given by the model.
         If there is a * next to a predicted word, then this word is from
         the general list of mostly used words, and means that the model
         has not been able to find 5 words with high enough scores from
         the sampled text.")
    ),
    
    # Main panel to show top words
    mainPanel(
       dataTableOutput('topwords')
    )
  )
))
