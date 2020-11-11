library(shiny)
library(shinyWidgets)
library(quanteda)
library(dplyr)
library(tidyr)

shinyUI(fluidPage(
    setBackgroundColor(color = c("#db5e63", "#e6b1b3", "#7aa9bf", "#225564"), gradient = "linear", direction = "bottom"),
    
    tags$style(HTML('body {font-family: Trebuchet MS,Lucida Grande,Lucida Sans Unicode,Lucida Sans,Tahoma,sans-serif}')),
    
    titlePanel(
        h1(strong("Word Prediction App"), align = "center", style = "color:#3c434d")
    ),
    
    sidebarLayout(
        sidebarPanel(
            textInput("text", label = h3(strong("Text input")), placeholder = "Enter your text here"),
            
            numericInput("n", label = h4(strong("Select how many words to predict:")), value = 4, 
                min = 1, max = 10, step = 1),
            
            radioButtons("choice", label = h4(strong("Choose the prediction method:")), 
                choiceNames = c("Observations only (slightly faster)", "Katz back-off"), choiceValues = c(0, 1)),
            
            actionButton("predict", label = strong("Submit"), style = "color:#3c434d"),
        ),
        
        mainPanel(
            tabsetPanel(id = "Tabs",
                        tabPanel(
                            title = strong("Prediction", style = "color:#3c434d"),
                            
                            h3(strong(htmlOutput("prediction")))
                        ),
                        
                        tabPanel(
                            title = strong("About", style = "color:black"),
                            
                            h4(strong("This app predicts the following word for an input text. The prediction is based on a
                    machine learning algorithm combining a Markov chain approach for conditional probabilities and a Katz 
                    back-off model to handle unseen n-grams. The datasets (corpora) used to develop the algorithm come from 
                    three sources: blogs, news and tweets."), style = "color:black"),
                            
                            h4(strong("To use the app, type a phrase as text input, choose how many words to predict (set by 
                    default to 4), and if the prediction will be based on observations only or discounting when backing off 
                    to lower n-grams (Katz back-off). Click Submit to obtain the predictions."), style = "color:black"),
                            
                            h4(strong(HTML("The files to recreate this app can be found 
                    <a href=https://github.com/Johan-VM/DataScienceCapstone>here</a>."), style = "color:black")),
                            
                            h4(strong(HTML("Developed by Johan V&aacute;squez Mazo. Powered by RStudio Shiny.")), 
                               style = "color:#0f0947"))
            )
        )
    )
))
