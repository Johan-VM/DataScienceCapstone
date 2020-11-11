library(shiny)
library(shinyWidgets)
library(quanteda)
library(dplyr)
library(tidyr)

shinyServer(function(input, output) {
    coloring <- colorRampPalette(c("forestgreen", "darkblue"))
    rv <- reactiveValues()
    
    observeEvent(input$predict, {
        rv$options <- NULL
        
        if (input$choice == 0) {
            rv$prediction <- predict(input$text, mainModel, n = input$n)
        } else {
            rv$prediction <- predictBO(input$text, mainModel, n = input$n)
        }
        
        Colors <- coloring(input$n)
        for (i in 1:input$n) {
            rv$options[i] <- paste0("<span style=\"color:", Colors[i], "\">", rv$prediction$ngram[i], "</span>")
        }
    })
    
    output$prediction <- renderText({
        HTML(paste(rv$options, collapse = "<br/>"))
    })
})
