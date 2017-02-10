# server.R
library(shiny)
source("helpers.R")
Ns_small <- as.matrix(1:10000)
Ns_big <- as.matrix(c(seq(1, 9999, 1), seq(1000, 100000000, 1000)))
shinyServer(
  function(input, output) {

    randomVals <- eventReactive(input$go, {
      alpha <- switch(input$alpha,
                      "1" = 10,
                      "2" = 50,
                      "3" = 100)
      bbb<-xxx(x(alpha))
      return(runif(bbb))
    })
    
    randomValss <- eventReactive(input$go, {
      alpha <- switch(input$alpha,
                      "1" = 10,
                      "2" = 50,
                      "3" = 100)
      kkk<-xxx(x(alpha))
      return((kkk))
    })
    
    
    output$plot <- renderPlot({
      hist(randomVals())
    })
    
    output$texti <- renderText({
      randomValss()
    })
    
  })