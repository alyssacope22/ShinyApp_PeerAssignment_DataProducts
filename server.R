#This is the server logic of a Shiny web application. You can run the application
# by clicking 'Run App' above.

#Find out more about buildling applications with Shiny here:

#http://shiny.rstudio.com/

data("diamonds")

library(shiny)
library(ggplot2)
library(dplyr)
# select columns to be used in the analysis
diam <- diamonds[,1:7]# Define server logic required to draw a plot

# Define server logic required to draw a plot
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    # select diamonds depending of user input
    diam <- filter(diamonds, grepl(input$col, color), grepl(input$clar, clarity))
    # build linear regression model
    fit <- lm( price~carat, diam)
    # predicts the price 
    pred <- predict(fit, newdata = data.frame(carat = input$car,
                                              color = input$col,
                                              clarity = input$clar))
    # Drow the plot using ggplot2
    plot <- ggplot(data=diam, aes(x=carat, y = price))+
      geom_point(aes(color = cut), alpha = 0.3)+
      geom_smooth(method = "lm")+
      geom_vline(xintercept = input$car, color = "mediumpurple2")+
      geom_hline(yintercept = pred, color = "tomato1")
    plot
  })
  output$result <- renderText({
    # renders the text for the prediction below the graph
    diam <- filter(diamonds, grepl(input$col, color), grepl(input$clar, clarity))
    fit <- lm( price~carat, diam)
    pred <- predict(fit, newdata = data.frame(carat = input$car,
                                              color = input$col,
                                              clarity = input$clar))
    res <- paste(round(pred, digits = 2), "$")
    res
  })
  
})

