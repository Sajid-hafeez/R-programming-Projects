library(shiny)
library(tidyverse)
library(shinyWidgets)
a= c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")
ui <- fluidPage(
  h1(id="big-heading", "Sajid Hafeez"),
  tags$style(HTML("#big-heading{color: blue;}")),
  h1(id="small-heading", "Basic Shiny App Iris Data"),
  tags$style(HTML("#small-heading{color: red;}")),
  selectInput("var3", "Iris variables", choices = a),
  plotOutput("hist"),
  plotOutput("plot2"),
  verbatimTextOutput("summ"),
  setBackgroundColor("yellow"),

)

server <- function(input, output) {
  output$plot2 <- renderPlot({
    ggplot(iris, aes( x = .data[[input$var3]])) +
      geom_boxplot(color = "blue",fill='red') 
  })
  output$hist <- renderPlot({
    ggplot(iris, aes( x = .data[[input$var3]])) +
      geom_histogram(fill = "blue",color="red",bins = input$ii)
  })
  output$summ <- renderPrint({
    
    summary(iris)
  })
}

shinyApp(ui = ui, server = server)

