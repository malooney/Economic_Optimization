

library(shiny)
library(dygraphs)
library(DT)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Energy Portfolio Optimization"),
  
   sidebarLayout(
     
     sidebarPanel(
        sliderInput("cost",
                   "Maximum expected cost [million USD/TWh]:",
                    min = 0,
                    max = 150,
                    value = 80),
        sliderInput("demand",
                    "ERCOT Demand in 2016 [TWh]:",
                    min = 300,
                    max = 500,
                    value = 351)
     ),
     
    mainPanel(
      tabsetPanel(
        tabPanel("Historical - Source Mix: Plot", plotOutput("HistPlot"), DT::dataTableOutput('tbl1')),
        
        tabPanel("Model - Source Mix: Plot", plotOutput("distPlot"), DT::dataTableOutput('tbl')), 
        
        tabPanel("Model - Risk: Plot", dygraphOutput("gatePlot"))
      )
    ))
)
)
