library(shiny)

#===============================================================================
# pre-requisite
source("config.R")
if (!require("pacman")) install.packages("pacman", repos='https://stat.ethz.ch/CRAN/'); 
library(pacman)
p_load(ggplot2, 
       grid, 
       DT, 
       dplyr, 
       tidyr, 
       knitr, 
       httpuv, 
       shiny, 
       scatterD3, 
       ggvis,
       cowplot)

theme_set(theme_gray())

#===============================================================================
# global namespace

studyDf <- NULL

#===============================================================================
# data generator

## module ui
dataGeneratorUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("rowCount"), "Number of data rows:", 1, 100, 30, 1),
    hr(),
    dataTableOutput(ns("dfTable"))
  )
}

## module server
dataGenerator <- function(input, output, session) {
  ns <- session$ns
  
  df <- reactive({
    n <- input$rowCount
    satLevels <- c("Strongly dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Strongly satisfied")
    data.frame(
      UserId=1:n, 
      Major=sample(c("Math", "Science", "Language", "Arts"), n, replace=T),
      Satisfaction=factor(sample(satLevels, n, replace=T), levels = satLevels),
      TaskCompletionTime=rnorm(n, mean = 15, sd = 2)
    )
  })
  
  output$dfTable <- renderDataTable({
    df()
  })
  
  # return value of the module
  return(df)
}

#===============================================================================
# histogram binning

## module ui
histBinningUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("binWidth"), "Bin width:", 1, 5, 1, 0.5),
    ggvisOutput(ns("plotHist"))
  )
}

## module server
histBinning <- function(input, output, session, dfReact) {
  ns <- session$ns
  
  hisVis <- reactive({
    df <- dfReact()
    binWidth <- input$binWidth
    df %>%
      ggvis( ~ TaskCompletionTime) %>%
      scale_numeric("x", domain = c(0, max(df$TaskCompletionTime) + 2), nice = FALSE) %>%
      layer_histograms(width = binWidth, fill := "gray") %>%
      layer_points(y = 0, fillOpacity := 0.5, fill := "blue")
  })
  hisVis %>% bind_shiny(ns("plotHist"))
}

#===============================================================================
# main app
ui <- navbarPage(
  "Statistics lecture 1",
  tabPanel("Data and histogram", 
    histBinningUI("histbinning"),
    dataGeneratorUI("datagen")
  ),
  tabPanel("Descriptives"),
  tabPanel("Central limit theorem")
)

server <- function(input, output, session) {
  studyDf <- callModule(dataGenerator, "datagen")
  callModule(histBinning, "histbinning", studyDf)
}

shinyApp(ui, server)