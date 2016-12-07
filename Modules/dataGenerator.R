#===============================================================================
# data generator

## module ui
dataGeneratorUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("rowCount"), "Number of rows in the dataset:", 1, 100, 30, 1),
    hr(),
    dataTableOutput(ns("dfTable"))
  )
}

## module server
dataGenerator <- function(input, output, session) {
  ns <- session$ns
  
  df <- reactive({
    n <- input$rowCount
    tcts <- round(rnorm(n, mean = 15, sd = 2), 2)
    meanTct <- mean(tcts)
    tctDiff <- round(tcts - meanTct, 2)
    satLevels <- c("Strongly dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Strongly satisfied")
    data.frame(
      UserId=1:n, 
      Major=sample(c("Math", "Science", "Language", "Arts"), n, replace=T),
      Satisfaction=factor(sample(satLevels, n, replace=T), levels = satLevels),
      TaskCompletionTime=tcts,
      DifferenceofTCTFromTheAverage = tctDiff
    )
  })
  
  height <- 200
  output$dfTable <- renderDataTable({df()}, 
    class="compact", 
    height = height+50, 
    options = list(
      searching = F,
      paging = F,
      scrollY = paste0(height,"px"), 
      scrollCollapse = T, 
      info = F))
  
  # return value of the module
  return(df)
}