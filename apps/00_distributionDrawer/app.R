if (!require("pacman")) install.packages("pacman", repos='https://stat.ethz.ch/CRAN/'); library(pacman)
p_load(shiny,
  knitr,
  markdown,
  ggplot2, 
  grid,
  DT,
  dplyr, 
  tidyr, 
  knitr, 
  httpuv, 
  ggvis)

# options(shiny.trace = FALSE)

source("../../util.R")


ui <- basicPage(
  # rmarkdownOutput("../../Instructions/centralLimitTheorem.Rmd"),
  sidebarLayout(position = "right",
    sidebarPanel(
      sliderInput("xRange", "Range of x-axis:", -20, 20, c(-1, 16), 0.5)
    ),
    mainPanel(
      p("Click anywhere on this plot to add twenty data points."),
      plotOutput("plotScatter1", click = "plot_click1", width = "400px", height = "150px"),
      ggvisOutput("plotHist1"),
      verbatimTextOutput("serialized")
    )
  )
)


server <- function(input, output,session) {
  
  x <- c(3, 10, 15, 3, 4, 7, 1, 12)
  y <- c(4, 10, 12, 17, 15, 20, 14, 3)
  
  
  # initialize reactive values with existing data
  val <- reactiveValues(data = cbind (x = x, y = y), 
                        isPlotInitialized = FALSE,
                        statMean = NULL, 
                        statMedian = NULL, 
                        statMode = NULL,
                        statSD = NULL,
                        sampleDf = NULL,
                        meanValDf = NULL,
                        barDf = NULL,
                        sampleMeanDf = NULL)
  
  # observe click on the scatterplot
  observeEvent(input$plot_click1, {
      xRand <- rnorm(20, mean = input$plot_click1$x, sd = 1)
      yRand <- rnorm(20, mean = input$plot_click1$y, sd = 1)
      val$data <- rbind(val$data, cbind(x = xRand, y = yRand))
  })        
  
  # render scatterplot
  output$plotScatter1 <- renderPlot({
    p <- ggplot(data = NULL, aes(x=val$data[,1], y=val$data[,2])) +
      geom_point() +
      theme_bw() +
      theme(legend.position="none") +
      xlim(input$xRange[1], input$xRange[2]) +
      xlab("x") +
      ylab("y")
      
    p
  })
  
  
 
  # render histogram (and calculate statistics)
  hisVis <- reactive({
    
    histData <- data.frame(x = val$data[,1])
    
    val$statMean <- mean(histData$x)
    val$statSD <- sd(histData$x)
    val$statMedian <- median(histData$x)
    val$statMode <- findModes(histData$x)$values
    
    # pack descriptive statistics for plotting
    statData <- data.frame(
      value = c(val$statMean), #, val$statMedian, val$statMode),
      stat = c("mean"), #, "median", rep("mode", length(val$statMode)) ),
      color = c("blue") #, "green", rep("orange", length(val$statMode)))
    )
    statSDDf <- data.frame(
      x <- c(val$statMean - val$statSD, val$statMean + val$statSD),
      y <- c(1, 1)
    )
    meanVbarDf <- data.frame(x = val$statMean - 0.01, x2 =val$statMean + 0.01)
    
    # plot histogram
    histData %>%
      ggvis(~x) %>% 
      add_axis("x", title = "x") %>%
      scale_numeric("x", domain = input$xRange) %>%
      set_options(width = 400, height = 200, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas") %>%
      hide_legend('fill') %>%
      
      # histogram of the population
      layer_histograms(width = 1, fill := "lightgray", stroke := NA) %>%
      
      
      # population mean
      layer_points(data = statData, x = ~value, y = 0, fillOpacity := 0.8, fill := ~color) %>%
      layer_rects(data = meanVbarDf, x = ~x, x2 = ~x2, y := 0, y2 = 0, stroke := "blue") %>%
      
      
      # population SD
      layer_paths(data = statSDDf, x = ~x, y = 0, stroke := "blue")
      
      
  })
  hisVis %>% bind_shiny("plotHist1")
  
  
  # text output
  output$serialized <- renderPrint({
    dput(val$data, control = c("keepNA", "keepInteger"))
  })
  
}

shinyApp(ui, server)