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
  shinyjs,
  assertthat,
  ggvis)

# options(shiny.trace = FALSE)

source("../../util.R")


ui <- basicPage(
  useShinyjs(),
  rmarkdownOutput("../../Instructions/inference.Rmd"),
  sidebarLayout(position = "right",
    sidebarPanel(
      sliderInput("obsCount", "How many observations?:", 5, 50, 30, 1),
      actionButton("sampleBtn", "Draw a sample"),
      hr(),
      
      sliderInput("confPercent", "% confidence:", 1, 99.99, 95, 1),
      hr(),
      
      sliderInput("benchmarkX", "Benchmark value:", 0, 15, 0, 1),
      
      downloadButton('downloadData', 'Download data'),
      fileInput('file1', 'Upload data:',
        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
    ),
    mainPanel(
      plotOutput("plotScatter", click = "plot_click", width = "400px", height = "150px"),
      ggvisOutput("plotSample"),
      ggvisOutput("plotCI"),
      "Typical output from statistical software:",
      verbatimTextOutput("tTest"),
      "Reporting:",
      verbatimTextOutput("report")
    )
  )
  
  
  
)


server <- function(input, output,session) {
  
  x <- c(3, 10, 15, 3, 4, 7, 1, 12)
  y <- c(4, 10, 12, 17, 15, 20, 14, 3)
  plotRange <- c(-1, 16)
  
  # initialize reactive values with existing data
  val <- reactiveValues(
    data = cbind(x = x, y = y), 
    isPlotInitialized = FALSE,
    sample = NULL,
    sampleStat = NULL,
    popStat = NULL
  )
  
  # observe click on the scatterplot
  observeEvent(input$plot_click, {
      xRand <- rnorm(20, mean = input$plot_click$x, sd = 1)
      yRand <- rnorm(20, mean = input$plot_click$y, sd = 1)
      data <- rbind(val$data, cbind(x = xRand, y = yRand))
      data <- tail(data, 200) # cap at 200 data points
      
      # calculate statsitics for the population
      confLevel <- input$confPercent / 100
      popStat <- mean_cl_normal(data[,1], conf.int = confLevel)
      names(popStat) <- c("Mean", "CILower", "CIUpper")
      val$popStat <- popStat
      
      
      val$data <- data
  })        
  
  # render scatterplot
  output$plotScatter <- renderPlot({
    p <- ggplot(data = NULL, aes(x=val$data[,1], y=val$data[,2])) +
      geom_point() +
      geom_vline(aes(xintercept = input$benchmarkX),  colour = "lightgreen") +
      theme_bw() +
      theme(legend.position="none") +
      xlim(plotRange[1], plotRange[2]) +
      xlab("x") +
      ylab("y")
      
    if (!is.null(val$popStat)) {
      p <- p + geom_vline(aes(xintercept = val$popStat$Mean), colour = "blue")
    }
    p
  })
  
  
  # handle file upload
  handleUpload <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    val$data <- read.csv(inFile$datapath)  
  })
  
  # handle download button
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(val$data, file, row.names = F, na = "")
    }
  )
  
  # plot samples drawn
  sampleVis <- reactive({
    aSample <- val$sample
    
    aSample %>%
      ggvis() %>%
      set_options(width = 400, height = 100, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas",
        padding = padding(10, 10, 40, 43)) %>%
      add_axis("x", title = "Observations in this sample", grid = FALSE) %>%
      add_axis("y", ticks = 0, grid = FALSE) %>%
      scale_numeric("x", domain = plotRange, nice = FALSE, clamp = TRUE) %>%
      scale_numeric("y", domain = c(-2, 2), nice = FALSE, clamp = TRUE) %>%
      hide_legend('fill') %>%
      layer_points(x = ~x, y = 0, fill := "lightgray", fillOpacity := 0.5) %>%
      layer_rects(x = input$benchmarkX, x2 = input$benchmarkX, y := 0, y2 = -2, stroke := "lightgreen")
  })
  
  # plot samples drawn
  ciVis <- reactive({
    sampleStat <- val$sampleStat
    ciDf <- data.frame(x = sampleStat$Mean, ci = c(sampleStat$CILower, sampleStat$CIUpper))
    
    ciColor = "grey"
    if (val$popStat$Mean < sampleStat$CILower | val$popStat$Mean > sampleStat$CIUpper) {
      ciColor = "red"
    }
    
    ciDf %>%
      ggvis() %>%
      set_options(width = 400, height = 100, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas",
        padding = padding(10, 10, 40, 43)) %>%
      add_axis("x", title = paste0(input$confPercent, "% confidence interval"), grid = FALSE) %>%
      add_axis("y", ticks = 0, grid = FALSE) %>%
      scale_numeric("x", domain = plotRange, nice = FALSE, clamp = TRUE) %>%
      scale_numeric("y", domain = c(-2, 2), nice = FALSE, clamp = TRUE) %>%
      hide_legend('fill') %>%
      layer_rects(x = input$benchmarkX, x2 = input$benchmarkX, y := 0, y2 = -2, stroke := "lightgreen") %>% 
      layer_paths(x = ~ci, y = 0, stroke := ciColor, strokeWidth := 2) %>% 
      layer_points(x = ~x, y = 0, shape := "diamond", fill := "grey")
  })
  
  # handle sampling
  observeEvent(c(input$obsCount, input$sampleBtn, input$confPercent), {
    data <- isolate(val$data)
    
    # draw samples
    sampleRowIdxs <- sample.int(nrow(data), input$obsCount, replace = TRUE)
    aSample <- as.data.frame(data[sampleRowIdxs,])
    val$sample <- aSample
    
    
  })
  
  # handle confidence interval
  observeEvent(c(input$obsCount, input$sampleBtn, input$confPercent), {
    aSample <- val$sample
    
    # calculate statistics for the samples
    confLevel <- input$confPercent / 100
    sampleStat <- mean_cl_normal(aSample$x, conf.int = confLevel)
    names(sampleStat) <- c("Mean", "CILower", "CIUpper")
    val$sampleStat <- sampleStat
    
    # t-test
    tTestResults <- t.test(aSample$x, mu = input$benchmarkX, conf.level = confLevel)
    output$tTest <- renderPrint({
      tTestResults
    })  
    output$report <- renderPrint({
      "TODO"
    })
    
    # start the vis
    if (!val$isPlotInitialized)
    {
      # calculate statsitics for the population
      data <- isolate(val$data)
      confLevel <- input$confPercent / 100
      popStat <- mean_cl_normal(data[,1], conf.int = confLevel)
      names(popStat) <- c("Mean", "CILower", "CIUpper")
      val$popStat <- popStat
      
      sampleVis %>% bind_shiny("plotSample")
      ciVis %>% bind_shiny("plotCI")
      # sampleIntervalsVis %>% bind_shiny("plotSampleIntervals")  
      val$isPlotInitialized <- TRUE
    }
    
  })
  
  
}

shinyApp(ui, server)