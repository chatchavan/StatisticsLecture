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
  rmarkdownOutput("../../Instructions/intervalEstimates.Rmd"),
  sidebarLayout(position = "right",
    sidebarPanel(
      sliderInput("sampleCount", "How many times to sample?:", 10, 500, 100, 10),
      sliderInput("obsCount", "How many observations in each sample?:", 5, 50, 30, 1),
      actionButton("sampleBtn", "Draw samples"),
      hr(),
      
      sliderInput("sampleWindow", "Showing from sample:", 1, 980, 1, 20),
      sliderInput("xRange", "Range of x-axis:", -20, 20, c(-1, 16), 0.5),
      hr(),
      
      radioButtons("barType", "Interval estimate type:", 
        c("Standard deviation (SD)" = "sdBarDf", 
          "Standard error (SE)" = "seBarDf", 
          "Confidence interval (CI)"= "ciBarDf")),
      sliderInput("confPercent", "% confidence:", 1, 100, 95, 1),
      hr(),
      
      downloadButton('downloadData', 'Download data'),
      fileInput('file1', 'Upload data:',
        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
    ),
    mainPanel(
      plotOutput("plotScatter", click = "plot_click", width = "400px", height = "150px"),
      ggvisOutput("plotHist"),
      ggvisOutput("plotSamples"),
      ggvisOutput("plotSampleHist"),
      ggvisOutput("plotSampleIntervals"),
      p(textOutput("nonCaptureCount", inline = TRUE),
        " intervals that doesn't capture the population mean (shown in red)")
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
                        sdBarDf = NULL,
                        seBarDf = NULL,
                        ciBarDf = NULL,
                        nonCaptureCount = NULL,
                        sampleMeanDf = NULL)
  
  # observe click on the scatterplot
  observeEvent(input$plot_click, {
      xRand <- rnorm(20, mean = input$plot_click$x, sd = 1)
      yRand <- rnorm(20, mean = input$plot_click$y, sd = 1)
      data <- rbind(val$data, cbind(x = xRand, y = yRand))
      data <- tail(data, 200) # cap at 200 data points
      
      val$data <- data
  })        
  
  # render scatterplot
  output$plotScatter <- renderPlot({
    p <- ggplot(data = NULL, aes(x=val$data[,1], y=val$data[,2])) +
      geom_point() +
      theme_bw() +
      theme(legend.position="none") +
      xlim(input$xRange[1], input$xRange[2]) +
      xlab("x") +
      ylab("y")
      
    p
  })
  
  
  # handle file upload
  handleUpload <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    val$data <- read.csv(inFile$datapath)  
  })
  
  # render histogram (and calculate statistics)
  hisVis <- reactive({
    handleUpload()
    
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
  hisVis %>% bind_shiny("plotHist")
  
  
  # text output
  output$nonCaptureCount <- renderText({
    val$nonCaptureCount
  })
  
  
  # handle download button
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(val$data, file, row.names = F, na = "")
    }
  )
  
 
  # showing sampled observations
  sampleVis <- reactive({
    sampleDf <- val$sampleDf
    meanValDf <- val$meanValDf
    barDf <- val[[input$barType]]
    
    plotRange <- input$sampleWindow:(input$sampleWindow + 9)
    
    # for error bar
    meanValDf <- meanValDf[plotRange,]
    barDf <- barDf[barDf$SampleId %in% plotRange,]
    
    # observation indices
    obsIdx <- vapply(((plotRange - 1) * input$obsCount) + 1, seq, rep(1.0, input$obsCount), length.out = input$obsCount)[]
    
    # actual plot
    sampleDf[obsIdx,] %>%
      ggvis(~x, ~SampleId) %>%
      
      # observations
      layer_points(fill := "lightgray", fillOpacity := 0.8, size := 10) %>%
      
      # mean of each sample
      layer_points(data = meanValDf, x = ~Mean, y = ~SampleId, shape := "diamond", fill := "grey", fillOpacity := 1.0) %>%
      
      # SD of each sample
      layer_rects(data = barDf, x = ~x, x2 = ~x2, y = ~y, y2 = ~y2, fill := "grey", stroke := NA) %>%
      
      
      # other plot parameters
      scale_numeric("x", domain = input$xRange) %>%
      add_axis("y", title = "Sample ID", values = plotRange, subdivide = 1, tick_size_minor = 0, format = "#")  %>%
      add_axis("x", title = "Observations (dots) and mean of each sample (diamonds)") %>%
      hide_legend("stroke") %>%
      set_options(width = 400, height = 200, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas")
  }) 
  
  
  # showing sampled observations
  sampleIntervalsVis <- reactive({
    barDf <- val[[input$barType]]
    
    # population mean
    popMean <- isolate(val$statMean)
    meanVbarDf <- data.frame(x = popMean - 0.01, x2 = popMean + 0.01)
    
    # highlight bars that doesn't include the population mean
    exclPopMean <- (barDf$x > popMean | barDf$x2 < popMean)
    barDf$fill <- ifelse(exclPopMean, "red", "grey")
    barDf$y <- barDf$y + ifelse(exclPopMean, 0.2, 0)
    barDf$y2 <- barDf$y2 - ifelse(exclPopMean, 0.2, 0)
    val$nonCaptureCount <- length(which(exclPopMean))

    # bar title
    barNames <- c("sdBarDf" = "Standard deviation", 
      "seBarDf" = "Standard error",
      "ciBarDf" = paste0(input$confPercent, "% Confidence Interval"))
    xTitle <- as.character(barNames[input$barType])
    
    # actual plot
    barDf %>%
      ggvis(~x) %>%
      
      # interval of each sample
      layer_rects(x = ~x, x2 = ~x2, y = ~y, y2 = ~y2, fill := ~fill, stroke := NA) %>%
      
      # population
      layer_rects(data = meanVbarDf, x = ~x, x2 = ~x2, y := 0, y2 = 0, stroke := "blue") %>%
      
      # other plot parameters
      scale_numeric("x", domain = input$xRange) %>%
      add_axis("y", title = "Sample ID", format = "#", grid = FALSE)  %>%
      add_axis("x", title = xTitle) %>%
      hide_legend("stroke") %>%
      hide_legend("fill") %>%
      set_options(width = 400, height = 400, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas")
  }) 
  
  # plot histogram of samples
  sampleHistVis <- reactive({
    meanValDf <- val$meanValDf
    sampleMeanDf <- val$sampleMeanDf
    
    sdOfSampleMeans <- sd(meanValDf$Mean)
    sdLeft <- sampleMeanDf$SampleMean - sdOfSampleMeans
    sdRight <- sampleMeanDf$SampleMean + sdOfSampleMeans
    sdDf <- data.frame(x = sdLeft, x2 = sdRight)
    
    meanVbarDf <- data.frame(x = sampleMeanDf$SampleMean - 0.01, x2 = sampleMeanDf$SampleMean + 0.01)
    
    meanValDf %>%
      ggvis(~Mean) %>% 
      set_options(width = 400, height = 200, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas") %>%
      add_axis("x", title = "Green dot: Mean of the means and its SD") %>%
      add_axis("y", title = "Count of means") %>% 
      hide_legend('fill') %>%
      scale_numeric("x", domain = input$xRange) %>%
    
      # standard deviation of the sample means
      layer_rects(data = sdDf, x = ~x, x2 = ~x2, y = 0, y2 = 0, stroke := "green") %>%
      
      # distribution of means
      layer_histograms(width = 0.1, fill := "grey", fillOpacity := 0.5, stroke := NA) %>%
      
      # mean of the sample means (sample mean)
      layer_points(data = sampleMeanDf, x = ~SampleMean, y = ~y, fill := "white", stroke := "green") %>%
      layer_rects(data = meanVbarDf, x = ~x, x2 = ~x2, y := 0, y2 = 0, stroke := "green")
  }) 
  
  
  
  # update sample navigation slider
  observeEvent(input$sampleCount, {
    updateSliderInput(session, "sampleWindow", max = input$sampleCount - 9)
  })
  
  
  # handle sampling
  observeEvent(c(input$sampleCount, input$obsCount, input$sampleBtn), {
    data <- isolate(val$data)
    
    # draw samples
    sampleRowIdxs <- matrix(sample.int(nrow(data), input$obsCount * input$sampleCount, replace = TRUE), nrow = input$sampleCount)
    sampleVals <- matrix(data[sampleRowIdxs], nrow = input$sampleCount)
    sampleDf <- data.frame(x = as.numeric(sampleVals), SampleId = rep(1:input$sampleCount, each = input$obsCount))
    
    # calculate mean and SD of each sample (sample distribution)
    meanVals <- apply(sampleVals, 1, mean)
    sdVals <- apply(sampleVals, 1, sd)
    meanValDf <- data.frame(Mean = meanVals, SD = sdVals, SampleId = 1:input$sampleCount)
    
    # calculate the intervals for plotting
    valSE <- meanValDf$SD / sqrt(input$obsCount)
    sdBarDf <- makeBarDf(meanValDf, meanValDf$SD)
    seBarDf <- makeBarDf(meanValDf, valSE)
    # NOTE: CI is calculated in a separate reactive block below
    
    # calculate the sample mean (mean of means)
    sampleMean <- mean(meanVals)
    sampleMeanDf <- data.frame(SampleMean = sampleMean, y = 0)
    
    # update reactive values
    val$sampleDf <- sampleDf
    val$meanValDf <- meanValDf
    val$sampleMeanDf <- sampleMeanDf
    val$sdBarDf <- sdBarDf
    val$seBarDf <- seBarDf
    
    
    # start the vis
    if (!val$isPlotInitialized)
    {
      sampleVis %>% bind_shiny("plotSamples")
      sampleHistVis %>% bind_shiny("plotSampleHist")
      sampleIntervalsVis %>% bind_shiny("plotSampleIntervals")  
      val$isPlotInitialized <- TRUE
    }
  })
  
  # handle confidence percent change
  observeEvent(c(input$sampleCount, input$obsCount, input$sampleBtn, input$confPercent), {
    confPercent <- input$confPercent / 100
    valSE <- val$meanValDf$SD / sqrt(input$obsCount)
    valCI <- valSE * qt(1 - (1 - confPercent) / 2, df = input$obsCount - 1)
    ciBarDf <- makeBarDf(val$meanValDf, valCI)
    
    # update reactive values
    val$ciBarDf <- ciBarDf
  })
  
  
  observeEvent(input$barType, {
    if (input$barType != "ciBarDf") {
      disable("confPercent")
    } else {
      enable("confPercent")
    }
  })

  
}

shinyApp(ui, server)