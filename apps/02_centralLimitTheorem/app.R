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
  rmarkdownOutput("../../Instructions/centralLimitTheorem.Rmd"),
  sidebarLayout(position = "right",
    sidebarPanel(
      sliderInput("sampleCount", "How many times to sample?:", 10, 5000, 1000, 10),
      sliderInput("obsCount", "How many observations in each sample?:", 5, 50, 30, 1),
      actionButton("sampleBtn", "Draw samples"),
      hr(),
      sliderInput("sampleWindow", "Showing from sample:", 1, 980, 1, 20),
      hr(),
      sliderInput("xRange", "Range of x-axis:", -20, 20, c(-1, 16), 0.5),
      hr(),
      downloadButton('downloadData', 'Download data'),
      fileInput('file1', 'Upload data:',
        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
    ),
    mainPanel(
      p("Click anywhere on this plot to add twenty data points."),
      plotOutput("plotScatter", click = "plot_click", width = "400px", height = "150px"),
      ggvisOutput("plotHist"),
      ggvisOutput("plotSamples"),
      ggvisOutput("plotSampleHist")
    )
  )
  
  
  
)


server <- function(input, output,session) {
  
  x <- c(3, 10, 15, 3, 4, 7, 1, 12)
  y <- c(4, 10, 12, 17, 15, 20, 14, 3)
  
  
  # initialize reactive values with existing data
  val <- reactiveValues(data = cbind (x = x, y = y), 
                        statMean = NULL, 
                        statMedian = NULL, 
                        statMode = NULL,
                        statSD = NULL,
                        sampleDf = NULL,
                        meanValDf = NULL,
                        barDf = NULL,
                        sampleMeanDf = NULL)
  
  # observe click on the scatterplot
  observeEvent(input$plot_click, {
      xRand <- rnorm(20, mean = input$plot_click$x, sd = 1)
      yRand <- rnorm(20, mean = input$plot_click$y, sd = 1)
      val$data <- rbind(val$data, cbind(x = xRand, y = yRand))
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
  output$statOutput <- renderText({
    val$data
    outText <- sprintf("Mean (Blue): %.2f\nMedian (Green): %.2f\nMode(s) (Orange): %s", 
            isolate(val$statMean),
            isolate(val$statMedian),
            paste(formatC(isolate(val$statMode), digits = 2), collapse = ", "),
            isolate(val$statSD))
    outText
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
    barDf <- val$barDf
    
    plotRange <- input$sampleWindow:(input$sampleWindow + 9)
    
    # for SD
    meanValDf <- meanValDf[plotRange,]
    barDf <- barDf[barDf$SampleId %in% plotRange,]
    
    # observation indices
    obsIdx <- vapply(((plotRange - 1) * input$obsCount) + 1, seq, rep(1.0, input$obsCount), length.out = input$obsCount)[]
    
    # actual plot
    sampleDf[obsIdx,] %>%
      ggvis(~x, ~SampleId) %>%
      
      # observations
      layer_points(fill := "lightgray", fillOpacity := 0.5) %>%
      
      # mean of each sample
      layer_points(data = meanValDf, x = ~Mean, y = ~SampleId, shape := "diamond", fill := "red") %>%
      
      # SD of each sample
      layer_rects(data = barDf, x = ~x, x2 = ~x2, y = ~y, y2 = ~y2, fill := "red", stroke := NA) %>%
      
      
      # other plot parameters
      scale_numeric("x", domain = input$xRange) %>%
      add_axis("y", title = "Sample ID", values = plotRange, subdivide = 1, tick_size_minor = 0, format = "#")  %>%
      add_axis("x", title = "Observations (blue) and mean of each sample (red)") %>%
      hide_legend("stroke") %>%
      set_options(width = 400, height = 200, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas")
    
    
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
      add_axis("x", title = "Red histogram: mean of the samples. Green dot: Mean of the means and its SD") %>%
      hide_legend('fill') %>%
      scale_numeric("x", domain = input$xRange) %>%
    
      # standard deviation of the sample means
      layer_rects(data = sdDf, x = ~x, x2 = ~x2, y = -1, y2 = 1, fill := "green", stroke := NA) %>%
      
      # distribution of means
      layer_histograms(width = 0.1, fill := "red", fillOpacity := 0.3, stroke := NA) %>%
      
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
    
    # calculate the interval for plotting SD
    meanValDf$barMin <- meanValDf$Mean - meanValDf$SD
    meanValDf$barMax <- meanValDf$Mean + meanValDf$SD
    barDf <- meanValDf[,c("SampleId", "barMin", "barMax")]
    names(barDf)[names(barDf) %in% c("barMin", "barMax")] <- c("x", "x2")
    barWidth <- 0.1
    barDf$y2 <- barDf$SampleId - (barWidth / 2)
    barDf$y <- barDf$SampleId +  (barWidth / 2)
    
    
    # calculate the sample mean (mean of means)
    sampleMean <- mean(meanVals)
    sampleMeanDf <- data.frame(SampleMean = sampleMean, y = 0)
    
    # update reactive values
    val$sampleDf <- sampleDf
    val$meanValDf <- meanValDf
    val$sampleMeanDf <- sampleMeanDf
    val$barDf <- barDf
    
    # start the vis
    sampleVis %>% bind_shiny("plotSamples")
    sampleHistVis %>% bind_shiny("plotSampleHist")
  })
  
}

shinyApp(ui, server)