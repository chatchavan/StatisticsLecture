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
  rmarkdownOutput("../../Instructions/confidenceInterval-1.Rmd"),
  sidebarLayout(position = "right",
    sidebarPanel(
      sliderInput("sampleCount", "How many times to sample?:", 10, 500, 100, 10),
      sliderInput("obsCount", "How many observations in each sample?:", 5, 50, 10, 1),
      actionButton("sampleBtn", "Draw samples")
    ),
    mainPanel(
      plotOutput("plotScatter", click = "plot_click", width = "400px", height = "150px"),
      ggvisOutput("plotHist"),
      ggvisOutput("plotSampleHist"),
      ggvisOutput("plotNormMean")
      # "*: Each of the mean is centered around the population mean (corresponding to zero in this plot) and scaled by the SE of each sample."
    )
  ),
  rmarkdownOutput("../../Instructions/confidenceInterval-2.Rmd"),
  sidebarLayout(position = "right",
    sidebarPanel(
      sliderInput("tArea", "Area under the blue line:", 0, 0.999, 0.95, 0.05)
    ),
    mainPanel(
      ggvisOutput("plotTAreas"),
      ggvisOutput("plotCINorm"),
      ggvisOutput("plotCI")
    )
  )
)


server <- function(input, output,session) {
  
  x <- c(3, 10, 15, 3, 4, 7, 1, 12)
  y <- c(4, 10, 12, 17, 15, 20, 14, 3)
  normXRange <- c(-5, 5)
  normXResolution <- 0.01
  
  
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
                        normMeanVis = NULL,
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
      xlim(-1, 16) +
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
      scale_numeric("x", domain = c(-1,16)) %>%
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
      scale_numeric("x", domain = c(-1, 16)) %>%
    
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
  
  # plot histogram of rescaled samples
  normMeanVis <- reactive({
    meanValDf <- val$meanValDf
    
    # adjust location and scale
    popMean <- val$statMean
    meanValDf$normMean <- (meanValDf$Mean - popMean) / (meanValDf$SD / sqrt(input$obsCount))
    
    # remove samples that are -Inf (all observations are the same data point)
    meanValDf <- meanValDf[which(!is.infinite(meanValDf$normMean)),]
    
    # binning data and calculate probability histogram
    suppressMessages(
      bins <- compute_bin(meanValDf, ~normMean)
    )
    bins$prob <- bins$count_ / ( bins$width_[1] * sum(bins$count_))
    
    
    # generate the t location-scale distribution
    dtX <- seq(normXRange[1], normXRange[2], normXResolution)
    dtY <- dt(dtX, df = input$obsCount - 1)
    dtDf <- data.frame(x = dtX, y = dtY)
    
    # for plotting mean
    statData <- data.frame(
      x = 0,
      y = 0
    )
    
    # plot (and save to reactive variable for reuse)
    val$normMeanVis <- bins %>%
      ggvis(x = ~x_, y = ~prob) %>%
      set_options(width = 400, height = 200, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas") %>%
      add_axis("x", title = "Normalized means") %>%
      add_axis("y", title = "Relative frequency density") %>% 
      scale_numeric("x", domain = normXRange, nice = FALSE, clamp = TRUE) %>%
      hide_legend('fill') %>%

      # distribution of means
      layer_bars(width = bins$width_[1], stack = FALSE, fill := "lightgrey", stroke := NA) %>% 
      
      # t distribution
      layer_paths(data = dtDf, x = ~x, y = ~y, stroke := "lightblue", strokeWidth := 3) %>%

      # standard error is roughly equal to 1
      layer_rects(data = statData,x = -1, x2 = 1, y = ~y, y2 = ~y, stroke := "green") %>% 
      
      # the scale is centered around population mean
      layer_points(data = statData, x = ~x, y = ~y, fillOpacity := 0.8, fill := "blue") %>%
      layer_rects(data = statData, x = ~x, x2 = ~x, y := 0, y2 = 0, stroke := "blue")
  
    val$normMeanVis
  })
  
  tAreaVis <- reactive({
    tDOF <- input$obsCount - 1
    tX <- seq(normXRange[1], normXRange[2], normXResolution)
    tY <-  dt(tX, df = tDOF)
    tArea <- input$tArea
    tVals <- sort(qt(c(tArea, 1 - tArea) , df = tDOF))
    selected <- ifelse(tX < tVals[1] | tX > tVals[2], FALSE, TRUE)
    fill <- ifelse(selected, "blue", NA)
    distDf <- data.frame(x = tX, y = tY, selected = selected, fill = fill)
    
    selDf <- distDf[which(distDf$selected == TRUE),]
    
    val$normMeanVis %>% 
      layer_ribbons(data = distDf, x = ~x, y = ~y, y2 = 0, fill := "white", fillOpacity := 0.8) %>%
      layer_ribbons(data = selDf, x = ~x, y = ~y, y2 = 0, fill := "lightblue", fillOpacity := 0.6) %>%
      hide_legend("fill")
  })
  
  tCINormVis <- reactive({
    tDOF <- input$obsCount - 1
    tArea <- input$tArea
    tVals <- sort(qt(c(tArea, 1 - tArea) , df = tDOF))
    ciDf <- data.frame(x = 0, ci = tVals)
    
    ciDf %>%
      ggvis() %>%
      set_options(width = 400, height = 100, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas",
        padding = padding(10, 10, 40, 43)) %>%
      add_axis("x", title = "Interval in the 'Normalized means' scale", grid = FALSE) %>%
      add_axis("y", ticks = 0, grid = FALSE) %>%
      scale_numeric("x", domain = normXRange, nice = FALSE, clamp = TRUE) %>%
      scale_numeric("y", domain = c(-2, 2), nice = FALSE, clamp = TRUE) %>%
      hide_legend('fill') %>%
      layer_paths(x = ~ci, y = 0, stroke := "lightblue", strokeWidth := 2) %>% 
      layer_points(x = ~x, y = 0, shape := "diamond", fill := "grey")
  })
  
  tCIVis <- reactive({
    tDOF <- input$obsCount - 1
    tArea <- input$tArea
    tVals <- sort(qt(c(tArea, 1 - tArea) , df = tDOF))
    aMean <- val$meanValDf$Mean[1]
    aSE <- val$meanValDf$SD[1] / sqrt(input$obsCount)
    ciDf <- data.frame(x = aMean, ci = c(aMean - tVals * aSE, aMean - tVals * aSE))
    
    ciDf %>%
      ggvis() %>%
      set_options(width = 400, height = 100, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas",
        padding = padding(10, 10, 40, 43)) %>%
      add_axis("x", title = "Interval in the original scale", grid = FALSE) %>%
      add_axis("y", ticks = 0, grid = FALSE) %>%
      scale_numeric("x", domain = c(-1, 16), nice = FALSE, clamp = TRUE) %>%
      scale_numeric("y", domain = c(-2, 2), nice = FALSE, clamp = TRUE) %>%
      hide_legend('fill') %>%
      layer_paths(x = ~ci, y = 0, stroke := "grey", strokeWidth := 2) %>% 
      layer_points(x = ~x, y = 0, shape := "diamond", fill := "grey")
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
      sampleHistVis %>% bind_shiny("plotSampleHist")
      normMeanVis %>% bind_shiny("plotNormMean")
      tAreaVis %>% bind_shiny("plotTAreas")
      tCINormVis %>% bind_shiny("plotCINorm")
      tCIVis %>% bind_shiny("plotCI")
      val$isPlotInitialized <- TRUE
    }
  })
  
  
}

shinyApp(ui, server)