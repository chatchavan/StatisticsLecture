library(shiny)
library(ggplot2)
library(ggvis)
options(shiny.trace = FALSE)

# calculate descriptive statistics
findModes<-function(x){
  xtab<-table(x)
  modes<-xtab[max(xtab)==xtab]
  themodes<-names(modes)
  mode(themodes) <- typeof(x[1])
  mout<-list(values=themodes)
  return(mout)
}



ui <- basicPage(
  plotOutput("plotScatter", click = "plot_click", width = "400px", height = "200px"),
  ggvisOutput("plotHist"),
  ggvisOutput("plotSamples"),
  ggvisOutput("plotSampleHist"),
  actionButton("sampleBtn", "Sample 1000 times"),
  sliderInput("sampleWindow", "Showing from sample:", 1, 980, 1, 20),
  downloadButton('downloadData', 'Download data'),
  fileInput('file1', 'Upload data:',
            accept=c('text/csv', 
                     'text/comma-separated-values,text/plain', 
                     '.csv'))
)


server <- function(input, output) {
  
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
      xlim(-1, 16) +
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
      value = c(val$statMean, val$statMedian, val$statMode),
      stat = c("mean", "median", rep("mode", length(val$statMode)) ),
      color = c("blue", "green", rep("orange", length(val$statMode)))
    )
    statSDDf <- data.frame(
      x <- c(val$statMean - val$statSD, val$statMean + val$statSD),
      y <- c(1, 1)
    )
    
    # plot histogram
    histData %>%
      ggvis(~x) %>% 
      add_axis("x", title = "x") %>%
      scale_numeric("x", domain = c(-1, 16)) %>%
      layer_histograms(width = 0.1, fill := "lightgray", stroke := NA) %>%
      layer_points(data = statData, x = ~value, y = 0, fillOpacity := 0.8, fill := ~color) %>%
      layer_paths(data = statSDDf, x = ~x, y = 0, stroke := "blue") %>%
      set_options(width = 400, height = 200) %>%
      hide_legend('fill')
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
    # 
    # if (is.null(sampleDf))
    #   return(ggvis(data.frame(x = c(0, 1))))
    # 
    
    plotRange <- input$sampleWindow:(input$sampleWindow + 10)
    obsIdx <- vapply(((plotRange - 1) * obsCount) + 1, seq, rep(1.0, obsCount), length.out = obsCount)[]
    sampleDf[obsIdx,] %>%
      ggvis(~x, ~SampleId) %>%
      scale_numeric("x", domain = c(-1, 16)) %>%
      add_axis("y", title = "Sample ID", values = plotRange, subdivide = 1, tick_size_minor = 0, format = "#")  %>%
      add_axis("x", title = "Observations (blue) and mean of each sample (red)") %>%
      layer_points(fill := "lightblue") %>%
      layer_points(data = meanValDf[plotRange,], x = ~Mean, y = ~SampleId, shape := "diamond", fill := "red") %>%
      set_options(width = 400, height = 200)
  }) 
  
  # plot histogram of samples
  sampleHistVis <- reactive({
    meanValDf <- val$meanValDf
    sampleMeanDf <- val$sampleMeanDf
    meanValDf %>%
      ggvis(~Mean) %>% 
      scale_numeric("x", domain = c(-1, 16)) %>%
      add_axis("x", title = "Histogram: mean of the samples. Green dot: Mean of the means") %>%
      layer_histograms(width = 0.1, fill := "red", fillOpacity := 0.5, stroke := NA) %>%
      layer_points(data = sampleMeanDf, x = ~SampleMean, y = ~y, fill = "blue") %>%
      set_options(width = 400, height = 200) %>%
      hide_legend('fill')
  }) 
  
  
  # handle sampling
  observeEvent(input$sampleBtn, {
    input$sampleBtn
    
    # draw samples
    sampleCount <- 1000 # TODO: allow adjustment
    obsCount <- 30 # TODO: allow adjustment
    sampleRowIdxs <- matrix(sample.int(nrow(val$data), obsCount * sampleCount, replace = TRUE), nrow = sampleCount)
    sampleVals <- matrix(val$data[sampleRowIdxs], nrow = sampleCount)
    sampleDf <- data.frame(x = as.numeric(sampleVals), SampleId = rep(1:sampleCount, each = obsCount))
    
    # calculate mean of each sample (sample distribution)
    meanVals <- apply(sampleVals, 1, mean)
    meanValDf <- data.frame(Mean = meanVals, SampleId = 1:sampleCount)
    
    # calculate the sample mean (mean of means)
    sampleMean <- mean(meanVals)
    sampleMeanDf <- data.frame(SampleMean = sampleMean, y = 0)
    
    # update reactive values
    val$sampleDf <- sampleDf
    val$meanValDf <- meanValDf
    val$sampleMeanDf <- sampleMeanDf
    
    # start the vis
    sampleVis %>% bind_shiny("plotSamples")
    sampleHistVis %>% bind_shiny("plotSampleHist")
  })
  
}

shinyApp(ui, server)