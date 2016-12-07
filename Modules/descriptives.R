#===============================================================================
# descriptive statistics

## module ui
descriptivesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(position = "right",
      sidebarPanel(
        radioButtons(ns("oneOrMany"), "Add:", choices = list("one point" = 1, "20 points" = 20), selected = 1, inline = TRUE),
        sliderInput(ns("spread"), "Spread (when adding >1 one point):", 0, 5, 1, 0.1),
        hr(),
        sliderInput(ns("binwidth"), "Histogram bin width:", 0.1, 8, 1, 0.1),
        hr(),
        downloadButton(ns("downloadData"), "Download data"),
        fileInput(ns("file1"), "Upload data:",
          accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      ),
      mainPanel(
        plotOutput(ns("plotScatter"), click = ns("scatterClick"), width = "400px", height = "200px"),
        ggvisOutput(ns("plotHist")),
        verbatimTextOutput(ns("statOutput"))
      )
    )
  )
}

## module server
descriptives <- function(input, output, session) {
  ns <- session$ns
  
  x <- c(3, 10, 15, 3, 4, 7, 1, 12)
  y <- c(4, 10, 12, 17, 15, 20, 14, 3)
  
  # initialize reactive values with existing data
  val <- reactiveValues(data = cbind (x = x, y = y), 
    statMean = NULL, 
    statMedian = NULL, 
    statMode = NULL,
    statSD = NULL)
  
  # observe click on the scatterplot
  observeEvent(ns("scatterClick"), {
    if (input$oneOrMany == 1) {
      val$data <- rbind(val$data, cbind(x = input$plot_click$x, y = input$plot_click$y ))
    } else {
      
      xRand <- rnorm(input$oneOrMany, mean = input$plot_click$x, sd = input$spread)
      yRand <- rnorm(input$oneOrMany, mean = input$plot_click$y, sd = input$spread)
      val$data <- rbind(val$data, cbind(x = xRand, y = yRand))
    }
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
      value = val$statMode,
      stat = rep("mode", length(val$statMode) ),
      color = rep("orange", length(val$statMode))
    )
    statSDDf <- data.frame(
      x <- c(val$statMean - val$statSD, val$statMean + val$statSD),
      y <- c(1, 1)
    )
    
    # for plotting vertical bars
    statVars <- c(val$statMean, val$statMedian)
    statVbarDf <- data.frame(
      x = statVars - 0.01, x2 = statVars + 0.01, stroke = c("blue", "green"))
    
    # plot histogram
    histData %>%
      ggvis(~x) %>% 
      scale_numeric("x", domain = c(-1, 16)) %>%
      add_axis("x", title = "x") %>%
      set_options(width = 400, height = 200, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas") %>%
      hide_legend('fill') %>%
      
      # data histogram
      layer_histograms(width = input$binwidth, fill := "lightgray", stroke := NA) %>%
      
      # mean and median are shown as vertical lines
      layer_rects(data = statVbarDf, x = ~x, x2 = ~x2, y := 0, y2 = 0, stroke := ~stroke) %>%
      
      # SD is shown as horizontal line
      layer_paths(data = statSDDf, x = ~x, y = 0, stroke := "blue") %>%
      
      # modes are shown as dots
      layer_points(data = statData, x = ~value, y = 0, fillOpacity := 0.8, fill := ~color)
  })
  hisVis %>% bind_shiny(ns("plotHist"))
  
  
  # text output
  output$statOutput <- renderText({
    val$data
    outText <- sprintf("Mean (Blue vertical line):\t%.2f\nMedian (Green vertical line):\t%.2f\nMode(s) (Orange dots):\t%s\n\nSD (half of the horizontal blue line): %.2f", 
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

}
