library(shiny)
library(ggplot2)
library(ggvis)
# options(shiny.trace = FALSE)

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
  verbatimTextOutput("statOutput"),
  sliderInput("binwidth", "Histogram bin width:", 1, 8, 1, 0.5),
  radioButtons("oneOrMany", "Add:", choices = list("one point" = 1, "20 points" = 20), selected = 1, inline = TRUE),
  sliderInput("spread", "Spread (when adding >1 one point):", 0, 5, 1, 0.1),
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
                        statSD = NULL)
  
  # observe click on the scatterplot
  observeEvent(input$plot_click, {
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
      layer_histograms(width = input$binwidth, fill := "lightgray", stroke := NA) %>%
      layer_points(data = statData, x = ~value, y = 0, fillOpacity := 0.8, fill := ~color) %>%
      layer_paths(data = statSDDf, x = ~x, y = 0, stroke := "blue") %>%
      set_options(width = 400, height = 200) %>%
      hide_legend('fill')
  })
  hisVis %>% bind_shiny("plotHist")
  
  
  # text output
  output$statOutput <- renderText({
    val$data
    outText <- sprintf("Mean (Blue): %.2f\nMedian (Green): %.2f\nMode(s) (Orange): %s\n\nSD (half of the blue line): %.2f", 
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

shinyApp(ui, server)