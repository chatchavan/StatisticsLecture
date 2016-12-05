
library(shiny)
library(ggplot2)
options(shiny.trace = FALSE)
ui <- basicPage(
  plotOutput("plotScatter", click = "plot_click"),
  plotOutput("plotHist"),
  radioButtons("cls", "Class:", choices = list("Red" = -1, "Blue" = 1), selected = 1, inline = TRUE), 
  radioButtons("oneOrMany", "Add:", choices = list("one point" = 1, "20 points" = 20), selected = 1, inline = TRUE),
  sliderInput("spread", "Spread:", 0, 5, 1, 0.1),
  verbatimTextOutput("data")
)

server <- function(input, output) {
  
  x1 <- c(3, 10, 15, 3, 4, 7, 1, 12, 8, 18, 20, 4, 4, 5, 10)   #x
  x2 <- c(4, 10, 12, 17, 15, 20, 14, 3, 4, 15, 12, 5, 5, 6, 2) #y
  cls <- c(-1, 1, -1, 1, 1, 1, -1, 1, -1, 1, 1, 1, 1, -1, 1)   #class
  
  # initialize reactive values with existing data
  val <- reactiveValues( clickx = NULL, clicky = NULL, data = cbind (x1, x2, cls))
  
  observeEvent(input$updateData, {
    if (input$updateData > 0) {
      val$data <- rbind(val$data, cbind(input$plot_click$x, input$plot_click$y, as.numeric(input$cls)))
    }
  })
  
  
  observeEvent(input$plot_click, {
    if (input$oneOrMany == 1) {
      val$clickx <- c(val$clickx, input$plot_click$x)
      val$clicky <- c(val$clicky, input$plot_click$y)    
      val$data <- rbind(val$data, cbind(input$plot_click$x, input$plot_click$y, as.numeric(input$cls) ))
    } else {
      
      xRand <- rnorm(input$oneOrMany, mean = input$plot_click$x, sd = input$spread)
      yRand <- rnorm(input$oneOrMany, mean = input$plot_click$y, sd = input$spread)
      val$clickx <- c(val$clickx, xRand)
      val$clicky <- c(val$clicky, yRand)
      val$data <- rbind(val$data, cbind(xRand, yRand, rep(as.numeric(input$cls), input$oneOrMany)  ))
    }
  })        
  
  output$plotScatter <- renderPlot({
    p <- ggplot(data = NULL, aes(x=val$data[,1], y=val$data[,2], color = ifelse(val$data[,3] > 0, "Class 1","Class -1")))
    p <- p + geom_point()
    p <- p + xlab("x1")  
    p <- p + ylab("x2") 
    p <- p + scale_color_manual(name="Class Labels", values=c('#f8766d','#00BFC4'))
    p <- p + guides(color = guide_legend(override.aes = list(linetype = 0 )), 
                    linetype = guide_legend())
    p <- p + theme_bw() 
    p
  })
  
  output$plotHist <- renderPlot({
    p <- ggplot(data = NULL, aes(x=val$data[,1])) +
      geom_histogram(binwidth=1, colour = "black")
    p
  })
  
  output$info <- renderText({
    input$plot_click
    paste0("x = ", val$clickx, ", y = ",val$clicky, "\n")
  })
  output$data <- renderPrint({
    val$data
  })
  
}

shinyApp(ui, server)