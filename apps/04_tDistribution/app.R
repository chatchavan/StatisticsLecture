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
  MASS,
  ggvis)

# options(shiny.trace = TRUE)

source("../../util.R")

ui <- basicPage(
  useShinyjs(),
  rmarkdownOutput("../../Instructions/tDistribution-1.Rmd"),
  sidebarLayout(position = "right",
    sidebarPanel(
      sliderInput("sampleCount", "How many times to sample?:", 2, 1000, 750, 1),
      sliderInput("obsCount", "How many observations per sample?:", 2, 50, 4, 1),
      hr(),
      sliderInput("zoomX", "Zoom X:", -5, 5, c(-3.2, -1.2), 0.1),
      sliderInput("zoomY", "Zoom Y:", 0, 1, c(0, 0.2), 0.001)
    ),
    mainPanel(
      plotOutput("plotGosset"),
      "Zoomed-in:",
      plotOutput("plotGossetZoom")
    )
  ),
  rmarkdownOutput("../../Instructions/tDistribution-2.Rmd"),
  sidebarLayout(position = "right",
    sidebarPanel(
      sliderInput("tDOF", "Degrees of freedom:", 1, 100, 1, 1),
      sliderInput("xSel", "x:", -5, 5, -1.64, 0.1)
    ),
    mainPanel(
      ggvisOutput("plotTvsNorm"),
      tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: blue}")),
      tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: red}")),
      tags$style(HTML(".js-irs-4 > .irs-slider {display: none}")),
      tags$style(HTML(".js-irs-5 > .irs-slider {display: none}")),
      sliderInput("areaTSlider", "Highlighted area under t distribution:", 0, 1, 0.2, 0.001),
      sliderInput("areaNormSlider", "Highlighted area under normal distribution:", 0, 1, 0.2, 0.001)
    )
  )
)


server <- function(input, output, session) {
  val <- reactiveValues(gossetData = NULL,
        isPlotInitialized = FALSE,
        xVal = -1.64)
  
  output$plotGosset <- renderPlot({
    nSamples <- input$sampleCount
    nObs <- input$obsCount
    
    # prepare the data
    if (is.null(val$gossetHeight)) {
      heIn <- round(as.numeric(colnames(crimtab)) / 2.54)
      val$gossetData <- data.frame(height = rep(heIn, colSums(crimtab)))
    }
    data <- val$gossetData
    
    # draw samples
    sampleRowIdxs <- matrix(sample.int(nrow(data), size = nSamples* nObs, replace = TRUE), nrow = nSamples)
    sampleVals <- matrix(data$height[sampleRowIdxs], nrow = nSamples)
    
    # calculate mean and SD of each sample (sample distribution)
    meanVals <- apply(sampleVals, 1, mean)
    sdVals <- apply(sampleVals, 1, sd)  # * sqrt(3/4)
    
    # Student's Z: the difference between mean of each sample and the mean of the 
    # population divided by the standard deviation of the sample
    studentZ <- (meanVals - mean(data$height)) / sdVals
    
    # Replace infinite values by +/- 6 as in Student's paper
    infZ <- which(is.infinite(studentZ))
    studentZ[infZ] <- 6 * sign(studentZ[infZ])
    
    # plot
    tDOF <- nObs - 1
    
    zoomX <- input$zoomX
    zoomY <- input$zoomY
    output$plotGossetZoom <- renderPlot({
      truehist(studentZ, prob = TRUE, col = "lightgrey", xlim = zoomX, ylim = zoomY, xlab = "Gosset statistics")
      curve(dt(x, df = tDOF), add = TRUE, col = "lightblue", lwd = 3)
      curve(dnorm(x), add = TRUE, col = "red", lty = 2)
    })
    
    # plot
    truehist(studentZ, prob = TRUE, col = "lightgrey", xlim = c(-5, 5), ylim = c(0,1), xlab = "Gosset statistics")
    curve(dt(x, df = tDOF), add = TRUE, col = "lightblue", lwd = 3)
    curve(dnorm(x), add = TRUE, col = "red", lty = 2)
    rect(zoomX[1], zoomY[1], zoomX[2], zoomY[2], border = "orange")
  })
 
  plotTvsNorm <- reactive({ 
    tDOF <- input$tDOF
    x <- seq(-5, 5, 0.01)
    yNorm <- dnorm(x)
    yT <- dt(x, df = tDOF)
    
    xVal <- input$xSel
    selected <- ifelse(x < xVal, TRUE, FALSE)
    fill <- ifelse(selected, "blue", NA)
    distDf <- data.frame(x = x, yNorm = yNorm, yT = yT, selected = selected, fill = fill)
    
    # update area under the selected section
    areaT <- pt(xVal, tDOF)
    areaNorm <- pnorm(xVal)
    updateSliderInput(session, "areaTSlider", value = areaT)
    updateSliderInput(session, "areaNormSlider", value = areaNorm)
    
    # plot
    distDf %>% 
      ggvis() %>% 
      scale_numeric("x", domain = c(-5, 5)) %>%
      set_options(width = 450, height = 250, resizable = FALSE, keep_aspect = TRUE) %>%
      add_axis("y", title = "density", title_offset = 50) %>% 
      add_axis("x", title = "") %>% 
      layer_paths(~x, ~yT, stroke := "lightblue", strokeWidth := 5) %>% 
      layer_paths(~x, ~yNorm, strokeDash := 5, stroke := "red") %>% 
      filter(selected) %>% 
      layer_ribbons(~x, ~yNorm, y2 = 0, fill := "red", fillOpacity := 0.2) %>% 
      layer_ribbons(~x, ~yT, y2 = 0, fill := "blue", fillOpacity := 0.2) %>% 
      hide_legend("fill")
      
  })
  plotTvsNorm %>% bind_shiny("plotTvsNorm")
  
  
  # observe changes in the area sliders
  # observeEvent("xSel", {
  #   val$xVal <- input$xSel
  # })
  # 
  # observeEvent("areaNormSlider", {
  #   val$xVal <- qnorm(input$areaNormSlider)
  # })
  # 
  # observeEvent("areaTSlider", {
  #   val$xVal <- qt(input$areaTSlider, input$tDOF)
  # })
}

shinyApp(ui, server)