if (!require("pacman")) install.packages("pacman", repos='https://stat.ethz.ch/CRAN/'); library(pacman)
p_load(shiny,
  knitr,
  markdown,
  tidyverse,
  grid,
  DT,
  tidyr, 
  knitr, 
  httpuv, 
  ggvis)

p_load(
  nortest   # for lillie.test(): The Kolmogorov-Smirnov test with a correct p-value
  )

# options(shiny.trace = FALSE)

source("../../util.R")


ui <- basicPage(
  tags$head(
    tags$style(HTML("
      #plotHistDiff-container, #plotQQ {
        margin:auto;
      }
    "))
  ),
  tags$h1("The normality assumption (two samples)"),
  
  splitLayout(
    div(
      plotOutput("plotScatter1", click = "plot1_click", width = "400px", height = "150px"),
      ggvisOutput("plotHist1")
    ),
    div(
      plotOutput("plotScatter2", click = "plot2_click", width = "400px", height = "150px"),
      ggvisOutput("plotHist2")
    )
  ),
  div(
    ggvisOutput("plotHistDiff")
  ),
  tags$ul(style = "font-size: 80%",
    tags$li("Histogram: The blue dot is mean; the blue line is 95% CI of the data."),
    tags$li("x1 - x2: The data are randomly trimmed to the same length before calculating the difference.")
  ),
  h4("Diagnostics"),
  splitLayout(cellWidths = c("200px", "auto"),
    div(
      plotOutput("plotQQ", width = "200px", height = "200px")
    ),
    div(# style="margin-top:100px",
      div(style = "font-size: 80%",
        "QQ plot:",
        tags$ul(
          tags$li("The vertical axis is the same as the horizontal axis in the plot of (x1 - x2) above."),
          tags$li("The points are sorted residuals."),
          tags$li("The horizontal axis is the quantile."),
          tags$li("The line indicates perfect normal distribution."),
          tags$li("The points should stay near the line.")
        )
      ),
      verbatimTextOutput("ks_result"),
      div(style = "font-size: 80%",
        tags$ul(
          tags$li("H0: (x1 - x2) are normally distributed."),
          tags$li("p-value more than .05 means the assumption is not violated.")
        )
      )
    )
  )
)


server <- function(input, output,session) {
  
  starting_n <- 10
  click_n <- 5
  x1 <- rnorm(starting_n, mean = 5, sd = 2)
  x2 <- rnorm(starting_n, mean = 10, sd = 2)
  y1 <- rnorm(starting_n) + 5
  y2 <- rnorm(starting_n) + 5
  xRange <- c(-1, 16)
  
  
  
  # initialize reactive values with existing data
  val <- reactiveValues(
    data1 = tibble(x = x1, y = y1), 
    data2 = tibble(x = x2, y = y2),
    diff = tibble(x  = x1 - x2)
    )
  
  #=============================================================================
  # mutators
  
  addPoint <- function(x, y, data) {
    xRand <- rnorm(click_n, mean = x, sd = 1)
    yRand <- rnorm(click_n, mean = y, sd = 1)
    data <- bind_rows(data, tibble(x = xRand, y = yRand))
    data <- tail(data, 200) # cap at 200 data points
    data
  }
  
  #=============================================================================
  # event handling
  
  observeEvent(input$plot1_click, {
    val$data1 <- addPoint(input$plot1_click$x, input$plot1_click$y, val$data1)
  })
  
  observeEvent(input$plot2_click, {
    val$data2 <- addPoint(input$plot2_click$x, input$plot2_click$y, val$data2)
  })
  
  #=============================================================================
  # renderers
  
  renderScatter <- function(data, xlab) {
    data %>% 
    ggplot(aes(x=x, y=y)) +
      geom_point() +
      theme_bw() +
      theme(legend.position="none") +
      coord_cartesian(xlim = c(xRange[1], xRange[2]),
        ylim = c(0, 20)) +
      xlab(xlab) +
      theme(
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
        )
  }
  
  
  renderHistogram <- function(data, xlab, xlim) {
    statMean <- mean(data$x)
    t_result <- t.test(data$x)
    ci_lower <- t_result$conf.int[1]
    ci_upper <- t_result$conf.int[2]
    
    data %>%
      ggvis(~x) %>% 
      
      # histogram
      layer_histograms(width = 1, fill := "lightgray", stroke := NA) %>%
      
      # mean
      layer_points(data = tibble(value = statMean), 
        x = ~value, y = 0, fillOpacity := 0.8, fill := "blue") %>%
      
      # vertical bar for the mean
      # layer_rects(data = tibble(x = statMean - 0.01, x2 =statMean + 0.01), 
      #   x = ~x, x2 = ~x2, y = 0, y2 := 0, stroke := "blue") %>%
      
      # error bar
      layer_paths(data = tibble(x = c(ci_lower, ci_upper)), 
        x = ~x, y = 0, stroke := "blue") %>% 
    
      add_axis("x", title = xlab) %>%
      scale_numeric("x", domain = xlim) %>%
      
      set_options(width = 400, height = 200, 
        resizable = FALSE, keep_aspect = TRUE, renderer = "canvas") %>%
      hide_legend('fill')
  }
  
  # QQ plot
  renderQQ <- function(data, ylab) {
    prev_par <- par(mar = c(5,4.1,0,0))
    car::qqPlot(data$x, ylab = ylab)
    par(prev_par)
  }
  
  
  #=============================================================================
  # bind renderers
  output$plotScatter1 <- renderPlot({
    renderScatter(val$data1, "x1")
  })
  
  output$plotScatter2 <- renderPlot({
    renderScatter(val$data2, "x2")
  })
  
  reactive({ renderHistogram(val$data1, "x1", xlim = xRange) }) %>% 
    bind_shiny("plotHist1")
  
  reactive({ renderHistogram(val$data2, "x2", xlim = xRange) }) %>% 
    bind_shiny("plotHist2")
  
  reactive({ 
    x1 <- val$data1$x
    x2 <- val$data2$x
    
    # trim to the same length
    min_n <- min(c(length(x1), length(x2)))
    x1_samples <- base::sample(x1, min_n)
    x2_samples <- base::sample(x2, min_n)
    
    # calculate difference and save for later use
    diff <- x1_samples - x2_samples
    val$diff <- tibble(x  = diff)
    
    # plot
    renderHistogram(val$diff, "x1 - x2", xlim = range(as.numeric(val$diff$x))) 
  }) %>% 
    bind_shiny("plotHistDiff")
  
  output$plotQQ <- renderPlot({
    renderQQ(val$diff, ylab = "x1 - x2")
  })
  
  #=============================================================================
  # bind raw text output
  output$ks_result <- renderPrint({
    nortest::lillie.test(val$diff$x)
  })
}

shinyApp(ui, server)