
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

#===============================================================================
# pre-requisite

theme_set(theme_gray())

source("util.R")



#===============================================================================
# data generator

## module ui
dataGeneratorUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("rowCount"), "Number of rows in the dataset:", 1, 100, 30, 1),
    hr(),
    dataTableOutput(ns("dfTable"))
  )
}

## module server
dataGenerator <- function(input, output, session) {
  ns <- session$ns
  
  df <- reactive({
    n <- input$rowCount
    tcts <- round(rnorm(n, mean = 15, sd = 2), 2)
    meanTct <- mean(tcts)
    tctDiff <- round(tcts - meanTct, 2)
    satLevels <- c("Strongly dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Strongly satisfied")
    data.frame(
      UserId=1:n, 
      Major=sample(c("Math", "Science", "Language", "Arts"), n, replace=T),
      Satisfaction=factor(sample(satLevels, n, replace=T), levels = satLevels),
      TaskCompletionTime=tcts,
      DifferenceofTCTFromTheAverage = tctDiff
    )
  })
  
  height <- 200
  output$dfTable <- renderDataTable({df()}, 
    class="compact", 
    height = height+50, 
    options = list(
      searching = F,
      paging = F,
      scrollY = paste0(height,"px"), 
      scrollCollapse = T, 
      info = F))
  
  # return value of the module
  return(df)
}

#===============================================================================
# basic plots

## module ui
basicPlotsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(6,
        uiOutput(ns("varAll")),
        "Stacked dots plot",
        plotOutput(ns("dotPlot"), height = "100%")
      ),
      column(6,
        uiOutput(ns("varInterval")),
        "Histogram",
        plotOutput(ns("histPlot"), height = "100%")
      )
    ),
    uiOutput(ns("varCat")),
    fluidRow(
      column(4, 
        "Bar chart (1)",
        plotOutput(ns("plotBarCount"), height = "100%")
      ),
      column(4, 
        "Bar chart (2)",
        plotOutput(ns("plotBarPercent"), height = "100%")
      ),
      column(4, 
        "Bar chart (3)",
        plotOutput(ns("plotBarPercent100"), height = "100%")
      )
    ),
    fluidRow(
      "Waffle plot",
      plotOutput(ns("plotWaffle"), height = "100%"))
  )
}

## module server
basicPlots <- function(input, output, session, dfReact) {
  ns <- session$ns
  
  # populate selects
  output$varAll <- renderUI({
    df <- dfReact()
    selectInput(ns("varAll"), "", names(df)[2:length(names(df))])
  })
  
  output$varInterval <- renderUI({
    df <- dfReact()
    selectInput(ns("varInterval"), "", names(df)[4:length(names(df))])
  })
  
  output$varCat <- renderUI({
    df <- dfReact()
    selectInput(ns("varCat"), "", names(df)[2:3])
  })
  
  
  # stacked dot plot
  observeEvent(ns("valAll"), {
    output$dotPlot <- renderPlot({
      
      df <- dfReact()
      
      ggplot(df, aes_string(x=input$varAll)) + 
        geom_dotplot(binwidth = 1/10) +
        theme_bw(base_size = 14) +
        theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
    }, width = 250, height = 150)
  })
  
  
  
  # histogram
  observeEvent(ns("varInterval"), {
    output$histPlot <- renderPlot({
      
      df <- dfReact()
      
      ggplot(df, aes_string(x=input$varInterval)) + 
        geom_histogram(binwidth = 0.5) +
        theme_bw(base_size = 14)
    }, width = 250, height = 150)
  })
  
  
  # bar plot (count)
  observeEvent(ns("varCat"), {
    output$plotBarCount <- renderPlot({
      df <- dfReact()
      tb <- df %>%
        group_by_(input$varCat) %>%
        summarize(n = n()) %>%
        mutate(freq = n / sum(n))

      ggplot(tb, aes_string(x = input$varCat, weight = "n", fill = input$varCat)) +
        geom_bar() +
        theme_bw(base_size = 14) +
        theme(legend.position="none")
    }, width = 250, height = 150)
  
    
    # bar plot (%)
    output$plotBarPercent <- renderPlot({
      if (input$varCat == "") return()
      
      df <- dfReact()
      
      tb <- df %>%
        group_by_(input$varCat) %>%
        summarize(n = n()) %>%
        mutate(freq = n / sum(n))
      
      ggplot(tb, aes_string(x = input$varCat, weight = "n/sum(n)", fill = input$varCat)) +
        geom_bar() +
        scale_y_continuous(labels = scales::percent) +
        theme_bw(base_size = 14) +
        theme(legend.position="none")
    }, width = 250, height = 150)
    
    # bar plot (100%)
    output$plotBarPercent100 <- renderPlot({
      if (input$varCat == "") return()
      
      df <- dfReact()
      
      tb <- df %>%
        group_by_(input$varCat) %>%
        summarize(n = n()) %>%
        mutate(freq = n / sum(n))
      
      ggplot(tb, aes_string(x = input$varCat, weight = "n/sum(n)", fill = input$varCat)) +
        geom_bar() +
        scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
        theme_bw(base_size = 14) +
        theme(legend.position="none")
    }, width = 250, height = 150)
    
    # waffle plot
    output$plotWaffle <- renderPlot({
      if (input$varCat == "") return()
      
      df <- dfReact()
      
      ndeep <- ceiling(sqrt(nrow(df)))  # number of waffle rows
      
      tb <- df %>%
        group_by_(input$varCat) %>%
        summarize(n = n()) %>%
        mutate(freq = n / sum(n))
      
      tb4waffles <- expand.grid(y = 1:ndeep,
        x = seq_len(ceiling(sum(tb$n) / ndeep)))
      
      lvlChars <- unlist(lapply(tb[,input$varCat], as.character))
      majorvec <- rep(lvlChars, tb$n)
      
      tb4waffles[,input$varCat] <- c(majorvec, rep(NA, nrow(tb4waffles) - length(majorvec)))
      
      ggplot(tb4waffles, aes_string(x = "x", y = "y", fill = input$varCat)) +
        geom_tile(color = "white") +
        coord_flip() +
        scale_fill_manual(input$varCat,
          limits = rev(levels(df[,input$varCat])),
          values = RColorBrewer::brewer.pal(length(levels(df[,input$varCat])), "Dark2")) +
        theme_bw(base_size = 14) +
        theme(
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()
        )
      
    }, height = 100, width = 300)
  })
  
  
  
  
  
  
}


#===============================================================================
# histogram binning

## module ui
histBinningUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # uiOutput(ns("plotHist_ui")),  
    # TODO: the UI slider integrated in ggvis doesn't work with namespacing yet, so we use Shiny slider which is less responsive for now
    sliderInput(ns("binWidth"), "Bin width:", 1, 5, 1, 0.5),
    ggvisOutput(ns("plotHist"))
  )
}

## module server
histBinning <- function(input, output, session, dfReact) {
  ns <- session$ns
  
  output$plotHist_ui <- renderUI({})
  
  hisVis <- reactive({
    df <- dfReact()
    binWidth <- input$binWidth
    df %>%
      ggvis( ~ TaskCompletionTime) %>%
      set_options(width = 500, height = 300, resizable = FALSE, keep_aspect = TRUE, renderer = "canvas") %>%
      scale_numeric("x", domain = c(0, max(df$TaskCompletionTime) + 2), nice = FALSE) %>%
      layer_histograms(width = input$binWidth,
          # input_slider(min = 1, max = 5, value = 1, step = 0.5),  # TODO: this doesn't work yet, so we revert to shiny-based widget
        fill := "gray") %>%
      layer_points(y = 0, fillOpacity := 0.5, fill := "blue")
  })
  hisVis %>% bind_shiny(ns("plotHist"), ns("plotHist_ui"))
}

#===============================================================================
# main app
ui <- navbarPage(
  "Statistics lecture 1",
  tabPanel("Data and scales", 
    rmarkdownOutput("Instructions/scales.Rmd"),
    dataGeneratorUI("datagen1")
  ),
  tabPanel("Basic plots",
    rmarkdownOutput("Instructions/basicPlots.Rmd"),
    basicPlotsUI("basicplots"),
    dataGeneratorUI("datagen2")
  ),
  tabPanel("Histogram", 
    sidebarLayout(position = "left",
      sidebarPanel(
        rmarkdownOutput("Instructions/histBinning.Rmd")
      ),
      mainPanel(
        histBinningUI("histbinning"),
        dataGeneratorUI("datagen3")
      )
    )
  ),
  tabPanel("Descriptives"),
  tabPanel("Central limit theorem")
)

server <- function(input, output, session) {
  # page: scales
  studyDf <- callModule(dataGenerator, "datagen1")
  
  # page: plots
  studyDf2 <- callModule(dataGenerator, "datagen2")
  callModule(basicPlots, "basicplots", studyDf2)
  
  # page: histogram binning
  studyDf3 <- callModule(dataGenerator, "datagen3")
  callModule(histBinning, "histbinning", studyDf3)
  
}

shinyApp(ui, server)