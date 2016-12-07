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
