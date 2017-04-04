# The "pacman" package automatically install missing packages and load them
if (!require("pacman")) install.packages("pacman", repos='https://stat.ethz.ch/CRAN/'); library(pacman)
p_load(
  DT,         # for showing data table with navigation/search controls
  tidyverse,  # collection of the tidyverse packages (this automatically load the following):
  #dplyr,     #   - for data wrangling
  #tibble,    #   - a stricter alternative to data.frame
  #readr,     #   - a stricter alternative to read.csv
  #ggplot2,   #   - for plotting
  # other packages in tidyverse that are non-core
  stringr,    #   - for string functions
  tidyr,      #   - for data tidying
  broom,      # for cleaing output from models, e.g., lm()
  cowplot,    # adds plot_grid() to put multiple ggplot()'s togeter
  GGally,     # adds ggpairs() which is a smarter scatterplot matrix
  assertthat, # for unit-testing your functions
  car,        # grab bag of useful functions for NHST
  GetoptLong, # string interpolation. See qq() explanation below
  lubridate,  # utility for parsing and performing arithematic on dates 
  forcats,    # utility for working with factor levels
  Hmisc     # for plotting mean and CI in ggplot
)

# string interpolation
qq.options("code.pattern" = "#\\{CODE\\}") 

# plot theme
myTheme <- theme(panel.background = element_blank(), panel.grid.major = element_line(color="lightgrey", size = 0.2))

# Decimal output
##   NOTE: This option might cause output to be printed with rounding. (default value = 7)
# options(digits=2)   

# datatable
options(DT.options = list(pageLength = 10))
options(DT.autoHideNavigation = TRUE)

# packages for Shiny
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
  ggvis,
  shinyjs)

# color
p_load(RColorBrewer)

#===============================================================================

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Interaction in linear regression"),
  
  withMathJax(),
  p("A linear regression that have two regressors \\(x_1\\) and \\(x_2\\) can be 
    expressed by:"),
  p("$$y = b_0 + b_1 x_1 + b_2 x_2$$"),
  p("An interaction effect between the two regressors can be express by the fact 
    that one regressors (here, \\(x_1\\)) influences the coefficient of another 
    regressor (here, \\(x_2\\)):"),
  p("$$ 
    \\begin{align} 
    y & = b_0 + b_1 x_1 + (b_2 + b_3 x_1) x_2 \\\\ 
      & = b_0 + b_1x_1 + b_2x_2 + b_3x_1x_2
    \\end{align}
    $$"),
  div("Use the following simulator to explore the effect of each coefficients to 
    the \\(y\\) value. In particular, try the following combinations of the 
    coefficients, individually and in combination. (If not mentioned, set the 
    coefficient to zero.)"),
  tags$ol(
    tags$li("Set \\(b_2\\) and \\(b_3\\) to zero. This reduces the regression to 
      only one regressor: \\( y = b_0 + b_1 x_1 \\). Explore how 
      \\(b_0\\) and \\(b_1\\) influence \\(y\\). First change each of them individually, 
      then try combining them."),
    tags$li("Explore how the combination of \\(b_0\\) and \\(b_2\\) influences \\(y\\).
      How is this regression equation look like? What does it mean?"),
    tags$li("Explore \\(b_1\\) and \\(b_2\\)."),
    tags$li("Fix \\(b_1\\) and \\(b_2\\) to any level that is not zero. Explore 
      how \\(b_3\\) influences \\(y\\).")
  ),
  
  sidebarLayout(
    
    # coefficient controllers
    sidebarPanel(
      sliderInput("eq3_b0", "\\(b_0\\) (intercept):", min = -10, max = 10, value = 0),
      sliderInput("eq3_b1", "\\(b_1\\):", min = -10, max = 10, value = 0),
      sliderInput("eq3_b2", "\\(b_2\\):", min = -10, max = 10, value = 0),
      sliderInput("eq3_b3", "\\(b_3\\):", min = -10, max = 10, value = 0)
    ),
    
    # plot: two regressors with interaction
    mainPanel(
      ggvisOutput("plot_continuous"),
      tags$small(strong("Note:"), 
        "The vertical axis and the horizontal axis are scaled differently.
        Use the tick labels in your interpretations.
        (This is done intentionally to maintain the constant range of the plot
        and to make the exploration of the coefficent's effect visually explicit.)")
    )
  )
)

#===============================================================================
server <- function(input, output) {
  
  # generate combination of two regressors
  x1 <- -10:10
  x2 <- 0:3
  x1_count <- length(x1)
  x2_count <- length(x2)
  x1 <- rep(x1, x2_count)
  x2 <- rep(x2, each = x1_count)
  
  # render plot
  plot_continuous <- reactive({
    
    # coefficients
    b0 <- input$eq3_b0
    b1 <- input$eq3_b1
    b2 <- input$eq3_b2
    b3 <- input$eq3_b3
    y <- b0 + (b1 * x1) + (b2 * x2) + (b3 * x1 * x2)
    
    # plot
    myPalette <- brewer.pal(4, "Blues")
    tibble(
      x1 = x1, 
      x2 = factor(x2),
      y = y) %>%
      
      # crop the data that is out of bound to preserve the shape of the plot
      filter(y <= 100 & y >= -100) %>%
      
      # vis elements
      ggvis(~x1, ~y, fill = ~x2, stroke = ~x2) %>% 
      layer_points() %>% 
      layer_lines() %>% 
      
      # ensure the scale
      scale_numeric("x", domain = c(-11, 11)) %>%
      scale_numeric("y", domain = c(-100, 100)) %>%
      
      # coloring
      scale_ordinal("fill", range = myPalette) %>%
      scale_ordinal("stroke", range = myPalette) %>% 
      
      # misc display options
      set_options(
        resizable = FALSE,
        keep_aspect = TRUE,
        renderer = "canvas")
    
  })
  plot_continuous %>% bind_shiny("plot_continuous")
}

# Run the application 
shinyApp(ui = ui, server = server)

