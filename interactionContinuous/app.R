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

#===============================================================================

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Interaction in linear regression"),
  
  sidebarLayout(
    # 
    sidebarPanel(
      sliderInput("eq3_b0", "b0:", min = 0, max = 20, value = 0),
      sliderInput("eq3_b1", "b1:", min = 0, max = 20, value = 0),
      sliderInput("eq3_b2", "b2:", min = 0, max = 20, value = 0),
      sliderInput("eq3_b3", "b3:", min = 0, max = 20, value = 0)
    ),
    
    # plot: two regressors with interaction
    mainPanel(
      plotOutput("plot3")
    )
  )
)

#===============================================================================
server <- function(input, output) {
  
  output$plot3 <- renderPlot({
    
    # generate combination of two regressors
    x1 <- 0:10
    x2 <- 0:3
    x1_count <- length(x1)
    x2_count <- length(x2)
    x1 <- rep(x1, x2_count)
    x2 <- rep(x2, each = x1_count)
    
    # coefficients
    b0 <- input$eq3_b0
    b1 <- input$eq3_b1
    b2 <- input$eq3_b2
    b3 <- input$eq3_b3
    y <- b0 + (b1 * x1) + (b2 * x2) + (b3 * x1 * x2)
    
    
    # plot
    tibble(
      x1 = x1, 
      x2 = x2,
      y = y) %>% 
      ggplot(aes(x = x1, y = y, color = factor(x2))) +
      list(
        geom_smooth(method="lm"),
        geom_point(),
        coord_cartesian(xlim = c(0,11), ylim = c(0,100)),
        scale_color_discrete(name = "x2"),
        theme(legend.position="bottom")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

