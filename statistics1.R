
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
source("Modules/dataGenerator.R")
source("Modules/basicPlots.R")
source("Modules/histBinning.R")
source("Modules/descriptives.R")




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
  tabPanel("Descriptives",
    "Stop this file and run 'apps/01_descriptives/app.R'."
    # rmarkdownOutput("Instructions/descriptives.Rmd"),
    # descriptivesUI("descStat")
  ),
  tabPanel("Central limit theorem",
    "Stop this file and run 'apps/02_centralLimitTheorem/app.R'."
  )
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
  
  # page: descriptive statistics
  # TODO: data input doesn't work when embedding in the app
  # callModule(descriptives, "descStat")
  
}

shinyApp(ui, server)