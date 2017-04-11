
####
# BEGIN: Chat's R header (revision: 10.4.17)
####

# The "pacman" package allows p_load() and p_load_gh() which
# automatically install missing packages and load them
if (!require("pacman")) install.packages("pacman", repos='https://stat.ethz.ch/CRAN/'); library(pacman)

p_load(
  car,        # grab bag of useful functions for NHST
  multcomp,   # for glht (planned comparison, post-hoc test)
  tidyverse,  # collection of the tidyverse packages (this automatically load the following):
  #dplyr,     #   - for data wrangling
  #tibble,    #   - a stricter alternative to data.frame
  #readr,     #   - a stricter alternative to read.csv
  #ggplot2,   #   - for plotting
  # other packages in tidyverse that are non-core
  stringr,    #   - for string functions
  tidyr,      #   - for data tidying
  forcats,    #   - utility functions for working with factor levels
  # extensions of tidyverse
  broom,      #   - for cleaing output from models, e.g., lm()
  cowplot,    #   - adds plot_grid() to put multiple ggplot()'s togeter
  GGally,     #   - adds ggpairs() which is a smarter scatterplot matrix
  # data structure
  GetoptLong, #   - string interpolation. See qq() explanation below
  lubridate,  #   - utility for parsing and performing arithematic on dates 
  # 
  # visualization & interactivity
  Hmisc,      #   - for plotting mean and CI in ggplot
  rafalib,    #   - for imagemat function (visualize contrast codings)
  DT,         #   - for showing data table with navigation/search controls
  # testing:
  assertthat  #   - unit-testing functions
)

p_load_gh(
  "eclarke/ggbeeswarm" # beeswarm plot extension for ggplot2
)

# GetoptLong config (string interpolation)
qq.options("code.pattern" = "#\\{CODE\\}") 

# ggplot2 config (plot theme)
myTheme <- theme(
  panel.background = element_blank(), 
  panel.grid.major = element_line(color="lightgrey", size = 0.2)
)

# DT config
options(DT.options = list(pageLength = 10))
options(DT.autoHideNavigation = TRUE)

# Optional: Decimal output readability
# options(digits=2)
##   NOTE: This option might cause output to be printed with rounding. (default value = 7)

####
# END: Chat's R header
####







data_game <- tibble(
  Device = rep(c("Mouse", "Trackpad", "Joystick", "Wiimote", "Kinect"), each = 20),
  Time = c(
    rnorm(20, 10, 2), # Mouse
    rnorm(20, 12, 1.8), # Trackpad
    rnorm(20, 15, 2), # Joystick
    rnorm(20, 18, 1.5), # Wiimote
    rnorm(20, 17, 1.8)  # Kinect
  )
)

write_csv(data_game, "../GameControllers.csv", na = "")