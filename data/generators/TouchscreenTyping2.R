
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


set.seed(12)
data_typing2 <- tibble(
  UserId = paste0("P", 1:96),
  Device = rep(c("Phone", "Tablet", "Watch"), each = 16 * 2),
  Hands = rep(rep(c("TwoThumbs", "OneIndex"), each = 16), 3),
  Time = c(
    rnorm(16, 1.8, 0.6),   # Phone, TwoThumbs
    rnorm(16, 1.5, 0.8),   # Phone, OneIndex
    rnorm(16, 2.5, 0.5),   # Tablet, TwoThumbs
    rnorm(16, 1.6, 0.4),   # Tablet, OneIndex
    rnorm(16, 3.0, 0.5),   # Watch, TwoThumbs
    rnorm(16, 1.8, 0.7)    # Watch, OneIndex
  )
)

write_csv(data_typing2, "../data/TouchscreenTyping2.csv", na = "")