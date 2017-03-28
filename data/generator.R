
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
  forcats     # utility for working with factor levels
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




#===============================================================================
# one sample
wpm <- rnorm(20, mean = 50, sd = 8)
errRate <- rnorm(20, mean = 5, sd = 5)
tct <- rnorm(20, mean = 80, sd = 20)
errRate[errRate < 0] <- 0
df <- data.frame(WPM = wpm, ErrorRate = errRate, TCT = tct)
write.csv(df, "data/typing-OneSample.csv", row.names = F, na = "")



#===============================================================================
# two samples
set.seed(12)
data_kb <- tibble(
 Method = rep(c("Mechanical", "SiliconeDome"), each = 20),
 WPM = c(rnorm(20, mean = 50, sd = 8), rnorm(20, mean = 45.2, sd = 8))
)

data_kb %>% 
  ggplot(aes(x = Method, y = WPM)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, fun.args = list( conf.int=.95), geom = "errorbar", width = 0)

t_unpaired <- t.test(WPM ~ Method, data_kb)
t_paired <- t.test(WPM ~ Method, data_kb, paired = TRUE)
bind_rows(tidy(t_unpaired), tidy(t_paired))

data_kb %>% 
  mutate(UserID = rep(paste0("P",seq(1:20)), 2)) -> 
  data_kb_paired

write_csv(data_kb, "data/keyboard1.csv")
write_csv(data_kb_paired, "data/keyboard2.csv")