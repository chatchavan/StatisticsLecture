
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


p_load(readxl); p_unload(readxl)

# Original dataset from Moser et al. (2016)
# http://labinthewild.org/data/index.php
meal_code <- readxl::read_excel("data/TechAtMeals_CHI2016/TechAtMeals_CHI2016.xlsx", sheet = 2, col_names = c("ColumnName", "Question"))
write_csv(meal_code, "data/TechAtMeals_CHI2016/meal_code.csv", na = "")


meal_data <- readxl::read_excel("data/TechAtMeals_CHI2016/TechAtMeals_CHI2016.xlsx")
meal_data %>%

  # replace "NULL" with NA
  mutate_each(funs(replace(., . == "NULL", NA)), matches("question.*")) %>%

  # unifying the column types
  mutate_each(funs(as.integer(.)), matches("question.*")) %>%
  mutate(
    participant_id = as.character(participant_id),
    retake = if_else(retake == 1, TRUE, FALSE),
    gender = as.integer(gender),
    age = as.integer(age),
    employed = as.integer(employed),
    housemates = as.integer(housemates),
    child_included = as.integer(child_included)
  ) ->
  meal_data

write_csv(meal_data, "data/TechAtMeals_CHI2016/meal_data.csv", na = "")

# NOTE: questionnable coding
# * question 11: Do you own a mobile phone? (1 = yes, 2 = no)
# * question 13: Is your mobile phone a smart phone? (1 = yes, 2 = no)