# compile R markdown
rmarkdownOutput <- function(rmdPath) {
  withMathJax(
    HTML(
      markdownToHTML(
        knit(rmdPath, output = tempfile(), quiet = TRUE)
      )
    )
  )
}

# calculate mode
findModes <- function(x){
  xtab <- table(x)
  if (max(xtab) == 1) {
    return("") # no duplicate data points
  }
  modes <- xtab[max(xtab) == xtab]
  themodes <- names(modes)
  mode(themodes)  <-  typeof(x[1])
  mode_out <- paste(themodes, sep = ", ")
  return(mode_out)
}
# Create a data frame with rectangle coordinates 
# from the given data frame of mean and sample ID.
# Margin of error (moes) may vary
makeBarDf <- function (df, moes, barWidth = 0.1) {
  data.frame(SampleId = df$SampleId,
    x = df$Mean - moes,
    x2 = df$Mean + moes,
    y = df$SampleId + (barWidth / 2),
    y2 = df$SampleId - (barWidth / 2))
}