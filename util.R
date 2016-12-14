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

# calculate descriptive statistics
findModes <- function(x){
  xtab <- table(x)
  modes <- xtab[max(xtab)==xtab]
  themodes <- names(modes)
  mode(themodes)  <-  typeof(x[1])
  mout <- list(values=themodes)
  return(mout)
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