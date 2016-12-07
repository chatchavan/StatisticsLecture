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