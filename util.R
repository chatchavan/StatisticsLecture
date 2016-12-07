rmarkdownOutput <- function(rmdPath) {
  withMathJax(
    HTML(
      markdownToHTML(
        knit(rmdPath, quiet = TRUE)
      )
    )
  )
}