rmarkdownOutput <- function(rmdPath) {
  withMathJax(
    HTML(
      markdownToHTML(
        knit(rmdPath, output = tempfile(), quiet = TRUE)
      )
    )
  )
}