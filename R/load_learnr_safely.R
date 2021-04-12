#' Load the learnr package safely
#'
#' When rendering `{learnr}` tutorials, we generally want to
#' load the `{learnr}` package. But if we are rendering the document
#' as ordinary HTML or PDF, `library('learnr')` will throw an error.
#' This function allows `{learnr}` to be loaded in appropriate circumstances
#' and not otherwise. NOTE: It does **not** load `{learnr}` if it's not safe.
#'

#' @export
load_learnr_safely <- function() {
  if ("runtime" %in% names(rmarkdown::metadata)) {
    if (rmarkdown::metadata$runtime == "shiny_prerendered")
      library(learnr)
  }
}
