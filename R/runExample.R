#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "appckwx", package = "journalabbr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `journalabbr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


