#' @title The Shiny Program for the 'journalabbr' Package
#' @importFrom shiny runApp
#'
#' @export
#'
run_example <- function() {
  app_dir <- system.file("shiny-examples", "appckwx", package = "journalabbr")
  if (app_dir == "") {
    stop("Could not find example directory. Try re-installing `journalabbr`.", call. = FALSE)
  }
  runApp(app_dir, display.mode = "normal")
}
