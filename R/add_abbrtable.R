#' @title Add user's journal abbreviation table
#'
#' @param file character, bib file path
#' @param ...  see also \code{data.table::fread}
#'
#' @export
#' @return a data.table
#'
#' @examples
#' csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
#' abbrtable_user <- add_abbrtable(file = csvpath, header = FALSE, sep = ",")
#' colnames(abbrtable_user)
#'


add_abbrtable <- function(file, ...) {
  if (as.numeric(file.access(file, mode = 4)) != 0) {
    stop("Invalid file path: File is not readable.", call. = FALSE)
  }

  dt <- data.table::fread(file = file, ...)

  stopifnot(ncol(dt) == 2)

  colnames(dt) <- c("journal", "journal_abbr")
  dt$journal_lower <- stringr::str_squish(tolower(dt$journal))
  dt$originFile <- "user.csv"
  dt[, c("journal_lower", "journal_abbr", "originFile")]
}
