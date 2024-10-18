#' @title Get default columns to delete
#' @description Returns the default list of columns that will be deleted.
#' @return A character vector containing the default column names to be excluded.
#' @export
get_delcolumns <- function() {
  columns <- c("LCCN", "month", "DOI", "URL", "NOTE", "TYPE", "ANNOTATION", "KEYWORDS", "SHORTTITLE", "abstract")
  return(toupper(columns))
}
