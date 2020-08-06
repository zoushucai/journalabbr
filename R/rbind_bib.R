#' @title Merging multiple Bib file
#' @description Use \code{read_bib2tib} to read multiple Bib files. For example, the returned objects are tib1, rib2,tib3 ,.... In general, the column names of tib1, tib2,..., etc. are not the same. Therefore, this function is used to merge tib1, TiB2,.... if the column name does not exist, let it be NA
#' @param ... (generalized) tibble or data.frame.
#' @param fill logical,if \code{TRUE} the fills missing columns with NAs. By default FALSE.
#' @return A \code{tibble}.
#' @author ShuCai Zou
#'
#' @examples
#' # Read from .bib file:
#' require(journalabbr)
#' path1 = system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
#' path2 = system.file("extdata", "testfile_3.bib", package = "journalabbr", mustWork = TRUE)
#' path3 = system.file("extdata", "testfile_4.bib", package = "journalabbr", mustWork = TRUE)
#' tib1 <- read_bib2tib(path1)
#' tib2 <- read_bib2tib(path2)
#' tib3 <- read_bib2tib(path3)
#' df = rbind_bib(tib1,tib2,fill=TRUE)
#' df1 = rbind_bib(tib1,tib2,tib3,fill=TRUE)
#' ## dont run
#' # rbind_bib(tib1,tib2)
#' @export



rbind_bib = function(..., fill = FALSE) {
  args <- list(...)
  if (fill == TRUE) {
    col_name = c()
    for (i in args) {
      col_name = c(col_name, colnames(i))
    }
    new_args = list()
    col_name = unique(col_name)
    k = 1
    for (df in args) {
      no_col = setdiff(col_name, colnames(df))
      for (j in no_col) {
        df[[j]] = NA
      }
      new_args[[k]] = df
      k = k + 1
    }
    args = new_args
  }
  tid_data = do.call(rbind, args)
  return(tid_data)
}
