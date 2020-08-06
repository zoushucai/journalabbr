#' @title Delete duplicate.
#' @description Delete duplicates based on a column (\code{keybib}).
#' @param tib A tilbbe.
#' @param on character, A column in a tibble data frame,The default is \code{keybib}
#' @param retent_method character, Keep an entry from a duplicate in some way.
#' \describe{
#' \item{"namin"}{If it is repeated, the row containing the smallest number of NA values is retained. If there are multiple minimum values, the first one is retained}
#' \item{"first"}{If repeated, keep the first item}
#' \item{"last"}{If repeated, keep the last item}
#' }
#'
#' @return A \code{tibble}.
#' @importFrom rlang .data
#' @importFrom dplyr group_by slice '%>%'
#' @export
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
#' m1 = unique_bib(df1,retent_method='namin')
#' m2 = unique_bib(df1,retent_method='first')
#' m3 = unique_bib(df1,retent_method='last')
#' identical(m1,m2)
#' identical(m1,m3)
#' identical(m2,m3)

unique_bib = function(tib,
                      on = 'keybib',
                      retent_method = c("namin", "first", "last")) {
  if (retent_method[1] == 'namin') {
    if ("cout_na_temp" %in% colnames(tib)) {
      message(
        "The value of column name `cout_na_temp` will be changed.
        Do not use the column name `cout_na_temp`"
      )
    }
    tib[["cout_na_temp"]] = apply(tib, 1, function(x)sum(is.na(x)))
    tib_new = tib %>%  group_by(.data[[on]]) %>% slice(which.min(.data[['cout_na_temp']]))
    tib_new[["cout_na_temp"]] = NULL
  }
  if (retent_method[1] == 'first') {
    tib_new = tib[!duplicated(tib[[on]], fromLast = FALSE), ]
  }
  if (retent_method[1] == 'last') {
    tib_new = tib[!duplicated(tib[[on]], fromLast = TRUE), ]
  }
  return(tib_new)
}

