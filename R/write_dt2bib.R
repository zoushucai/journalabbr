#' @title Export a BibTeX \code{data.table} to a .bib file.
#' @description The BibTeX \code{data.table} is written to a .bib file.
#' @param dt \code{data.table}, in the format as returned by \code{\link{read_bib2dt}}.
#' @param file character, file path to write the .bib file.
#' @return \code{file} as a character string, invisibly.
#' @export
#' @examples
#' # Read from .bib file:
#' require(journalabbr)
#' file <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
#' bib <- read_bib2dt(file)
#'
#' # Write to .bib file:
#' write_dt2bib(bib, file = tempfile(fileext = ".bib"))
#'
write_dt2bib <- function(dt, file = tempfile(fileext = ".bib")) {
  colunm_name <- colnames(dt)
  col <- colunm_name[colunm_name == toupper(colunm_name)]
  rm(colunm_name)
  # stop('This tibble has a lowercase column name, which does not meet the requirements')
  stopifnot(length(col) > 0L, all(c("CKEY", "ITYPE") %in% col))
  rest_col <- setdiff(col, c("CKEY", "ITYPE"))
  if (sum(is.na(dt$ITYPE)) != 0 || sum(is.na(dt$CKEY)) != 0 ) warning("NA value in CKEY or ITYPE field")

  # Complete braces
  for (ii in rest_col) {
    leftnum <- ifelse(is.na(dt[[ii]]), 0, str_count(dt[[ii]], "[^\\\\]?\\{"))
    rightnum <- ifelse(is.na(dt[[ii]]), 0, str_count(dt[[ii]], "[^\\\\]?\\}"))
    if (!identical(leftnum, rightnum)) {
      m1 <- ifelse(leftnum - rightnum >= 0, leftnum - rightnum, 0)
      temp1 <- str_dup("}", times = m1)
      dt[[ii]] <- ifelse(is.na(dt[[ii]]), NA, paste0(dt[[ii]], temp1))

      m2 <- ifelse(rightnum - leftnum >= 0, rightnum - leftnum, 0)
      temp2 <- str_dup("{", times = m2)
      dt[[ii]] <- ifelse(is.na(dt[[ii]]), NA, paste0(temp2, dt[[ii]]))
    }
  }

  # paste itype and ckey
  bibtxt <- sprintf("@%s{%s,\n", tolower(dt$ITYPE), dt$CKEY)
  # paste rest field
  field_width <- max(nchar(rest_col))
  temp_df <- purrr::imodify(dt[, rest_col, with = FALSE], function(x, nx) {
    ifelse(is.na(x),
      "",
      sprintf("\t%s  =  {%s},\n", str_pad(tolower(nx), field_width, "right"), x)
    )
  })
  temp_str <- purrr::reduce(temp_df, paste0)
  bibtxt <- paste0(bibtxt, temp_str)
  bibtxt <- map(bibtxt, function(x) gsub("(.*?)(,\\n)$", "\\1\n}", x))

  cat(paste0(bibtxt, collapse = "\n\n"), file = file)
  invisible(file)
}
