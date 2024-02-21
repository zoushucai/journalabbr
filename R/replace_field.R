#' @title Replace field \code{journal} with built-in data sets and user provided data sets.
#'
#' @param dt data.table, the object returned by the function \code{read_bib2dt}.
#' @param abbrtable_user data.table, the object returned by the function \code{add_abbrtable}.
#'
#' @return data.table
#' @rdname replace_field
#'
#' @export
#'
#' @examples
#' csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
#' abbrtable_user <- add_abbrtable(file = csvpath, header = FALSE, sep = ",")
#' colnames(abbrtable_user)
#'
#' file <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
#' dt <- read_bib2dt(file)
#'
#' newdt <- replace_field_journal(dt, abbrtable_user)
#' newdt1 <- replace_field_author(dt, author.connect = "and")
#' newdt2 <- replace_field_author(dt, author.connect = "&")
#'


replace_field_journal <- function(dt, abbrtable_user) {
  # internal data
  if (!c("journal_lower" %in% colnames(dt))) {
    dt$journal_lower <- str_squish(tolower(dt$JOURNAL))
  } else {
    dt$journal_lower <- str_squish(tolower(dt$journal_lower))
  }

  abbrtable_sys_new <- abbrtable_sys[, lapply(.SD, stringi::stri_unescape_unicode)]
  new_dt <- abbrtable_sys_new[dt, on = "journal_lower"]

  # user data
  if (!is.null(abbrtable_user)) {
    for (i in seq_len(nrow(new_dt))) {
      for (j in seq_len(nrow(abbrtable_user))) {
        if (identical(new_dt$journal_lower[i], abbrtable_user$journal_lower[j])) {
          new_dt$journal_abbr[i] <- abbrtable_user$journal_abbr[j]
          new_dt$originFile[i] <- abbrtable_user$originFile[j]
        }
      }
    }
  }

  return(new_dt)
}





#' @title Replace field \code{author}
#'
#' @param dt is data.table, the object returned by the function \code{read_bib2dt}.
#' @param author.connect is character, what symbols are used to connect multiple authors, \code{'nothing','\\\\&',
#'  '&', 'and'}, where \code{'nothing'} stand for do nothing(default).
#'
#' @return data.table
#' @export
#' @rdname replace_field

replace_field_author <- function(dt, author.connect = c("nothing", "\\\\&", "&", "and")) {
  connect_symbol <- match.arg(author.connect, choices = c("nothing", "\\\\&", "&", "and"))
  if (connect_symbol != "nothing") {
    rest_symbol <- setdiff(c("\\\\&", "&", "and"), connect_symbol)
    s1 <- sprintf("(?<= )%s(?= )", rest_symbol[1])
    dt$AUTHOR <- gsub(s1, connect_symbol, dt$AUTHOR, perl = TRUE, ignore.case = TRUE)

    s2 <- sprintf("(?<= )%s(?= )", rest_symbol[2])
    dt$AUTHOR <- gsub(s2, connect_symbol, dt$AUTHOR, perl = TRUE, ignore.case = TRUE)
  }

  return(dt)
}
