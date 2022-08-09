#' @title Journal field abbreviation of BibTeX file.
#' @description Input Bib file with complete journal, output a \code{.bib} file and return a data object.
#' @param file character, file path to write the \code{.bib} file.
#' @param out.file character, file path to write the \code{.bib} file.
#' @param author.connect character, what symbols are used to connect multiple authors, \code{'nothing','\\\\&',
#'   '&', 'and'}, where \code{'nothing'} stand for do nothing(default).
#' @param user.csv character, csv file path, Users can customize the path of journal abbreviation.
#'   The csv file requires semicolon to segment data without header.
#'   The first column is the full name of the journal and the second column is the journal abbreviation.
#' @param ... (generalized), Parameters from \code{data.table::fread}.
#' @return {
#'   output a new Bib file and return a \code{data.table} object.
#' }
#'
#' @rdname abbr_bib
#' @export
#' @examples
#' require(journalabbr)
#' path <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
#' temptab <- abbr_bib(file = path, out.file = tempfile(fileext = ".bib"))
#'
#' # add user csv
#' csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
#' temptab1 <- abbr_bib(file = path, out.file = tempfile(fileext = ".bib"), user.csv = csvpath)
#'
abbr_bib <- function(file, out.file = tempfile(fileext = ".bib"),
                     author.connect = c("nothing", "\\\\&", "&", "and"),
                     user.csv = "", ...) {
  outfile <- normalizePath(out.file, mustWork = FALSE)

  # 1. Add user-defined journal abbreviations
  abbrtable_user <- tryCatch(
    {
      if (file.exists(user.csv)) {
        add_abbrtable(file = user.csv, ...)
      } else {
        NULL
      }
    },
    error = function(e) {
      NULL
    }
  )
  # 2. Read bib file -- to data.table
  item_dt <- read_bib2dt(file)

  # 3. replace journal
  item_dt <- replace_field_journal(item_dt, abbrtable_user)

  # 4. replace author
  item_dt <- replace_field_author(item_dt, author.connect)

  # 5. journal_abbe  replace JOURNAL
  item_dt$JOURNAL = ifelse(is.na(item_dt$journal_abbr), item_dt$JOURNAL, item_dt$journal_abbr)

  # 6. Write dt as a bib file and output only the upper case column names
  write_dt2bib(item_dt, outfile)
  invisible(outfile)

  return(item_dt)
}





#' @title Journal field abbreviation of BibTeX file.
#' @description Input Bib file with complete journal, output a \code{.bib} file, this function only carries out
#'   journal abbreviations, and the rest is output as it is.
#' @param file character, file path to write the \code{.bib} file.
#' @param out.file character, file path to write the \code{.bib} file.
#' @param user.csv character, csv file path, Users can customize the path of journal abbreviation.
#'   The csv file requires semicolon to segment data without header.
#'   The first column is the full name of the journal and the second column is the journal abbreviation.
#' @param ... (generalized), Parameters from \code{data.table::fread}.
#' @return NULL
#' @export
#'
#' @rdname abbr_bib
#' @examples
#' # no return value
#' abbr_bib_only_journal(file = path, out.file = tempfile(fileext = ".bib"), user.csv = csvpath)
#'
abbr_bib_only_journal <- function(file, out.file = tempfile(fileext = ".bib"),
                                  user.csv = "", ...) {
  # 1. Add user-defined journal abbreviations
  abbrtable_user <- tryCatch(
    {
      if (file.exists(user.csv)) {
        add_abbrtable(file = user.csv, ...)
      } else {
        NULL
      }
    },
    error = function(e) {
      NULL
    }
  )
  dt <- suppressWarnings(read_bib2dt(file))
  dt <- suppressWarnings(replace_field_journal(dt, abbrtable_user))

  ## 2, read bib file
  bib <- readLines(file)

  ind <- grep(pattern = "journal.*?=", bib, ignore.case = TRUE)
  stopifnot(length(ind) == sum(!is.na(dt$JOURNAL)))
  ## Journal field processing

  k <- 1
  for (i in ind) {
    for (j in seq_len(nrow(dt))) {
      if (!is.na(dt$JOURNAL[j]) && !is.na(dt$journal_abbr[j])) {
         s0 = gsub(" {2,}"," ", bib[i])
         bib[i] = gsub(dt$JOURNAL[j], dt$journal_abbr[j], s0 ,ignore.case = TRUE)
         k <- k + 1
      }
    }
  }

  cat(paste0(bib, collapse = "\n"), file = out.file)
  invisible(file)
  return(NULL)
}
