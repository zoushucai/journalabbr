#' @title Journal field abbreviation of BibTeX file
#' @description Input Bib file with complete journal, output Bib file after abbreviation of journal, and return to the abbreviation table of journal
#' @import data.table
#' @importFrom rlang is_empty
#' @importFrom stringr str_trim str_to_lower str_replace_all str_extract str_replace
#' @importFrom bib2df bib2df
#' @importFrom stats complete.cases
#' @importFrom httr GET
#' @param file character, input a .bib file.
#' @param outfile character, file path to write the .bib file. An empty character string writes to \code{stdout} (default).
#' @param separate_names logical, should authors' and editors' names be separated into first and given name?
#' @return {
#' output a new Bib file in the current directory,
#' which only abbreviates the journal fields, and the rest remains unchanged.
#' The default new file name is \code{abbr.bib}. And return to a list.
#' List consists of three data structures as follows:
#' }
#' \describe{
#' \item{abbrtable}{A data.table, Bib abbreviation table of all items, where NA means that there is no journal field in a bib}
#' \item{noabbr}{A data.table, No abbreviated journals found}
#' \item{noindex}{A vector, Index corresponding to noabbr}
#' }
#'
#' @keywords List of journal abbreviations for input bib file
#' @export
#' @examples
#' require(journalabbr)
#' path = system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
#' temptab = abbr2bib(file = path, outfile =  tempfile(fileext = ".bib"))
#'

abbr2bib <- function(file, outfile = tempfile(fileext = ".bib"), separate_names = FALSE) {
  # if (!is.character(file)) {
  #   stop("Invalid file path: Non-character supplied.", call. = FALSE)
  # }
  # if (grepl("http://|https://|www.", file)) {
  #   tryCatch({
  #     GET(file)
  #   },
  #   error = function(e) {
  #     stop("Invalid URL: File is not readable.", call. = FALSE)
  #   })
  # } else {
  #   if (as.numeric(file.access(file, mode = 4)) != 0) {
  #     stop("Invalid file path: File is not readable.", call. = FALSE)
  #   }
  # }
  if (!is.character(outfile)) {
    stop("Invalid file path: Non-character supplied.", call. = FALSE)
  }
  if (as.numeric(file.access(dirname(outfile), mode = 2)) != 0 && file != "") {
    stop("Invalid file path: File is not writeable.", call. = FALSE)
  }
  outfile <- normalizePath(outfile, mustWork = FALSE)

  ############################################################
  ########### Read file -- establish corresponding relationship with built-in database
  bib_journal = journal_abbr = journal_lower = NULL
  bib_file = bib2df(file)
  temp = data.table(
    'bib_journal' = str_trim(bib_file$JOURNAL, side = 'both'),
    "bib_journal_lower" = str_to_lower(str_trim(bib_file$JOURNAL, side = 'both'))
  )
  ### Load internal data table
  dt_lower_unique = as.data.table(journal_abbreviations_lower_all)
  dt_lower_unique = dt_lower_unique[, list(journal_lower, journal_abbr)]
  bib_journal_abbr_table = merge(temp, dt_lower_unique,
                                 by.x = "bib_journal_lower", by.y =  'journal_lower',
                                 all.x = TRUE, sort = FALSE )
  bib_journal_abbr_table = bib_journal_abbr_table[, list(bib_journal, journal_abbr)]
  ### bib_journal_abbr_table, It is the abbreviation table of journals
  #########################################################
  #### If the abbreviation cannot be found,
  #### the original journal will be replaced directly
  ########################################################
  noabbr_index = which( is.na(bib_journal_abbr_table$journal_abbr) & (!is.na(bib_journal_abbr_table$bib_journal)) )
  noabbr = bib_journal_abbr_table[noabbr_index, ]

  bib_journal_abbr_table[noabbr_index, journal_abbr := bib_journal]
  abbr_table = bib_journal_abbr_table[complete.cases(bib_journal_abbr_table), ]
  journal_number = abbr_table[, .N]   # The number of non journal fields in bib
  ########################################################
  ########## Read the input file again
  ########################################################
  bib <- readLines(file)
  # Matches any printable character, but does not include spaces
  bib <- str_replace_all(bib, "[^[:graph:]]", " ")
  ind = grep(pattern = "journal.*?=", bib, ignore.case = TRUE)
  if (length(ind) != journal_number) {
    stop('Note that the number of BibTex entries is not equal to the number of journals extracted')
  }
  if (rlang::is_empty(ind)) {
    stop("The number of journals extracted is 0")
  }
  ## Journal field processing
  for (k  in ind) {
    for (i in seq_len(journal_number)) {
      tempstr = str_extract(bib[k], abbr_table$bib_journal[i])
      if (!is.na(tempstr)) {
        bib[k] = str_replace(bib[k],
                             abbr_table$bib_journal[i],
                             abbr_table$journal_abbr[i])
        break
      }
    }
  }
  writeLines(bib, con = outfile)
  return(list(
    'abbrtable' = bib_journal_abbr_table,
    'noabbr' = noabbr,
    'noindex' = noabbr_index
  ))
}


