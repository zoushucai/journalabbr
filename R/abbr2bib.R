#' @title Journal field abbreviation of BibTeX file
#' @description Input Bib file with complete journal, output Bib file after abbreviation of journal, and return to the abbreviation table of journal
#' @importFrom rlang is_empty
#' @importFrom stringr str_trim str_to_lower str_replace_all str_extract str_replace
#' @importFrom stringi stri_unescape_unicode
#' @importFrom stats complete.cases
#' @importFrom dplyr left_join
#' @importFrom httr GET
#' @importFrom purrr map
#' @param file character, input a .bib file.
#' @param outfile character, file path to write the .bib file. An empty character string writes to \code{stdout} (default).
#' @return {
#' output a new Bib file in the current directory,
#' which only abbreviates the journal fields, and the rest remains unchanged.
#' And return to a tibble, it has four columns:
#'}
#' \describe{
#' \item{JOURNAL}{Original journal field in bib}
#' \item{journal_abbr}{Field after abbreviation of original journal}
#' \item{originFile}{Abbreviate the source of the database file, see \url{https://github.com/JabRef/abbrv.jabref.org/tree/master/journals}}
#' \item{is_abbr}{There are only three cases, 1 represents that the item has a JOURNAL field and has been abbreviated successfully, - 1 means that the item does not have a JOURNAL field, and 0 represents that the item has a JOURNAL field, but the corresponding abbreviation is not found in the database, so the original JOURNAL field is used instead}
#' }
#'
#' @keywords List of journal abbreviations for input bib file
#' @export
#' @examples
#' require(journalabbr)
#' path = system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
#' temptab = abbr2bib(file = path, outfile =  tempfile(fileext = ".bib"))
#'

abbr2bib <- function(file, outfile = tempfile(fileext = ".bib")) {
  if (!is.character(file)) {
    stop("Invalid file path: Non-character supplied.", call. = FALSE)
  }
  if (grepl("http://|https://|www.", file)) {
    tryCatch({
      GET(file)
    },
    error = function(e) {
      stop("Invalid URL: File is not readable.", call. = FALSE)
    })
  } else {
    if (as.numeric(file.access(file, mode = 4)) != 0) {
      stop("Invalid file path: File is not readable.", call. = FALSE)
    }
  }
  if (!is.character(outfile)) {
    stop("Invalid file path: Non-character supplied.", call. = FALSE)
  }
  if (as.numeric(file.access(dirname(outfile), mode = 2)) != 0 && file != "") {
    stop("Invalid file path: File is not writeable.", call. = FALSE)
  }
  outfile <- normalizePath(outfile, mustWork = FALSE)

  ############################################################
  ########### Read file -- establish corresponding relationship with built-in database
  item_tib = read_bib2tib(file)
  item_tib$journal_lower = purrr::map_chr(item_tib$JOURNAL,function(x){
    temp = str_trim(str_to_lower(x),side = 'both')
    gsub("[\t ]{2,}"," ", temp)
  })
  # # Unicode to UTF-8
  # library(data.table)
  # library(stringi)
  # abbrTable = data.table::as.data.table(abbrTable)
  # abbrTable = abbrTable[,lapply(.SD, function(x)stringi::stri_unescape_unicode(x))]
  abbrTable = as.data.frame(lapply(abbrTable, function(x)stringi::stri_unescape_unicode(x)))
  abbrTableSub = tibble::as_tibble(abbrTable)
  abbrTableSub = abbrTableSub[,c("journal_lower",'journal_abbr','originFile')]
  tib = dplyr::left_join(item_tib, abbrTableSub, by = "journal_lower")

  tib = tib[,c("JOURNAL","journal_abbr","originFile")]
  #########################################################
  #### If the abbreviation cannot be found,
  #### the original journal will be replaced directly
  ########################################################
  tib$is_abbr = ifelse(is.na(tib$JOURNAL) & is.na(tib$journal_abbr),-1,
                       ifelse(!is.na(tib$JOURNAL) & !is.na(tib$journal_abbr), 1, 0)
  )

  tib$journal_abbr = ifelse(tib$is_abbr == 0,tib$JOURNAL , tib$journal_abbr)
  ########################################################
  ########## Read the input file again
  ########################################################
  bib <- readLines(file)
  # Matches any printable character, but does not include spaces
  bib <- str_replace_all(bib, "[^[:graph:]]", " ")
  ind = grep(pattern = "journal.*?=", bib, ignore.case = TRUE)

  if (length(ind) != sum(!is.na(tib$journal_abbr)) ) {
    stop('Note that the number of BibTex entries is not equal to the number of journals extracted')
  }
  if (rlang::is_empty(ind)) {
    stop("The number of journals extracted is 0")
  }
  ## Journal field processing
  for (k  in ind) {
    for (i in 1:length(ind)) {
      tempstr = str_extract(bib[k], tib$JOURNAL[i])
      if (!is.na(tempstr)) {
        bib[k] = str_replace(bib[k],
                             tib$JOURNAL[i],
                             tib$journal_abbr[i])
        break
      }
    }
  }

  writeLines(bib, con = outfile)
  invisible(outfile)

  return(tib)
}


