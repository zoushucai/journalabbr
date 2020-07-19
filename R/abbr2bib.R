#' @title Journal field abbreviation of BibTeX file
#' @description Input Bib file with complete journal, output Bib file after abbreviation of journal, and return to the abbreviation table of journal
#' @importFrom rlang is_empty
#' @importFrom stringr str_trim str_to_lower str_replace_all str_extract str_replace
#' @importFrom stats complete.cases
#' @importFrom dplyr left_join
#' @importFrom httr GET
#' @importFrom purrr map
#' @param file character, input a .bib file.
#' @param outfile character, file path to write the .bib file. An empty character string writes to \code{stdout} (default).
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
  tib = read_bib2tib(file,isabbr = TRUE) %>% as.data.frame()
  tib = tib[,c("JOURNAL","journal_abbr","originFile")]
  #########################################################
  #### If the abbreviation cannot be found,
  #### the original journal will be replaced directly
  ########################################################
  tib$is_abbr = ifelse(is.na(tib$JOURNAL) & is.na(tib$journal_abbr),-1,
                       ifelse(!is.na(tib$JOURNAL) & !is.na(tib$journal_abbr), 1, 0)
  )## 1 代表缩写成功,   -1 代表 两个都是NA , 0 代表数据库中代表没找到,
  # 没找的,用原有期刊字段进行替代
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
  return(tib)
}


