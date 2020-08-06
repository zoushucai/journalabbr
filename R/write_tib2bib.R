#' @title Export a BibTeX \code{tibble} to a .bib file.
#' @description The BibTeX \code{tibble} is written to a .bib file.
#' @param tib \code{tibble}, in the format as returned by \code{\link{read_bib2tib}}.
#' @param file character, file path to write the .bib file.
#' @param append logical, if \code{TRUE} the \code{tibble} will be appended to an existing file.
#' @param isformat logical, if \code{TRUE} the  Fields in \code{tibble} will complete braces to make them appear in pairs.
#' @param connect_author character, what symbols are used to connect multiple authors, \code{'nothing','\\\\&', '&', 'and'}, where \code{'nothing'} stand for do nothing(default).
#' @return \code{file} as a character string, invisibly.
#' @importFrom stringr str_count str_dup str_to_title str_to_upper
#' @importFrom purrr map map2
#' @importFrom rlang is_empty
#' @importFrom dplyr arrange
#' @export
#' @examples
#' # Read from .bib file:
# require(journalabbr)
# path = system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
# bib <- read_bib2tib(path)
#
# # Write to .bib file:
# write_tib2bib(bib, file = tempfile(fileext = ".bib"))

# Use `append = TRUE` to add lines to an existing .bib file:


write_tib2bib = function(tib,
                         file = tempfile(fileext = ".bib"),
                         append = FALSE,
                         isformat = TRUE,
                         connect_author = c('nothing','\\\\&','&','and')) {
  detcol = c("typebib", "keybib", "sitenum", "value", "rawchar", "journal_abbr", "originFile")
  col = setdiff(colnames(tib),detcol)
  rm(detcol)
  if (!all(col %in% str_to_upper(col))) {
    warning('This tibble has a lowercase column name, which does not meet the requirements')
  }
  Value = ifelse(!is.na(tib$typebib),
                 paste("@", str_to_lower(tib$typebib), "{", sep = ""),
                 stop('on typebib')
                )
  Value = ifelse(!is.na(tib$keybib),
                  paste(Value, tib$keybib, ",\n", sep = ""),
                  paste(tib$keybib, ",\n", sep = "")
                )

  Value = as.list(Value)
  if (isformat) {
    for (ii in col) {
      leftnum = ifelse(is.na(tib[[ii]]), 0, str_count(tib[[ii]], '[^\\\\]?\\{'))
      rightnum = ifelse(is.na(tib[[ii]]), 0, str_count(tib[[ii]], '[^\\\\]?\\}'))
      if (!identical(leftnum, rightnum)) {
        m1 = ifelse(leftnum - rightnum >= 0, leftnum - rightnum, 0)
        temp1 = str_dup("}", times = m1)
        tib[[ii]] = ifelse(is.na(tib[[ii]]), NA, paste(tib[[ii]], temp1, sep = ""))
        m2 = ifelse(rightnum - leftnum >= 0, rightnum - leftnum, 0)
        temp2 = str_dup("{", times = m2)
        tib[[ii]] = ifelse(is.na(tib[[ii]]), NA, paste(temp2, tib[[ii]], sep = ""))
      }
    }
  }

  if (!connect_author[1] %in% c('nothing', '\\\\&', '&', 'and')) {
    stop('The parameter entered is incorrect')
  }else{
    if (connect_author[1] != 'nothing') {
      temp = setdiff(c('\\\\&', '&', 'and'), connect_author[1])
      s1 = paste0('(?<= )', temp[1], "(?= )")
      tib$AUTHOR = gsub(s1, connect_author[1], tib$AUTHOR, perl = TRUE, ignore.case = TRUE)

      s2 = paste0('(?<= )', temp[2], "(?= )")
      tib$AUTHOR = gsub(s2, connect_author[1], tib$AUTHOR, perl = TRUE, ignore.case = TRUE)
    }
  }


  for (ii in col) {
    temp = ifelse(!is.na(tib[[ii]]),
                  paste("\t", str_to_lower(ii), " = {", tib[[ii]], "},\n", sep = ""),
                  "")
    Value = map2(Value, temp, function(x, y) { paste0(x, y) })
  }

  Value =  map(Value,function(x)gsub('(.*?)(,\\n)$','\\1\n}',x))

  cat(paste0(Value,collapse = '\n\n'),
      file = file,
      append = append)
  invisible(file)
 }
