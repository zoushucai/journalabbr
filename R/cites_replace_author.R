#' Add Parentheses to Citation Commands in LaTeX Text
#'
#' \code{single_replace_author} function modifies LaTeX citation commands to add parentheses around them,
#' transforming commands such as \code{\\cite\{...\}} into \code{(\\cite\{...\})}.
#'
#' @param text A single string of LaTeX content.
#' @param latex_prefix A character vector of LaTeX citation commands to be matched (default: \code{c("cite", "upcite", "citep", "citet")}).
#' @return A string where all matched citation commands are wrapped in parentheses.
#' @details
#' The function searches for LaTeX citation commands using regular expressions.
#' For each matched command, it appends and prepends parentheses to create the author-year citation style.
#' Excessive whitespace in the resulting text is removed to ensure formatting consistency.
#'
#' If no matching citation commands are found, the input text is returned unchanged.
#' @rdname cites_replace_author
#' @importFrom stringr str_trim
#' @export
#'
#'
#'
single_replace_author <- function(text, latex_prefix=c("cite", "upcite", "citep", "citet")) {
  stopifnot(is.character(text), length(text) == 1)

  pattern <-  sprintf("\\\\(%s)\\{([^}]+)\\}", paste(latex_prefix, collapse = "|"))
  matches <- gregexpr(pattern, text, perl = TRUE)
  matched_texts <- regmatches(text, matches)[[1]]

  if (length(matched_texts) ==0 ) return(text)


  for (i in seq_along(matched_texts)) {
    text <- gsub(matched_texts[i], paste(" (", matched_texts[i], ")", sep = ""), text, fixed = TRUE)
  }
  # 把多余的空格去掉
  text = gsub(" +", " ", text)

  return(text)
}

#' Convert All Citations to Author-Year Style in LaTeX Document
#'
#' \code{cites_replace_author} function processes a LaTeX document to transform all numeric citations into author-year style
#' by wrapping citation commands in parentheses.
#'
#' @param tex A character vector where each element represents a line or section of LaTeX content.
#' @return A character vector with LaTeX content where all citation commands are converted to author-year style.
#' @details
#' This function applies \code{single_replace_author} to each line or section of the LaTeX document,
#' ensuring that citation commands such as \code{\\cite\{...\}} and \code{\\citet\{...\}} are converted into
#' \code{(\\cite\{...\})} and \code{(\\citet\{...\})}, respectively.
#' @example inst/example/cites_replace_author_example.R
#' @seealso \code{\link{single_replace_author}}
#' @export
#' @rdname cites_replace_author
cites_replace_author = function(tex){
  new_tex = sapply(tex, single_replace_author, USE.NAMES =FALSE)
  return(new_tex)
}




#' Remove Parentheses from Citation Commands in LaTeX Text
#'
#' \code{single_replace_number} function transforms LaTeX citation commands from an author-year style (e.g., \code{(\\cite\{...\})})
#' back to their original form (e.g., \code{\\cite\{...\}}).
#'
#' @param text A single string of LaTeX content.
#' @param latex_prefix A character vector of LaTeX citation commands to be matched
#' (default: \code{c("cite", "upcite", "citep", "citet")}).
#' @return A string where parentheses around matched citation commands are removed.
#' @details
#' The function identifies citation commands enclosed in parentheses and removes the parentheses,
#' restoring the original citation format.
#' Excessive whitespace in the resulting text is removed for formatting consistency.
#'
#' If no matching citation commands are found, the input text is returned unchanged.
#' @rdname cites_replace_author
#' @importFrom stringr str_trim
#' @export
single_replace_number <- function(text, latex_prefix=c("cite", "upcite", "citep", "citet")) {
  stopifnot(is.character(text), length(text) == 1)

  pattern <-  sprintf("\\(\\s*\\\\(%s)\\{([^}]+)\\}\\s*\\)", paste(latex_prefix, collapse = "|"))
  matches <- gregexpr(pattern, text, perl = TRUE)
  matched_texts <- regmatches(text, matches)[[1]]


  if (length(matched_texts) ==0 ) return(text)
  for (i in seq_along(matched_texts)) {
    new_text  = gsub("^\\(\\s*", "", matched_texts[i])
    new_text  = gsub("\\s*\\)$", "", new_text)
    text <- gsub(matched_texts[i], new_text, text, fixed = TRUE)
  }
  # 把多余的空格去掉
  text = gsub(" +", " ", text)
  return(text)
}

#' Convert All Citations in a LaTeX Document to Numeric Style
#'
#' \code{cites_replace_number} function processes a LaTeX document to remove parentheses around citation commands,
#' converting citations from an author-year style back to numeric style.
#'
#' @param tex A character vector where each element represents a line or section of LaTeX content.
#' @return A character vector with LaTeX content where parentheses around citation commands are removed.
#' @details
#' This function applies \code{single_replace_number} to each line or section of the LaTeX document,
#' ensuring that citations like \code{(\\cite\{...\})} are transformed back into \code{\\cite\{...\}}.
#' @rdname cites_replace_author
#' @seealso \code{\link{single_replace_number}}
#' @export
cites_replace_number = function(tex){
  new_tex = sapply(tex, single_replace_number, USE.NAMES =FALSE)
  return(new_tex)
}



