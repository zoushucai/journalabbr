#' Clean up and standardize spacing in a given text
#'
#' This function processes a given string by cleaning up extra spaces, handling punctuation,
#' and adjusting mathematical symbols according to specific formatting rules.
#'
#' @param text A single character string (input text) that needs to be processed.
#' @return A cleaned-up string where multiple spaces are reduced to one, unnecessary spaces
#'         are removed, and spaces are appropriately added around certain punctuation and
#'         mathematical symbols.
#' @details
#' see function `single_add_space` source code for details.

#' @export
#' @rdname tex_funs
#'
single_add_space <- function(text) {
  stopifnot(is.character(text) && length(text) == 1)
  # 1. remove special "zero-width" spaces
  text <- gsub("\ufeff", "", text, perl = TRUE)

  # 2. skip lines with "@" or lines starting with "%"
  if (grepl("@", text)) {
    return(text)
  }

  if (grepl("^\\s*%", text)) {
    return(text)
  }

  # 3. remove multiple spaces between non-space characters
  text <- gsub("(?<=\\S)\\s+(?=\\S)", " ", text, perl = TRUE)

  #### 4. remove spaces before punctuation
  text <- gsub("\\s+(?=[,.?;])", "", text, perl = TRUE)

  ### 5. add spaces around LaTeX-style \left( and \right)
  text <- gsub("\\\\left\\s*\\(", "\\\\left( ", text)
  text <- gsub("\\\\right\\s*\\)", "\\\\right) ", text)

  #### 6. add spaces after certain punctuation marks
  text <- gsub("(?<=[,.?;])\\s+(?=[a-zA-Z])", " ", text, perl = TRUE)

  #### 7. remove trailing spaces at the end of the string
  text <- gsub("\\s+$", "", text)

  #### 8. handle LaTeX math symbols
  # text <- gsub("(?<=[^\\s])=(?=[^\\s])", " = ", text, perl = TRUE)
  # text <- gsub("(?<=[^\\s])>(?=[^\\s])", " > ", text, perl = TRUE)
  # text <- gsub("(?<=[^\\s])<(?=[^\\s])", " < ", text, perl = TRUE)
  text <- gsub("\\s*\\^", "^", text, perl = TRUE)
  text <- gsub("\\^\\s*", "^", text, perl = TRUE)

  #### remove multiple spaces between non-space characters
  text <- gsub("(?<=\\S)\\s+(?=\\S)", " ", text, perl = TRUE)

  return(text)
}

#' Apply the `single_add_space` function to a vector of LaTeX text
#'
#' This function takes a vector of LaTeX-formatted strings and applies the `single_add_space`
#' function to each string to standardize spacing throughout the document.
#'
#' @param tex A vector of strings (LaTeX document or text) to be processed.
#' @param fun The function to be applied to each string in the vector (default is `single_add_space`).
#' @param ... Additional arguments to be passed to the function.
#' @return A vector of processed strings where unnecessary spaces have been removed or adjusted.
#' @example inst/example/tex_funs_example.R
#' @export
#' @rdname tex_funs
#'
tex_funs <- function(tex, fun = single_add_space, ...) {
  tex <- unlist(lapply(tex, FUN = fun, ...))
  return(tex)
}
