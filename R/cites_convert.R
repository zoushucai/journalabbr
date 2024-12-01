#' @title Convert citations between LaTeX and Markdown formats
#'
#' @description
#' This function converts citations in a given text between LaTeX and Markdown formats.
#' It can handle multiple LaTeX citation styles and produces the appropriate format based on the specified direction.
#' Note that the input text must have a length of 1.
#'
#' @param text A character string containing the text with citations, Note that the input text must have a length of 1.
#' @param pattern A regular expression pattern for matching citations. If NULL, defaults to a pattern based on the citation direction.
#' @param tex2md Logical; if TRUE, converts from LaTeX to Markdown; if FALSE, converts from Markdown to LaTeX.
#' @param latex_prefix A character vector of LaTeX citation prefixes to use.
#' Defaults to c("cite", "upcite", "citep", "citet").
#' @return the `single_cites_convert` function return a character string with citations converted to the specified format.
#'
#' @export
#' @rdname cites_convert
#'

single_cites_convert <- function(text, pattern=NULL, tex2md = TRUE, latex_prefix=c("cite", "upcite", "citep", "citet")) {
  stopifnot(is.character(text), length(text) == 1, is.character(pattern) || is.null(pattern) )
  if (is.null(pattern)) {
    pattern <- if (tex2md) {
      sprintf("\\\\(%s)\\{([^}]+)\\}", paste(latex_prefix, collapse = "|"))
    } else {
      "\\[\\@([^\\]]+)\\]"
    }
  }

  matches <- gregexpr(pattern, text, perl = TRUE)
  matched_texts <- regmatches(text, matches)[[1]]

  if (length(matched_texts) ==0 ) return(text)



  replacements <- lapply(matched_texts, function(match) {
    if (tex2md) {
      temp <- gsub(pattern, "\\2", match, perl = TRUE)
      refs <- trimws(unlist(strsplit(temp, ",")))
      paste0("[@", paste0(refs, collapse = "; @"), "]")
    } else {
      refs <- gsub(pattern, "\\1", match, perl = TRUE)
      refs <- gsub(" ", "", refs, perl = TRUE)

      refs_vec <- if (grepl(";@", refs)) {
        trimws(unlist(strsplit(refs, split = ";@", fixed = TRUE)))
      } else {
        refs
      }

      paste0(sprintf("\\%s{", latex_prefix[1]), paste(refs_vec, collapse = ","), "}")
    }
  })

  for (i in seq_along(matched_texts)) {
    text <- gsub(matched_texts[i], replacements[[i]], text, fixed = TRUE)
  }

  return(text)
}

#' @title Convert a vector of citations between formats
#' @description
#' the `cites_convert` function applies `single_cites_convert` to each element of the input vector.
#' @param x A character vector containing texts with citations.
#' @param ... Additional arguments passed to \code{single_cites_convert}.
#' @return the `cites_convert` function  return a character vector with citations converted to the specified format.
#'
#' @rdname cites_convert
#' @export
#' @example inst/example/cites_convert_example.R
#'
cites_convert <- function(x, ...) {
  sapply(x, single_cites_convert, ..., USE.NAMES=F)
}
