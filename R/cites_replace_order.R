#' Replace Citation Order in LaTeX Text
#'
#' \code{single_replace_order} function modifies LaTeX citation commands to reorder citation keys based on a provided order.
#'
#' @param text A single string of LaTeX content.
#' @param citesall A character vector containing all citation keys in the desired order.
#' @param latex_prefix A character vector of LaTeX citation commands to be matched (default: \code{c("cite", "upcite", "citep", "citet")}).
#' @return A string with citation keys reordered within the specified citation commands.
#' @details
#' The function identifies LaTeX citation commands and reorders their arguments (\code{e.g., \\cite\{key1,key2\}})
#' to match the order specified in \code{citesall}. If a citation contains only one key, it remains unchanged.
#'
#' Matched citation keys not found in \code{citesall} are excluded from the reordered result.
#' @example inst/example/cites_replace_order_example.R
#' @export
single_replace_order <- function(text, citesall=NULL, latex_prefix=c("cite", "upcite", "citep", "citet")) {

  if (is.null(citesall)) {
    stop("citesall must be provided")
  }

  stopifnot(is.character(text), length(text) == 1)
  pattern <-  sprintf("\\\\(%s)\\{([^}]+)\\}", paste(latex_prefix, collapse = "|"))
  matches <- gregexpr(pattern, text, perl = TRUE)
  matched_texts <- regmatches(text, matches)[[1]]

  if (length(matched_texts) ==0 ) return(text)

  replacements <- lapply(matched_texts, function(match) {
    temp <- gsub(pattern, "\\2", match, perl = TRUE)
    refs <- trimws(unlist(strsplit(temp, ",")))
    if(length(refs) == 1){
      return( paste0(sprintf("\\%s{", latex_prefix[1]), refs, "}"))
    }
    # 按照cites_order的顺序排列
    refs_order = refs[match(citesall, refs, nomatch = 0)]

    refs_order = paste(refs_order, collapse = ", ")
    paste0(sprintf("\\%s{", latex_prefix[1]), refs_order, "}")
  })
  for (i in seq_along(matched_texts)) {
    text <- gsub(matched_texts[i], replacements[[i]], text, fixed = TRUE)
  }
  return(text)
}



#' Replace All Citations in a LaTeX Document Based on Order
#'
#' \code{cites_replace_order} function processes a LaTeX document to reorder all citation keys within citation commands
#' based on their first appearance in the document.
#'
#' @param tex A character vector where each element represents a line or section of LaTeX content.
#' @param citesall A character vector containing all citation keys in the desired order. If not provided, default applies \code{cites_extract}.
#' @return A character vector with updated LaTeX content where citation keys in commands are reordered.
#' @details
#' The function extracts all citation keys using \code{cites_extract}, determines their order based on first occurrence,
#' and applies \code{single_replace_order} to reorder keys in each citation command.
#' @example inst/example/cites_replace_order_example.R
#' @seealso \code{\link{single_replace_order}}, \code{\link{cites_extract}}
#' @export
#' @rdname cites_replace_order
cites_replace_order = function(tex, citesall=NULL){
  if (is.null(citesall)) {
    citesall = cites_extract(tex)
  }
  new_tex = sapply(tex, function(x) single_replace_order(x, citesall), USE.NAMES =FALSE)
  return(new_tex)
}


