#' Extract Single Citation Keys from LaTeX Text
#'
#' \code{single_cites_extract} function extracts all citation keys from a LaTeX string based on specific citation commands.
#'
#' @param text A character vector containing LaTeX content.
#' @return A character vector of citation keys, or `NULL` if no citations are found.
#' @details
#' The function identifies citation commands such as \code{cite}, \code{upcite}, \code{citep}, and \code{citet}.
#' It extracts the content within braces following these commands, and splits multiple keys separated by commas.
#' @example inst/example/cites_extract_example.R
#' @importFrom stringr str_extract_all str_trim
#' @export
#' @rdname cites_extract

single_cites_extract = function(text){
  ## 提取tex中的所有引用
  latex_prefix=c("cite", "upcite", "citep", "citet")
  # 提取tex中的所有引用
  pattern <- sprintf("(?<=\\\\(%s)\\{)([^}]+)(?=\\})", paste(latex_prefix, collapse = "|"))

  cites = stringr::str_trim(unlist(str_extract_all(text, pattern)))
  if(length(cites) == 0){
    return(NULL)
  }
  #按照逗号分隔
  cites = stringr::str_trim(unlist(strsplit(cites, ",")))
  return(cites)
}



#' Extract Unique Citation Keys from LaTeX Document
#'
#' \code{cites_extract} function extracts all unique citation keys from a LaTeX document.
#'
#' @param tex A character vector where each element represents a line or section of LaTeX content.
#' @return A unique character vector of citation keys.
#' @details
#' This function uses \code{single_cites_extract} to extract citation keys from each line
#' or section of the LaTeX document. It removes duplicates and trims unnecessary whitespace.
#' @example inst/example/cites_extract_example.R
#' @importFrom stringr str_trim
#' @export
cites_extract = function(tex){
  cites =stringr::str_trim(unlist(lapply(tex, single_cites_extract)))
  cites = unique(cites)
  return(cites)
}
