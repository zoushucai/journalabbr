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
#' The function performs the following operations:
#' 1. Removes special "zero-width" spaces.
#' 2. Removes multiple spaces between non-space characters and replaces them with a single space.
#' 3. Removes spaces before punctuation (e.g., commas, periods).
#' 4. Handles LaTeX-style `\left(` and `\right)` by removing spaces around them.
#' 5. Adds spaces after certain punctuation marks (e.g., comma, period).
#' 6. Removes trailing spaces at the end of the string.
#' 7. Handles LaTeX math symbols like `=` and `^` to ensure correct spacing.
#' 8. Skips lines with "@" or lines starting with "%" to prevent unwanted formatting.
#'
#' @export
#' @rdname tex_funs
#'
single_add_space <- function(text) {
  ### 运行之前一定要备份文件，运行之后也需要仔细检查
  # 确保输入是一个单一字符的字符串
  stopifnot(is.character(text) && length(text) == 1)

  #### 1. 移除特殊空格
  text <- gsub("\ufeff", "", text, perl = TRUE)

  ### 2.跳过有@行的情况 和 跳过以 % 开头的行或者以空格+ % 开头的行
  if (grepl("@", text)) {
    return(text)
  }

  if (grepl("^\\s*%", text)) {
    return(text)
  }

  # 3. 将多个空格变为一个空格
  text <- gsub("(?<=\\S)\\s+(?=\\S)", " ", text, perl = TRUE)

  #### 4. 移除标点符号前面的空格
  text <- gsub("\\s+(?=[,.?;])", "", text, perl = TRUE)

  ### 5. 检测并去除 `\left(` 和 `\right)` 之间的空格
  text <- gsub("\\\\left\\s*\\(", "\\\\left( ", text)
  text <- gsub("\\\\right\\s*\\)", "\\\\right) ", text)

  #### 6. 标点符号后面添加空格
  text <- gsub("(?<=[,.?;])\\s+(?=[a-zA-Z])", " ", text, perl = TRUE)

  #### 7. 如果字符串末尾有空格，去掉它
  text <- gsub("\\s+$", "", text)

  #### 8. 对数学公式中的空格进行处理
  ### 8.1 移除等号两边的空格
  # text <- gsub("(?<=[^\\s])=(?=[^\\s])", " = ", text, perl = TRUE)
  ### 8.2 箭头两边添加空格
  # text <- gsub("(?<=[^\\s])>(?=[^\\s])", " > ", text, perl = TRUE)
  # text <- gsub("(?<=[^\\s])<(?=[^\\s])", " < ", text, perl = TRUE)
  ### 8.3 ^移除两边的空格
  text <- gsub("\\s*\\^", "^", text, perl = TRUE)
  text <- gsub("\\^\\s*", "^", text, perl = TRUE)

  #### 8.5 处理文字中间的多余空格
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
  # tex 是一个字符串向量
  # 仅对 tex 的文档进行处理

  tex <- unlist(lapply(tex, FUN = fun, ...))
  return(tex)
}
