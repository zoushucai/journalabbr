#' @title Export a BibTeX \code{data.table} to a .bib file.
#' @description The BibTeX \code{data.table} is written to a .bib file.
#' @param dt \code{data.table}, in the format as returned by \code{\link{read_bib2dt}}.
#' @param file character, file path to write the .bib file.
#' @param columns character vector, names of the columns to exclude from the output. a default list of columns will
#' be used from \code{\link{get_delcolumns}}.
#' @param indent character, the indentation to use for the .bib file. Default is `"\t"`
#' @return \code{file} as a character string, invisibly.
#' @export
#' @importFrom rlang is_empty
#' @example inst/example/write_example.R
#'
write_dt2bib <- function(dt, file = tempfile(fileext = ".bib"), columns = get_delcolumns(), indent = "\t") {
  delcol <- toupper(columns)
  # 删除指定列，如果列名不存在则跳过
  dt <- dt[, !colnames(dt) %in% delcol, with = FALSE]

  # 只把列名大写的列全部导出，且包含 CKEY 和 ITYPE
  col_names <- colnames(dt)
  upper_cols <- col_names[col_names == toupper(col_names)]

  # stop('This tibble has a lowercase column name, which does not meet the requirements')
  stopifnot(length(upper_cols) > 0L, all(c("CKEY", "ITYPE") %in% upper_cols))

  rest_cols <- setdiff(upper_cols, c("CKEY", "ITYPE"))

  # 警告 NA 值
  if (any(is.na(dt$ITYPE)) || any(is.na(dt$CKEY))) {
    warning("NA value in CKEY or ITYPE field. These entries will be removed: ", which(is.na(dt$ITYPE) | is.na(dt$CKEY)))
  }

  # 删除 NA 值的条目
  dt <- dt[!(is.na(dt$ITYPE) | is.na(dt$CKEY)), ]
  # 设置缩进符号
  indent_str <- ifelse(is.na(indent) || is_empty(indent), "", indent)
  # 构建 BibTeX 文本, 把所有的字段按照固定的格式写入
  dt$bibtex <- sprintf("@%s{%s,\n", tolower(dt$ITYPE), dt$CKEY)
  # 循环处理其他字段并拼接
  for (col in rest_cols) {
    # 检查 NA 值并拼接字段
    field_str <- ifelse(is.na(dt[[col]]), "", sprintf("%s%s = {%s},\n", indent_str, tolower(col), dt[[col]]))
    dt$bibtex <- paste0(dt$bibtex, field_str)
  }
  # 尝试删除最后一个逗号
  dt$bibtex <- gsub(",\n$", "", dt$bibtex, perl = TRUE)
  # 添加最后一个右大括号
  dt$bibtex <- paste0(dt$bibtex, "\n}")

  # 把dt$bibtex 拼接起来写入文件
  bibtxt <- paste0(dt$bibtex, collapse = "\n\n")
  cat(bibtxt, file = file, sep = "\n\n")
  invisible(file)
}
