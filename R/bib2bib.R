#' @title Load .bib, Process .bib and Output .bib File.
#'
#' @description
#' The `bib2bib` function reads a BibTeX file, replaces the journal field using a custom or
#' system-provided abbreviation table, optionally applies a function to the author field,
#' and writes the result back to a new BibTeX file.
#'
#' @param file A string representing the path to the input `.bib` file to be processed.
#' @param out.file A string representing the path where the output `.bib` file should be saved.
#' @param user_table A user-provided abbreviation table. It can be a file path (string),
#' a `data.frame`, or a `data.table`.
#' The table must contain columns `journal_lower` and `journal_abbr`. Default is `NULL`.
#' @param use_sys_table Logical. Whether to include the system abbreviation table (`abbrtable_sys`)
#' in the merged result. Default is `TRUE`.
#' @param fun A function (optional) to apply to the "author" field in the `.bib` file.
#' If provided, this function will be applied to the "author" field
#' @param ... Additional arguments passed to the custom function `fun`, if provided.
#' @return None. The function writes the processed data to the specified output file.
#'
#' @details
#' The function works in the following steps:
#' 1. Reads a custom journal abbreviation table from a CSV file (if provided).
#' 2. Reads the input BibTeX file and converts it into a `data.table`.
#' 3. Replaces the `JOURNAL` field with abbreviations from the provided or system abbreviation table.
#' 4. Optionally applies the provided function to the `AUTHOR` field.
#' 5. Writes the modified data back to a new BibTeX file.
#'
#' @example inst/example/bib2bib_example.R
#' @importFrom rlang is_empty
#' @export
#'
bib2bib <- function(file,
                    out.file,
                    user_table = NULL,
                    use_sys_table = TRUE,
                    fun = NULL,
                    ...) {
  # 2. 处理 JOURNAL 字段
  dt <- read_bib2dt(file)
  dt <- replace_field(dt,
    oldfield = "JOURNAL",
    newfield = "JOURNAL",
    user_table = user_table,
    use_sys_table = use_sys_table,
    fun = NULL
  )


  # 3. 处理 AUTHOR 字段（如果提供了函数）
  if (!is_empty(fun)) {
    dt <- replace_field(dt,
      oldfield = "AUTHOR",
      newfield = "AUTHOR",
      user_table = NULL,
      use_sys_table = FALSE,
      fun = fun, ...
    )
  }

  # 4. 将处理后的数据写入 .bib 文件
  write_dt2bib(dt, file = out.file)

  message("Processing complete. Output saved to: ", out.file)
}
