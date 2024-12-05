#' @title  Create an abbreviation table by merging user-provided and system tables
#'
#' @description
#' This function processes a user-provided abbreviation table and optionally merges it with a system abbreviation table.
#' The user table can be provided as a file path, `data.frame`, or `data.table`.
#' The system table is included by default but can be excluded.
#'
#' @param user_table A user-provided abbreviation table. It can be a file path (string), a `data.frame`,
#' or a `data.table`. The table must contain columns `journal_lower` and `journal_abbr`. Default is `NULL`.
#' @param use_sys_table Logical. Whether to include the system abbreviation table (`abbrtable_sys`)
#' in the merged result. Default is `TRUE`.
#'
#' @return A `data.table` containing the merged abbreviation table, with any duplicates (by `journal_lower`) removed.
#' The resulting table contains two columns: `journal_lower` and `journal_abbr`.
#'
#' @details The function first processes the user-provided table by checking its type:
#' - If it's a file path, it reads the file using `data.table::fread`.
#' - If it's a `data.frame`, it is converted to a `data.table`.
#' - If it's already a `data.table`, no conversion is needed.
#'
#' After processing the user table, the system table (`abbrtable_sys`) is optionally included in the final merged table.
#' Duplicate entries based on `journal_lower` are removed.
#'
#' @example inst/example/get_abbrtable_example.R
#' @importFrom rlang is_empty
#' @export
get_abbrtable <- function(user_table = NULL, use_sys_table = TRUE) {
  stopifnot(!is.logical(user_table), is.logical(use_sys_table))

  if (is_empty(user_table) && !use_sys_table) {
    return(NULL)
  }

  if (!is_empty(user_table)) {
    if (is.character(user_table)) {
      if (file.exists(user_table)) {
        user_table <- data.table::fread(user_table, header = TRUE)
      } else {
        stop("The provided user_table file path does not exist.")
      }
    } else if (is.data.frame(user_table)) {
      user_table <- data.table::as.data.table(user_table)
    } else if (!is.data.table(user_table)) {
      stop("user_table must be a file path, data.frame, or data.table")
    }

    if (!all(c("journal_lower", "journal_abbr") %in% colnames(user_table))) {
      stop("user_table must contain 'journal_lower' and 'journal_abbr' columns")
    }

    user_table <- user_table[, c("journal_lower", "journal_abbr"), with = FALSE]
    user_table <- user_table[, "journal_lower" := stringr::str_squish(tolower(get("journal_lower")))]
  }

  sys_table <- NULL
  if (use_sys_table) {
    sys_table <- abbrtable_sys[, c("journal_lower", "journal_abbr"), with = FALSE]
    # if (exists("abbrtable_sys", where = "package:journalabbr")) {
    #   abbrtable_sys <- get("abbrtable_sys", envir = asNamespace("journalabbr"))
    #   sys_table <- abbrtable_sys[, c("journal_lower", "journal_abbr"), with = FALSE]
    # } else {
    #   warning("System abbreviation table 'abbrtable_sys' from the journalabbr package not found. Skipping.")
    #   use_sys_table <- FALSE
    # }
  }

  abbrtable <- data.table::rbindlist(list(user_table, sys_table))
  abbrtable <- abbrtable[!duplicated(abbrtable, by = "journal_lower")]

  return(abbrtable)
}
