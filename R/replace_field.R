#' @title Replace or process a specified field in a data.table
#'
#' @description
#'  This function processes a specified field in a `data.table` by using a journal abbreviation table
#' (either user-specified, system-provided, or both) to abbreviate journal names in the `oldfield`.
#' Additionally, a custom function can be applied to the processed field if specified.
#'
#' @param dt A `data.table`. The input data table that contains the field to be processed.
#' @param oldfield A character string. The name of the field to be processed, typically in uppercase
#' (e.g., "JOURNAL"). Must be a valid column name in `dt`.
#' @param newfield A character string. The name of the new field where the processed result will be stored.
#' If this field does not exist in `dt`, it will be created.
#' @param user_table A `data.table`. Optional. A user-provided journal abbreviation table with at least
#' two columns: `journal_lower` and `journal_abbr`. Defaults to `NULL`. If provided, it will be merged
#' with the system abbreviation table if `use_sys_table = TRUE`.
#' @param use_sys_table A logical. Whether to use the system-provided journal abbreviation table.
#' Defaults to `TRUE`. If `TRUE`, the system abbreviation table is used alongside the user-provided one.
#' @param fun A function. Optional. A custom function to apply to the processed field after abbreviation
#' (if applicable). Defaults to `NULL`. The function should accept a column from `dt` as its first argument
#' and return the processed values.
#' @param ... Additional arguments passed to the custom function `fun`, if provided.
#'
#' @details
#' If the `oldfield` is "JOURNAL", the function will attempt to apply journal abbreviations using the provided
#' abbreviation table(s). The abbreviation process involves converting the journal names to lowercase and
#' removing excess whitespace before matching against the abbreviation table.
#'
#' The `user_table` and `use_sys_table` parameters allow flexibility in choosing which abbreviation
#' tables to use. If both are used, they will be merged, and duplicates will be removed.
#'
#' If a custom function is provided via `fun`, it will be applied to the processed field after any abbreviations.
#'
#' @return The function returns the modified `data.table` with the processed field stored in `newfield`.
#' @export
#' @importFrom rlang is_empty
#' @example inst/example/replace_example.R
#' @rdname replace_field
#'
replace_field <- function(dt, oldfield, newfield, user_table = NULL, use_sys_table = TRUE, fun = NULL, ...) {
  stopifnot(is.data.table(dt))
  stopifnot(is.character(oldfield) && length(oldfield) == 1)
  stopifnot(is.character(newfield) && length(newfield) == 1)
  stopifnot(oldfield %in% names(dt))
  dt <- data.table::copy(dt)
  # Create the abbreviation table
  abbrtable <- get_abbrtable(user_table, use_sys_table)

  # If abbreviation table is provided, process the field
  if (!is_empty(abbrtable)) {
    # only keep necessary columns from the abbreviation table, e.g., journal name and abbreviation
    abbrtable <- abbrtable[, c("journal_lower", "journal_abbr"), with = FALSE]
  }

  # Handle JOURNAL field specifically
  if ("JOURNAL" == oldfield && !is_empty(abbrtable)) {
    dt <- process_journal(dt, oldfield, newfield, abbrtable)
  }

  # Apply custom function if provided
  if (!is_empty(fun)) {
    dt[, (newfield) := fun(get(newfield), ...)]
  }

  return(dt)
}

# Helper function to process journal abbreviation
process_journal <- function(dt, oldfield, newfield, abbrtable) {
  dt[, (newfield) := stringr::str_squish(tolower(dt[[oldfield]]))]
  dt <- merge(dt, abbrtable, by.x = newfield, by.y = "journal_lower", all.x = TRUE, suffixes = c("", "_abbr"))
  dt[, (newfield) := ifelse(is.na(get("journal_abbr")), dt[[oldfield]], get("journal_abbr"))]
  dt[, c("journal_abbr") := NULL] # Remove temporary column
  return(dt)
}
