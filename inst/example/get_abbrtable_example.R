# Example 1: Provide a user table as a CSV file path and merge with system table
user_table_path <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
abbr_table <- get_abbrtable(user_table = user_table_path, use_sys_table = TRUE)

# Example 2: Provide a user table as a data.frame
user_df <- data.frame(journal_lower = c("example journal", "sample journal"),
                      journal_abbr = c("EJ", "SJ"))
abbr_table <- get_abbrtable(user_table = user_df, use_sys_table = FALSE)

# Example 3: Use only the system table
abbr_table <- get_abbrtable(use_sys_table = TRUE)

abbr_table <- get_abbrtable(user_table=NULL, use_sys_table = FALSE)
