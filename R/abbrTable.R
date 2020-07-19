#' data abbrTable data
#'
#' @description  There are four columns, each separated by a semicolon (;), as follows
#' journal  journal_abbr journal_lower. count_dot
#'
#' @source https://github.com/JabRef/abbrv.jabref.org/journals
#' @format A data frame with columns:
#' \describe{
#' \item{journal}{Full name of Journal}
#' \item{journal_abbr}{Abbreviated format of Journal with dots}
#' \item{journal_lower}{The full name of the journal should be in lowercase and unique}
#' \item{count_dot}{The number of points in abbreviations of Journal with dots}
#' \item{originFile}{File source}
#' \item{abbr_len}{The char length  of abbreviations of Journal }
#' }
"abbrTable"

