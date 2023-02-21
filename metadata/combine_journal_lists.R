#' @title  combine journals and output csv file
#'
#' @import data.table
#' @importFrom stringr str_trim str_to_lower str_count str_replace_all str_glue str_squish
#' @importFrom stringi stri_escape_unicode
#' @importFrom usethis use_data
#' @return NULL
#' @keywords  combine journals and output CSV
#' @examples
#' \dontrun{
#' journalabbr:::combine_journal_lists()
#' }
combine_journal_lists <- function() {
  ## CSV file is divided by semicolon(;)
  rm(list = ls())
  journal <- journal_abbr <- journal_lower <- fz_count_dot <- fz_count_abbrlen <- fz_count_upper <- NULL
  filedf <- tibble::tribble(
    ~file,  ~weight,
    "myabbr.csv", 1,
    "woodward_library.csv", 2,
    "journal_abbreviations_webofscience-dots.csv", 3,
    "journal_abbreviations_acs.csv", 3,
    "journal_abbreviations_ams.csv", 3,
    "journal_abbreviations_annee-philologique.csv", 3,
    "journal_abbreviations_dainst.csv", 3,
    "journal_abbreviations_entrez.csv", 3,
    "journal_abbreviations_general.csv", 3,
    "journal_abbreviations_geology_physics_variations.csv", 3,
    "journal_abbreviations_geology_physics.csv", 3,
    # "journal_abbreviations_ieee_strings.csv", 3,
    "journal_abbreviations_ieee.csv", 3,
    "journal_abbreviations_lifescience.csv", 3,
    "journal_abbreviations_mathematics.csv", 3,
    "journal_abbreviations_mechanical.csv", 3,
    "journal_abbreviations_medicus.csv", 3,
    "journal_abbreviations_meteorology.csv", 3,
    "journal_abbreviations_sociology.csv", 3,
    "journal_abbreviations_webofscience.csv", 3
  )

  filelist <- paste0("./metadata/journals/", filedf$file)
  library(data.table)
  dt_list <- list()
  k <- 1
  for (i in filelist) {
    if (file.exists(i)) {
      dt_list[[k]] <- data.table::fread(i, sep = ";", quote = "\"", header = FALSE, fill = TRUE)
      dt_list[[k]][, "originFile" := filedf$file[k]]
      k <- k + 1
    }
  }

  dt <- data.table::rbindlist(dt_list, fill = TRUE) # Merge multiple data
  dt <- dt[, c("V1", "V2", "originFile"), with = FALSE]
  dt = merge(dt,filedf,sort=F,all.x=T, by.x ='originFile', by.y = 'file')
  dt <- dt[, c("V1", "V2", "originFile","weight"), with = FALSE]

  library(purrr)
  library(stringr)
  cat(sprintf("After the merger, there are %d journals in total.\n", dt[, .N]))
  setnames(dt, c("V1", "V2", "originFile"), c("journal", "journal_abbr", "originFile"))

  dt <- dt[, lapply(.SD, str_squish)]

  dt_1 <- copy(dt)
  dt_2 <- copy(dt)
  dt_3 <- copy(dt)


  dt_1[, journal := str_replace_all(journal, "(?<= )\\&(?= )", "&")]
  dt_1[, journal := str_replace_all(journal, "(?<= )\\\\&(?= )", "&")]
  dt_1[, journal := str_replace_all(journal, "(?<= )[aA][nN][dD](?= )", "&")]

  dt_2[, journal := str_replace_all(journal, "(?<= )&(?= )", "and")]
  dt_2[, journal := str_replace_all(journal, "(?<= )\\\\&(?= )", "and")]
  dt_2[, journal := str_replace_all(journal, "(?<= )\\&(?= )", "and")]

  dt_3[, journal := str_replace_all(journal, "(?<= )&(?= )", "\\\\&")]
  dt_3[, journal := str_replace_all(journal, "(?<= )[aA][nN][dD](?= )", "\\\\&")]
  dt_3[, journal := str_replace_all(journal, "(?<= )\\&(?= )", "\\\\&")]


  dt <- unique(rbindlist(list(dt_1, dt_2, dt_3), use.names = TRUE, fill = TRUE))
  cat(sprintf("After 'and' and '&' are replaced and merged, there are %d journals in total.\n", dt[, .N]))

  ############ 1. Journal special value processing
  #### 1.1 Delete lines with backslashes and forward slashes -- journals are too special
  dt <- dt[!grepl(pattern = "\\\\", journal), ]
  dt <- dt[!grepl(pattern = "/{1,10}", journal), ]
  #### 1.2 Delete lines with double quotes -- journals are too special
  dt <- dt[!grepl(pattern = '"', journal), ]
  ##### 1.3 Delete journals whose journal field is more than 80 or less than 5 characters
  dt <- dt[nchar(journal) <= 80 & nchar(journal) >= 5, ]

  #dt <- dt[!grepl(pattern = "\\\\", journal_abbr), ]
  #dt <- dt[!grepl(pattern = "/{1,10}", journal_abbr), ]
  dt <- dt[!grepl(pattern = '"', journal_abbr), ]


  ########### 2. Journal field to lowercase and add some auxiliary columns to help filter duplicate journals later.
  dt[, journal_lower := str_to_lower(journal)]
  dt[, fz_count_dot := str_count(journal_abbr, "\\.")] # Calculate the number of dots in the abbr field.
  dt[, fz_count_abbrlen := str_length(journal_abbr)] # Calculate the length of the abbr field.
  # Calculate the number of uppercase letters in the abbr field.
  dt[, fz_count_upper := str_count(journal_abbr, "[A-Z]")]

  ########## 3. Remove duplicate items, Filter according to certain conditions.
  dt_new <- dt[dt[, .I[order(weight,-fz_count_dot, -fz_count_upper, fz_count_abbrlen)[1]], by = journal_lower]$V1, ]

  dt_new_sub <- dt_new[, c("journal_lower", "journal_abbr", "originFile"), with = FALSE]
  stopifnot(uniqueN(dt_new_sub[, c("journal_lower"), with = FALSE]) == dt_new_sub[, .N])
  cat(sprintf(
    "Delete duplicate items. Finally, a total of %d journals with abbreviations were used.\n",
    dt_new_sub[, .N]
  ))

  abbrtable_sys <- dt_new_sub[, lapply(.SD, stringi::stri_escape_unicode)]
  usethis::use_data(abbrtable_sys, compress = "xz", internal = TRUE, overwrite = TRUE, version = 3)
}

combine_journal_lists()
