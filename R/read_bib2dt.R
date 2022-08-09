#' @title Parse a BibTeX file to a \code{data.table}.
#' @description The BibTeX file is read, parsed, tidied and written to a \code{data.table}.
#' @details Read, parse and collate bibtex file to form a data.table.
#'   Different BIB may produce different data.table columns.
#' @param file character, path or URL to a bib file.
#' @return A \code{data.table}.
#'
#' @export
#'
#' @author ShuCai Zou
#' @examples
#' # Read from .bib file:
#' file1 <- system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
#' dt1 <- read_bib2dt(file1)
#' colnames(dt1)
#'
#' file2 <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
#' dt2 <- read_bib2dt(file2)
#' colnames(dt2)
#'
#' @testexamples
#' expect_true(is.data.table(dt1))
#' expect_true(is.data.table(dt2))
#' expect_warning(read_bib2dt(file1),"Duplicate key in uploaded Bib file")
#' expect_warning(read_bib2dt(file1),"NA value exists in Citation Key, please check the bib file")
#' expect_warning(read_bib2dt(file2),"Duplicate key in uploaded Bib file")


read_bib2dt <- function(file) {
  if (!is.character(file)) {
    stop("Invalid file path: Non-character supplied.", call. = FALSE)
  }
  if (grepl("http://|https://|www.", file)) {
    tryCatch(
      {
        httr::GET(file)
      },
      error = function(e) {
        stop("Invalid URL: File is not readable.", call. = FALSE)
      }
    )
  } else {
    if (as.numeric(file.access(file, mode = 4)) != 0) {
      stop("Invalid file path: File is not readable.", call. = FALSE)
    }
  }

  ### Read the bib  and return atibble
  ################################################
  ## 1. reaf  bib file, Extract fields
  ################################################
  bib <- readLines(file, encoding = "UTF-8")
  bib <- str_squish(bib)
  # delete jabref style
  temp <- grep("^@Comment\\{jabref-meta: databaseType:bibtex;\\}$", bib, perl = TRUE)
  if (length(temp) != 0) {
    warning('This file is exported from "JabRef" and we will automatically remove the "JabRef" tag.
            tag: @Comment{jabref-meta: databaseType:bibtex;}')
    bib[temp] <- ""
  }
  rm(temp)
  #########################################################
  ########### 2.  Bib file processing  #####################
  #########################################################
  from <- which(str_extract(bib, "[:graph:]") == "@")
  to <- c(from[-1] - 1, length(bib))
  if (length(from) == 0L) {
    stop("There are no available references, please check the bib file.")
  }

  itemslist <- mapply(function(x, y) {
    return(bib[x:y])
  }, x = from, y = to - 1, SIMPLIFY = FALSE)

  dt <- tidytable::enframe.(itemslist, name = "fz_id", value = "fz_rawchar")

  dt$fz_char <- NA
  dt$fz_char <- map(dt$fz_rawchar, function(x) {
    str_squish(x)
  })

  len <- map(dt$fz_char, function(x) {
    max(grep("\\}", x))
  })

  if (sum(sapply(len, is.infinite)) > 0) {
    message("Some BibTeX entries may have been dropped. The results may not be correct.
            Check the. Bib file to make sure that each entry starts with '@' and ends with '}'")
  }

  dt$fz_char <- map2(dt$fz_char, len, function(x, y) {
    x[1:y]
  })
  dt$fz_char <- map(dt$fz_char, function(x) {
    gsub("(^,?)(.*?)(,?$)", "\\2", x)
  })

  dt$fz_char <- map(dt$fz_char, function(x) {
    x[which(x != "")]
  })

  len <- map(dt$fz_char, function(x) {
    max(grep("\\}", x))
  })

  dt$fz_char <- map2(dt$fz_char, len, function(x, y) {
    x[y] <- gsub("\\}$", "", x[y])
    x
  })
  dt$fz_char <- map(dt$fz_char, function(x) {
    x[which(x != "")]
  })
  #####################################################
  ##### 4. for 'fz_char' list Processing cuts by '=' - form two columns
  ####################################################
  ###  4.1 extract CitationKey  --->  Abbreviation, CKEY
  dt$CKEY <- NA
  dt$CKEY <- map_chr(dt$fz_char, function(x) {
    str_squish(str_extract(x[1], "(?<=\\{)[^,]+"))
  })
  # Check whether there is NA value in  CKEY in data.table
  if (sum(is.na(dt$CKEY)) > 0) {
    warning("NA value exists in Citation Key, please check the bib file")
  }
  # Check if the CKEY is repeated after removing NA
  if (any(duplicated(dt$CKEY, incomparables = NA))) {
    warning(
      "\n====== Duplicate key in uploaded Bib file =======:\n",
      paste0(dt$CKEY[duplicated(dt$CKEY)], collapse = "\n"),
      "\n===============================================\n"
    )
  }

  ###  4.2  extract ITEM TYPE  ---> Abbreviation, ITYPE
  dt$ITYPE <- NA
  dt$ITYPE <- map_chr(dt$fz_char, function(x) {
    str_to_upper(str_squish(str_extract(x[1], "(?<=@)(.+?)(?=\\{)")))
  })

  ### 4.3 The rest of the fields are cut with the first occurrence of '='
  items <- map(dt$fz_char, function(x) {
    # return a list, list element is a two column data.frame
    lt <- str_split(x[-1], "[ ]*=[ ]*", n = 2, simplify = TRUE)
    lt[, 1] <- toupper(lt[, 1])
    lt[, 2] <- gsub('^(\\")(.*?)(\\")$', "\\2", lt[, 2]) # remove both sides " *** "
    lt[, 2] <- gsub("^(\\{)(.*?)(\\})$", "\\2", lt[, 2]) # and remove both sides { *** }
    tlt <- t(lt)
    colnames(tlt) <- tlt[1, ]
    tlt <- as.data.table(as.data.frame(tlt))
    tlt[-1, ]
  })

  field <- tidytable::as_tidytable(rbindlist(items, use.names = TRUE, fill = TRUE))
  field$fz_id <- seq_len(nrow(field))

  stopifnot(dt[, .N] == field[, .N])
  vec = intersect(colnames(dt),colnames(field))
  if(length(vec) != 1 || vec != "fz_id"){
    setnames(field, setdiff(vec,"fz_id"), paste0(setdiff(vec,"fz_id"),"_2"))
    warning("The field in the bib file conflicts with the default field (eg,'CKEY', 'ITYPE') and has been modified.")
  }
  new_dt <- merge.data.table(dt, field, by = "fz_id", all.x = TRUE, sort = FALSE)

  pat <- "([\u4e00-\u9fa5])(\\}){1,}([ ]{1,})(and)([ ]{1,})(\\{){1,}([\u4e00-\u9fa5])"
  new_dt$AUTHOR <- gsub(pat, "\\1 and \\7", new_dt$AUTHOR)
  return(new_dt)
}
