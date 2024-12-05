#' @title Parse a BibTeX file to a \code{data.table}.
#' @description The BibTeX file is read, parsed, tidied and written to a \code{data.table}.
#' @details Read, parse and collate bibtex file to form a data.table.
#'   Different BIB may produce different data.table columns.
#' @param file character, path or URL to a bib file.
#' @param encoding character, encoding to be passed to \code{base::readLines}. Default is "UTF-8".
#' @return A \code{data.table}.
#'
#' @seealso \code{\link[base]{readLines}} for more details on reading text files.
#' @export
#' @import stringr purrr
#' @author ShuCai Zou
#' @example inst/example/read_example.R
#'
read_bib2dt <- function(file, encoding = "UTF-8") {

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


  ################################################
  ## 1. read the file
  ################################################
  bib <- readLines(file, encoding = encoding)
  bib <- str_squish(bib)

  temp <- grep("^@Comment\\{jabref-meta: databaseType:bibtex;\\}$", bib, perl = TRUE)
  if (length(temp) != 0) {
    warning('This file is exported from "JabRef" and we will automatically remove the "JabRef" tag.
            tag: @Comment{jabref-meta: databaseType:bibtex;}')
    bib[temp] <- ""
  }
  rm(temp)
  ###############################################
  # 1. remove comments
  bib <- bib[!str_detect(bib, "^%")]
  bib <- bib[bib != ""]
  #########################################################
  ########### 2. Process Bib file entries #############
  #########################################################
  # find the start and end of each entry
  from <- which(str_extract(bib, "[:graph:]") == "@")
  to <- c(from[-1] - 1, length(bib))
  if (length(from) == 0L) {
    stop("There are no available references, please check the bib file.")
  }

  # extract each item
  itemslist <- mapply(function(x, y) {
    return(bib[x:y])
  }, x = from, y = to, SIMPLIFY = FALSE)

  # remove empty lines in each item
  itemslist <- map(itemslist, function(item) {
    item <- str_squish(item)
    item <- item[item != ""]
    return(item)
  })
  # calculate the number of lines in each item
  itemslist_len <- map_int(itemslist, length)
  # if any item has less than 5 lines, remove it and issue a warning
  n <- 5
  if (any(itemslist_len < n)) {
    warning(str_glue("Some entries have less than {n} lines, these entries will be removed:"), immediate. = TRUE)
    for (i in itemslist[itemslist_len < n]) {
      cat(i, sep = "\n")
    }
    cat("=========================================\n")
  }
  # remove items with less than 5 lines
  itemslist <- itemslist[itemslist_len >= n]

  # if any item is not valid, remove it and issue a warning
  checkresult <- map_lgl(itemslist, checkitem_valid)
  if (any(!checkresult)) {
    warning("Some entries are not valid BibTeX entries, these entries will be removed:", immediate. = TRUE)
    cat("=========================================\n")
    for (i in itemslist[!checkresult]) {
      cat(i, sep = "\n")
    }
    cat("=========================================\n")
  }
  # remove invalid items
  itemslist <- itemslist[checkresult]


  # extract fields from each item
  itemslist_fields <- map(itemslist, extract_fields)

    # combine all items into a data.table
  dt <- rbindlist(itemslist_fields, use.names = TRUE, fill = TRUE)

  # Check if the CKEY is repeated after removing NA
  if (any(duplicated(dt$CKEY, incomparables = NA))) {
    warning(
      "====== Duplicate key in uploaded Bib file =======:\n",
      paste0(dt$CKEY[duplicated(dt$CKEY)], collapse = "\n"),
      "\n===============================================\n"
    )
  }
  return(dt)
}



#' @title Check if BibTeX item is valid
#' @description This function checks if a given BibTeX entry is valid by ensuring it follows the correct format.
#' A valid BibTeX entry should:
#' 1. The first line should match the format: \code{@TYPE{CITATION_KEY},
#' 2. The second to second-last lines should be key-value pairs in the format: key = value,
#' 3. The last line should be a closing brace: }
#' @param item A character vector representing a single BibTeX entry, where each element is a line from the entry.
#' @return TRUE if the item is valid, FALSE otherwise.
#'
#' @importFrom stringr str_detect str_count
#' @example inst/example/read_example.R
#' @noRd
#'
checkitem_valid <- function(item) {
  # 1. Check if the first line matches the pattern: @TYPE{CITATION_KEY,
  if (!str_detect(item[1], "^@\\S+\\{\\S+,$")) {
    return(FALSE)
  }
  # 2. Check if all lines from the second to the second-last follow the pattern: key = value,
  for (i in 2:(length(item) - 1)) {
    if (!str_detect(item[i], "^[a-zA-Z0-9\\s]+=.+[,]?$")) {
      return(FALSE)
    }
  }
  # 3. Check if the last line is a closing brace: }
  lastline <- item[length(item)]
  if (!str_detect(lastline, "^\\}")) {
    if (str_detect(lastline, "\\}$")) {
      # count the number of { and }
      open_braces_count <- str_count(lastline, "\\{")
      close_braces_count <- str_count(lastline, "\\}")
      if (close_braces_count - open_braces_count != 1) {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  }
  # If all checks pass, return TRUE
  return(TRUE)
}

#' @title Extract fields from BibTeX entry
#' @description This function extracts all the fields from a BibTeX entry. It retrieves the citation key, entry type,
#' and all key-value pairs in the entry, cleaning the values by removing commas, quotes, and braces.
#' @param item A character vector representing a BibTeX entry, where each element is a line from the entry.
#' @param  check Logical.
#' @return A list containing the citation key (`CKEY`), the entry type (`ITYPE`), and all key-value pairs.
#'
#' @importFrom stringr str_squish str_glue
#' @example inst/example/read_example.R
#' @noRd
#'
extract_fields <- function(item, check = FALSE) {
  if (check) {
    if (!checkitem_valid(item)) {
      warning("The provided item is not valid. Returning an empty list.")
      return(list())
    }
  }

  # Extract the citation key from the first line (text between the first { and comma)
  ckey <- str_squish(str_extract(item[1], "(?<=\\{)[^,]+"))
  # Extract the entry type from the first line (text after @ and before {)
  itype <- str_squish(str_extract(item[1], "(?<=@)[^\\{]+"))
  # Initialize a list with the citation key and entry type
  fields <- list(CKEY = ckey, ITYPE = itype)
  n <- length(item)
  lastline <- item[n]
  if (!str_detect(lastline, "^\\}") && str_detect(lastline, "\\}$")) {
    # count the number of { and }
    open_braces_count <- str_count(lastline, "\\{")
    close_braces_count <- str_count(lastline, "\\}")
    if (close_braces_count - open_braces_count == 1) {
      lastline <- gsub("\\}$", "", lastline, perl = TRUE)
      item[n] <- lastline
      item[n + 1] <- "}"
    }
  }

  # Loop through each line of the BibTeX entry (except the first and last)
  for (i in 2:(length(item) - 1)) {
    # Split each line by "=" into a key and a value
    lt <- str_split(item[i], "[ ]*=[ ]*", n = 2, simplify = TRUE)
    key <- toupper(str_squish(lt[1])) # Extract and clean the key, converting to uppercase
    value <- str_squish(lt[2]) # Extract and clean the value

    # Remove trailing commas from the value
    value <- gsub(",$", "", value, perl = TRUE)
    # Remove quotes around the value, if present
    value <- gsub('^(\\")(.*?)(\\")$', "\\2", value, perl = TRUE)
    # Remove braces around the value, if present
    value <- gsub("^(\\{)(.*?)(\\})$", "\\2", value, perl = TRUE)

    # Check if the key is empty
    if (key == "") {
      # If the key is empty, issue a warning and skip this field
      warning(str_glue("Detected empty keys in the BibTeX entries.
                       Please check the bib file.\nEmpty key: '{key}' in Citation Key: '{ckey}'."))
      next
    }
    # Check if the key is duplicated
    if (key %in% names(fields)) {
      # If the key is duplicated, issue a warning and skip this field
      warning(str_glue("Detected duplicate keys in the BibTeX entries.
                       Please check the bib file.\nDuplicate key: '{key}' in Citation Key: '{ckey}'."))
      next
    }
    # Add the key-value pair to the fields list
    fields[[key]] <- value
  }
  return(fields)
}
