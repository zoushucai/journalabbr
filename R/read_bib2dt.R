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
  # 检查文件路径是否为字符类型
  if (!is.character(file)) {
    stop("Invalid file path: Non-character supplied.", call. = FALSE)
  }

  # 如果是 URL，尝试读取该 URL
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
    # 如果是本地文件路径，检查文件是否可读
    if (as.numeric(file.access(file, mode = 4)) != 0) {
      stop("Invalid file path: File is not readable.", call. = FALSE)
    }
  }

  ### 读取 bib 文件并返回 tibble 格式
  ################################################
  ## 1. 读取 bib 文件并提取字段
  ################################################
  bib <- readLines(file, encoding = encoding)
  bib <- str_squish(bib) # 去除每行多余空格

  # 删除 JabRef 特定的注释
  temp <- grep("^@Comment\\{jabref-meta: databaseType:bibtex;\\}$", bib, perl = TRUE)
  if (length(temp) != 0) {
    warning('This file is exported from "JabRef" and we will automatically remove the "JabRef" tag.
            tag: @Comment{jabref-meta: databaseType:bibtex;}')
    bib[temp] <- "" # 将 JabRef 标签内容置空
  }
  rm(temp) # 删除该变量, 释放内存
  ###### 删除注释 ########################################
  # 1. 删除注释行 （不处理文本中的注释, 感觉很少有人这样写）
  bib <- bib[!str_detect(bib, "^%")]
  bib <- bib[bib != ""]
  #########################################################
  ########### 2.  处理 Bib 文件条目 #####################
  #########################################################
  from <- which(str_extract(bib, "[:graph:]") == "@") # 查找每个条目开始的行号
  to <- c(from[-1] - 1, length(bib)) # 查找每个条目结束的行号
  if (length(from) == 0L) {
    stop("There are no available references, please check the bib file.")
  }

  # 提取每个条目
  itemslist <- mapply(function(x, y) {
    return(bib[x:y])
  }, x = from, y = to, SIMPLIFY = FALSE)

  # 删除每个条目中的空行
  itemslist <- map(itemslist, function(item) {
    item <- str_squish(item)
    item <- item[item != ""]
    return(item)
  })
  # 计算每个条目的行数
  itemslist_len <- map_int(itemslist, length)
  # 如果条目行数小于5，则发出警告并删除
  n <- 5
  if (any(itemslist_len < n)) {
    warning(str_glue("Some entries have less than {n} lines, these entries will be removed:"), immediate. = TRUE)
    for (i in itemslist[itemslist_len < n]) {
      # 打印 list
      cat(i, sep = "\n")
    }
    cat("=========================================\n")
  }
  # 删除行数小于5的条目
  itemslist <- itemslist[itemslist_len >= n]

  # 如果存在无效的条目，则发出警告并删除
  checkresult <- map_lgl(itemslist, checkitem_valid)
  if (any(!checkresult)) {
    warning("Some entries are not valid BibTeX entries, these entries will be removed:", immediate. = TRUE)
    cat("=========================================\n")
    for (i in itemslist[!checkresult]) {
      cat(i, sep = "\n")
    }
    cat("=========================================\n")
  }
  # 删除无效的条目
  itemslist <- itemslist[checkresult]


  # 将每个条目的所有字段提取出来
  itemslist_fields <- map(itemslist, extract_fields)
  # 将所有条目的字段合并为一个数据框
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
#' 1. The first line should match the format: @TYPE{CITATION_KEY,
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
      # 统计{ 和} 的数量
      open_braces_count <- str_count(lastline, "\\{") # 统计 { 的数量
      close_braces_count <- str_count(lastline, "\\}") # 统计 } 的数量
      # 判断 } 的数量是否比 { 多 1
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
      # 提示警告信息
      warning("The provided item is not valid. Returning an empty list.")
      # 返回空列表
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
    # 统计{ 和} 的数量
    open_braces_count <- str_count(lastline, "\\{") # 统计 { 的数量
    close_braces_count <- str_count(lastline, "\\}") # 统计 } 的数量
    # 判断 } 的数量是否比 { 多 1
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
