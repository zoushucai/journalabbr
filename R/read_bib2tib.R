#' @title Parse a BibTeX file to a \code{tibble}
#' @description The BibTeX file is read, parsed, tidied and written to a \code{tibble}
#' @details For simplicity \code{read_bib2tib()} unifies the reading, parsing and tidying of a BibTeX file while being aware of a standardized output format, different BibTeX styles and missing values in the BibTeX file.
#' @param file character, path or URL to a .bib file.
#' @param isabbr logical,  if \code{TRUE}, If true, two columns \code{journal_abbr} and \code{originFile} will be added. \code{journal_abbr} is the abbreviation for the journal and \code{originFile} is the source of the abbreviated journal
#' @seealso \code{\link{abbrTable}}
#' @return A \code{tibble}.
#'
#' @importFrom stringr str_replace_all str_extract str_trim str_split str_to_lower
#' @importFrom purrr map map2 map_chr
#' @importFrom dplyr left_join '%>%'
#' @importFrom rlang is_empty
#' @importFrom httr GET
#' @author ShuCai Zou
#'
#' @examples
#' # Read from .bib file:
#' require(journalabbr)
#' path = system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
#' bib <- read_bib2tib(path)
#' str(bib)
#' @export

read_bib2tib = function(file,isabbr=FALSE){
  if (!is.character(file)) {
    stop("Invalid file path: Non-character supplied.", call. = FALSE)
  }
  if (grepl("http://|https://|www.", file)) {
    tryCatch(
      { GET(file) },
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
  bib = readLines(file,encoding = "UTF-8")
  bib <- str_replace_all(bib, "[^[:graph:]]", " ")
  # delete jabref style
  temp = grep("^( {0,})@Comment{jabref-meta: databaseType:bibtex;}( {0,})$",bib,perl =T)
  if(!rlang::is_empty(temp)){
    warning('This file is exported from "JabRef" and we will automatically remove the "JabRef" tag.
            tag: @Comment{jabref-meta: databaseType:bibtex;}')
    bib[temp] = ""
  }
  rm(temp)
  ################################################
  ########### 2.  Bib file processing #####################
  ################################################
  ####3.1  提取按照@ 的位置进行提取--形成tibble
  from <- which(str_extract(bib, "[:graph:]") == "@")
  to  <- c(from[-1] - 1, length(bib))
  if (!length(from)) {
    return(NULL)
  }
  itemslist <- mapply(
    function(x, y) return(bib[x:y]),
    x = from,
    y = to - 1,
    SIMPLIFY = FALSE
  )

  item_tib =  tibble::enframe(itemslist,name = "sitenum", value = "value")

  ####3.2 对 形成的tibble 数据库进行处理
  item_tib$rawchar = NA
  item_tib$rawchar  = map(item_tib$value, function(x){
    str_trim(x,side = 'both')
  })# 移除字符串两边的空格

  item_tib$rawchar  = map(item_tib$rawchar, function(x){
    gsub('[ \t]{2,}',' ',x)
  })# 移除中间多余的空格

  len = map(item_tib$rawchar, function(x){
    max(grep('\\}',x))
  })  # 移除最后出现的那个} 以外的所有内容

  dupl = sum(sapply(len, is.infinite))
  if (dupl > 0) {
    message("Some BibTeX entries may have been dropped.
            The results may not be correct.
            Check the. Bib file to make sure that each entry starts with '@' and ends with '}'")
  }

  item_tib$rawchar = map2(item_tib$rawchar, len, function(x,y){
    x[1:y]
  })# 移除最后出现的那个右大括号} 以外的所有内容
  item_tib$rawchar  =  map(item_tib$rawchar, function(x){
    gsub('(^,?)(.*?)(,?$)','\\2',x)  ## 移除两头两尾的逗号
  })

  item_tib$rawchar  = map(item_tib$rawchar, function(x){
    x[which(x !='')]
  })# 移除空行

  # 且移除最后的那个右大括号}
  len = map(item_tib$rawchar, function(x){
    max(grep('\\}',x))  # 移除最后出现的那个} 以外的所有内容
  })
  item_tib$rawchar = map2(item_tib$rawchar, len, function(x,y){
    x[y]=gsub('\\}$','',x[y]);x
  })
  item_tib$rawchar  = map(item_tib$rawchar, function(x){
    x[which(x !='')]
  })# 再次移除空行
  #####################################################
  ##### 4. 对 rawchar 这个list进行处理切割 --形成 两列 一列数字段 一列是值
  ####################################################
  ###  4.1 提取keybib 第一行必须是@***{*** 形式, 提取{以后的所有内容
  item_tib$keybib = NA
  item_tib$keybib = purrr::map_chr(item_tib$rawchar,
                                   function(x) {
                                     temp = str_extract(x[1], "(?<=\\{)[^,]+")
                                     str_trim(temp, side = "both")
                                   }
  )

  # # 检查keybib键 是否有重复(去除NA值以后的keybib)
  temp = NULL
  temp =  unlist(map(item_tib$keybib, function(x)!is.na(x)))
  if(any(duplicated(temp))){
    warning('Duplicate key in uploaded Bib file')
  }
  rm(temp)
  ###  4.2 提取typebib --- 提取条目类型 第一行必须是@***{*** 形式, 提取@***{ 的所有内容,
  item_tib$typebib = NA
  item_tib$typebib = purrr::map_chr(item_tib$rawchar,
                                     function(x) {
                                      temp =  str_extract(x[1], "(?<=@)[^\\{]+")
                                      str_trim(temp, side = "both")
                                     })
  item_tib$typebib =  purrr::map_chr(item_tib$typebib , toupper)

  ### 4.3 对其余字段进行切割---- 以 第一次出现等号(=)的地方为切割线,每个list都是两列的data.frame
  items = map(item_tib$rawchar,function(x){
    str_split(x[-1],'[ ]*=[ ]*', n = 2, simplify = T)
  })  # 以第一次出现 = 的位置进行切割
  #### 4.4 对每个list中的第一列进行处理 --- 转变为大写
  items = map(items,function(x){
    x[,1] = toupper(x[,1]);x
  })
  #### 4.5 对每个list中的第二列的大括号和双引号进行成对删除 -- 防止多层嵌套--循环5次
  items = map(items,function(x){
    for(i in seq_len(1)){
      x[,2] = gsub('^(\\")(.*?)(\\")$',"\\2",x[,2])
    }# 处理 双引号括起来的字符--一般情况下不会嵌套双引号
    x
  })
  items = map(items,function(x){
    for(i in seq_len(5)){
      x[,2] = gsub('^(\\{)(.*?)(\\})$',"\\2",x[,2])
    }
    x
  })

  ## 对第一列的分类进行整合 --- 暂时不考虑排序
  categories_field = unlist(map(items,function(x)x[,1]))

  for (ii in categories_field) {
    item_tib[[ii]] = NA
    item_tib[[ii]] = map_chr(items,function(x){
      temp = which(x[,1] == ii) #grep(ii,x[,1] )
      ifelse(!is_empty(temp),x[temp[1],2], NA) # 如果标题有相同的,则取第一个即可
    })
  }

  if(isabbr){
    item_tib[['journal_lower']] = NA
    item_tib$journal_lower = purrr::map_chr(item_tib$JOURNAL,function(x){
        temp = str_trim(str_to_lower(x),side = 'both')
        gsub("[\t ]{2,}"," ", temp)
    })

    abbrTableSub = tibble::as_tibble(abbrTable)
    abbrTableSub = abbrTableSub[,c("journal_lower",'journal_abbr','originFile')]
    tib = dplyr::left_join(item_tib, abbrTableSub, by = "journal_lower")
    tib[["journal_lower"]] = NULL
    return(tib)
  }
  return(item_tib)
}
