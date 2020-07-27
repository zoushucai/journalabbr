#' @title Parse a BibTeX file to a \code{tibble}
#' @description The BibTeX file is read, parsed, tidied and written to a \code{tibble}.
#' @details Read, parse and collate bibtex file to form a Tibble. Different BIB may produce different tibble columns.
#' @param file character, path or URL to a bib file.
#' @seealso \code{\link{abbrTable}}.
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


read_bib2tib = function(file){
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

  item_tib$rawchar = NA
  item_tib$rawchar  = map(item_tib$value, function(x){
    str_trim(x,side = 'both')
  })

  item_tib$rawchar  = map(item_tib$rawchar, function(x){
    gsub('[ \t]{2,}',' ',x)
  })

  len = map(item_tib$rawchar, function(x){
    max(grep('\\}',x))
  })

  dupl = sum(sapply(len, is.infinite))
  if (dupl > 0) {
    message("Some BibTeX entries may have been dropped.
            The results may not be correct.
            Check the. Bib file to make sure that each entry starts with '@' and ends with '}'")
  }

  item_tib$rawchar = map2(item_tib$rawchar, len, function(x,y){
    x[1:y]
  })
  item_tib$rawchar  =  map(item_tib$rawchar, function(x){
    gsub('(^,?)(.*?)(,?$)','\\2',x)
  })

  item_tib$rawchar  = map(item_tib$rawchar, function(x){
    x[which(x !='')]
  })

  len = map(item_tib$rawchar, function(x){
    max(grep('\\}',x))
  })
  item_tib$rawchar = map2(item_tib$rawchar, len, function(x,y){
    x[y]=gsub('\\}$','',x[y]);x
  })
  item_tib$rawchar  = map(item_tib$rawchar, function(x){
    x[which(x !='')]
  })
  #####################################################
  ##### 4. for 'rawchar' list Processing cuts by '=' - form two columns
  ####################################################
  ###  4.1 extract keybib
  item_tib$keybib = NA
  item_tib$keybib = purrr::map_chr(item_tib$rawchar,
                                   function(x) {
                                     temp = str_extract(x[1], "(?<=\\{)[^,]+")
                                     str_trim(temp, side = "both")
                                   }
  )

  # Check if the keybib key is repeated after removing Na
  temp = NULL
  temp =  unlist(map(item_tib$keybib, function(x)!is.na(x)))
  temp = item_tib$keybib[temp]
  if(any(duplicated(temp))){
    s =     paste0(temp[duplicated(temp)],collapse = '\n')
    s = paste0('Duplicate key in uploaded Bib file\n',s)
    warning(s)
  }
  rm(temp)
  ###  4.2  extract typebib
  item_tib$typebib = NA
  item_tib$typebib = purrr::map_chr(item_tib$rawchar,
                                     function(x) {
                                      temp =  str_extract(x[1], "(?<=@)[^\\{]+")
                                      str_trim(temp, side = "both")
                                     })
  item_tib$typebib =  purrr::map_chr(item_tib$typebib , toupper)

  ### 4.3 The rest of the fields are cut with the first occurrence of '='
  items = map(item_tib$rawchar,function(x){
    str_split(x[-1],'[ ]*=[ ]*', n = 2, simplify = T)
  })
  items = map(items,function(x){
    x[,1] = toupper(x[,1]);x
  })
  items = map(items,function(x){
    for(i in seq_len(1)){
      x[,2] = gsub('^(\\")(.*?)(\\")$',"\\2",x[,2])
    }
    x
  })
  items = map(items,function(x){
    for(i in seq_len(5)){
      x[,2] = gsub('^(\\{)(.*?)(\\})$',"\\2",x[,2])
    }
    x
  })
  categories_field = unlist(map(items,function(x)x[,1]))

  for (ii in categories_field) {
    item_tib[[ii]] = NA
    item_tib[[ii]] = map_chr(items,function(x){
      temp = which(x[,1] == ii)
      ifelse(!is_empty(temp),x[temp[1],2], NA)
    })
  }
  return(item_tib)
}
