#' @title The BibTeX \code{tibble} is written to a .bib file
#' @description Input Bib file with complete journal, output Bib file after abbreviation of journa, and return to the abbreviation table of journal
#' @import data.table
#' @importFrom rlang is_empty
#' @importFrom stringr str_trim str_to_lower str_replace_all str_extract str_replace
#' @importFrom bib2df bib2df
#' @importFrom stats complete.cases
#' @importFrom httr GET
#' @param file character, path or URL to a .bib file.
#' @param separate_names logical, should authors' and editors' names be separated into first and given name?
#' @param outfile_abbr character, file path to write the .bib file. An empty character string writes to \code{stdout} (default).
#' @return {
#' Return to a list and  output the BibTex file after the abbreviation of Journal.
#' List consists of three data structures as follows:
#' }
#' \describe{
#' \item{abbrtable}{A data.table, the abbreviation table of journal}
#' \item{noabbr}{A data.table, No abbreviated journals found}
#' \item{noindex}{A vector, Index corresponding to noabbr}
#' }
#'
#' @keywords List of journal abbreviations for input BibTex file
#' @export
#' @examples
#' \dontrun{
#' library(journalabbr)
#' path = system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
#' abbr2bib(file = path,outfile_abbr= "abbr.bib")
#' }
#
abbr2bib <- function(file, outfile_abbr= "abbr.bib", separate_names = FALSE) {
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
  ############################################################
  ########### 读取文件----与内置数据库建立对应关系
  bib_file = bib2df(file)
  temp = data.table('bib_journal' = str_trim(bib_file$JOURNAL,side = 'both'),
                    "bib_journal_lower"= str_to_lower(str_trim(bib_file$JOURNAL,side = 'both'))
                    )
  #加载内部数据
  dt_lower_unique = as.data.table(journal_abbreviations_lower_all)
  dt_lower_unique = dt_lower_unique[, c("journal_lower","journal_abbr"), with=F]
  #setnames(dt_lower_unique,c("V1","V2"),c("journal_lower","journal_abbr"))
  bib_journal_abbr_table = merge(temp, dt_lower_unique, by.x = "bib_journal_lower",by.y =  'journal_lower', all.x = T,sort = F)

  bib_journal_abbr_table = bib_journal_abbr_table[, c("bib_journal","journal_abbr"), with =F]  # 提取期刊以及缩写期刊
  ### bib_journal_abbr_table 为期刊缩写对照表
  no_abbr = which(is.na(bib_journal_abbr_table$journal_abbr) & (!is.na(bib_journal_abbr_table$bib_journal)) ) # 没有缩写的期刊
  noabbr = bib_journal_abbr_table[no_abbr,]
  bib_journal_abbr_table[no_abbr,("journal_abbr"):= lapply(.SD,function(x)x),.SDcol = "bib_journal"]# 进行替换
  ### 最后返回 bib_journal_abbr_table 期刊对应的缩写表
  #########################################################
  #### 直接在原bibtex文件进行期刊替换
  abbr_table = bib_journal_abbr_table[complete.cases(bib_journal_abbr_table),]# 删除带有NA的行
  journal_number = abbr_table[,.N]

  bib <- readLines(file)# 再次读取输入的文件
  bib <- str_replace_all(bib, "[^[:graph:]]", " ")#匹配任何一个可打印字符,但不包括空格
  ind = grep(pattern = "journal",bib, ignore.case = T) #提取含有journal字符的行
  if (length(ind) != journal_number){
    stop('Note that the number of BibTex entries is not equal to the number of journals extracted')
  }
  if(rlang::is_empty(ind)){
    stop("The number of journals extracted is 0")
  }

  for (k  in ind) {
    for (i in seq_len(journal_number)) {
      ttt = str_extract(bib[k],abbr_table$bib_journal[i])
      if(!is.na(ttt)){
        bib[k] = str_replace(bib[k],abbr_table$bib_journal[i],abbr_table$journal_abbr[i])
        break;
      }
    }
  }
  writeLines(bib,con = outfile_abbr)
  return(list('abbrtable'=bib_journal_abbr_table, 'noabbr'= noabbr,'noindex' = no_abbr))# 还回期刊缩写对照表
}


