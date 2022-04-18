#' combine journals and output CSV
#'
#' @import data.table
#' @importFrom purrr map "%>%"
#' @importFrom stringr str_trim str_to_lower str_count str_replace_all
#' @importFrom stringi stri_escape_unicode
#' @importFrom usethis use_data
#' @param putjabref  boolvale Whether to output jabref format file
#' @return NULL
#' @keywords  combine journals and output CSV
#' @examples
#' \dontrun{
#'  journalabbr:::combine_journal_lists()
#' }
combine_journal_lists = function(){
  ## CSV file is divided by semicolon
  #dd = list.files("./metadata/journals/",pattern = '.csv$')
  # paste(dd,collapse = '",\n"') %>% cat
  dd = c("journal_abbreviations_webofscience-dots.csv",
         "journal_abbreviations_acs.csv",
         "journal_abbreviations_ams.csv",
         "journal_abbreviations_annee-philologique.csv",
         "journal_abbreviations_dainst.csv",
         "journal_abbreviations_entrez.csv",
         "journal_abbreviations_general.csv",
         "journal_abbreviations_geology_physics_variations.csv",
         "journal_abbreviations_geology_physics.csv",
         #"journal_abbreviations_ieee_strings.csv",
         "journal_abbreviations_ieee.csv",
         "journal_abbreviations_lifescience.csv",
         "journal_abbreviations_mathematics.csv",
         "journal_abbreviations_mechanical.csv",
         "journal_abbreviations_medicus.csv",
         "journal_abbreviations_meteorology.csv",
         "journal_abbreviations_sociology.csv",
         "journal_abbreviations_webofscience.csv",
         'myabbr.csv')
  dd2 = paste0('./metadata/journals/',dd)
  library(data.table)
  dt_list = list()
  k = 1
  for (i in dd2) {
    if (file.exists(i)) {
      dt_list[[k]] = data.table::fread(i,sep = ";", quote = "\"",header = FALSE, fill = TRUE)
      dt_list[[k]][,'originFile':=dd[k]]
      k = k+1
    }
  }

  dt = data.table::rbindlist(dt_list,fill=TRUE) # 合并多个数据
  dt = dt[,list(V1,V2,originFile)]
  library(purrr)
  library(stringr)
  sprintf("合并以后一共有 %d 篇期刊\n", dt[,.N]) %>% cat()
  setnames(dt,c("V1","V2","originFile"),c("journal","journal_abbr","originFile")) # 重命名

  dt[,journal_lower := str_trim(journal_abbr, side = 'both')]
  dt[,journal := str_trim(journal, side = 'both')]

  dt_1 = copy(dt)
  dt_2 = copy(dt)

  dt_1[,journal:=str_replace_all(journal,'(?<= )\\&(?= )','&')]
  dt_1[,journal:=str_replace_all(journal,'(?<= )\\\\&(?= )','&')]
  dt_1[,journal:=str_replace_all(journal,'(?<= )[aA][nN][dD](?= )','&')]

  dt_2[,journal:=str_replace_all(journal,'(?<= )&(?= )','and')]
  dt_2[,journal:=str_replace_all(journal,'(?<= )\\\\&(?= )','and')]
  dt_2[,journal:=str_replace_all(journal,'(?<= )\\&(?= )','and')]


  dt = list(dt_1,dt_2)  %>% rbindlist(., use.names=TRUE, fill=TRUE) %>% unique()
  sprintf("and 与 & 替换后并合并以后一共有 %d 篇期刊\n", dt[,.N]) %>% cat


  ## 删除具有反斜杠的行 以及正斜杠 -- 期刊太特殊了
  dt = dt[!grepl(pattern = '\\\\',journal), ]
  dt = dt[!grepl(pattern = '\\\\',journal_abbr), ]
  dt = dt[!grepl(pattern = '/{1,10}',journal), ] # 正斜杠的行,有些期刊有,但是太少了,不加入
  dt = dt[!grepl(pattern = '/{1,10}',journal_abbr), ]
  ## 删除具有双引号的行 -- 期刊太特殊了
  dt = dt[!grepl(pattern = '"',journal),]
  dt = dt[!grepl(pattern = '"',journal_abbr),]

  ### 删除 原始的 journal字段超过 80 个字符的期刊
  dt[,journal_len := nchar(journal)]
  dt = dt[journal_len<=80,]

  #setorder(dt, journal)# 重新排序--为什么
  ###########################
  ############## 把期刊字段转变为小写
  dt[,journal := str_replace(journal,'[\t ]{2,}'," ")]# 替换多余的空格和\t
  dt[,journal_lower :=str_trim(str_to_lower(journal), side = 'both')]
  dt[,count_dot := str_count(journal_abbr,'\\.')]  ##### 计算缩写字段中带点的个数
  dt[,abbr_len := str_length(journal_abbr)]### 计算缩写字段的长度
  #### 去除重复项, 按照 journal 期刊进行分组,
  #### 并找出各个分组中 count_dot 值最大的行,如有多个最大值,则选择缩写字段最少的那个
  ## 方法一:
  dtf = dt[dt[, .I[order(-count_dot,abbr_len)[1]], by=journal]$V1,]
  #fwrite(dtf_t,file = '../b.csv')
  ## 方法二:
  # dtf = dt[dt[, .I[{
  #   temp = which(count_dot == max(count_dot))
  #   ifelse(length(temp) == 1, temp, which.min(abbr_len))
  # }], by = journal]$V1,]
  sprintf("最后, 整理后期刊具有缩写的一共用%d篇\n", dtf[, .N]) %>% cat
  library(stringi)
  dtf = lapply(dtf,function(x)stri_escape_unicode(x))
  #dtf = dtf[,lapply(.SD, function(x)stringi::stri_escape_unicode(x))]
  abbrTable = as.data.frame(dtf)
  usethis::use_data(abbrTable, compress = "xz", internal = TRUE,overwrite = TRUE,version=3)
  # 这样可以在任何内部函数中使用,并且不需要加载
  # file.copy(from = './R/sysdata.rda', to = './data/abbrTable.rda',overwrite=T)
}
combine_journal_lists()
#fwrite(as.data.table(abbrTable),file = '../b.csv')
