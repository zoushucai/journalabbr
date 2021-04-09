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
  ## csv以分号进行分割
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
    dt_list[[k]] = data.table::fread(i,sep = ";", header = FALSE, fill = TRUE)
    dt_list[[k]][,'originFile':=dd[k]]
    k = k+1
  }

  dt = data.table::rbindlist(dt_list,fill=TRUE) # 合并多个数据
  dt = dt[,list(V1,V2,originFile)]
  sprintf("合并以后一共有 %d 篇期刊\n", dt[,.N]) %>% cat()
  setnames(dt,c("V1","V2","originFile"),c("journal","journal_abbr","originFile")) # 重命名

  dt_1 = copy(dt)
  dt_2 = copy(dt)
  dt_3 = copy(dt)


  dt_1[,journal:=str_replace_all(journal, '(?<= )&(?= )','\\\\&')]# 提取& 符号,且& 前后有空格
  dt_1[,journal:=str_replace_all(journal,'(?<= )and(?= )','\\\\&')]

  dt_2[,journal:=str_replace_all(journal,'(?<= )\\\\&(?= )','&')]
  dt_2[,journal:=str_replace_all(journal,'(?<= )and(?= )','&')]

  dt_3[,journal:=str_replace_all(journal,'(?<= )&(?= )','and')]
  dt_3[,journal:=str_replace_all(journal,'(?<= )\\\\&(?= )','and')]

  dt = list(dt_1,dt_2,dt_3)  %>% rbindlist(., use.names=TRUE, fill=TRUE) %>% unique()
  sprintf("and 与& 替换后并合并以后一共有 %d 篇期刊\n", dt[,.N]) %>% cat

  setorder(dt, journal)# 重新排序
  ###########################
  ############## 把期刊字段转变为小写
  dt[,journal := str_replace(journal,'[\t ]{2,}'," ")]
  dt[,journal_lower :=str_trim(str_to_lower(journal), side = 'both')]
  dt[,count_dot := str_count(journal_abbr,'\\.')]  ##### 计算缩写字段中带点的个数
  dt[,abbr_len := str_length(journal_abbr)]### 计算缩写字段的长度
  #### 去除重复项, 按照journal_lower期刊进行分组,
  #### 并找出各个分组中 count_dot 值最大的行,如有多个最大值,则选择缩写字段最少的那个
  dtf = dt[dt[, .I[{
    temp = which(count_dot== max(count_dot))
    ifelse(length(temp) == 1, temp, which.min(abbr_len))
  }], by = journal_lower]$V1]
  sprintf("最后,整理后期刊具有缩写的一共用%d篇\n", dtf[, .N]) %>% cat

  #dtf = dtf[,lapply(.SD, function(x)stri_escape_unicode(x))]
  abbrTable = as.data.frame(dtf)
  usethis::use_data(abbrTable, internal = TRUE,overwrite = TRUE)
  # 这样可以在任何内部函数中使用,并且不需要加载
  file.copy(from = './R/sysdata.rda', to = './data/abbrTable.rda',overwrite=T)
}
combine_journal_lists()

