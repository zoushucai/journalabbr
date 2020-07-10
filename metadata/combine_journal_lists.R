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
#'  journalabbr:::combine_journal_lists(putjabref = T)
#' }
combine_journal_lists = function(putjabref = FALSE){
  ## csv以分号进行分割
  dd = c('journal_abbreviations_acs.csv',
         'journal_abbreviations_ams.csv',
         'journal_abbreviations_general.csv',
         'journal_abbreviations_geology_physics_variations.csv',
         'journal_abbreviations_geology_physics.csv',
         'journal_abbreviations_ieee.csv',
         'journal_abbreviations_lifescience.csv',
         'journal_abbreviations_mathematics.csv',
         'journal_abbreviations_mechanical.csv',
         'journal_abbreviations_meteorology.csv',
         'journal_abbreviations_sociology.csv',
         'journal_abbreviations_webofscience-dots.csv')
  dd = paste0('./metadata/journals/',dd)
  dt_list = list()
  k = 1
  for (i in dd) {
    dt_list[[k]] = fread(i,sep = ";", header = FALSE, fill = TRUE)
    k = k+1
  }

  dt_list = purrr::map(dt_list,function(x){x[, c(1:2)]})# 提取每个list的前两列
  dt = rbindlist(dt_list) %>% unique() # 合并多个数据
  sprintf("合并以后一共有 %d 篇期刊\n", dt[,.N]) %>% cat()

  setnames(dt,c("V1","V2"),c("journal","journal_abbr")) # 重命名
  #dt = dt[journal !=journal_abbr,]
  sprintf("期刊名长度大于4的期刊数目为: %d\n", dt[nchar(journal)>4,.N]) %>% cat()
  sprintf("故,期刊名长度小于等于4的期刊数目为: %d\n", dt[nchar(journal)<=4,.N]) %>% cat()


  dt = dt[nchar(journal)>4,] %>% unique()
  dt_1 = copy(dt)
  dt_2 = copy(dt)
  dt_3 = copy(dt)


  dt_1[,journal:=str_replace_all(journal, '(?<= )&(?= )','\\\\&')]# 提取& 符号,且& 前后有空格
  dt_1[,journal:=str_replace_all(journal,'(?<= )and(?= )','\\\\&')]
  #dt_1[,journal:=str_replace_all(journal,'\\band\\b','\\\\&')]# 提取and单词,且单词 前后有空格


  dt_2[,journal:=str_replace_all(journal,'(?<= )\\\\&(?= )','&')]
  dt_2[,journal:=str_replace_all(journal,'(?<= )and(?= )','&')]

  dt_3[,journal:=str_replace_all(journal,'(?<= )&(?= )','and')]
  dt_3[,journal:=str_replace_all(journal,'(?<= )\\\\&(?= )','and')]

  dt_finally = list(dt_1,dt_2,dt_3)  %>% rbindlist(., use.names=TRUE, fill=TRUE) %>% unique()
  #dt_finally = dt_finally[journal !=journal_abbr, ]
  sprintf("and 与& 替换后并合并以后一共有 %d 篇期刊\n", dt_finally[,.N]) %>% cat
  sprintf("and 与& 替换后,期刊名长度小于等于4的期刊数目为: %d\n", dt_finally[nchar(journal)<=4, .N]) %>% cat

  setorder(dt_finally, journal)# 重新排序
  ###########################
  ############## 缩写期刊,以便于找到合适的缩写.
  dt_lower = copy(dt_finally)
  setnames(dt_lower,c("journal","journal_abbr"),c('journal','journal_abbr'))
  dt_lower[,journal_lower :=str_trim(str_to_lower(journal),side = 'both')] # 变量名为journal_lower 代表期刊的小写
  ##### 找出期刊中缩写项带点的个数,----  变量名 count_dot 用计数,期刊缩写带点的个数
  dt_lower[,count_dot := str_count(journal_abbr,'\\.')]
  #### 按照journal_lower期刊进行分组,并找出各个分组中 count_dot 值最大的行
  dt_lower_unique = dt_lower[dt_lower[, .I[which.max(count_dot)], by = journal_lower]$V1]
  #方法二: 慢:---dt_lower[, .SD[which.max(count_dot)], by = lower_journal] #168512
  sprintf("期刊变成小写,一共有%d 个 \n",dt_lower[,.N]) %>% cat
  sprintf("期刊变成小写,选择'.'个数多的,一共有%d 个 \n",dt_lower_unique[,.N]) %>% cat
  sprintf("期刊变成小写,重复项一共有%d 个\n",dt_lower[,.N] - dt_lower_unique[,.N]) %>% cat

  #dt_lower_unique = dt_lower_unique[,.(journal,journal_abbr,journal_lower)] #提取新的列
  #dt_lower_unique[,c("count_dot", "journal_lower"):=NULL] # 删除变量,提取新列
  dt_lower_unique %>%
    fwrite(.,sep = ";",col.names = T,file = './metadata/journal_abbreviations_lower_all.csv')
  if(putjabref){
    dt_lower_unique[,.(journal_lower,journal_abbr)] %>%
      fwrite(.,sep = ";",col.names = F,file = './metadata/journal_abbreviations_lower_jabref.csv')
  }
  journal_abbreviations_lower_all = dt_lower_unique
  #journal_abbreviations_lower_all = journal_abbreviations_lower_all[,lapply(.SD,stri_escape_unicode)]         # 对weather20141001$prov列转码
  #save(journal_abbreviations_lower_all,file = "./data/journal_abbreviations_lower_all.rda",compress =TRUE)
  usethis::use_data(journal_abbreviations_lower_all, internal = TRUE,overwrite = TRUE)
  # 这样可以在任何内部函数中使用,并且不需要加载
  }
combine_journal_lists()
