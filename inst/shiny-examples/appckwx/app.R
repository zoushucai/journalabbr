library(shiny)
library(stringr)
library(stringi)
library(data.table)
require(rclipboard)
library(knitr)
library(rmarkdown)
library(purrr)
library(dplyr)
library(tinytex)
library(DT)
library(journalabbr)
#library(lubridate)
#options(shiny.fullstacktrace = TRUE)
options(shiny.sanitize.errors = FALSE)
rm(list = ls())

clear_file = function(mypattern = '(.*\\.R$)|(.*\\.Rproj$)' ){
  tryCatch(
    {
      now_dir= list.files()
      old_dir = dir(pattern= mypattern)

      delete_dir = setdiff(now_dir,old_dir)
      for(i in delete_dir){
        if(!file_test("-d", i)){#不是目录则删除
          file.remove(i)
        }
      }
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      print("出错啦")
      clear_file()
      stop(safeError(e))
    }
  )
}

is_empty = rlang::is_empty

tidy_tex = function(tex){
  ## 提取页面的显示信息--有可能是数字,有可能是作者,有可能是作者-年
  from1 = grep('\\\\begin\\{document\\}',tex)
  from2 = grep('\\\\maketitle',tex)
  if(is_empty(from1) && is_empty(from2)){
    clear_file()
    stop('没找到开始的地方')
  }else if(!is_empty(from1) && is_empty(from2)){
    ind_from = from1
  }else if(is_empty(from1) && !is_empty(from2)){
    ind_from = from2
  }else if (from1 <= from2){
    ind_from = from2
  }else{
    clear_file()
    stop('索引值出错,没找到开始的地方')
  }
  to1 = grep('\\\\hypertarget\\{refs\\}',tex,ignore.case = T)
  to2 = grep('\\\\begin\\{cslreferences\\}',tex,ignore.case = T)
  if(is_empty(to1) && is_empty(to2)){
    clear_file()
    stop('没找到结束的地方')
  }else if(!is_empty(to1) && is_empty(to2)){
    ind_to = to1
  }else if(is_empty(to1) && !is_empty(to2)){
    ind_to = to2
  }else if (to1 <= to2){
    ind_to = to1
  }else{
    clear_file()
    stop('索引值出错,没找到结束的地方')
  }

  tex_show = tex[(ind_from + 1):(ind_to - 1)]# 提权显示页面

  for (i in seq_len(100)) {
    value = which(tex_show[1] =="")
    if(!is_empty(value) && value ==1){
      tex_show = tex_show[2:length(tex_show)]
    }else{
      break
    }
  }
  for (i in seq_len(100)) {
    max_len = length(tex_show)
    value = which(tex_show[max_len] =="")
    if(!is_empty(value) && value ==1){
      tex_show = tex_show[1:(max_len-1)]
    }else{
      break
    }
  }
  ######### 把tex_show 进行合并,因为有些是两行表示一个字段
  tex_show[tex_show == ""] = '\n@@\n\n\n'
  tex_show = paste(tex_show,collapse = ' ',sep = "")
  tex_show = unlist(str_split(tex_show,'\n@@\n\n\n'))
  tex_show = str_trim(tex_show,side = "both")
  return(tex_show)
}
cls_type = function(tex_show){
  ######## 确定这篇文章是作者引用还是数字引用
  ## 利用 \protect\hyperlink{ref-howell2012statistical}{***} 这个*** 是数字还是字母混合
  ## 数字范围1  数字字母混合范围0
  extra_text = str_extract_all(tex_show,'(\\\\protect\\\\hyperlink\\{ref-[0-9A-Za-z\\-]*?)(\\}\\{)(.*?)(\\})')
  extra_text_1 = str_replace_all(extra_text,'(\\\\protect\\\\hyperlink\\{ref-[0-9A-Za-z\\-]*?)(\\}\\{)(.*?)(\\})$','\\3')
  extra_text_1 = str_replace_all(extra_text_1,'^\\{\\[\\}(.*?)\\{\\]\\}$','\\1')
  # 删除取错的字符如{ [
  extra_text_1 = str_replace_all(extra_text_1,'[\\{\\}\\[\\]]',"")
  ## 检测是否为空字符串
  if( all(nchar(extra_text_1) == 0) ){
    is_type = 1;  # 如果为空字符串,则表明为数字标签
    return(is_type)
  }

  ## 利用正则表达式 -- 检测提取到的是否存在非数字
  if( all(grepl('[a-zA-Z]+?',extra_text_1)) ){
    # 表明是非数字标签
    is_type = 0
  }else{
    temp = as.numeric(extra_text_1)
    if(any(!is.na(temp))){
      # 表明有数字转换成功--说明是数字标签
      is_type = 1
    }else{
      # 表明是非数字标签
      is_type = 0
    }
  }
  return(is_type)
}

style_fun = function(texfile){
  #texfile='/Users/zsc/Desktop/未命名文件夹/我的论文终稿/shiny测试/stylefile/ams-review.tex'
  tex =  readLines(texfile, encoding = "UTF-8")
  style_raw = paste0(tex ,collapse = "\n")     # 直接返回rmd生成的tex文件样式

  #clear_file()
  tex_show = tidy_tex(tex)
  #####  合并完成了######
  is_type = cls_type(tex_show)# 1表示数字标签, 0 表示作者标签
  if(is_type){
    # 数字标签
    ########## 从tex_show 找那个提取 bibkey
    extra_text = str_extract_all(tex_show,'(\\\\protect\\\\hyperlink\\{ref-[0-9A-Za-z\\-]*?)(\\}\\{)(.*?)(\\})')
    extra_text_1 = str_replace_all(extra_text,'(\\\\protect\\\\hyperlink\\{ref-[0-9A-Za-z\\-]*?)(\\}\\{)(.*?)(\\}$)','\\3')
    extra_text_1 = str_replace_all(extra_text_1,'^\\{\\[\\}(.*?)\\{\\]\\}$','\\1')
    # 删除取错的字符如{ [
    extra_text_1 = str_replace_all(extra_text_1,'[\\{\\}\\[\\]]',"")
    word_show = extra_text_1

    key_show = str_replace_all(extra_text,'\\\\protect\\\\hyperlink\\{ref-([0-9A-Za-z\\-]*?)\\}\\{(.*?)(\\}$)','\\1')

    show_df = data.frame('key' = key_show,'word'=word_show)

  }else{
    # 表明是非数字标签--- 即 作者标签
    ########## 从tex_show 找那个提取 bibkey
    extra_text = str_extract_all(tex_show,'(\\\\protect\\\\hyperlink\\{ref-[0-9A-Za-z\\-]*?)(\\}\\{)(.*?)(\\})')
    extra_text_1 = str_replace_all(extra_text,'(\\\\protect\\\\hyperlink\\{ref-[0-9A-Za-z\\-]*?)(\\}\\{)(.*?)(\\}$)','\\3')
    extra_text_1 = str_replace_all(extra_text_1,'^\\{\\[\\}(.*?)\\{\\]\\}$','\\1')
    word_show = extra_text_1
    key_show = str_replace_all(extra_text,'\\\\protect\\\\hyperlink\\{ref-([0-9A-Za-z\\-]*?)\\}\\{(.*?)(\\}$)','\\1')
    show_df = data.frame('key' = key_show,'word'=word_show)
  }

  # 参考文献部分
  ind_end  = grep("\\\\end\\{cslreferences\\}",tex,ignore.case = T)
  to2 = grep('\\\\begin\\{cslreferences\\}',tex,ignore.case = T)
  if(is_empty(to2)){
    clear_file()
    stop("参考文献没有开始标记")
  }
  if(is_empty(ind_end)){
    clear_file()
    stop('参考文献没有结束标记')
  }

  tex_sub = tex[(to2 + 1):(ind_end - 1)] # 提取tex 的一个子页面
  ### 同样,根据数字或作者类型 提取 参考文献 部分
  ## 进行分类处理
  if(is_type){
    # 如果是数字格式
    ### 新版本中还要提取 CSLRightInline  CSLLeftMargin  (rmarkdown 2.6,knitr 1.30,pandoc_version 2.11.2)

    # 删除 \\CSLLeftMargin{{[}44{]} } 行
    ind = grepl('^\\\\CSLLeftMargin',tex_sub,perl = T,ignore.case = T)
    tex_sub_new = tex_sub[!ind]
    ## 把字符 CSLRightInline 替换为空
    tex_sub_new = str_replace(tex_sub_new,"^\\\\CSLRightInline\\{",'')
    tex_sub_new = str_replace(tex_sub_new,"\\}$",'')

    ##### 6.1 把tex_sub中的 '\leavevmode\hypertarget{ref-****}{}%'  字段变成'\bibitem{***}' 字段
    key_index = grep('(?<=\\\\leavevmode\\\\hypertarget\\{ref-)(.*?)(?=\\})',tex_sub,perl = TRUE)
    tex_sub_new = str_replace(tex_sub_new,'(^\\\\leavevmode\\\\hypertarget\\{ref-)(.*?)(\\})(\\{\\}%$)',replacement= "\\\\bibitem{\\2} ")

    ## 删除 {[}M{]}  这样的格式, 保留中间的字母
    tex_sub_new = str_replace_all(tex_sub_new,"(^\\{\\[\\}[0-9]{0,3}\\{\\]\\})( ){0,}",'') %>% str_trim(.,side = "both") # 特殊情况处理
    tex_sub_new = str_replace_all(tex_sub_new,"\\{\\[\\}(.*?)\\{\\]\\}",'[\\1]') %>% str_trim(.,side = "both") # 特殊情况处理
    #### 样式1 ----  每一个参考文献在不同行
    style_1 = c('\\section*{References}', '\\begin{thebibliography}{99}',tex_sub_new,'\\end{thebibliography}')
    style_1 = paste(style_1,collapse = "\n") #

    #### 样式2 ---- 每一个参考文献在同一行
    tex_sub_new[tex_sub_new == ""] = "\n\n"
    style_2 = c('\\section*{References}\n\n', '\\begin{thebibliography}{99}\n\n',tex_sub_new,'\n\n\\end{thebibliography}')
    style_2 = paste(style_2, collapse = " ")
    style_2 = str_replace_all(style_2, pattern = " {2,}",replacement = " ") # 处理多余的空格 或 [:blank:]
    style_2 =  str_replace_all(style_2, pattern = "\n\n ",replacement = "\n\n") # 处理换行后的空格
    return(list(style_1,style_2,style_raw,is_type))
  }else{
    # 如果是作者格式
    #### 6, 首先处理tex_sub的参考文献字段,使其变为bibitem样式
    ##### 6.1 把tex_sub中的 '\leavevmode\hypertarget{ref-****}{}%'  字段变成'\bibitem{***}' 字段
    key_index = grep('(?<=\\\\leavevmode\\\\hypertarget\\{ref-)(.*?)(?=\\})',tex_sub,perl = TRUE)
    tex_sub_new = str_replace(tex_sub,'(^\\\\leavevmode\\\\hypertarget\\{ref-)(.*?)(\\})(\\{\\}%$)',replacement= "\\2")
    ##### 6.2 如果tex_sub_new 存在这种样式的 key{[}***{]} 给替换为空
    tex_sub_new = str_replace_all(tex_sub_new,"(^\\{\\[\\}[0-9]{0,3}\\{\\]\\})( ){0,}",'') %>% str_trim(.,side = "both") # 特殊情况处理
    tex_sub_new = str_replace_all(tex_sub_new,"\\{\\[\\}(.*?)\\{\\]\\}",'[\\1]') %>% str_trim(.,side = "both") # 特殊情况处理
    #### 6.3 把标签key用上面显示的文本进行替换, 没找到则用空进行替换
    for (i in key_index) {
      value_temp = show_df[show_df[['key']] == tex_sub_new[i], 2] #赋值
      if ( !is_empty(value_temp) ) {
        tex_sub_new[i] = paste0('\\bibitem[',value_temp,']{',tex_sub_new[i],'}')
      }else{
        tex_sub_new[i] = paste0('\\bibitem[]{',tex_sub_new[i],'}')
      }
    }

    ### 样式0 ---- 返回rmd处理的最原始的引用样式
    # style_raw = paste0(tex[to2:ind_end] ,collapse = "\n")

    #### 样式1 ----  每一个参考文献在不同行
    style_1 = c('\\section*{References}', '\\begin{thebibliography}{99}',tex_sub_new,'\\end{thebibliography}')
    style_1 = paste(style_1,collapse = "\n") #
    #### 样式2 ---- 每一个参考文献在同一行
    tex_sub_new[tex_sub_new == ""] = "\n\n"
    style_2 = c('\\section*{References}\n\n', '\\begin{thebibliography}{99}\n\n',tex_sub_new,'\n\n\\end{thebibliography}')
    style_2 = paste(style_2, collapse = " ")
    style_2 = str_replace_all(style_2, pattern = " {2,}",replacement = " ") # 处理多余的空格 或 [:blank:]
    style_2 =  str_replace_all(style_2, pattern = "\n\n ",replacement = "\n\n") # 处理换行后的空格
    return(list(style_1,style_2,style_raw,is_type))
  }
}

extract_key = function(texfile){
  #### 1.开始读取原始的tex文件,并提取key---- 准备形成 rmd 的后半部分
  ###############################################
  document <- readLines(texfile,encoding = "UTF-8")
  document = str_replace_all(document,'^[[:blank:]]*?%.*','')# 删除以% 开头的所有内容
  document = str_replace_all(document,'([^\\\\])(%.*)','\\1') # 删除前面不是以\\开始的% 以后的所有内容
  document = document[which(document != '')] %>% paste(collapse = ' ')
  # 提取key
  tex_key = unlist(str_extract_all(document,'(?<=\\\\cite[pt]?\\{).*?(?=\\})'))
  # 如果存在逗号分隔的需要考虑 并 删除两边的空格,后 删除重复标签
  tex_key = unlist(str_split(tex_key,','))  %>% str_trim(., side = "both") %>% unique()
  return(tex_key)
}

reorder_bib_fun = function(filepath){

  #tex = readLines("/Users/zsc/Desktop/未命名文件夹/我的论文终稿/参考文献排序/MeasuringTrans02的副本.tex")
  tex = readLines(filepath,encoding = "UTF-8")
  ##1 删除注释


  # 首先替换以%开头的所有内容
  tex = str_replace(tex,pattern = '^%.*','')
  # 在替换 % 后面的内容, \% 后面的内容不做替换
  tex = str_replace(tex,pattern = '(?<=[^\\\\])%.*','')
  ## 是否要删除空白呢? 不需要

  #####tex文档的主要结构...
  # \begin{document}
  # ....
  # \begin{thebibliography}
  # ....
  # \end{thebibliography}
  # \end{document}

  ## 找 \begin{thebibliography} .... \end{thebibliography}
  # 找 文章的开头
  from1 = grep('\\\\begin\\{document\\}',tex)[1]
  to1 = grep('\\\\end\\{document\\}',tex)[1]
  from2 =  grep('\\\\begin\\{thebibliography\\}',tex)[1]
  to2 = grep('\\\\end\\{thebibliography\\}',tex)[1]
  if(is.na(from1)){
    from1 = 1
  }
  if(is.na(to1)){
    to1 = length(tex)
  }

  if(is.na(from2)){
    duoyu_diff ='没发现 thebibliography 环境'
    queshao_diff = he = bib_sorted = duoyu_diff
    #stop('没发现 thebibliography 环境')
    return(list(duoyu_diff,queshao_diff, bib_sorted,he))

  }
  if(is.na(to2)){
    duoyu_diff ='没发现 thebibliography 环境'
    queshao_diff = he = bib_sorted = duoyu_diff
    #stop('没发现 thebibliography 环境')
    return(list(duoyu_diff,queshao_diff, bib_sorted,he))
  }

  ## 提取正文部分
  tex_body = tex[(from1+1):(from2-1)] %>% paste(.,collapse = "") # 删除document 以前的所有内容
  ### 从正文中提取 cite
  cite1 = str_extract_all(tex_body,pattern = '\\\\((up)|())cite(([pts])|())\\{[a-zA-Z0-9, -].*?\\}') %>% unlist()
  #### 从cite 中提取 label
  cite_label = str_extract_all(cite1,pattern = '(?<=\\{)[a-zA-Z0-9, -].*?(?=\\})') %>% unlist()
  #### 以逗号进行分割
  cite_label  = str_split(cite_label,pattern = ',') %>% unlist() %>% str_trim(.,side = "both")
  cite_label_fin = unique(cite_label) # 得到文中的自然引用顺序

  ## 提取thebibliography环境
  tex_bib = tex[(from2+1):(to2-1)] # 删除document 以前的所有内容
  #tex_bib = tex_bib %>% paste(.,collapse = '\n')
  #### 查找到 \\bibitem 字样的位置,进行合并
  index = grep(pattern='\\\\bibitem',tex_bib)
  m = length(index)
  index2 = c(index[2:m],length(tex_bib))
  if(length(index) != length(index2)){
    clear_file()
    stop('cite长度不等')
  }
  if(length(index)==0){
    clear_file()
    stop('cite长度居然为0,直接退出')
  }
  tex_bib_he = mapply(function(x,y){paste(tex_bib[x:(y-1)],collapse = '')}, index,index2)

  ##### 提取\bibitem{***},变形 \bibitem[xxx]{***},\bibitem{XXX}{***}, \bibitem{}{***}
  temp = str_extract(tex_bib_he,'\\\\bibitem(\\[.*?\\]|\\{.*?\\}|())\\{[a-zA-Z0-9, -].*?\\}')
  bib_lable = str_extract(temp,'(?<=\\{)[^\\{]*?(?=\\}$)')
  cite_df = data.table('clab'=cite_label_fin)
  bib_df = data.table('blab'=bib_lable,'rawbib'=tex_bib_he)

  ## 两个数据库的差
  duoyu_diff = setdiff(unique(bib_df$blab),unique(cite_df$clab))
  queshao_diff = setdiff(unique(cite_df$clab),unique(bib_df$blab))

  if(length(duoyu_diff)<=0){
    duoyu_diff = '没有多余的key'
  }else{
    duoyu_diff = paste(duoyu_diff,collapse = '\n')
  }
  if(length(queshao_diff)<=0){
    queshao_diff = '无'
  }else{
    queshao_diff = paste(queshao_diff,collapse = '\n')
  }
  he = merge(cite_df,bib_df,all.x=T,by.x = 'clab',by.y = 'blab')
  ## 排序后的结果
  bib_sorted = paste(he$rawbib,collapse = '\n\n')
  ## 重要的三个变量
  # duoyu_diff queshao_diff bib_sorted
  # write(bib_sorted,'/Users/zsc/Desktop/未命名文件夹/我的论文终稿/参考文献排序/a.tex')
  return(list(duoyu_diff,queshao_diff, bib_sorted,he))
}

clear_file()
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel( title = h2("自用参考文献样式调整", align = "left"), windowTitle = '自用参考文献样式调整' ),
  rclipboardSetup(), # 剪切板设置,必须在开头声明,后面才能用,这是一段js的调用
  tabsetPanel(
    tabPanel("Input",
             wellPanel(
               fileInput("file1_tex", "Choose tex File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.tex')),
               fileInput("file2_csl", "Choose csl File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.csl')),
               fileInput("file3_bib", "Choose bib File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.bbl')),

               fluidRow(
                 column(4,
                        selectInput("bibabbr", "是否只对期刊文献进行缩写:",
                                    choices = c(
                                      'yes' = TRUE,
                                      'no' = FALSE),
                                    selected = TRUE)
                 ),
                 column(4,
                        selectInput('bibformat', '是否美化bib文件(否,则多个作者处理无效)',
                                    choices = c('no' = FALSE,
                                                'yes' = TRUE),
                                    selected = FALSE)
                 ),
                 column(4,
                        selectInput("connectauthor", "多个作者处理",
                                    choices = c('nothing' = "nothing",
                                                'and' = 'and',
                                                '\\\\&' = '\\\\&',
                                                '&' = '&' ),
                                    selected = "nothing")
                 )
               ),


               actionButton("goButton", "Submit")
             )),
    tabPanel("Warning",
             helpText("1, 上传文件时采用 UTF-8 编码.", br(),
                      "2, 上传文件到生成参考文献样式,需要花一定时间,还请耐心等待.",br(),
                      "3, 最终输出的参考文献结果还需要仔细检查,符合期刊要求.")
             #,HTML("<p><font color='red'>\n警告显示如下:\n</font></p>")  # 方法一:  直接使用HTML标签
             ,p("警告显示如下:", style="font-weight:bold;color:red;")
             ,verbatimTextOutput("out_warning")),
    tabPanel("Items Info",
             helpText("注意:最终的结果可能还需要细调"),
             dataTableOutput(outputId="jouranl_abbr")
    ),
    tabPanel("Abbr source",
             helpText("注意:最终的结果可能还需要细调", br(),
                      "如果是非数字标签,可能还需要引用 natbib 宏包,并且使用 \\citep{***} 来引用"),
             dataTableOutput(outputId="abbrsource")
    ),
    tabPanel("Style 1",
             helpText("注意:最终的结果可能还需要细调", br(),
                      "如果是非数字标签,可能还需要引用 natbib 宏包,并且使用 \\citep{***} 来引用"),
             verbatimTextOutput("out_is_clstype1"),
             uiOutput("out_style1_clip"),
             verbatimTextOutput("out_style1")),
    tabPanel("Style 2",
             helpText("注意:最终的结果可能还需要细调", br(),
                      "如果是非数字标签,可能还需要引用 natbib 宏包,并且使用 \\citep{***} 来引用"),
             verbatimTextOutput("out_is_clstype2"),
             uiOutput("out_style2_clip"),
             verbatimTextOutput("out_style2")),
    tabPanel("Style 0",
             helpText("注意:最终的结果可能还需要细调"),
             verbatimTextOutput("out_is_clstype0"),
             uiOutput("out_style0_clip"),
             verbatimTextOutput("out_style0")),
    tabPanel("Cite bib",
             fluidRow(
               column(12,
                      p("输出引用的bib项和key值")
               )
             ),
             radioButtons("inSelect", "bib文件和key的输出顺序,会同步变化",
                          c("按tex文中引用顺序", "按key字母升序" , "按key字母降序","按文献类型字母升序排序","按文献类型字母降序排序","按文献类型排序")),
             fluidRow(
               column(9, p('引用bib'), wellPanel(
                 uiOutput("bib01yinyongclip"),
                 verbatimTextOutput("bib01yinyong"),
               )),
               column(3, p('引用key'), wellPanel(
                 uiOutput("key01yinyongclip"),
                 verbatimTextOutput("key01yinyong")
               ))
             )
    ),
    tabPanel("Nocite bib",
             helpText("输出未引用的bib项和key值"),
             fluidRow(
               column(9, p('未引用bib'), wellPanel(
                 uiOutput("bib02noyinyongclip"),
                 verbatimTextOutput("bib02noyinyong")
               )),
               column(3, p('未引用key'), wellPanel(
                 uiOutput("key02noyinyongclip"),
                 verbatimTextOutput("key02noyinyong")
               ))
             )
    ),
    tabPanel("Abbr query",
             # 自适应宽度
             textAreaInput("Journame", "Journal name", value = "", width = "100%",rows = 10, resize = "both") %>%
               shiny::tagAppendAttributes(style = 'width: 100%;'),
             helpText("以行为单位进行包含查询(忽略大小写、两边的空格和中间多余的空格只保留一个,以及不查询期刊名字符个数小于4的期刊)"),
             helpText("最多返回400条数据,希望能提供精确输入"),
             actionButton("goJournameQuery", "Submit"),
             dataTableOutput(outputId="JournameAbbr")
    ),
    tabPanel("Reorder thebibliography",
             wellPanel(
               fileInput("file_bibtex", "Choose tex File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.tex')),
               actionButton("ReorderBibSubmit", "Submit")
             ),
             helpText("tex文本中引用了,但是 thebib环境中没有的key"),
             fluidRow(
               column(12, p('一般结果是无,建议先在本地正常编译tex文件'), wellPanel(
                 textOutput("TexNoCiteLabel")
               ))
             ),
             helpText("输出重排序后的结果以及未引用(thebib环境多余)的key"),
             fluidRow(
               column(9, p('重排序后的结果'), wellPanel(
                 uiOutput("ReorderBibclips"),
                 verbatimTextOutput("ReorderBib")
               )),
               column(3, p('未引用的key'), wellPanel(
                 uiOutput("ReorderNoBibclips"),
                 verbatimTextOutput("ReorderNoBib")
               ))
             ),
    ),
    tabPanel("SessionInfo",
             verbatimTextOutput("out_runenvir")
    )

  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {


  ###0. 处理 bib 文件, csl, tex 文件
  randomVals <- eventReactive(input$goButton, {
    clear_file()

    tryCatch(
      {
        texpath = input$file1_tex$datapath
        clspath = input$file2_csl$datapath
        bibpath = input$file3_bib$datapath
        #
        # texpath = '/Users/zsc/Desktop/rmdtest/shinytest/FlexilityDTFSTwoC.tex'
        # texpath = '/Users/zsc/Desktop/rmdtest/shinytest/document2.tex'
        # clspath = '/Users/zsc/Desktop/rmdtest/shinytest/ieee-transactions-on-cybernetics.csl'
        # bibpath = "/Users/zsc/Desktop/rmdtest/shinytest/real3.bib"

        file_csl = readLines(clspath,encoding = "UTF-8")
        writeLines(file_csl,'./navigation_default.csl')


        file_bib = readLines(bibpath,encoding = "UTF-8")
        file_bib = c('\n',file_bib,'\n')# 对第一个参考文献添加换行,与最后一个参考文件添加换行
        writeLines(file_bib,'./MEMIO_default.bib')
        abbrtable = NULL
        if (input$bibabbr == "TRUE"){
          #file.copy('MEMIO_default.bib','MEMIO_defaultcopy.bib')
          abbrtable =  journalabbr::abbr2bib(file = "MEMIO_default.bib",
                                             outfile = 'MEMIO_default.bib')

        }
        if (input$bibformat == "TRUE"){
          templogic2 = eval(parse(text = input$bibformat))
          temptib = journalabbr::read_bib2tib(file = 'MEMIO_default.bib')
          journalabbr::write_tib2bib(temptib,file = 'MEMIO_default.bib',
                                     append = FALSE,
                                     isformat = templogic2,
                                     connect_author = input$connectauthor )
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        clear_file()
        stop(safeError(e))
      }
    )


    ###############################################
    #### 1 , 开始读取原始的tex文件,并提取key---- 准备形成 rmd 的后半部分
    tex_key = extract_key(texfile = texpath )
    ################################################
    ###### 2. 提取 bib文件,把key和type组合成数据框
    tib = journalabbr::read_bib2tib(file = 'MEMIO_default.bib')
    tex_key_df = tibble::tibble('key' = tex_key, 'name' = 1:length(tex_key))
    df =  dplyr::full_join(tex_key_df,tib, by =c("key"= "keybib"),suffix = c("_tex","_bib"))

    tex_diff = !is.na(df$key)  & is.na(df$sitenum) # tex 有 , 而bib没有
    if(sum(tex_diff) >=1){
      s = paste0(df$key[tex_diff],collapse = "\n")
      clear_file()
      stop(paste0("出错,以下参考文献在tex文件中引用了,但是在数据库中不存在该文献!!\n", s,'\n请检查key后重新运行') )
    }else{

      rmd_yaml = "---\ntitle: \"how to use cite\"\nauthor: \"zsc\"\ndate: \"`r Sys.Date()`\"\noutput:\n  pdf_document: \n    keep_tex: true\n    latex_engine: xelatex\n    extra_dependencies: [\"ctex\",\"caption\"]\nlink-citations: yes \ncsl: navigation_default.csl\nbibliography: MEMIO_default.bib\n---\n"
      rmd_key = paste0('\n[@',tex_key,']\n') %>%  paste(.,collapse = "")
      rmd_key %>%  paste0(rmd_yaml, . ) %>%  writeLines(.,"rmd_finally.Rmd")
      return(list(df,abbrtable))
    }
  })


  tex_cite_style = reactive({
    df = randomVals()[[1]]
    ## 读取新的tex文件,并且提取tex 中具有参考文献样式的字段
    # 并通过pandoc 把Rmd文件转变为latex文件
    progress <- shiny::Progress$new()
    on.exit(progress$close())

    progress$set(message = "正在计算中,请耐心等待!!!", value = 0)
    rmarkdown::render("rmd_finally.Rmd", output_format = latex_document())
    value = style_fun(texfile = 'rmd_finally.tex')
    clear_file()
    return(value)
  })

  ######  期刊缩写查询  开始 #######
  journamevalue <- eventReactive(input$goJournameQuery, {
    Journame = input$Journame
    Journame = str_replace_all(Journame,'( ){2,}',' ')
    Journame_lower = str_to_lower(Journame)#把输入的值转变为小写
    # 分割---以换行符分割
    Journame_lower = str_split(Journame_lower,pattern = '\\n')[[1]] %>%
      str_trim(., side ="both")
    l = nchar(Journame_lower) # 检查输入的长度
    Journame_lower = Journame_lower[l>=4] # 过滤长度小于4的期刊
    Journame_lower = str_replace_all(Journame_lower,'( ){2,}',' ')
    # Unicode to UTF-8
    # abbrTable = as.data.table(abbrTable)
    # abbrTable = abbrTable[,lapply(.SD, function(x)stringi::stri_escape_unicode(x))]
    abbrTable = as.data.frame(lapply(abbrTable, function(x)stringi::stri_unescape_unicode(x)))
    adf = as.data.table(abbrTable)
    temp = str_to_lower(adf$journal)
    index = lapply(Journame_lower,function(x) grep(x,temp)) # 返回的是包含该字符串的list
    index = unlist(index)#
    if(length(index)<=0){
      return(data.table(V1='没找到相应的缩写'))
    }else{
      return(adf[index,.(journal,journal_abbr,originFile)])
    }
  })
  output$JournameAbbr <- renderDataTable({
    temp = journamevalue()
    if(nrow(temp) > 400){
      #warning('查询返回的数量太多,您可能需要重新定位.最多返回300条数据')
      temp = temp[1:400]
    }
    temp
  },options = list(pageLength = 100))
  ######  期刊缩写查询  结束 #######
  #######################################################


  #######################################################
  ######### 重排序 thebib环境  开始 #######
  reorderbib <- eventReactive(input$ReorderBibSubmit,{
    clear_file()
    tryCatch(
      {
        texpath = input$file_bibtex$datapath
        #texpath = '/Users/zsc/Desktop/未命名文件夹/我的论文终稿/参考文献排序/MeasuringTrans02的副本.tex'
        list_df = reorder_bib_fun(filepath = texpath)
        clear_file()
        return(list_df)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        clear_file()
        stop(safeError(e))
      }
    )
  })
  output$ReorderBib <- renderText({
    # list(duoyu_diff,queshao_diff, bib_sorted,he)
    reorderbib()[[3]]
  })
  output$ReorderBibclips <- renderUI({
    rclipButton("ReorderBibclips", "thebibliography Copy", reorderbib()[[3]], icon("clipboard"))
  })
  ### 未引用
  output$ReorderNoBib <- renderText({
    reorderbib()[[1]]
  })
  output$ReorderNoBibclips <- renderUI({
    rclipButton("ReorderNoBibclips", "bib no cite Copy", reorderbib()[[1]], icon("clipboard"))
  })
  output$TexNoCiteLabel <- renderText({
    reorderbib()[[2]]
  })
  ######### 重排序 thebib环境  结束 #######
  #######################################################


  #######################################################
  ######################### 显示运行环境函数 ###############


  ###############################################
  ############### 输出警告函数,----- 即 tex中有引用,但是bib数据库中不存在该参考文献  ###############
  output$out_warning <- renderText({
    df = randomVals()[[1]] # 返回一个list
    if(length(df)>1 & !is_empty(df)){
      return('无')
    }
  })

  ##########################################
  ##### 输出样式1################
  output$out_style1 <- renderText({
    tex_cite_style()[[1]]
  })
  output$out_style1_clip <- renderUI({
    rclipButton("out_style1_clip", "style1 Copy", tex_cite_style()[[1]], icon("clipboard"))
  })
  ##########################################
  ##### 输出样式2################
  output$out_style2 <- renderText({
    tex_cite_style()[[2]]
  })
  output$out_style2_clip <- renderUI({
    rclipButton("out_style2_clip", "style2 Copy", tex_cite_style()[[2]], icon("clipboard"))
  })

  ##########################################
  ##### 输出样式0################
  output$out_style0 <- renderText({
    tex_cite_style()[[3]]
  })
  output$out_style0_clip <- renderUI({
    rclipButton("out_style0_clip", "style0 Copy", tex_cite_style()[[3]], icon("clipboard"))
  })

  ##########################################
  #####  输出参考文献的类型,
  ##### 1 表示数字标签  0 表示非数字标签,即作者年份标签################
  output$out_is_clstype1 <- renderText({
    is_type = tex_cite_style()[[4]]
    if(is_type){
      "1 == 数字标签"
    }else{
      "0 === 非数字标签"
    }
  })
  output$out_is_clstype2 <- renderText({
    is_type = tex_cite_style()[[4]]
    if(is_type){
      "1 == 数字标签"
    }else{
      "0 === 非数字标签"
    }
  })
  output$out_is_clstype0 <- renderText({
    is_type = tex_cite_style()[[4]]
    if(is_type){
      "1 == 数字标签"
    }else{
      "0 === 非数字标签"
    }
  })
  ##########################################
  ##### 输出引用的bib和key################
  out_yinyong = reactive({
    df = randomVals()[[1]]
    df = df[!is.na(df$name),]
    if(input$inSelect == '按tex文中引用顺序'){
      df = dplyr::arrange(df,name)
    }else if(input$inSelect == '按key字母升序'){
      df = dplyr::arrange(df,key)
    }else if(input$inSelect=='按key字母降序'){
      df = dplyr::arrange(df,desc(key))
    }else if(input$inSelect == '按文献类型字母升序排序'){
      df = arrange(df,typebib)
    }else if(input$inSelect == '按文献类型字母降序排序'){
      df = arrange(df,desc(typebib))
    }else{
      df = arrange(df,typebib)################### 按照文献类型排序
    }

    if(is_empty(df)){
      s_temp = 'bib数据库中的文献,一篇文章都没有被引用'
      return(list(s_temp,s_temp))
    }else{
      dfSub = df
      tex_key = purrr::map(dfSub$value,function(x){
        paste0(x,collapse = '\n')
      })

      cite_bib = paste(tex_key,collapse = '\n\n\n')
      cite_bib = paste0('总计: ', nrow(dfSub),' 篇参考文献被引用\n\n',cite_bib)
      cite_key = paste(dfSub$key,collapse = '\n')
      return(list(cite_key,cite_bib ))
    }
  })
  out_no_yinyong = reactive({
    df = randomVals()[[1]]
    if(sum(is.na( df$name)) == 0 ){
      s_temp ='刚好没有引用的key,\n(即bib数据库中的文献全部被引用)'
      return(list(s_temp,s_temp))
    }else{
      dfSub = df[is.na(df$name),]
      nocite = purrr::map(dfSub$value,function(x){
        paste0(x,collapse = '\n')
      })
      nocite_bib = paste(nocite,collapse = '\n\n\n')
      nocite_bib = paste0('总计: ', nrow(dfSub),' 篇参考文献没有被引用\n\n',nocite_bib)
      nocite_key = paste(dfSub$key,collapse = '\n')
      return(list(nocite_key,nocite_bib))
    }
  })

  output$bib01yinyongclip <- renderUI({
    rclipButton("bib01yinyongclip", " bib cite Copy", out_yinyong()[[2]], icon("clipboard"))
  })
  output$bib01yinyong <- renderText({
    out_yinyong()[[2]]
  })
  output$key01yinyongclip <- renderUI({
    rclipButton("key01yinyongclip", "key cite Copy", out_yinyong()[[1]], icon("clipboard"))
  })
  output$key01yinyong <- renderText({
    out_yinyong()[[1]]
  })
  ############################################

  ############################################
  ###### 输出没有引用的bib和key##############
  output$bib02noyinyongclip <- renderUI({
    rclipButton("bib02noyinyongclip", "bib no cite Copy", out_no_yinyong()[[2]], icon("clipboard"))
  })
  output$bib02noyinyong <- renderText({
    out_no_yinyong()[[2]]
  })
  output$key02noyinyongclip <- renderUI({
    rclipButton("key02noyinyongclip", "key no cite Copy", out_no_yinyong()[[1]], icon("clipboard"))
  })
  output$key02noyinyong <- renderText({
    out_no_yinyong()[[1]]
  })
  ############################################

  ############################################
  ###### 输出运行环境 ####################
  output$out_runenvir <- renderPrint({
    print(list("sessionInfo"=sessionInfo(),
               "RStudio.Version"=RStudio.Version(),
               "devtools::session_info"=devtools::session_info(),
               "pandoc_version" = rmarkdown::pandoc_version(),
               "dir"=dir()))
  })
  ############################################
  ###### 输出期刊缩写对照表 ################
  output$jouranl_abbr <- renderDataTable({
    df = randomVals()[[1]]
    if(is_empty(df)){
      temp = data.frame('NOTE'='整个bib没有找到对应的缩写!!!')
      return(temp)
    }else{
      df$value =  purrr::map(df$value,function(x){
        paste0(x,collapse = '\n\n')
      })
      dff = df[,c("key",'name',"sitenum",'value','typebib','AUTHOR','JOURNAL')]
      return(dff)
    }
  },options = list(pageLength = 100)
  )
  output$abbrsource <- renderDataTable({
    abbrtable = randomVals()[[2]]
    if(is_empty(abbrtable)){
      temp = data.frame('NOTE'='整个bib没有找到对应的缩写!!!')
      return(temp)
    }else{
      return(abbrtable)
    }
  },options = list(pageLength = 100)
  )
}



##### Run the application
shinyApp(ui = ui, server = server)
