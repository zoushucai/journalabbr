library(shiny)
library(stringr)
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
      stop(safeError(e))
    }
  )
}

is_empty = rlang::is_empty

style_fun = function(texfile){
  tex =  readLines(texfile, encoding = "UTF-8")
  style_0 = paste0(tex ,collapse = "\n")     # 直接返回rmd生成的tex文件样式

  #clear_file()
  ## 提取页面的显示信息--有可能是数字,有可能是作者,有可能是作者-年
  from1 = grep('\\\\begin\\{document\\}',tex)
  from2 = grep('\\\\maketitle',tex)
  if(is_empty(from1) && is_empty(from2)){
    stop('没找到开始的地方')
  }else if(!is_empty(from1) && is_empty(from2)){
    ind_from = from1
  }else if(is_empty(from1) && !is_empty(from2)){
    ind_from = from2
  }else if (from1 <= from2){
    ind_from = from2
  }else{
    stop('索引值出错,没找到开始的地方')
  }
  to1 = grep('\\\\hypertarget\\{refs\\}',tex)
  to2 = grep('\\\\begin\\{cslreferences\\}',tex)
  if(is_empty(to1) && is_empty(to2)){
    stop('没找到结束的地方')
  }else if(!is_empty(to1) && is_empty(to2)){
    ind_to = to1
  }else if(is_empty(to1) && !is_empty(to2)){
    ind_to = to2
  }else if (to1 <= to2){
    ind_to = to1
  }else{
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
  tex_show[tex_show == ""] = '\n@@\n\n\n'
  tex_show = paste(tex_show,collapse = ' ',sep = "")
  tex_show = unlist(str_split(tex_show,'\n@@\n\n\n'))
  tex_show = str_trim(tex_show,side = "both")

  key_show = stringr::str_extract_all(tex_show,'(?<=\\\\protect\\\\hyperlink\\{ref-)(.*?)(?=\\})',simplify=TRUE)
  word_show = stringr::str_replace_all(tex_show,'\\\\protect\\\\hyperlink\\{ref-.*?\\}','')

  # 处理多余的大括号-- 处理大括号
  word_show = stringr::str_replace_all(word_show,'([^\\\\])(\\{)([^ ]*?)(\\})','\\1\\3')
  # word 部分进行删除, 删除数字标签 {[}三个数字以内{]}
  word_show = str_replace_all(word_show, '^\\{\\[\\}[0-9]{0,3}\\{\\]\\}$','')

  show_df = data.frame('key' = key_show,'word'=word_show)
  # 参考文献部分
  ind_end  = grep("\\\\end\\{cslreferences\\}",tex)
  to2 = grep('\\\\begin\\{cslreferences\\}',tex)
  if(is_empty(to2)){
    stop("参考文献没有开始标记")
  }
  if(is_empty(ind_end)){
    stop('参考文献没有结束标记')
  }

  tex_sub = tex[(to2 + 1):(ind_end - 1)] # 提取tex 的一个子页面
  ## 6. 处理tex_sub的参考文献字段,使其变为bibitem样式
  ##### 6.1 把tex_sub中的 '\leavevmode\hypertarget{ref-****}{}%'  字段变成'\bibitem{***}' 字段
  key_index = grep('(?<=\\\\leavevmode\\\\hypertarget\\{ref-)(.*?)(?=\\})',tex_sub,perl = TRUE)
  tex_sub_new = str_replace(tex_sub,'(^\\\\leavevmode\\\\hypertarget\\{ref-)(.*?)(\\})(\\{\\}%$)',replacement= "\\2")
  ##### 6.2 把tex_sub_new 中的key标签下一行的 {[}**{]} 给替换为空
  tex_sub_new = str_replace_all(tex_sub_new,"(^\\{\\[\\}[0-9]{0,3}\\{\\]\\})( ){0,}",'') %>% str_trim(.,side = "both") # 特殊情况处理
  #### 6.3 把标签key用上面显示的文本进行替换, 没找到则用空进行替换
  for (i in key_index) {
    value_temp = show_df[show_df[['key']] == tex_sub_new[i], 2]
    if (!is_empty(value_temp) ) {
      tex_sub_new[i] = paste0('\\bibitem{',tex_sub_new[i],'}{',value_temp,'}')
    }else{
      tex_sub_new[i] = paste0('\\bibitem{',tex_sub_new[i],'}{}')
    }
  }
  ### 样式0 ---- 返回rmd处理的最原始的引用样式
  # style_0 = paste0(tex[to2:ind_end] ,collapse = "\n")

  #### 样式1 ----  每一个参考文献在不同行
  style_1 = c('\\section*{References}', '\\begin{thebibliography}{99}',tex_sub_new,'\\end{thebibliography}')
  style_1 = paste(style_1,collapse = "\n") #

  #### 样式2 ---- 每一个参考文献在同一行
  tex_sub_new[tex_sub_new == ""] = "\n\n"
  style_2 = c('\\section*{References}\n\n', '\\begin{thebibliography}{99}\n\n',tex_sub_new,'\n\n\\end{thebibliography}')
  style_2 = paste(style_2, collapse = " ")
  style_2 = str_replace_all(style_2, pattern = " {2,}",replacement = " ") # 处理多余的空格 或 [:blank:]
  style_2 =  str_replace_all(style_2, pattern = "\n\n ",replacement = "\n\n") # 处理换行后的空格
  return(list(style_1,style_2,style_0))
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
             helpText("注意:最终的结果可能还需要细调"),
             dataTableOutput(outputId="abbrsource")
    ),
    tabPanel("Style 1",
             helpText("注意:最终的结果可能还需要细调"),
             uiOutput("out_style1_clip"),
             verbatimTextOutput("out_style1")),
    tabPanel("Style 2",
             helpText("注意:最终的结果可能还需要细调"),
             uiOutput("out_style2_clip"),
             verbatimTextOutput("out_style2")),
    tabPanel("Style 0",
             helpText("注意:最终的结果可能还需要细调"),
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
    print(list(sessionInfo(),dir()))
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
