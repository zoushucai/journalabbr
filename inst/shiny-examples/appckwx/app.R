if (!require("pacman")) install.packages("pacman")

# if (!require("remotes")) install.packages("remotes")
# remotes::install_github("zoushucai/journalabbr")

pacman::p_load(
  "shiny", "stringr", "stringi", "data.table",
  "rclipboard", "knitr", "rmarkdown", "purrr", "rlang",
  "tinytex", "DT", "shinydashboard", "journalabbr", "quarto"
)

options(shiny.sanitize.errors = FALSE)
rm(list = ls())


source("my_functions.R")
# global var
fixed_clsfile = "navigation_default.csl"
fixed_qmdfile = "qmd_default.qmd"
fixed_bibfile = "MEMIO_default.bib"
fixed_bibfile_old = "MEMIO_default_old.bib" # 上传的最原始的bib
output_qmd = "qmd_finally.qmd"
output_tex = gsub("\\.qmd$", ".tex", output_qmd)#这个是根据 output_qmd 自动产生的


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(title = h2("Reference style adjustment", align = "center"), windowTitle = "Reference style adjustment"),
  HTML("<p>Note: </p>"),
  HTML("<p>&emsp;&emsp; &ensp; 1. The uploaded tex file should be compiled normally locally</p>"),
  HTML("<p>&emsp;&emsp; &ensp; 2. The uploaded Bib file should be carefully checked. This involves the final generation format. <br>
       &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; For example: three character Chinese author,eg: Shucai, Zou  Should be written as: Shu-Cai, Zou </p>"),
  HTML("<p>&emsp;&emsp; &ensp; 3. Bug feedback:&nbsp;&nbsp;<a href='https://github.com/zoushucai/journalabbr/issues'>https://github.com/zoushucai/journalabbr/issues </a> </p>"),
  rclipboardSetup(), # rclipboard init
  tabsetPanel(
    tabPanel(
      "Input",
      wellPanel(
        fileInput("file1_tex", "Choose tex File(File coding: UTF-8)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".tex")
        ),
        fileInput("file2_csl", "Choose csl File(File coding: UTF-8)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csl")
        ),
        fileInput("file3_bib", "Choose bib File(File coding: UTF-8)",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".bib")
        ),
        fluidRow(
          column(8,
                 fileInput("file4_usercsv", "Choose user abbr csv File(File coding: UTF-8. Empty if there is no upload)",
                           accept = c("text/csv", "text/semicolon-separated-values,text/plain", ".csv")
                 )
          ),
          column(4,
                 selectInput("csvsep", "Separator of csv file:",
                             choices = c(
                               "auto" = "auto",
                               "," = "comma",
                               ";" = "semicolon"
                             ),
                             selected = "auto"
                 ))
        ),
        fluidRow(
          column(6,
                 selectInput("bibabbr", "handle 'journal' field:",
                             choices = c(
                               "abbr journal field" = "onlyabbrjournal",
                               "nothing" = "nothing"
                             ),selected = "onlyabbrjournal")
          ),
          column(6,
                 selectInput("connectauthor", "handle 'author' field:",
                             choices = c(
                               "and" = "and",
                               "\\&" = "\\&",
                               "&" = "&",
                               "nothing" = "nothing"
                             ),selected = "abbrwithbeatify")
          ),
        ),
        actionButton("goButton", "Submit")
      )
    ),
    tabPanel("Warning",
             helpText(
               "1, The default UTF-8 encoding is used for the uploaded files.", br(),
               "2, Uploading the file to the generated reference style will take some time. Please be patient.", br(),
               "3, The final output of the reference style, also need to be carefully checked to meet the requirements of the journal"
             ),
             p("The warning displays as follows:", style = "font-weight:bold;color:red;"),
             verbatimTextOutput("out_warning")
    ),
    tabPanel("Jouranl abbr",
             helpText("Note: The final result may need to be fine-tuned."),
             dataTableOutput(outputId = "jouranl_abbr")
    ),
    tabPanel("Style",
             helpText("Note: The final result may need to be fine-tuned.", br() ),
             verbatimTextOutput("out_is_clstype1"),
             uiOutput("out_style1_clip"),
             verbatimTextOutput("out_style1")
    ),
    tabPanel("Cite bib",
             helpText("The key exist in the .tex file"),
             fluidRow(
               column(9, p("Cite bib"), wellPanel(
                 uiOutput("bib01yinyongclip"),
                 verbatimTextOutput("bib01yinyong"),
               )),
               column(3, p("Cite key"), wellPanel(
                 uiOutput("key01yinyongclip"),
                 verbatimTextOutput("key01yinyong")
               ))
             )
    ),
    tabPanel("SessionInfo",
             verbatimTextOutput("out_runenvir")
    ),
    #创建一个新的选项卡 --- 展示清除按钮
    tabPanel("Clear",
             helpText("Note: Clear all files"),
             fluidRow(
               column(6, wellPanel(
                 actionButton("showButton", "Show files"),
                 verbatimTextOutput("showButton")
               )),
               column(6, wellPanel(
                 actionButton("clearButton", "Clear all files"),
                 verbatimTextOutput("clearButton")
               ))
             )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  ### 0. Processing bib, csl, tex files， return dt by read bib
  randomVals <- eventReactive(input$goButton, {
    clear_file()

    tryCatch(
      {
        texfile <- input$file1_tex$datapath
        clsfile <- input$file2_csl$datapath
        bibfile <- input$file3_bib$datapath
        csvfile <- input$file4_usercsv$datapath

        if (is.null(csvfile)) {
          csvfile <- ""
        }
        if (is.null(texfile) || is.null(clsfile) || is.null(csvfile)) {
          stop("The correct file is not uploaded. Please upload the file again.")
        }


        # texfile = "C:\\Users\\zscmm\\Documents\\GitHub\\journalabbr\\weak2abbr.tex"
        # clsfile = "C:\\Users\\zscmm\\Documents\\GitHub\\journalabbr\\european-journal-of-operational-research.csl"
        # clsfile = "C:\\Users\\zscmm\\Desktop\\shiny_cankaowenxian\\defalut\\chinese-gb7714-2005-numeric.csl"
        # bibfile = "C:\\Users\\zscmm\\Documents\\GitHub\\journalabbr\\weakabbr.bib"

        # 1.  拷贝 bib 和 cls 的文件到 固定的文件中
        if(is_empty(clsfile)){
          clsfile <- system.file("template", "chinese-gb7714-2005-numeric.csl", package = "journalabbr", mustWork = TRUE)
        }

        copy_file(clsfile, fixed_clsfile)

        copy_file(bibfile, fixed_bibfile_old)

        ### 对bib 文件进行处理
        if (input$bibabbr == "onlyabbrjournal") {
          #仅对 journal 进行缩写
          bib2bib(file=fixed_bibfile_old, out.file = fixed_bibfile)
        } else {
          #不进行任何操作
          copy_file(bibfile, fixed_bibfile)
        }

        # 2. 从 texfile  文件中提取 ckey
        ckey = extract_key(texfile)

        # 3. 提取的 ckey 和固定的模板进行融合,然后导出 pdf, 生成 tex
        generate_qmd(ckey, output = output_qmd)

      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        clear_file()
        stop(safeError(e))
      }
    )

    #提取bib文件中的全部数据
    bibdt <- read_bib2dt(fixed_bibfile, encoding = "UTF-8")

    #只提取 tex中引用的 ckey
    citesall_dt = data.table("CKEY" = ckey)
    bibdt0 = merge.data.table(citesall_dt, bibdt, by = "CKEY", all.x = TRUE, sort = FALSE)

    return(list("bibdt" = bibdt, "bibdt0" = bibdt0))
  })


  # 编译 qmd 文件 to tex 文件
  compileqmd <- reactive({
    bibdt <- randomVals()$bibdt
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating, please wait...", value = 0)
    # Convert Rmd files to latex files by pandoc
    quarto::quarto_render(output_qmd, output_format = "latex")
    return(bibdt)
  })


  #从输出的tex文件中提取数据，返回一个数据框
  get_citedt <- reactive({
    bibdt = compileqmd()
    # 1. 读取 tex 文件
    doc = read_tex(output_tex)

    # 2. 通过对 doc 进行提取, 找到 body 部分,并进行清洗, 提取出一个 dt_body 数据框, 只含有两列: ckey 和 refvalue
    docbody = extract_body(doc)
    dt_body = extract_citeproc(docbody)

    # 3.  通过对 doc 进行提取, 找到 ref 部分,并进行清洗, 提取出一个 dt_ref 数据框, 只含有两列: ckey 和 bibitem
    docref = extract_ref(doc)
    dt_ref = extra_bibitem(docref)
    # 4. 合并
    dt = merge.data.table(dt_body, dt_ref, by.x = "ckey", by.y = "ckey", all=TRUE, sort=FALSE)
    # 检查,如果有 NA 值或者 NULL 则发出警告, 并删除 NA 值或 NULL 值所在的行
    if (any(is.na(dt))) {
      warning("数据框中存在 NA 值，正在删除含有 NA 的行。")
      # 删除含有 NA 值的行
      dt <- na.omit(dt)
    }
    # 5. 对bibitem列进行清洗
    # # 如果是数字列， 形如：
    # \bibitem[\citeproctext]{ref-cavallo2016ensuring}
    # \CSLLeftMargin{{[}5{]} }%
    # \CSLRightInline{CAVALLO B, D'APUZZO L. Ensuring reliability of the
    # weighting vector: Weak consistent pairwise comparison matrices{[}J{]}.
    # Fuzzy Sets Syst., 2016, 296: 21-34.}

    clear_fun1_bibitem = function(x){
      # 逐个检测
      x2 = str_extract(x, "(?<=\\\\CSLRightInline\\{)(.*?)(?=\\}$)")
      x3 = ifelse(is.na(x2), x, x2)
      return(x3)
    }
    #如果是作者-年份风格， 形如：
    # \bibitem[\citeproctext]{ref-aguaron2021reducing}
    # Aguarón, J., Escobar, M. T., \& Moreno-Jiménez, J. M. (2021). Reducing
    # inconsistency measured by the geometric consistency index in the
    # analytic hierarchy process. \emph{Eur. J. Oper. Res.}, \emph{288}(2),
    # 576--583.

    clear_fun2_bibitem = function(x){
      x2 = str_extract(x, "(?<=\\\\citeproc\\{)(.*?)(?=\\}$)")
      x3 = ifelse(is.na(x2), x, x2)
      return(x3)
    }

    if(check_ref_type(dt$refvalue)==1){
      # 数字风格
      dt[, isnum := 1]
      dt[, bibitem2 := clear_fun1_bibitem(bibitem)]
      dt[, value := sprintf("\\bibitem{%s}{%s}", ckey, bibitem2) ]
    }else{
      # 作者-年份风格
      dt[, isnum:= 2]
      dt[, bibitem2 := clear_fun2_bibitem(bibitem)]
      dt[, value := sprintf("\\bibitem[%s]{%s}{%s}", refvalue, ckey, bibitem2) ]
    }

    # clear_file()
    return(dt)
  })

  tex_cite_style1 <- reactive({
    dt = get_citedt()
    paste(dt[["value"]], collapse = "\n\n")
  })

  #
  # ###########################################################
  # ############ Output warning,----- The key exist in the .tex file, but don't exist in bib file ###############
  output$out_warning <- renderText({

    # 捕捉警告信息
    warning_info = capture.output({
      bibdt = compileqmd()
    })

    if (length(bibdt) > 1 & !is_empty(bibdt)) {
      return("no warning")
    }else{
      return(warning_info)
    }

  })
  # #######################################################
  #
  # ############ Output Style 1 -- unsegmented style ###################
  output$out_style1 <- renderText({
    tex_cite_style1()
  })
  output$out_style1_clip <- renderUI({
    rclipButton(inputId = "out_style1_clip", label = "style Copy", clipText = tex_cite_style1(), icon = icon("clipboard"))
  })
  output$out_is_clstype1 <- renderText({
    dt = get_citedt()
    if(dt[["isnum"]][1] == 1){
      return("numeric style.")
    }else{
      return("author-year style.")
    }
  })
  # #######################################################

  # #######################################################
  output$key01yinyong <- renderText({
      bibdt0 = randomVals()$bibdt0
      CKEY = bibdt0$CKEY
      paste(CKEY, collapse = "\n")
  })

  output$bib01yinyong <- renderText({


    bibdt0 = randomVals()$bibdt0
    write_dt2bib(bibdt0, file = "tempckey.bib")
    bib = readLines("tempckey.bib", encoding = "UTF-8")
    # 删除临时文件
    file.remove("tempckey.bib")
    paste(bib, collapse = "\n")
  })


  ######################### Clear all files ###############
  output$out_clear_after <- renderPrint({
    files = list.files(recursive = TRUE)
    if (length(files) > 0) {
      return(files)
    } else {
      return("No files in the directory")
    }
  })

  output$clearButton <- eventReactive(input$clearButton, {
    clear_file()
    files = list.files(recursive = TRUE)
    now_time = Sys.time()
    files_str = paste0("Now the files in the directory are:\n\n", now_time, "\n\n",
                       paste(files, collapse = "\n"))
    return(files_str)
  })
  output$showButton <- eventReactive(input$showButton, {
    files = list.files(recursive = TRUE)
    files_str = paste0("Show the files in the directory:\n\n", Sys.time(), "\n\n",
                       paste(files, collapse = "\n"))
    return(files_str)
  })


  ######################### Output journal abbreviation comparison table -- begin ###############
  output$jouranl_abbr <- renderDataTable(
    {
      bibdt <- randomVals()$bibdt # return a list
      if (is_empty(bibdt)) {
        temp <- data.frame("NOTE" = "No corresponding abbreviation was found in the whole bib file!!!")
        return(temp)
      } else {
        col <- c("CKEY", "JOURNAL")
        stopifnot(all(col %in% colnames(bibdt)))
        tempdt <- bibdt[, col, with = F]
        return(as.data.frame(tempdt))
      }
    },
    options = list(pageLength = 100)
  )
  ######################### Output journal abbreviation comparison table -- end ###############

  ######################### sessionInfo  ###############
  output$out_runenvir <- renderPrint({
    print(list(
      "sessionInfo" = sessionInfo(),
      "devtools::session_info" = devtools::session_info(),
      "pandoc_version" = rmarkdown::pandoc_version(),
      "dir" = dir()
    ))
  })
  ######################### sessionInfo  ###############
}



##### Run the application
shinyApp(ui = ui, server = server)
