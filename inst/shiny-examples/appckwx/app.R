if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "shiny", "stringr", "stringi", "data.table",
  "rclipboard", "knitr", "rmarkdown", "purrr", "tidytable",
  "tinytex", "DT", "shinydashboard", "journalabbr"
)

# library(lubridate)
# options(shiny.fullstacktrace = TRUE)
options(shiny.sanitize.errors = FALSE)
rm(list = ls())

clear_file <- function(mypattern = "(.*\\.R$)|(.*\\.Rproj$)") {
  tryCatch(
    {
      now_dir <- list.files()
      old_dir <- dir(pattern = mypattern)

      delete_dir <- setdiff(now_dir, old_dir)
      for (i in delete_dir) {
        if (!file_test("-d", i)) { # Delete if it is not a directory
          file.remove(i)
        }
      }
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      print("error")
      clear_file()
      stop(safeError(e))
    }
  )
}

is_empty <- function(x) {
  length(x) == 0
}

tidy_tex <- function(tex) {
  ## Extract the display information of the page -- it may be a number, an author, or an author-year
  from1 <- grep("\\\\begin\\{document\\}", tex)
  from2 <- grep("\\\\maketitle", tex)
  if (is_empty(from1) && is_empty(from2)) {
    clear_file()
    stop("There is no '\\begin{document}' in the .tex file")
  } else if (!is_empty(from1) && is_empty(from2)) {
    ind_from <- from1
  } else if (is_empty(from1) && !is_empty(from2)) {
    ind_from <- from2
  } else if (from1 <= from2) {
    ind_from <- from2
  } else {
    clear_file()
    stop("Index error, there is no '\\begin{document}' in the .tex file")
  }
  to1 <- grep("\\\\hypertarget\\{refs\\}", tex, ignore.case = TRUE)
  to2 <- grep("\\\\begin\\{cslreferences\\}", tex, ignore.case = TRUE)
  if (is_empty(to1) && is_empty(to2)) {
    clear_file()
    stop("there is no '\\begin{cslreferences}' in the .tex file.")
  } else if (!is_empty(to1) && is_empty(to2)) {
    ind_to <- to1
  } else if (is_empty(to1) && !is_empty(to2)) {
    ind_to <- to2
  } else if (to1 <= to2) {
    ind_to <- to1
  } else {
    clear_file()
    stop("Index error, there is no '\\begin{cslreferences}' in the .tex file")
  }

  tex_show <- tex[(ind_from + 1):(ind_to - 1)] # Extract display page

  #### Delete empty lines before the tex_show document
  for (i in seq_len(100)) {
    value <- which(tex_show[1] == "")
    if (!is_empty(value) && value == 1) {
      tex_show <- tex_show[2:length(tex_show)]
    } else {
      break
    }
  }
  #### Delete empty lines after the tex_show document
  for (i in seq_len(100)) {
    max_len <- length(tex_show)
    value <- which(tex_show[max_len] == "")
    if (!is_empty(value) && value == 1) {
      tex_show <- tex_show[1:(max_len - 1)]
    } else {
      break
    }
  }
  ######### Merge tex_show, because some two lines represent one field
  tex_show[tex_show == ""] <- "\n@@@\n\n\n" # Special value
  tex_show <- paste(tex_show, collapse = " ", sep = "")
  tex_show <- unlist(str_split(tex_show, "\n@@@\n\n\n"))
  tex_show <- str_trim(tex_show, side = "both")
  return(tex_show)
}

style_fun <- function(texfile) {
  # texfile='./rmd_finally5.tex'
  tex <- readLines(texfile, encoding = "UTF-8")
  style_raw <- paste0(tex, collapse = "\n") # rmd raw style

  # clear_file()
  tex_show <- tidy_tex(tex)
  ################################################################
  ##### style \bbitem[author-year/number]{CEKY}{reference format} was extracted from tex_show ######
  ################################################################
  # 1. extract CEKY and author-year/number
  extra_text <- str_extract_all(tex_show, "(\\\\protect)(.*?)(\\\\hyperlink)(\\{ref-)([0-9A-Za-z\\-]*?)(\\}.*)")
  ### 1.1 extract CEKY
  key_show <- str_replace_all(extra_text, "(\\\\protect)(.*?)(\\\\hyperlink)(\\{ref-)([0-9A-Za-z\\-]*?)(\\}.*)", "\\5")
  ### 1.2 extract author-year/number
  word_show <- gsub("(\\\\protect)(.*?)(\\\\hyperlink)(\\{ref-)([0-9A-Za-z\\-]*?)(\\}.*)", "\\6", extra_text, perl = T)
  word_show2 <- gsub("(\\{\\[\\})(.*?)(\\{\\]\\})", "\\2", word_show, perl = T) # Special case handling
  word_show3 <- gsub("(^\\}\\{)(.*?)(\\})(.*)", "\\2", word_show2, perl = T) #  Normal handling

  stopifnot(length(key_show) == length(word_show))
  show_df <- data.frame("key" = key_show, "word" = word_show3)

  # 2. Extract reference format from tex
  ### 2.1 Extract part of references
  ind_begin <- grep("\\\\begin\\{cslreferences\\}", tex, ignore.case = TRUE)
  ind_end <- grep("\\\\end\\{cslreferences\\}", tex, ignore.case = TRUE)

  if (is_empty(ind_begin)) {
    clear_file()
    stop("There is no '\\begin{cslreferences}' in tex")
  }
  if (is_empty(ind_end)) {
    clear_file()
    stop("There is no '\\end{cslreferences}' in tex")
  }

  tex_sub <- tex[(ind_begin + 1):(ind_end - 1)] # Extract part of references

  #### 2.2 Processing tex_sub, so that it becomes \bbitem[author-year/number]{CEKY}{reference format}
  #### 2.2.1  Delete the line of'\\CSLLeftMargin'
  ind <- grepl("^\\\\CSLLeftMargin", tex_sub, perl = T, ignore.case = T)
  tex_sub_new <- str_squish(tex_sub[!ind])
  ##### 2.2.2 '\leavevmode\hypertarget{ref-****}{}%' field in tex_sub_new becomes the '\bbitem[author-year/number]{CEKY}{' field
  pat <- sprintf("(\\\\leavevmode)(.*?)(\\\\hypertarget)(\\{ref-)(%s)(\\})(.*%%$)", show_df$key)
  ####  If show_df$word are all numbers, then '\bibitem{***}{' field
  ####  otherwise, then '\bibitem[***]{***}{' field
  is_num <- if (sum(is.na(as.numeric(unlist(show_df$word)))) > 0L) {
    0L
  } else {
    1L
  }

  if (is_num) {
    s0 <- sprintf("\\\\bibitem{%s}{", show_df$key) # \\bibitem{***}{ field
  } else {
    s0 <- sprintf("\\\\bibitem[%s]{%s}{", show_df$word, show_df$key) # \\bibitem[***]{***}{
  }

  for (i in seq_along(pat)) {
    tex_sub_new <- gsub(pat[i], s0[i], tex_sub_new, perl = T, ignore.case = T)
  }
  #### 2.2.3  Replace the character '\\CSLRightInline{***' with '',  and keep the ***
  tex_sub_new <- gsub("^\\\\CSLRightInline\\{", "", tex_sub_new, ignore.case = T)

  ##### 2.2.4 If there is a '{[}***{]}' pattern in tex_sub_new, and keep the ***
  tex_sub_new <- gsub("(\\{\\[\\})([A-Za-z0-9]*?)(\\{\\]\\})", "[\\2]", tex_sub_new, perl = T) # Special case handling

  #### style 1 ----  Fixed width to make a reference in multiple lines
  style_1 <- c("\\section*{References}", "\\begin{thebibliography}{99}", tex_sub_new, "\\end{thebibliography}")
  style_1 <- paste(style_1, collapse = "\n") #
  #### style 2 ---- Make a reference on one line
  tex_sub_new[tex_sub_new == ""] <- "\n\n"
  style_2 <- c("\\section*{References}\n\n", "\\begin{thebibliography}{99}\n\n", tex_sub_new, "\n\n\\end{thebibliography}")
  style_2 <- paste(style_2, collapse = " ")
  style_2 <- str_replace_all(style_2, pattern = " {2,}", replacement = " ") # Handle excess space  or [:blank:]
  style_2 <- str_replace_all(style_2, pattern = "\n\n {1,}", replacement = "\n\n") # Handles space after a newline
  return(list(style_1, style_2, is_num))
}

extract_key <- function(texfile) {
  #### 1.Read the original Tex file and extract the key---- ready to form the second half of the .Rmd
  ###############################################
  document <- readLines(texfile, encoding = "UTF-8")
  document <- str_replace_all(document, "^[[:blank:]]*?%.*", "") # Delete everything starting with %.
  document <- str_replace_all(document, "([^\\\\])(%.*)", "\\1") # Delete everything after % that doesn't start with \\
  document <- document[which(document != "")] %>% paste(collapse = " ")
  document <- str_squish(document)
  # extract key
  tex_key <- unlist(str_extract_all(document, "(?<=\\\\cite[pt]?\\{).*?(?=\\})"))
  # There are multiple keys in a \cite{***,***,***}, Split with ',' and remove any extra Spaces,
  tex_key <- unique(str_squish(unlist(str_split(tex_key, ","))))
  return(tex_key)
}


tex_diff_bib <- function(texfile, dt) {
  ###############################################
  #### 1.Read the original Tex file and extract the key---- ready to form the second half of the .Rmd
  texkey <- extract_key(texfile = texfile)
  texkey_dt <- as.data.table(data.frame("CKEY" = texkey, "tex_id" = seq_along(texkey)))

  #### 2. merge
  new_dt <- merge.data.table(texkey_dt, dt, by = "CKEY", all = TRUE, suffixes = c("_tex", "_bib"), sort = FALSE)

  ### 3. diff
  tex_diff <- !is.na(new_dt$tex_id) & is.na(new_dt$fz_id) # tex has it, bib hasn't  it
  if (sum(tex_diff) >= 1) {
    s <- paste0(new_dt$CKEY[tex_diff], collapse = "\n")
    clear_file()
    stop(paste0("Error, the following key is used in the Tex file, but not in the bib file!!\n", s, "\nPlease check the key and run again\n"))
  } else {
    rmd_yaml <- "---\ntitle: \"how to use cite\"\nauthor: \"zsc\"\ndate: \"`r Sys.Date()`\"\noutput:\n  pdf_document: \n    keep_tex: true\n    latex_engine: xelatex\n    extra_dependencies: [\"ctex\",\"caption\"]\nlink-citations: yes \ncsl: navigation_default.csl\nbibliography: MEMIO_default.bib\n---\n"
    rmd_key <- paste0("\n[@", texkey, "]\n") %>% paste(., collapse = "")
    rmd_key %>%
      paste0(rmd_yaml, .) %>%
      writeLines(., "rmd_finally.Rmd")
    return(new_dt)
  }
}



clear_file()
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
          column(4, uiOutput(outputId = "csvsep"))
        ),
        fluidRow(
          column(6,
                 selectInput("bibabbr", "Abbreviate the journal field:",
                             choices = c(
                               "only abbr journal field" = "onlyabbrjournal",
                               "abbr journal with beautify" = "abbrwithbeatify",
                               "nothing" = "nothing"
                             ),selected = "abbrwithbeatify")
          ),
          column(6,uiOutput(outputId = "connectauthor"))
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
    tabPanel("Style 1",
             helpText("Note: The final result may need to be fine-tuned.", br() ),
             verbatimTextOutput("out_is_clstype1"),
             uiOutput("out_style1_clip"),
             verbatimTextOutput("out_style1")
    ),
    tabPanel("Style 2",
             helpText("Note: The final result may need to be fine-tuned.", br()),
             verbatimTextOutput("out_is_clstype2"),
             uiOutput("out_style2_clip"),
             verbatimTextOutput("out_style2")
    ),
    tabPanel("Cite bib",
             helpText("The key exist in the .tex file"),
             #fluidRow( column(12, p("The key exist in the .tex file"))),
             radioButtons("inSelect", "Sorting key",
                          c("In the order cited in Tex", "Ascending order by key", "Descending order by key",
                            "Ascending order by bib type", "Descending order by bib type", "Sort by bib file")
             ),
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
    tabPanel( "Nocite bib",
              helpText("The key don't exist in the .tex file"),
              fluidRow(
                column(9, p("Nocite bib"), wellPanel(
                  uiOutput("bib02noyinyongclip"),
                  verbatimTextOutput("bib02noyinyong")
                )),
                column(3, p("Nocite key"), wellPanel(
                  uiOutput("key02noyinyongclip"),
                  verbatimTextOutput("key02noyinyong")
                ))
              )
    ),
    tabPanel("Abbr query",
             textAreaInput("Journame", "Journal name", value = "", width = "100%", rows = 10, resize = "both") %>%
               shiny::tagAppendAttributes(style = "width: 100%;"),
             helpText("Perform matching query by unit of behavior.(ignore case and extra spaces)"),
             helpText("The maximum number of returned data is 400, and we hope to provide accurate input."),
             actionButton("goJournameQuery", "Submit"),
             dataTableOutput(outputId = "JournameAbbr")
    ),
    tabPanel("SessionInfo",
             verbatimTextOutput("out_runenvir")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  ### 0. Processing bib, csl, tex files
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

        #
        # texfile = '/Users/zsc/Desktop/rmd/weakfuzzyaik.tex'
        # #clsfile = '/Users/zsc/Desktop/rmd/chinese-gb7714-2005-numeric.csl'
        # clsfile = "/Users/zsc/Desktop/rmd/chinese-gb7714-2005-author-date.csl"
        # bibfile = "/Users/zsc/Desktop/rmd/weakfuzzyaik.bib"

        file_csl <- readLines(clsfile, encoding = "UTF-8")
        writeLines(file_csl, "./navigation_default.csl")


        file_bib <- readLines(bibfile, encoding = "UTF-8")
        writeLines(c("\n", file_bib, "\n"), "./MEMIO.bib")

        if (input$bibabbr == "onlyabbrjournal") {
          # print("s1")
          abbr_bib_only_journal(file = "MEMIO.bib", out.file = "MEMIO_default.bib",
                                user.csv = csvfile, sep = input$csvsep)

        } else if (input$bibabbr == "abbrwithbeatify") {
          # print("s2")
          dt_temp <- abbr_bib( file = "MEMIO.bib", out.file = "MEMIO_default.bib",
                               input$connectauthor, user.csv = csvfile, sep = input$csvsep)
          rm(dt_temp)
        } else {
          # print("s3")
          file.copy("MEMIO.bib", "MEMIO_default.bib", overwrite = TRUE)
        }

        dt <- abbr_bib( file = "MEMIO_default.bib", out.file = "MEMIO_default_temp.bib",
                        input$connectauthor, user.csv = csvfile, sep = input$csvsep)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        clear_file()
        stop(safeError(e))
      }
    )

    new_dt <- tex_diff_bib(texfile = texfile, dt)
    return(new_dt)
  })

  observeEvent(input$bibabbr, ignoreNULL = TRUE, {
    if (input$bibabbr == "onlyabbrjournal") {
      output$connectauthor <- renderUI( NULL )
    } else {
      output$connectauthor <- renderUI({
        selectInput("connectauthor", "Multiple author connectives:",
                    choices = c(
                      "nothing" = "nothing",
                      "and" = "and",
                      "\\\\&" = "\\\\&",
                      "&" = "&"
                    ),
                    selected = "nothing"
        )
      })
    }
  })

  observeEvent(input$file4_usercsv$datapath, ignoreNULL = TRUE, {
    if (is.null(input$file4_usercsv$datapath)) {
      output$csvsep <- renderUI( NULL )
    } else {
      output$csvsep <- renderUI({
        selectInput("csvsep", "Separator of csv file:",
                    choices = c(
                      "auto" = "auto",
                      "," = "comma",
                      ";" = "semicolon"
                    ),
                    selected = "auto"
        )
      })
    }
  })
  tex_cite_style <- reactive({
    new_dt <- randomVals()
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "It is being calculated, please wait patiently!!!", value = 0)

    # Convert Rmd files to latex files by pandoc
    rmarkdown::render("rmd_finally.Rmd", output_format = latex_document())
    value <- style_fun(texfile = "rmd_finally.tex")
    clear_file()
    return(value)
  })


  ###########################################################
  ############ Output warning,----- The key exist in the .tex file, but don't exist in bib file ###############
  output$out_warning <- renderText({
    new_dt <- randomVals() # return a list
    if (length(new_dt) > 1 & !is_empty(new_dt)) {
      return("no warning")
    }
  })
  #######################################################

  ############ Output Style 1 -- segmented style ###################
  output$out_style1 <- renderText({
    tex_cite_style()[[1]]
  })
  output$out_style1_clip <- renderUI({
    rclipButton(inputId = "out_style1_clip", label = "style1 Copy", clipText = tex_cite_style()[[1]], icon = icon("clipboard"))
  })
  ##########################################################

  ############ Output style 2 -- unsegmented style ####################
  output$out_style2 <- renderText({
    tex_cite_style()[[2]]
  })
  output$out_style2_clip <- renderUI({
    rclipButton(inputId = "out_style2_clip", label = "style2 Copy", clipText = tex_cite_style()[[2]], icon = icon("clipboard"))
  })

  # #####################################################################################
  # #####  Output the type of reference  #################################################
  # ##### 1 stands for the numeric label ,  0 stands for author-year label ################
  output$out_is_clstype2 <- output$out_is_clstype1 <- renderText({
    is_num <- tex_cite_style()[[3]]
    if (is_num) {
      "1 stands for the numeric label"
    } else {
      "0 stands for the author-year label"
    }
  })


  ######################### Output journal abbreviation comparison table -- begin ###############
  output$jouranl_abbr <- renderDataTable(
    {
      dt <- randomVals() # return a list
      if (is_empty(dt)) {
        temp <- data.frame("NOTE" = "No corresponding abbreviation was found in the whole bib file!!!")
        return(temp)
      } else {
        col <- c("fz_id", "tex_id", "CKEY", "JOURNAL", "journal_abbr", "originFile")
        stopifnot(all(col %in% colnames(dt)))
        temp_dt <- dt[, col, with = F]
        return(as.data.frame(temp_dt))
      }
    },
    options = list(pageLength = 100)
  )
  ######################### Output journal abbreviation comparison table -- end ###############


  ######################### Journal abbreviation query -- begin ###############
  journamevalue <- eventReactive(input$goJournameQuery, {
    journame_lower <- str_to_lower(input$Journame) # Convert the entered value to lowercase
    journame_lower <- str_split(journame_lower, pattern = "\\n")[[1]]
    journame_lower <- str_squish(journame_lower)
    journame_lower <- journame_lower[nchar(journame_lower) >= 4]
    journame_lower_dt <- data.table("journal_lower" = journame_lower) # vector

    abbrtable_sys <- journalabbr:::abbrtable_sys[, lapply(.SD, function(x) stringi::stri_escape_unicode(x))]
    abbrtable_sys[journame_lower_dt, on = "journal_lower"]
  })
  output$JournameAbbr <- renderDataTable(
    {
      temp <- journamevalue()
      if (nrow(temp) > 400) {
        warning("The number returned by the query is too large. The maximum number of returned data is 400")
        temp <- temp[1:400]
      }
      temp
    },
    options = list(pageLength = 100)
  )
  ######################### Journal abbreviation query -- end ###############
  ############################################################################


  ######################### Output cite bib and key   -- begin ######################
  out_yinyong <- reactive({
    new_dt <- randomVals()
    new_dt2 <- new_dt[!is.na(new_dt$tex_id), ]
    if (input$inSelect == "In the order cited in Tex") {
      new_dt2 <- new_dt2[order(tex_id), ]
    } else if (input$inSelect == "Ascending order by key") {
      new_dt2 <- new_dt2[order(CKEY), ]
    } else if (input$inSelect == "Descending order by key") {
      new_dt2 <- new_dt2[order(-CKEY), ]
    } else if (input$inSelect == "Ascending order by bib type") {
      new_dt2 <- new_dt2[order(ITYPE), ]
    } else if (input$inSelect == "Descending order by bib type") {
      new_dt2 <- new_dt2[order(-ITYPE), ]
    } else {
      new_dt2 <- new_dt2[order(fz_id), ] ##### Sort by bib file
    }

    if (is_empty(new_dt2)) {
      s_temp <- "None of the articles were cited in bib file."
      return(list(s_temp, s_temp))
    } else {
      tex_key <- purrr::map(new_dt2$fz_rawchar, function(x) {
        paste0(x, collapse = "\n")
      })

      cite_bib <- paste(tex_key, collapse = "\n\n")
      cite_bib <- paste0("Total: ", nrow(new_dt2), " references were cited.\n\n", cite_bib)
      cite_key <- paste(new_dt2$CKEY, collapse = "\n")
      return(list(cite_key, cite_bib))
    }
  })

  output$key01yinyongclip <- renderUI({
    rclipButton(inputId = "key01yinyongclip", label = "key cite Copy", clipText = out_yinyong()[[1]], icon = icon("clipboard"))
  })
  output$key01yinyong <- renderText({
    out_yinyong()[[1]]
  })
  output$bib01yinyongclip <- renderUI({
    rclipButton(inputId = "bib01yinyongclip", label = "bib cite Copy", clipText = out_yinyong()[[2]], icon = icon("clipboard"))
  })
  output$bib01yinyong <- renderText({
    out_yinyong()[[2]]
  })

  ######################### Output cite bib and key   -- end ######################

  ######################### Output nocite bib and key   -- begin ######################
  out_no_yinyong <- reactive({
    new_dt <- randomVals()
    if (sum(is.na(new_dt$tex_id)) == 0) {
      s_temp <- "Perfect. (that is, all references in the BIB file are cited)"
      return(list(s_temp, s_temp))
    } else {
      dtSub <- new_dt[is.na(new_dt$tex_id), ]
      nocite <- purrr::map(dtSub$fz_rawchar, function(x) {
        paste0(x, collapse = "\n")
      })
      nocite_bib <- paste(nocite, collapse = "\n\n")
      nocite_bib <- paste0("Total: ", nrow(dtSub), " references were not cited.\n\n", nocite_bib)
      nocite_key <- paste(dtSub$CKEY, collapse = "\n")
      return(list(nocite_key, nocite_bib))
    }
  })

  output$key02noyinyongclip <- renderUI({
    rclipButton(inputId = "key02noyinyongclip", label = "key no cite Copy", clipText = out_no_yinyong()[[1]], icon = icon("clipboard"))
  })
  output$key02noyinyong <- renderText({
    out_no_yinyong()[[1]]
  })
  output$bib02noyinyongclip <- renderUI({
    rclipButton(inputId = "bib02noyinyongclip", label = "bib no cite Copy", clipText = out_no_yinyong()[[2]], icon = icon("clipboard"))
  })
  output$bib02noyinyong <- renderText({
    out_no_yinyong()[[2]]
  })
  ######################### Output nocite bib and key   -- end ######################




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
