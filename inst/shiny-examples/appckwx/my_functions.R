pacman::p_load(
  "shiny", "stringr", "stringi", "data.table",
  "rclipboard", "knitr", "rmarkdown", "purrr", "tidytable",
  "tinytex", "DT", "shinydashboard", "journalabbr"
)



fixed_clsfile = "./default.csl"
fixed_qmdfile = "./qmd_default.qmd"
fixed_bibfile = "./MEMIO_default.bib"
output_qmd = "qmd_finally.qmd"
output_tex = gsub("\\.qmd$", ".tex", output_qmd)#这个是根据 output_qmd 自动产生的


clear_file <- function(pattern = "(.*\\.R$)|(.*\\.Rproj$)") {
  tryCatch(
    {
      now_dir <- list.files()
      old_dir <- list.files(pattern = pattern)
      delete_dir <- setdiff(now_dir, old_dir)
      files_to_remove <- delete_dir[!file_test("-d", delete_dir)] # 过滤非目录文件
      file.remove(files_to_remove) # 一次性删除文件
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      warning("An error occurred: ", e$message)
      clear_file()
      stop(safeError(e))
    }
  )
}
############################################################################################
## 1, 通过调用 qmd, 生成 tex 文件
## eg: output_qmd --> qmd_finally.tex, 然后对这个生成的 tex 进行数据清洗操作
############################################################################################
read_tex <- function(texfile){
  #### 1.Read the original Tex file and extract the key---- ready to form the second half of the .Rmd
  ###############################################
  document = readLines(texfile, encoding = "UTF-8")
  document <- str_squish(document)
  document <- gsub("^%.*", "", document, perl = TRUE) # Delete everything starting with %.
  document <- gsub("([^\\\\])(%.*)", "\\1", document, perl = TRUE) # Delete everything after % that doesn't start with \\
  document <- document[which(document != "")]
  return(document)
}
extract_key <- function(texfile) {
  #### 1.Read the original Tex file and extract the key---- ready to form the second half of the .Rmd
  document <- read_tex(texfile)
  # extract key
  tex_key <- unlist(str_extract_all(document, "(?<=\\\\cite[pt]?\\{).*?(?=\\})"))
  # There are multiple keys in a \cite{***,***,***}, Split with ',' and remove any extra Spaces,
  tex_key <- unique(str_squish(unlist(str_split(tex_key, ","))))
  return(tex_key)
}
copy_file = function(input, out){
  # 先读取后，写入实现拷贝文件的操作
  doc <- readLines(input, encoding = "UTF-8")
  writeLines(doc, out)
}
generate_qmd = function(ckey, output, qmd_template=NULL){
  ckeytext = paste(sprintf("[@%s]", ckey), collapse = "\n\n")
  if(is.null(qmd_template)){
    qmd_template <- system.file("template", "qmd_default.qmd", package = "journalabbr", mustWork = TRUE)

  }
  # 两个部分进行拼接
  head = readLines(qmd_template, encoding = "UTF-8")
  body = ckeytext
  writeLines(c(head, "\n\n", body, "\n\n"), output)
}







#### 开始对 qmd_finally.tex 进行操作 ##
# 0. 先加载qmd_finally.tex 文件,然后进行一定的清洗,得到原始的doc
# 1. 通过对 doc 进行提取, extract_body, 找到 body 部分,
#   然后对 body 部分进行清洗, 提取出一个 dt_body 数据框, 只含有两列: ckey 和 refvalue
# 2. 通过对 doc 进行提取, extract_ref, 找到 ref 部分,
#   然后对 ref 部分进行清洗, 提取出一个 dt_ref 数据框, 只含有两列: ckey 和 bibitem
# 3. 进行联合,得到一个 dt
# 4. 根据 dt 的 refvalue 列可以判断出引用的风格是数字还是作者年
# 4. 根据 dt 可以生成新的参考文献格式.

# 定义函数
extract_body <- function(doc) {
  # 找到 \maketitle 行的索引
  from <- grep("\\\\maketitle", doc) + 1

  # 找到所有 \citeproc{ 的行的索引
  indices <- grep("\\\\citeproc\\{", doc)

  # 获取最后一行的索引
  to <- if (length(indices) > 0) indices[length(indices)] else NA

  # 检查有效性
  if (is.na(to) || from > to) {
    warning("No valid body found between \\maketitle and the last \\citeproc{.")
    return(NULL)
  }

  # 提取文档主体
  docbody <- doc[from:to]
  return(docbody)
}
extract_ref <- function(doc) {
  # 找到 \begin{CSLReferences} 和 \end{CSLReferences} 的行索引
  begin_index <- grep("\\\\begin\\{CSLReferences\\}", doc)
  end_index <- grep("\\\\end\\{CSLReferences\\}", doc)

  # 检查是否找到索引
  if (length(begin_index) == 1 && length(end_index) == 1) {
    # 提取这两行之间的内容
    references_lines <- doc[(begin_index + 1):(end_index - 1)]

    # 返回提取的内容
    return(references_lines)
  } else {
    return(NULL)  # 如果没有找到，则返回 NULL
  }
}

#######
extract_citeproc <- function(text) {
  # 正则匹配模式，用于匹配 \citeproc{ref-***}{***} 结构
  pattern1 <- "(?<=\\\\citeproc\\{ref-)(.*?)(\\}\\{)(.*?)(?=\\})"
  temp <- str_extract(text, pattern1) # 获取 ***}{*** 的结构
  newtext <- str_split(temp, "\\}\\{", n = 2, simplify=T)  #然后按照 }{ 进行拆分,得到 key和 refvalue
  dt = as.data.table(newtext)
  colnames(dt) <- c("ckey", "refvalue")
  #去重
  dt = unique(dt)
  return(dt)
}

#######
ref2list = function(pattern, doc){
  # 向量doc, 根据搜索\\bibitem 进行划分, 划分为 list
  ##  \bibitem[\citeproctext]{ref-saaty2013modern}
  ## pattern = "\\\\bibitem\\[\\\\citeproctext"
  from <- grep(pattern, doc)
  to <- c(from[-1] - 1, length(doc))
  if (length(from) == 0L) {
    stop("There are no available references, please check the doc file.")
  }
  itemslist <- mapply(function(x, y) {
    return(doc[x:y])
  }, x = from, y = to, SIMPLIFY = FALSE)
  return(itemslist)
}
remove_brackets <- function(input_string) {
  # 使用 gsub 去掉 {[} 和 {]} 的括号
  cleaned_string <- gsub("\\{\\[\\}", "[", input_string, perl = TRUE)
  cleaned_string <- gsub("\\{\\]\\}", "]", cleaned_string, perl = TRUE)
  return(cleaned_string)
}
getfields <- function(item) {
  # 检查 item 是否至少有两个元素
  if (length(item) < 2) {
    stop("Item must have at least two elements")
  }

  # 提取 ckey
  ckey <- str_extract(item[1], "(?<=\\{ref-).*?(?=\\}$)")
  if (length(ckey) == 0) {
    ckey <- NA  # 如果没匹配到 ckey，设置为 NA
  }

  #  提取bibitem
  bibitem = paste(item[2:length(item)], collapse = " ")
  bibitem = remove_brackets(bibitem)
  return(list(ckey=ckey, bibitem=bibitem) )
}
extra_bibitem <- function(doc){
  # 1. 把文档划分为 list, 一个 list 代表一个参考文献
  itemslist = ref2list(pattern = "\\\\bibitem\\[\\\\citeproctext", doc)
  # 2. 对每个 list 进行信息提取
  itemslist = map(itemslist, getfields)
  # 3. 转为 data.table
  refdt = data.table(rbindlist(itemslist))
  return(refdt)
}

check_ref_type <- function(text) {
  # text 是一个字符向量

  # 定义正则表达式模式，检测是否为纯数字或数字范围（例如 12, 12-15）
  numeric_pattern <- "^\\d+(-\\d+)?$"

  # 检查每个元素是否符合数字风格
  is_numeric <- grepl(numeric_pattern, text)

  # 计算数字风格占比
  numeric_ratio <- sum(is_numeric) / length(text)

  # 判断占比是否大于等于 90%
  if (numeric_ratio >= 0.9) {
    warning("参考文献推算为： 数字风格")
    return(1)
  } else {
    warning("参考文献推算为： 作者-年风格")
    return(2)
  }
}
