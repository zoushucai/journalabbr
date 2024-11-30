library(stringr)

## 提取text文本中的所有引用
single_extract_cites = function(text){
  ## 提取tex中的所有引用
  latex_prefix=c("cite", "upcite", "citep", "citet")
  # 提取tex中的所有引用
  pattern <- sprintf("(?<=\\\\(%s)\\{)([^}]+)(?=\\})", paste(latex_prefix, collapse = "|"))

  cites = str_extract_all(text, pattern) |>  unlist()
  if(length(cites) == 0){
    return(NULL)
  }
  #按照逗号分隔
  cites = strsplit(cites, ",") |> unlist()
  return(cites)
}






single_order_replace <- function(text, cites_order_all,
                          latex_prefix=c("cite", "upcite", "citep", "citet")) {
  stopifnot(is.character(text), length(text) == 1)

  pattern <-  sprintf("\\\\(%s)\\{([^}]+)\\}", paste(latex_prefix, collapse = "|"))
  matches <- gregexpr(pattern, text, perl = TRUE)
  matched_texts <- regmatches(text, matches)[[1]]

  if (length(matched_texts) ==0 ) return(text)



  replacements <- lapply(matched_texts, function(match) {
    temp <- gsub(pattern, "\\2", match, perl = TRUE)
    refs <- trimws(unlist(strsplit(temp, ",")))
    if(length(refs) == 1){
      return( paste0(sprintf("\\%s{", latex_prefix[1]), refs, "}"))
    }
    # 按照cites_order的顺序排列
    refs_order = refs[match(cites_order_all, refs, nomatch = 0)]

    refs_order = paste(refs_order, collapse = ", ")
    paste0(sprintf("\\%s{", latex_prefix[1]), refs_order, "}")
  })
  for (i in seq_along(matched_texts)) {
    text <- gsub(matched_texts[i], replacements[[i]], text, fixed = TRUE)
  }
  return(text)
}


### 读取tex文件，找到所有的引用，然后按照引用的顺序，替换cite里面的引用
cites_order_replace = function(tex){
  # 1. 找到所有的引用
  #这是这个文章的所有引用的排序(按照出现的先后顺序排序的)
  cites_order_all = lapply(tex, single_extract_cites) |> unlist() |> str_trim() |> unique()
  # 2. 替换引用
  new_tex = sapply(tex, function(x) single_order_replace(x, cites_order_all))
  return(new_tex)
}


