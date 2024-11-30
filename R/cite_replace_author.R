#把数字引用转为 作者年份引用
single_replace_author <- function(text,
                          latex_prefix=c("cite", "upcite", "citep", "citet")) {
  stopifnot(is.character(text), length(text) == 1)

  pattern <-  sprintf("\\\\(%s)\\{([^}]+)\\}", paste(latex_prefix, collapse = "|"))
  matches <- gregexpr(pattern, text, perl = TRUE)
  matched_texts <- regmatches(text, matches)[[1]]

  if (length(matched_texts) ==0 ) return(text)


  for (i in seq_along(matched_texts)) {
    text <- gsub(matched_texts[i], paste(" (", matched_texts[i], ")", sep = ""), text, fixed = TRUE)
  }
  # 把多余的空格去掉
  text = gsub(" +", " ", text)

  return(text)
}


### 把数字引用转为 作者年份引用， 即在\cite{...} 前后加上括号，变为 (\cite{...})
cite_replace_author = function(tex){
  new_tex = sapply(tex, single_replace_author)
  return(new_tex)
}





