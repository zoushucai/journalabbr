## 功能

针对bibtex 文件中的`journal` 字段实现期刊缩写

## 安装

```R
devtools::install_github("zhoushucai/journalabbr")
```

## 使用

```{r}
library(journalabbr)
path = system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
abbr2bib(file = path,outfile_abbr= "abbr.bib")

```

