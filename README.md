## Function

Implementing journal abbreviation for the 'Journal' field in BibTex file

## Install

```R
devtools::install_github("zhoushucai/journalabbr")
```

## Use

```{r}
library(journalabbr)
path = system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
abbr2bib(file = path,outfile_abbr= "abbr.bib")

```

