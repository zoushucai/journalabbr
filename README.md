## Function

Implementing journal abbreviation for the 'Journal' field in BibTex file.

## Install

``` r
# CRAN
install.packages("journalabbr")

#
devtools::install_github("zoushucai/journalabbr")
# or
xfun::install_github("zoushucai/journalabbr")
```

## Require

The format of the bib file is as follows:

``` latex
@***{****,
  **** = {****},
  **** = "*****",
  *** = {{******}},
  **** = {*****}}

% or

@***{****,
  **** = {****},
  **** = "*****",
  *** = {{******}},
  **** = {*****}
  }
  
```

Except for the `@`character line, the rest of the field lines must have an equal sign `=`

## Use

``` r
library(journalabbr)
path = system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
temptab = abbr2bib(file = path, outfile =  tempfile(fileext = ".bib"))

# or
journalabbr::runExample()
```

### Access internal data

``` r
library(journalabbr)
library(stringi)
abbrTable = journalabbr:::abbrTable
# Unicode to UTF-8
abbrTable = as.data.frame(
    lapply(abbrTable,
           function(x)stringi::stri_unescape_unicode(x))
           )
```

## Online application

[shiny online](https://zoushucai.shinyapps.io/shiny_cankaowenxian/)

PS: Due to the R version of [www.shinyapps.io](https://www.shinyapps.io/), there may be some problems, so it is recommended to download the package and run it locally. The latest version is recommended (pandoc \>= 2.11.0).

``` r
journalabbr::runExample()
```
