
<!-- README.md is generated from README.Rmd. Please edit that file -->

# journalabbr

<!-- badges: start -->
<!-- badges: end -->

## Function

Implementing journal abbreviation for the ‘Journal’ field in BibTex
file.

## Install

``` r
# CRAN
install.packages("journalabbr")

# or 
devtools::install_github("zoushucai/journalabbr")
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

Except for the `@`character line, the rest of the field lines must have
an equal sign `=`

## Use

``` r
require(journalabbr)
#> Loading required package: journalabbr
path0 <- system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
temp <- abbr_bib(file = path0, out.file = tempfile(fileext = ".bib"))
#> Warning in read_bib2dt(file): NA value exists in Citation Key, please check the
#> bib file
#> Warning in read_bib2dt(file): 
#> ====== Duplicate key in uploaded Bib file =======:
#> Binmore2008
#> Binmore2008
#> BrandenburgerDekel1989
#> Osborne1994
#> ===============================================
#> Warning in write_dt2bib(item_dt, outfile): NA value in CKEY or ITYPE field
colnames(temp)
#>  [1] "journal_lower" "journal_abbr"  "originFile"    "fz_id"        
#>  [5] "fz_rawchar"    "fz_char"       "CKEY"          "ITYPE"        
#>  [9] "TITLE"         "AUTHOR"        "ORGANIZATION"  "ADDRESS"      
#> [13] "YEAR"          "URL"           "ABSTRACT"      "NOTE"         
#> [17] "JOURNAL"       "VOLUME"        "NUMBER"        "PAGES"        
#> [21] "MONTH"         "PUBLISHER"     "BOOKTITLE"     "CHAPTER"      
#> [25] "EDITOR"        "SHORTTITLE"    "DOI"           "EDITION"      
#> [29] "SCHOOL"        "TYPE"          "INSTITUTION"

path <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
temptab <- abbr_bib(file = path, out.file = tempfile(fileext = ".bib"))
#> Warning in read_bib2dt(file): 
#> ====== Duplicate key in uploaded Bib file =======:
#> meenakshi2019fuzzy
#> xu2014ordinal
#> ===============================================
colnames(temptab)
#>  [1] "journal_lower" "journal_abbr"  "originFile"    "fz_id"        
#>  [5] "fz_rawchar"    "fz_char"       "CKEY"          "ITYPE"        
#>  [9] "TITLE"         "AUTHOR"        "PUBLISHER"     "YEAR"         
#> [13] "SHORTTITLE"    "VOLUME"        "PAGES"         "JOURNAL"      
#> [17] "LANGUAGE"      "NUMBER"        "MONTH"         "URL"

# add user csv
csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
temptab1 <- abbr_bib(file = path, out.file = tempfile(fileext = ".bib"), user.csv = csvpath)
#> Warning in read_bib2dt(file): 
#> ====== Duplicate key in uploaded Bib file =======:
#> meenakshi2019fuzzy
#> xu2014ordinal
#> ===============================================
colnames(temptab1)
#>  [1] "journal_lower" "journal_abbr"  "originFile"    "fz_id"        
#>  [5] "fz_rawchar"    "fz_char"       "CKEY"          "ITYPE"        
#>  [9] "TITLE"         "AUTHOR"        "PUBLISHER"     "YEAR"         
#> [13] "SHORTTITLE"    "VOLUME"        "PAGES"         "JOURNAL"      
#> [17] "LANGUAGE"      "NUMBER"        "MONTH"         "URL"

# no return value
abbr_bib_only_journal(file = path, out.file = tempfile(fileext = ".bib"), user.csv = csvpath)
#> NULL
```

or run shiny

``` r
journalabbr::run_example()
```

### Access internal data

``` r
abbrtable = journalabbr:::abbrtable_sys
colnames(abbrtable)
#> [1] "journal_lower" "journal_abbr"  "originFile"
```
