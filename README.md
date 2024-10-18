
<!-- README.md is generated from README.Rmd. Please edit that file -->

# journalabbr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/journalabbr)](https://CRAN.R-project.org/package=journalabbr)
![r-universe](https://fastverse.r-universe.dev/badges/journalabbr)
[![downloads](http://cranlogs.r-pkg.org/badges/grand-total/journalabbr?color=blue)](https://cran.r-project.org/package=journalabbr)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/journalabbr?color=blue)](https://github.com/zoushucai/journalabbr)
[![R build
status](https://github.com/zoushucai/journalabbr/workflows/R-CMD-check/badge.svg)](https://github.com/zoushucai/journalabbr/actions)
[![Codecov test
coverage](https://codecov.io/gh/zoushucai/journalabbr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/zoushucai/journalabbr?branch=main)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
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
# 1. load abbrtable_user
csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
abbrtable_user <- data.table::fread(file = csvpath, header = TRUE, sep = ",")
abbrtable_user <- abbrtable_user[, lapply(.SD, stringr::str_squish)]
colnames(abbrtable_user) <- c("journal_lower", "journal_abbr")
abbrtable_user[, journal_lower := tolower(journal_lower)]

# 2.read *.bib file
file <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
dt <- read_bib2dt(file)
#> Warning in read_bib2dt(file): ====== Duplicate key in uploaded Bib file =======:
#> meenakshi2019fuzzy
#> xu2014ordinal
#> ===============================================

# 3. handle dt
dm1 = replace_field(dt,
                    oldfield = "JOURNAL",
                    newfield = "JOURNAL",
                    user_table = abbrtable_user,
                    use_sys_table =TRUE,
                    fun = NULL)
myauthor = function(x){ gsub(" and ", " & ", x, perl = TRUE, ignore.case = TRUE) }
dm2 = replace_field(dm1,
                    oldfield = "AUTHOR",
                    newfield = "AUTHOR",
                    user_table = NULL,
                    use_sys_table =FALSE,
                    fun = myauthor)
# 4. Write to .bib file:
write_dt2bib(dm2, file = tempfile(fileext = ".bib"))

print(head(dt)[, c("JOURNAL", "AUTHOR"), with = FALSE])
#>                                     JOURNAL
#>                                      <char>
#> 1:                                     <NA>
#> 2:                                     <NA>
#> 3: European Journal of Operational Research
#> 4:                   Fuzzy Sets and Systems
#> 5:                                     <NA>
#> 6:                            The R Journal
#>                                                 AUTHOR
#>                                                 <char>
#> 1:            Osborne, Martin J. and Rubinstein, Ariel
#> 2:                                       Meenakshi, AR
#> 3:        Aguar{\\'o}n, Juan and {Moreno-Jim{\\'e}nez}
#> 4: Xu, Yejun and Gupta, Jatinder N.D. and Wang, Huimin
#> 5:                                       Meenakshi, AR
#> 6:                                         Dianne Cook
print(head(dm1)[, c("JOURNAL", "AUTHOR"), with = FALSE])
#>               JOURNAL                                              AUTHOR
#>                <char>                                              <char>
#> 1:               <NA>            Osborne, Martin J. and Rubinstein, Ariel
#> 2:               <NA>                                       Meenakshi, AR
#> 3:               <NA>                                       Meenakshi, AR
#> 4: Eur. J. Oper. Res.        Aguar{\\'o}n, Juan and {Moreno-Jim{\\'e}nez}
#> 5:   Fuzzy Sets Syst. Xu, Yejun and Gupta, Jatinder N.D. and Wang, Huimin
#> 6:   Fuzzy Sets Syst. Xu, Yejun and Gupta, Jatinder N.D. and Wang, Huimin
print(head(dm2)[, c("JOURNAL", "AUTHOR"), with = FALSE])
#>               JOURNAL                                          AUTHOR
#>                <char>                                          <char>
#> 1:               <NA>          Osborne, Martin J. & Rubinstein, Ariel
#> 2:               <NA>                                   Meenakshi, AR
#> 3:               <NA>                                   Meenakshi, AR
#> 4: Eur. J. Oper. Res.      Aguar{\\'o}n, Juan & {Moreno-Jim{\\'e}nez}
#> 5:   Fuzzy Sets Syst. Xu, Yejun & Gupta, Jatinder N.D. & Wang, Huimin
#> 6:   Fuzzy Sets Syst. Xu, Yejun & Gupta, Jatinder N.D. & Wang, Huimin
```

or run shiny

``` r
journalabbr::run_example()
```

or run shiny online

website: <https://zoushucai.shinyapps.io/shiny_cankaowenxian/>

- It needs to be manually pushed to `shinyapps.io`, and the
  implementation is also very simple. Simply click the mouse in
  `Rstudio` to operate it

### Access internal data

``` r
abbrtable = journalabbr:::abbrtable_sys
colnames(abbrtable)
#> [1] "journal_lower" "journal_abbr"  "originFile"
```
