library(usethis)
library(devtools)


file.edit("devtools_history.R") # to create a file
usethis::use_build_ignore("devtools_history.R")   #  to ignore the file

usethis::use_build_ignore(".lintr")
file.edit(".lintr") # to create a file



use_gpl3_license()
use_cran_comments()
use_data_table()
use_pipe()
styler::style_file("metadata/combine_journal_lists.R")

lintr::lint("metadata/combine_journal_lists.R")
usethis::use_testthat()

list.files("R/", pattern = "*.R")
styler::style_file("R/bib2bib.R")
lintr::lint("R/bib2bib.R")

styler::style_file("R/get_abbrtable.R")
lintr::lint("R/get_abbrtable.R")

styler::style_file("R/get_delcolumns.R")
lintr::lint("R/get_delcolumns.R")

styler::style_file("R/read_bib2dt.R")
lintr::lint("R/read_bib2dt.R")

styler::style_file("R/replace_field.R")
lintr::lint("R/replace_field.R")


styler::style_file("R/write_dt2bib.R")
lintr::lint("R/write_dt2bib.R")

styler::style_file("R/bib2bib.R")
lintr::lint("R/bib2bib.R")

document()
styler::style_pkg()
lintr::lint_package()

use_package("purrr")
use_package("stringr")
use_r("bib2bib.R")

use_import_from("stringr",'str_detect')

use_import_from("stringi",'stri_unescape_unicode')
document()
check()


styler::style_file("inst/shiny-examples/appckwx/app.R")
lintr::lint("inst/shiny-examples/appckwx/app.R")


rm(list=ls())

load_all()
devtools::install()

devtools::document()
devtools::check(args = c('--as-cran'))
devtools::check_rhub()

# usethis::use_readme_rmd()
usethis::use_testthat()
roxygen2::roxygenise()
devtools::document(roclets=c('rd', 'collate', 'namespace'))


devtools::build_readme()
devtools::load_all()
devtools::document() #生成帮助文档
help(write_dt2bib)  # 查看生成的帮助文档,
devtools::check(args = c('--as-cran'))
devtools::check_win_devel()
rhub::check()

# check on different platforms
rhub::check_on_windows()
devtools::check_rhub()
rhub::check_on_debian()
rhub::check_for_cran()


use_travis()
use_travis_badge()
use_lifecycle_badge()
use_cran_badge()
covr::package_coverage(type="all")# test coverage badge
use_coverage() # add a badge to README.md

use_github_action("test-coverage") # test coverage action


# to create badges
use_cran_badge()
use_lifecycle_badge("stable")
use_lifecycle()

# add test code
use_test("add_abbrtable.R")
use_test("read_bib2dt")
use_test("replace_field.R")

devtools::build_readme()
rm(list = ls())
document()
devtools::test()
load_all()


