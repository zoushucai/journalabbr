library(usethis)
library(devtools)


file.edit("devtools_history.R") # 创建一个文件,
usethis::use_build_ignore("devtools_history.R")   # 把这个文件进行忽略

usethis::use_build_ignore(".lintr")
file.edit(".lintr") # 创建一个文件,



use_gpl3_license()
use_cran_comments()
use_data_table()
use_pipe()
styler::style_file("metadata/combine_journal_lists.R")

lintr::lint("metadata/combine_journal_lists.R")
usethis::use_testthat()


styler::style_file("R/read_bib2dt.R")
lintr::lint("R/read_bib2dt.R")



styler::style_file("R/add_abbrtable.R")
lintr::lint("R/add_abbrtable.R")

styler::style_file("R/replace_field.R")
lintr::lint("R/replace_field.R")

styler::style_file("R/abbr_bib.R")
lintr::lint("R/abbr_bib.R")
document()
styler::style_pkg()
lintr::lint_package()

use_package("purrr")
use_package("stringr")

use_import_from("purrr",'map')
use_import_from("purrr",'map2')
use_import_from("purrr",'map_chr')

use_import_from("tidytable",'as_tidytable')
use_import_from("tidytable",'enframe')

use_import_from("data.table",'fread')
use_import_from("data.table",'as.data.table')
use_import_from("data.table",'rbindlist')
use_import_from("data.table",'merge.data.table')

use_import_from("data.table",'is.data.table')
use_import_from("data.table",'setnames')

use_import_from("stringr",'str_squish')
use_import_from("stringr",'str_replace_all')
use_import_from("stringr",'str_extract')
use_import_from("stringr",'str_to_upper')
use_import_from("stringr",'str_split')
use_import_from("stringr",'str_dup')
use_import_from("stringr",'str_pad')
use_import_from("stringr",'str_count')
use_import_from("stringi",'stri_unescape_unicode')
document()
check()


styler::style_file("inst/shiny-examples/appckwx/app.R")
lintr::lint("inst/shiny-examples/appckwx/app.R")


rm(list=ls())

load_all()
devtools::install()

document()
devtools::check(args = c('--as-cran'))
devtools::check_rhub()

# usethis::use_readme_rmd()
usethis::use_testthat()
roxygen2::roxygenise()
devtools::document(roclets=c('rd', 'collate', 'namespace'))


devtools::build_readme()

document()
devtools::check(args = c('--as-cran'))
devtools::check_win_devel()
rhub::check()

rhub::check_on_windows()
devtools::check_rhub()
rhub::check_on_debian()
rhub::check_for_cran()


use_travis()
use_travis_badge()
use_lifecycle_badge()
use_cran_badge()
covr::package_coverage(type="all")# 本地查看代码覆盖率
use_coverage() # 添加一个徽章到readme中, 以及会创建一个yml, 每次提交github都会检查代码覆盖率

use_github_action("test-coverage")#检测 test的覆盖率

# 创建一系列徽章
use_cran_badge()
use_lifecycle_badge("stable")
use_lifecycle()

#添加测试
# ✖ add_abbrtable.R:14: @testexamples is not a known tag.
# ✖ read_bib2dt.R:21: @testexamples is not a known tag.
# ✖ replace_field.R:23: @testexamples is not a known tag.
use_test("add_abbrtable.R")

use_test("read_bib2dt")
use_test("replace_field.R")

devtools::build_readme()
rm(list = ls())
document()
devtools::test()
load_all()
file = "/Users/zsc/Desktop/rmd/weakfuzzyaik.bib"
out.file = "/Users/zsc/Desktop/aa3.bib"
user.csv = ''
author.connect = c("nothing", "\\\\&", "&", "and")
kk = abbr_bib(file = "/Users/zsc/Desktop/rmd/weakfuzzyaik.bib",
         out.file = "/Users/zsc/Desktop/aa3.bib",
         user.csv = '' )

abbr_bib_only_journal(file = "/Users/zsc/Desktop/rmd/weakfuzzyaik.bib",
                      out.file = "/Users/zsc/Desktop/aa2.bib",
                      user.csv = '' )

