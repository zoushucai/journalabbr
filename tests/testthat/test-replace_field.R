test_that("Determine data type, Function replace_field()", {

  csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
  file <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)

  dt <- read_bib2dt(file)
  abbrtable_user <- get_abbrtable(user_table = csvpath, use_sys_table =TRUE)

  dm1 <- replace_field(dt,
                      oldfield = "JOURNAL",
                      newfield = "JOURNAL",
                      user_table = csvpath,
                      use_sys_table =TRUE,
                      fun = NULL)


  myauthor <- function(x){ gsub(" and ", " & ", x, perl = TRUE, ignore.case = TRUE) }
  dm2 <- replace_field(dm1,
                      oldfield = "AUTHOR",
                      newfield = "AUTHOR",
                      user_table = NULL,
                      use_sys_table =FALSE,
                      fun = myauthor)

  expect_true(is.data.table(dm1) && is.data.table(dm2))
})
