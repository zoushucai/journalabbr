bib_file <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
csv_file <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
output_file <- tempfile(fileext = ".bib")

dt <- bib2bib(file = bib_file,
        out.file = output_file,
        user_table = csv_file,
        use_sys_table = TRUE,
        fun = function(x) {
          gsub(" and ", " & ", x, perl = TRUE, ignore.case = TRUE)
        })
