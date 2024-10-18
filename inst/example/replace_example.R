csvpath <- system.file("extdata", "myabbr.csv", package = "journalabbr", mustWork = TRUE)
file <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
dt <- read_bib2dt(file)

abbrtable_user = get_abbrtable(user_table = csvpath, use_sys_table =TRUE)
print(head(abbrtable_user))
dm1 = replace_field(dt,
                    oldfield = "JOURNAL",
                    newfield = "JOURNAL",
                    user_table = csvpath,
                    use_sys_table =TRUE,
                    fun = NULL)

myauthor = function(x){ gsub(" and ", " & ", x, perl = TRUE, ignore.case = TRUE) }
dm2 = replace_field(dm1,
                    oldfield = "AUTHOR",
                    newfield = "AUTHOR",
                    user_table = NULL,
                    use_sys_table =FALSE,
                    fun = myauthor)
print(head(dt)[, c("JOURNAL", "AUTHOR"), with = FALSE])
print(head(dm1)[, c("JOURNAL", "AUTHOR"), with = FALSE])
print(head(dm2)[, c("JOURNAL", "AUTHOR"), with = FALSE])

