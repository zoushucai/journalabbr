# Read from .bib file:
require(journalabbr)
file <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
bib <- read_bib2dt(file)

# Write to .bib file:
write_dt2bib(bib, file = tempfile(fileext = ".bib"))
