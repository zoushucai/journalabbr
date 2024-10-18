# Read from .bib file:
require(journalabbr)
require(purrr)
require(data.table)
file1 <- system.file("extdata", "testfile_1.bib", package = "journalabbr", mustWork = TRUE)
dt1 <- read_bib2dt(file1)
colnames(dt1)

file2 <- system.file("extdata", "testfile_2.bib", package = "journalabbr", mustWork = TRUE)
dt2 <- read_bib2dt(file2)
colnames(dt2)

# A valid BibTeX entry as a character vector
item1 <- c(
  "@Article{switalski2003general,",
  "author = {Switalski, Zbigniew},",
  "journal = {Fuzzy Sets and Systems},",
  "title = {General transitivity conditions for fuzzy reciprocal preference matrices},",
  "year = {2003},",
  "number = {1},",
  "pages = {85--100},",
  "volume = {137},",
  "publisher = {Elsevier},",
  "}"
)

item2 <- c(
  "@Article{switalski2003general,",
  "author = {Switalski, Zbigniew},",
  "journal = {Fuzzy Sets and Systems},",
  "title = {General transitivity conditions for fuzzy reciprocal preference matrices},",
  "year = {2003}}"
)

item3 <- c(
  "@Article{switalski2003general,",
  "author = {Switalski, Zbigniew},",
  "journal = {Fuzzy Sets and Systems},",
  "title = {General transitivity conditions for fuzzy reciprocal preference matrices},",
  "year = {2003}"
)
item = list(item1, item2, item3)

# Check validity of the entries (inner function)
map(item, journalabbr:::checkitem_valid)
map(item, journalabbr:::extract_fields)
rbindlist(map(item, journalabbr:::extract_fields), fill=TRUE, use.names = TRUE)
rbindlist(map(item, journalabbr:::extract_fields, check=TRUE), fill=TRUE, use.names = TRUE)

