# journalabbr 0.4.0

The underlying data has been reconstructed. Now, the `data.table` format is adopted.

- Use `use_this` and `lintr` package to build the R package
- Add `NEWS.md`, `cran-comments.md`
- Add shiny: `journalabbr::run_example()`
- Two main functions: `abbr_bib()` and `abbr_bib_only_journal()`, the other functions are auxiliary functions.






# journalabbr 0.3.1 before

-  allow user to supply custom journal abbreviations, use parameter `addcsvpath` in function `abbr2bib()`.
- add function: `rbind_bib()` merges multiple BiB data frames, which are returned by `read_bib2tib()`
- add function: rbindbib merges multiple Bib data frames, which are returned by .
- add a function: `unique_bib()` to remove duplicate tibble. Often, the bib file should ensure that the key is unique. When there is a duplicate, you should change the duplicate key or delete the duplicate item. Here we use the latter to delete the duplicate. Repeat and filter according to the `keybib` column
- add function: `read_bib2tib()`  and `write_tib2bib()`
- main function: `abbr2bib()`
- add shiny:   `journalabbr::runExample()`



