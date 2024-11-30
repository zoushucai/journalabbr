library(testthat)

test_that("conversion works for convert_citations_vec()", {

  # Test input for LaTeX to Markdown conversion
  text_latex = c(
    "This is a citation \\upcite{key1,key2} and \\citep{key3-a }.",
    "This is a citation \\citet{key1,key2} and \\citet{key3-a }.",
    "this is no citation",
    "",  # An empty string
    "This is a citation \\upcite{key1,key2} and \\cite{key3-a }."
  )

  # Expected output after conversion to Markdown
  expected_md = c(
    "This is a citation [@key1; @key2] and [@key3-a].",
    "This is a citation [@key1; @key2] and [@key3-a].",
    "this is no citation",
    "",  # An empty string
    "This is a citation [@key1; @key2] and [@key3-a]."
  )

  # Perform the conversion
  converted_md = convert_citations_vec(text_latex, tex2md = TRUE)

  # Check if the converted output matches expected output
  expect_equal(converted_md, expected_md)

  # Test input for Markdown to LaTeX conversion
  text_md = c(
    "This is a citation [@key1; @key2] and [@key3-a].",
    "This is a citation2 [@key1; @key2] and [@key3-a].",
    "this is no citation",
    "",  # An empty string
    "This is a citation3 [@key1; @key2] and [@key3-a]."
  )

  # Expected output after conversion to LaTeX with 'upcite' prefix
  expected_latex = c(
    "This is a citation \\upcite{key1,key2} and \\upcite{key3-a}.",
    "This is a citation2 \\upcite{key1,key2} and \\upcite{key3-a}.",
    "this is no citation",
    "",  # An empty string
    "This is a citation3 \\upcite{key1,key2} and \\upcite{key3-a}."
  )

  # Perform the conversion
  converted_latex = convert_citations_vec(text_md, tex2md = FALSE, latex_prefix = "upcite")

  # Check if the converted output matches expected output
  expect_equal(converted_latex, expected_latex)

})
