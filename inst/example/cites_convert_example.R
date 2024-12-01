# Example text containing various LaTeX citations
# Convert the LaTeX citations in the text to Markdown format
text = "This is a citation \\upcite{key1,key2} and \\citep{key3-a }.
        paper \\cite{key3-b, cky3-c}, paper \\citet{key4-a, cky4-b}"
converted_md = single_cites_convert(text, tex2md = TRUE)
print(converted_md)



# Example text containing Markdown citations
# Convert the Markdown citations back to LaTeX format

text = "This is a citation [@key1; @key2] and [@key3-a].
        paper [@key3-b; @cky3-c], paper [@key4-a; @cky4-b]"
# defalut prefix "cite"
converted_latex = single_cites_convert(text, tex2md = FALSE)
print(converted_latex)

# Convert Markdown citations to LaTeX using a custom prefix
custom_prefix = single_cites_convert(text, tex2md = FALSE, latex_prefix = "upcite")
print(custom_prefix)


# A vector of texts with Markdown citations
text_vector =c("This is a citation [@key1; @key2] and [@key3-a].",
        "This is a citation2 [@key1; @key2] and [@key3-a].",
        "this is no citation",
        "",
        "This is a citation3 [@key1; @key2] and [@key3-a].")

# vector version
vector_latex = cites_convert(text_vector, tex2md = FALSE)
print(vector_latex)



# Another vector of texts containing LaTeX citations
text_vector2= c("This is a citation \\upcite{key1,key2} and \\citep{key3-a }",
         "This is a citation \\citet{key1,key2} and \\citet{key3-a }",
         "this is no citation",
         "",
         "This is a citation \\upcite{key1,key2} and \\cite{key3-a }")
# Convert the vector of LaTeX citations to Markdown format
vector_md = cites_convert(text_vector2, tex2md = TRUE)
print(vector_md)


