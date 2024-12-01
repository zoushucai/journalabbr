tex= c("This is a citation \\upcite{key1,key2} and \\citep{key3-a }",
       "This is a citation \\citet{key1,key2} and \\citet{key3-a }",
       "this is no citation",
       "",
       "This is a citation \\upcite{key1,key4} and \\cite{key3-a }")


tex = cites_replace_author(tex)

print(tex)


tex2 = cites_replace_number(tex)
print(tex2)
