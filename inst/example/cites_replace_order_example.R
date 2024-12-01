tex= c("This is a citation \\upcite{key1,key2} and \\citep{key3-a }",
       "This is a citation \\citet{key1,key2} and \\citet{key3-a }",
       "this is no citation",
       "",
       "This is a citation \\upcite{key1,key4} and \\cite{key3-a }")
citesall = cites_extract(tex)

citesall = c("key2", "key1", "key3-a", "key4")

cites_replace_order(tex, citesall)
