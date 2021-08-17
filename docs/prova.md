Prova.



https://haskell.mooc.fi/part1#a-word-about-type-inference-and-polymorphism


-- given a sentence, decide whether it is a statement, question or exclamation
sentenceType :: String -> String
sentenceType sentence = case last sentence of '.' -> "statement"
                                              '?' -> "question"
                                              '!' -> "exclamation"
                                              _   -> "not a sentence"

-- same function, helper function instead of case-of
sentenceType sentence = classify (last sentence)
  where classify '.' = "statement"
        classify '?' = "question"
        classify '!' = "exclamation"
        classify _   = "not a sentence"