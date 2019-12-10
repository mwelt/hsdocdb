module Internal.Types
  (
    Token,
    Sentence,
    Document
  ) where

type Token = Int
type Sentence = [Token]
type Document = [Sentence]
