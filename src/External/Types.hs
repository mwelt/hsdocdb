module External.Types
  (
    Token,
    Sentence,
    Document
  ) where

import Data.Text

type Token = Text
type Sentence = [Token]
type Document = [Sentence]
