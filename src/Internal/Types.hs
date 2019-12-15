module Internal.Types
  (
    Token,
    Sentence,
    Document,
    putToken,
    putSentence,
    getToken,
    getSentence
  ) where

import qualified External.Types as Ext 
import Control.Monad

import Data.Binary.Put
import Data.Binary.Get
import Data.Word

type Token = Word32
type Sentence = [Token]
type Document = [Sentence]

putToken :: Token -> Put 
putToken = putWord32host

putSentence :: Sentence -> Put 
putSentence s = do
  let len = length s 
  putWord16host . fromIntegral $ len 
  mapM_ putToken s

getToken :: Get Token
getToken = getWord32host

getSentence :: Get Sentence
getSentence = do
  len <- getWord16host 
  replicateM (fromIntegral len) getToken  
