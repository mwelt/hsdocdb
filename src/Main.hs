{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Internal.Types as Int
import qualified External.Types as Ext
import Types
import Test 
import qualified SentencePersistence as SP
import qualified Dictionary as D
import qualified NLP 

import Control.Monad.Reader

import Data.List
import qualified Data.Text as T

import qualified System.Directory as Dir

-- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Sequence.html#v:cycleTaking
                
main :: IO ()
main = do
  appEnv <- newAppEnv
  runReaderT (runApp $ invertedIndexTest ) appEnv 
-- readDirectory "data2/" ".abstr"
readDirectory :: FilePath -> String -> AppT IO ()
readDirectory path fileEnding = do
  files <- liftIO $ Dir.listDirectory path 
  let files' = map ((++) path) $ filter (isSuffixOf fileEnding) files
  sIds <- mapM importFile files'
  D.close
  SP.close
  liftIO $ putStrLn ( (show . length $ sIds) ++ " sentences imported!" )

  where
    importFile :: FilePath -> AppT IO [Int.SentenceId] 
    importFile fp = do
      extDoc <- NLP.tokenizeDocument fp
      mapM addSentence extDoc
     
    addSentence :: Ext.Sentence -> AppT IO Int.SentenceId
    addSentence extSentence = do
      intSentence <- mapM D.addToken extSentence
      sId <- SP.append intSentence
      D.getCurrentIndex >>= liftIO . putStrLn . show
      pure sId 
