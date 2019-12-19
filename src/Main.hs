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
import Control.Exception

import Data.List
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text as T

import Prelude hiding (catch)

import System.Directory
import System.IO.Error hiding (catch)

-- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Sequence.html#v:cycleTaking
                
main :: IO ()
main = do
  appEnv <- newAppEnv
  runReaderT (runApp $ deleteFiles >> readDirectory "data2/" ".abstr") appEnv 
-- readDirectory "data2/" ".abstr"

filesToDelete = [ defaultSentencePersistenceBinFile 
                , defaultSentencePersistenceIdxFile
                , defaultDictionaryBinFile
                ]

deleteFiles :: AppT IO ()
deleteFiles = liftIO $ mapM_ removeIfExists filesToDelete 
  where
    removeIfExists :: FilePath -> IO ()
    removeIfExists fileName = removeFile fileName `catch` handleExists

    handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- Request is too long to be handled by server: 1149159 characters. Max length is 100000 characters.
readDirectory :: FilePath -> String -> AppT IO ()
readDirectory path fileEnding = do
  files <- liftIO $ listDirectory path 
  let files' = map ((++) path) $ filter (isSuffixOf fileEnding) files
  sIds <- concat <$> mapM importFile files'
  D.close
  SP.close
  --liftIO $ putStrLn ( (show . length $ sIds) ++ " sentences imported!" )

  where
    importFile :: FilePath -> AppT IO [Int.SentenceId] 
    importFile fp = do
      lines <- liftIO . fmap lines . readFile $ fp
      processLines 5 lines 

    processLines :: Int -> [String] -> AppT IO [Int.SentenceId]
    processLines _ [] = pure []  
    processLines n xs = do
      let lines = take n xs
      extDoc <- NLP.tokenizeByteString . BLU.fromString . concat $ lines 
      sentenceIds <- mapM addSentence extDoc
      -- liftIO . print . length $ sentenceIds
      (sentenceIds ++) <$> processLines n (drop n xs)
     
    addSentence :: Ext.Sentence -> AppT IO Int.SentenceId
    addSentence extSentence = do
      intSentence <- mapM D.addToken extSentence
      sId <- SP.append intSentence
      D.getCurrentIndex >>= liftIO . putStrLn . show
      pure sId 
