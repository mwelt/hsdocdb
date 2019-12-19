module Test
  ( invertedIndexTest
  , dictionaryEqualityTest
  , sentenceEqualityTest
  ) where

import qualified Dictionary as D
import qualified NLP 
import Types
import qualified InvertedIndex as II
import qualified SentencePersistence as SP
import qualified External.Types as Ext
import qualified Internal.Types as Int 

import Control.Monad.Reader

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

invertedIndexTest :: AppT IO ()
invertedIndexTest = do
  d <- NLP.tokenizeDocument sampleData 
  d' <- mapM addSentence d
  SP.close
  int2ext <- D.getDictionaryInt2Ext 
  let keys = IM.keys int2ext
  mapM_ printInvertedIndex $ take 4 keys
  pure ()

  where
    printInvertedIndex :: Int -> AppT IO ()
    printInvertedIndex t = do
      let t' = fromIntegral t :: Int.Token
      mt'' <- (D.translateToken t') :: AppT IO (Maybe Ext.Token)
      liftIO $ do
        putStr . show $ mt''
        putStr ": "
      sIds <- IS.toList <$> II.get t'
      sentences <- mapM (SP.get . fromIntegral) sIds
      sentences' <- mapM D.translateSentence sentences :: AppT IO Ext.Document 
      liftIO $ mapM_ (putStrLn . show) sentences'
    
    addSentence :: Ext.Sentence -> AppT IO Int.Sentence
    addSentence ss = do
      ss' <- mapM D.addToken ss 
      sId <- SP.append ss'
      II.put sId ss'
      pure ss'
  

sentenceEqualityTest :: AppT IO ()
sentenceEqualityTest = do
  d <- NLP.tokenizeDocument sampleData 

  -- translate
  d' <- D.addDocument d 
  sentenceIds <- mapM SP.append d'
  SP.close
  D.close
  --liftIO $ mapM_ (putStrLn . show) sentenceIds 
  mapM_ (SP.get
         >=> (D.translateSentence :: Int.Sentence -> AppT IO Ext.Sentence)
         >=> liftIO . putStrLn . show)
    $ take 10 sentenceIds
  

dictionaryEqualityTest :: AppT IO ()
dictionaryEqualityTest = do
  d <- NLP.tokenizeDocument sampleData

  -- translate and print document
  d' <- (D.translateDocument :: Ext.Document -> AppT IO Int.Document) d
  liftIO $ putStrLn . show $ d'

  -- close the file
  D.close

  -- get the current dictionaries
  int2ext <- D.getDictionaryInt2Ext
  ext2int <- D.getDictionaryExt2Int

  -- reread the dictionaries from disk
  D.fromPersistence 
  -- int2ext' <- getDictionaryInt2Ext
  -- ext2int' <- getDictionaryExt2Int

  -- translate the document back
  d'' <- D.translateDocument d'  
  liftIO $ putStrLn . show $ d == d''

sampleData = "sample.txt" :: String
