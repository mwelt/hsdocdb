{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Dictionary as D
import Types
import qualified InvertedIndex as II
import qualified SentencePersistence as SP
import qualified External.Types as Ext
import qualified Internal.Types as Int 

import Control.Exception 
import Control.Lens ((^..), (^?))
import Control.Monad.Reader

import Data.Aeson as JSON
import Data.Aeson.Lens (key, values, _String)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe

import qualified Network.HTTP.Client as HTTP

import System.IO

-- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Sequence.html#v:cycleTaking

fromMaybe' :: b -> Maybe a -> Either b a
fromMaybe' _ (Just x) = Right x
fromMaybe' e _ = Left e
                
main :: IO ()
main = do
  appEnv <- newAppEnv
  runReaderT (runApp invertedIndexTest) appEnv 

tokenizeDocument :: MonadIO m => String -> AppT m (Either String Ext.Document) 
tokenizeDocument fileName = do 
  liftIO $ do
    m <- HTTP.newManager HTTP.defaultManagerSettings
    txt <- LBS.readFile fileName 
    tokenize m txt

invertedIndexTest :: AppT IO ()
invertedIndexTest = do
  eitherDoc <- tokenizeDocument sampleData 
  case eitherDoc of
    Left e -> liftIO $ putStrLn $ "Error: " ++ (show e)
    Right d -> go d

  where

    go :: Ext.Document -> AppT IO ()
    go doc = do
      d' <- mapM addSentence doc
      SP.close
      int2ext <- D.getDictionaryInt2Ext 
      let keys = IM.keys int2ext
      mapM_ printInvertedIndex $ take 4 keys
      pure ()

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
  

sentenceEquality :: AppT IO ()
sentenceEquality = do
  eitherDoc <- tokenizeDocument sampleData 
  case eitherDoc of
    Left e -> liftIO $ putStrLn $ "Error: " ++ (show e)
    Right d -> go d

  where
    go :: Ext.Document -> AppT IO () 
    go doc = do
      -- translate
      d' <- D.addDocument doc 
      sentenceIds <- mapM SP.append d'
      SP.close
      D.close
      --liftIO $ mapM_ (putStrLn . show) sentenceIds 
      mapM_ (SP.get
             >=> (D.translateSentence :: Int.Sentence -> AppT IO Ext.Sentence)
             >=> liftIO . putStrLn . show)
        $ take 10 sentenceIds
  

dictionaryEquality :: AppT IO ()
dictionaryEquality = do
  eitherDoc <- tokenizeDocument sampleData
  case eitherDoc of
    Left e -> liftIO $ putStrLn $ "Error: " ++ (show e)
    Right d -> do 
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

query :: HTTP.Manager -> LBS.ByteString -> IO (Either SomeException (HTTP.Response LBS.ByteString))
query m d = do
 r <- HTTP.parseRequest url 
 let r'= r { HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS d}
 try $ HTTP.httpLbs r' m 
   where
     url = "http://localhost:9000/?properties={\"annotators\":\"tokenize,ssplit\",\"outputFormat\":\"json\"}"

tokenize :: HTTP.Manager -> LBS.ByteString -> IO (Either String Ext.Document)
tokenize m d = do 
 resp <- query m d
 case resp of
   Left e -> pure $ Left . show $ e
   Right r -> pure $ parseResponse . HTTP.responseBody $ r
   where
     parseResponse r = parseValue <$> fromMaybe'
                      "parsing JSON value failed!" 
                      (JSON.decode r :: Maybe Value)
     parseValue v = map
                    (\v' -> v' ^.. key "tokens" . values . key "word" . _String) $
                    v ^.. key "sentences" . values  
