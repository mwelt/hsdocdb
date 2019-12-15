{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Dictionary
import Types
import qualified SentencePersistence as SP
import qualified External.Types as Ext
import qualified Internal.Types as Int 

import Control.Exception 
import Control.Lens ((^..), (^?))
import Data.Maybe
import Control.Monad.Reader

import Data.Aeson as JSON
import Data.Aeson.Lens (key, values, _String)
import qualified Data.ByteString.Lazy as LBS

import qualified Network.HTTP.Client as HTTP

import System.IO

-- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Sequence.html#v:cycleTaking

fromMaybe' :: b -> Maybe a -> Either b a
fromMaybe' _ (Just x) = Right x
fromMaybe' e _ = Left e
                
main :: IO ()
main = do
  appEnv <- newAppEnv
  runReaderT (runApp sentenceEquality) appEnv 

tokenizeDocument :: MonadIO m => String -> AppT m (Either String Ext.Document) 
tokenizeDocument fileName = do 
  liftIO $ do
    m <- HTTP.newManager HTTP.defaultManagerSettings
    txt <- LBS.readFile fileName 
    tokenize m txt

sentenceEquality :: AppT IO ()
sentenceEquality = do
  eitherDoc <- tokenizeDocument sampleData 
  case eitherDoc of
    Left e -> liftIO $ putStrLn $ "Error: " ++ (show e)
    Right d -> go d

  where
    go doc = do
      -- translate
      d' <- translateDocument doc
      sentenceIds <- mapM SP.append d'
      SP.close
      Dictionary.close
      liftIO $ mapM_ (putStrLn . show) sentenceIds 

dictionaryEquality :: AppT IO ()
dictionaryEquality = do
  eitherDoc <- tokenizeDocument sampleData
  case eitherDoc of
    Left e -> liftIO $ putStrLn $ "Error: " ++ (show e)
    Right d -> do 
      -- translate and print document
      d' <- translateDocument d
      liftIO $ putStrLn . show $ d'

      -- close the file
      Dictionary.close

      -- get the current dictionaries
      int2ext <- getDictionaryInt2Ext
      ext2int <- getDictionaryExt2Int

      -- reread the dictionaries from disk
      fromPersistence 
      -- int2ext' <- getDictionaryInt2Ext
      -- ext2int' <- getDictionaryExt2Int

      -- translate the document back
      d'' <- translateDocument' d'  
      liftIO $ putStrLn . show $ d == d''

translateDocument' :: Int.Document -> AppT IO Ext.Document
translateDocument' = mapM translateSentence
  where
    translateSentence :: Int.Sentence -> AppT IO Ext.Sentence 
    translateSentence s = catMaybes <$> mapM translate s 

translateDocument :: Ext.Document -> AppT IO Int.Document
translateDocument = mapM translateSentence
  where
    translateSentence :: Ext.Sentence -> AppT IO Int.Sentence
    translateSentence s = mapM addToken s

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



-- >>> translate (T.pack "Hello") 
-- <interactive>:3775:2-27: error:
--     • Non type-variable argument in the constraint: CanTranslate Text b
--       (Use FlexibleContexts to permit this)
--     • When checking the inferred type
--         it :: forall b. CanTranslate Text b => b


-- >>> main
