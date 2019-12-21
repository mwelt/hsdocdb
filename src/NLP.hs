{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}  

module NLP
  ( tokenizeDocument
  , tokenizeByteString 
  ) where

import Types
import qualified External.Types as Ext

import Control.Exception 
import Control.Lens ((^..), (^?))
import Control.Monad.Reader

import Data.Aeson as JSON
import Data.Aeson.Lens (key, values, _String)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Client as HTTP

-- atm tokenizer "error"s the whole application out
-- should be more defensive
class (Monad m, MonadIO m) => CanTokenize m where
  tokenizeDocument :: FilePath -> m Ext.Document
  tokenizeByteString :: BS.ByteString -> m Ext.Document 
  tokenizeLByteString :: LBS.ByteString -> m Ext.Document 

class (Monad m, MonadIO m) => HasStanfordTokenizer m where
  getStanfordHttpManager :: m HTTP.Manager 
  getStanfordUrl :: m String

instance (Monad m, MonadIO m) => HasStanfordTokenizer (AppT m) where
  getStanfordHttpManager = asks (stEHttpManager . aEStanfordTokenizerEnv) 
  getStanfordUrl = asks (stEUrl . aEStanfordTokenizerEnv)
  
-- instance (Monad m, MonadIO m) => CanTokenize (AppT m) where
--   tokenizeDocument = tokenizeDocumentStanford
--   tokenizeByteString = tokenizeByteStringStanford
   
instance (Monad m, MonadIO m, HasStanfordTokenizer m) => CanTokenize m where
  tokenizeDocument = tokenizeDocumentStanford
  tokenizeLByteString = tokenizeLByteStringStanford
  tokenizeByteString = tokenizeByteStringStanford

tokenizeDocumentStanford :: (HasStanfordTokenizer m, MonadIO m) => FilePath -> m Ext.Document
tokenizeDocumentStanford fileName = do 
  lbs <- liftIO . LBS.readFile $ fileName 
  tokenizeLByteStringStanford lbs 

tokenizeByteStringStanford
  :: (HasStanfordTokenizer m, MonadIO m) =>  BS.ByteString -> m Ext.Document
tokenizeByteStringStanford = tokenizeLByteStringStanford . LBS.fromStrict

tokenizeLByteStringStanford
  :: (HasStanfordTokenizer m, MonadIO m) =>  LBS.ByteString -> m Ext.Document
tokenizeLByteStringStanford lbs = do 
 manager <- getStanfordHttpManager 
 url <- getStanfordUrl
 resp <- liftIO $ query url manager lbs 
 let lbs = HTTP.responseBody resp
 liftIO $ parseResponse lbs
   where
     parseResponse :: LBS.ByteString -> IO Ext.Document
     parseResponse r =
       let bs = LBS.toStrict r;
           jsonStr = TE.decodeUtf8 bs in 
         case parseValue <$> (JSON.decodeStrict bs :: Maybe Value) of
           Nothing -> do
             putStrLn $ "can not parse JSON: " ++ (show jsonStr)
             error "" 
           Just x -> pure x

     parseValue v = map
                    (\v' -> v' ^.. key "tokens" . values . key "word" . _String) $
                    v ^.. key "sentences" . values  

-- TODO: move to strict byte string in general
query
  :: String
  -> HTTP.Manager
  -> LBS.ByteString
  -> IO (HTTP.Response LBS.ByteString)
query url manager lbs = do
 r <- HTTP.parseRequest url 
 let r'= r { HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS lbs}
 HTTP.httpLbs r' manager 
