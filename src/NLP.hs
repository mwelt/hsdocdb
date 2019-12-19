{-# LANGUAGE OverloadedStrings #-}

module NLP
  (
    tokenizeDocument
  ) where

import Types
import qualified External.Types as Ext

import Control.Exception 
import Control.Lens ((^..), (^?))
import Control.Monad.Reader

import Data.Aeson as JSON
import Data.Aeson.Lens (key, values, _String)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe

import qualified Network.HTTP.Client as HTTP

-- atm tokenizer "error"s the whole application out
-- should be more defensive
class (Monad m, MonadIO m) => CanTokenize m where
  tokenizeDocument :: FilePath -> m Ext.Document
  tokenizeByteString :: LBS.ByteString -> m Ext.Document 

class (Monad m, MonadIO m) => HasStanfordTokenzier m where
  getStanfordHttpManager :: m HTTP.Manager 
  getStanfordUrl :: m String

instance (Monad m, MonadIO m) => HasStanfordTokenzier (AppT m) where
  getStanfordHttpManager = asks (stEHttpManager . aEStanfordTokenizerEnv) 
  getStanfordUrl = asks (stEUrl . aEStanfordTokenizerEnv)
  
instance (Monad m, MonadIO m) => CanTokenize (AppT m) where
  tokenizeDocument = tokenizeDocumentStanford
  tokenizeByteString = tokenizeByteStringStanford
   

tokenizeDocumentStanford :: (HasStanfordTokenzier m, MonadIO m) => FilePath -> m Ext.Document
tokenizeDocumentStanford fileName = do 
  lbs <- liftIO . LBS.readFile $ fileName 
  tokenizeByteStringStanford lbs 


tokenizeByteStringStanford
  :: (HasStanfordTokenzier m, MonadIO m) =>  LBS.ByteString -> m Ext.Document
tokenizeByteStringStanford lbs = do 
 manager <- getStanfordHttpManager 
 url <- getStanfordUrl
 resp <- liftIO $ query url manager lbs 
 pure . parseResponse . HTTP.responseBody $ resp
   where
     parseResponse r = case parseValue <$> (JSON.decode r :: Maybe Value) of
       Nothing -> error "can not parse JSON"
       Just x -> x
     parseValue v = map
                    (\v' -> v' ^.. key "tokens" . values . key "word" . _String) $
                    v ^.. key "sentences" . values  

query
  :: String
  -> HTTP.Manager
  -> LBS.ByteString
  -> IO (HTTP.Response LBS.ByteString)
query url manager lbs = do
 r <- HTTP.parseRequest url 
 let r'= r { HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS lbs}
 HTTP.httpLbs r' manager 
