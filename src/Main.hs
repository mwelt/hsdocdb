{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Dictionary
import Types
import qualified External.Types as Ext
import qualified Internal.Types as Int 
import Control.Exception 
import Control.Monad.Reader
import Data.Maybe
import System.IO
import qualified Network.HTTP.Client as HTTP
import Data.Aeson as JSON
import qualified Data.Vector as V 
import Control.Lens ((^..), (^?))
import Data.Aeson.Lens (key, values, _String)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Sequence.html#v:cycleTaking

fromMaybe' :: b -> Maybe a -> Either b a
fromMaybe' _ (Just x) = Right x
fromMaybe' e _ = Left e
                
main :: IO ()
main = do
  de <- newDictionaryEnv
  runReaderT (runApp runMe) de 

runMe :: AppT IO ()
runMe = do
  ed <- liftIO $ do
    m <- HTTP.newManager HTTP.defaultManagerSettings
    txt <- sampleData
    tokenize m txt
  case ed of
    Left e -> liftIO $ putStrLn $ "Error: " ++ (show e)
    Right d -> do 
      -- translate and print document
      d' <- translateDocument d
      liftIO $ putStrLn . show $ d'

      -- close the file
      handle <- binFileHandle 
      liftIO $ hClose handle

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

sampleData :: IO LBS.ByteString 
sampleData = LBS.readFile "sample.txt"

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
