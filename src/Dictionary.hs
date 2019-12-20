{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dictionary
  ( CanTranslate (..)
  , HasDictionary (..)
  , addToken
  , addSentence
  , addDocument
  , fromPersistence
  )
where

import qualified External.Types as Ext
import qualified Internal.Types as Int
import Types

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Exception

import System.IO

import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word

type Ext2Int = (HM.HashMap Ext.Token Int.Token)
type Int2Ext = (IM.IntMap Ext.Token)

class (Monad m, MonadIO m) => HasDictionary m where
  -- use words
  getDictionaryExt2Int :: m Ext2Int 
  getDictionaryInt2Ext :: m Int2Ext 
  getCurrentIndex :: m Word32 
  setDictionaryExt2Int :: Ext2Int -> m ()
  setDictionaryInt2Ext :: Int2Ext -> m ()
  setCurrentIndex :: Word32 -> m ()
  getBinFileHandle :: m Handle
  getBinFile :: m String
  getMutex :: m (MVar ())
  close :: m ()

-- translate* only translates it doesn't add new values, since
-- adding new values requires a mutex on dictionary read and the
-- additional FileIO.
class CanTranslate a b where
  translateToken :: (HasDictionary m) => a -> m (Maybe b)
  translateSentence :: (HasDictionary m) => [a] -> m [b]
  translateDocument :: (HasDictionary m) => [[a]] -> m [[b]]

instance CanTranslate Ext.Token Int.Token where
  translateToken t = getDictionaryExt2Int >>= pure . (HM.lookup t)
  translateSentence s = catMaybes <$> mapM translateToken s
  translateDocument = mapM translateSentence

instance CanTranslate Int.Token Ext.Token where
  translateToken t = getDictionaryInt2Ext >>= pure . (IM.lookup . fromIntegral $ t)
  translateSentence s = catMaybes <$> mapM translateToken s
  translateDocument = mapM translateSentence

instance MonadIO m => HasDictionary (AppT m) where
  getDictionaryExt2Int = asks (dEDictionaryExt2Int . aEDictionaryEnv)
    >>= liftIO . readMVar
  getDictionaryInt2Ext = asks (dEDictionaryInt2Ext . aEDictionaryEnv) 
    >>= liftIO . readMVar
  getCurrentIndex = asks (dECurrentIndex . aEDictionaryEnv)
   >>= liftIO . readMVar 
  setDictionaryExt2Int d = asks (dEDictionaryExt2Int . aEDictionaryEnv)
    >>= liftIO . flip swapMVar d >> pure ()
  setDictionaryInt2Ext d = asks (dEDictionaryInt2Ext . aEDictionaryEnv)
    >>= liftIO . flip swapMVar d >> pure ()
  setCurrentIndex i = asks (dECurrentIndex . aEDictionaryEnv)
    >>= liftIO . flip swapMVar i >> pure ()
  getBinFileHandle = asks (dEBinFileHandle . aEDictionaryEnv)
  getBinFile = asks (dEBinFile . aEDictionaryEnv)
  getMutex = asks (dEMutex . aEDictionaryEnv)
  close = getBinFileHandle >>= liftIO . hClose
 
addToken :: (MonadAsyncException m, HasDictionary m) => Ext.Token -> m Int.Token
addToken extToken = getMutex >>= flip withMutex (go extToken)
  where
    go extToken = do
      binFileH <- getBinFileHandle
      intToken <- translateToken extToken 
      case intToken of
        Just n -> pure n 
        Nothing -> do
          idx <- getCurrentIndex
          binFileH <- getBinFileHandle
          liftIO $ do
            (LBS.hPut binFileH) . BP.runPut $ putEntry idx extToken 
            hFlush binFileH
          IM.insert (fromIntegral idx) extToken <$> getDictionaryInt2Ext
            >>= setDictionaryInt2Ext
          HM.insert extToken idx <$> getDictionaryExt2Int
            >>= setDictionaryExt2Int
          setCurrentIndex $ (+1) idx
          pure idx

    putEntry :: Word32 -> T.Text -> BP.Put 
    putEntry idx token = do
      let bsToken = TE.encodeUtf8 token
      BP.putWord32host . fromIntegral $ idx
      BP.putWord16host . fromIntegral $ BS.length bsToken
      BP.putByteString bsToken

-- for convenience
addSentence :: (MonadAsyncException m, HasDictionary m) => Ext.Sentence -> m Int.Sentence  
addSentence = mapM addToken

addDocument :: (MonadAsyncException m, HasDictionary m) => Ext.Document -> m Int.Document  
addDocument = mapM addSentence

fromPersistence :: (MonadAsyncException m, HasDictionary m) => m ()  
fromPersistence = getBinFile >>= \f -> Types.withFile f ReadMode go

  where 
    go binFileH = do 
      (maxIdx, int2ext, ext2int) <- liftIO $ do
        lbs <- LBS.hGetContents binFileH 
        pure $ BG.runGet (readEntry 0 IM.empty HM.empty) lbs
      setDictionaryExt2Int ext2int
      setDictionaryInt2Ext int2ext
      setCurrentIndex maxIdx

    readEntry
      :: Word32
      -> Int2Ext
      -> Ext2Int
      -> BG.Get (Word32, Int2Ext, Ext2Int)
    readEntry maxIdx int2ext ext2int = do
      empty <- BG.isEmpty
      if empty then
        pure (maxIdx, int2ext, ext2int)
      else do
        idx <- BG.getWord32host
        len <- BG.getWord16host
        tokenBS <- BG.getByteString (fromIntegral len)
        let token = TE.decodeUtf8 tokenBS
        readEntry (max maxIdx idx)
         (IM.insert (fromIntegral idx) token int2ext)
         (HM.insert token idx ext2int)
