{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dictionary
  ( CanTranslate (..)
  , HasDictionary (..)
  , addToken
  , fromPersistence
  )
where

import qualified External.Types as Ext
import qualified Internal.Types as Int
import Types

import Control.Concurrent.MVar
--import Control.Exception
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

class CanTranslate a b where
  translate :: (HasDictionary m) => a -> m (Maybe b)

instance CanTranslate Ext.Token Int.Token where
  translate t = getDictionaryExt2Int >>= pure . (HM.lookup t)

instance CanTranslate Int.Token Ext.Token where
  translate t = getDictionaryInt2Ext >>= pure . (IM.lookup . fromIntegral $ t)

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

addToken :: (HasMutex m, HasDictionary m) => Ext.Token -> m Int.Token
addToken extToken = withMutex (go extToken)
  where
    go extToken = do
      binFileH <- getBinFileHandle
      intToken <- translate extToken 
      case intToken of
        Just n -> pure n 
        Nothing -> do
          idx <- getCurrentIndex
          binFileH <- getBinFileHandle
          liftIO $ (LBS.hPut binFileH) . BP.runPut $ putEntry idx extToken 
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
