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
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader

import System.IO

import qualified Data.Binary.Get as BG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Text.Encoding as TE


type Ext2Int = (HM.HashMap Ext.Token Int.Token)
type Int2Ext = (IM.IntMap Ext.Token)

class (Monad m, MonadIO m) => HasDictionary m where
  getDictionaryExt2Int :: m Ext2Int 
  getDictionaryInt2Ext :: m Int2Ext 
  getCurrentIndex :: m Int
  setDictionaryExt2Int :: Ext2Int -> m ()
  setDictionaryInt2Ext :: Int2Ext -> m ()
  setCurrentIndex :: Int -> m ()
  binFileHandle :: m Handle
  getMutex :: m (MVar ()) 

class CanTranslate a b where
  translate :: (HasDictionary m) => a -> m (Maybe b)

instance CanTranslate Ext.Token Int.Token where
  translate t = getDictionaryExt2Int >>= pure . (HM.lookup t)

instance CanTranslate Int.Token Ext.Token where
  translate t = getDictionaryInt2Ext >>= pure . (IM.lookup t)

instance MonadIO m => HasDictionary (AppT m) where
  getDictionaryExt2Int = asks dEDictionaryExt2Int >>= liftIO . readMVar
  getDictionaryInt2Ext = asks dEDictionaryInt2Ext >>= liftIO . readMVar
  getCurrentIndex = asks dECurrentIndex >>= liftIO . readMVar 
  setDictionaryExt2Int d = asks dEDictionaryExt2Int >>= liftIO . flip swapMVar d >> pure ()
  setDictionaryInt2Ext d = asks dEDictionaryInt2Ext >>= liftIO . flip swapMVar d >> pure ()
  setCurrentIndex i = asks dECurrentIndex >>= liftIO . flip swapMVar i >> pure ()
  binFileHandle = asks dEBinFileHandle
  getMutex = asks dEMutex 

addToken :: HasDictionary m => Ext.Token -> m Int.Token
addToken t = do
  -- synchronize writing of dictionary
  mutex <- getMutex
  liftIO $ takeMVar mutex 

  mi <- translate t
  case mi of
    Just n -> liftIO $ putMVar mutex () >> pure n 
    Nothing -> do
      idx <- (1+) <$> getCurrentIndex
      f <- binFileHandle
      seek <- liftIO $ do
        let bs = TE.encodeUtf8 t 
        -- TODO: wrap in bracket to release mutex
        BSB.hPutBuilder f $ (BSB.int32LE . fromIntegral $ idx)
          <> (BSB.int32LE . fromIntegral $ BS.length bs)
          <> BSB.byteString bs
        hTell f
      liftIO $ putStrLn . show $ seek
      setCurrentIndex idx
      IM.insert idx t <$> getDictionaryInt2Ext >>= setDictionaryInt2Ext
      HM.insert t idx <$> getDictionaryExt2Int >>= setDictionaryExt2Int
      liftIO $ putMVar mutex ()
      pure idx

fromPersistence :: HasDictionary m => m ()  
fromPersistence = do
  f <- liftIO $ openFile "dictionary.bin" ReadMode 
  (maxIdx, int2ext, ext2int) <- liftIO $ readEntry 0 f IM.empty HM.empty
  setDictionaryExt2Int ext2int
  setDictionaryInt2Ext int2ext
  setCurrentIndex maxIdx
  
  where
    readEntry
      :: Int
      -> Handle
      -> Int2Ext
      -> Ext2Int
      -> IO (Int, Int2Ext, Ext2Int) 
    readEntry maxIdx f int2ext ext2int = do
      -- read ouverture with 4 byte idx and 4 byte strlen
      ovBs <- LBS.hGet f (4 * 2)  
      -- if ouverture is empty, the end of file is reached
      if ovBs == LBS.empty then
        pure (maxIdx, int2ext, ext2int)
      else do
        -- parse these 8 bytes 
        let (idx, sSize) = BG.runGet readIdxAndStrLen ovBs
        -- and read the whole str
        tBs <- BS.hGet f sSize
        let t = TE.decodeUtf8 tBs 
        -- recur
        readEntry (max maxIdx idx) f (IM.insert idx t int2ext) (HM.insert t idx ext2int) 

    readIdxAndStrLen :: BG.Get (Int, Int) 
    readIdxAndStrLen = (\a b -> (fromIntegral a, fromIntegral b)) <$> BG.getInt32le <*> BG.getInt32le

    

      

      
