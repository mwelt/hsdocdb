{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Types
  ( AppT (..)
  , AppEnv (..)
  , DictionaryEnv (..)
  , SentencePersistenceEnv (..)
  , HasMutex (..)
  , withMutex
  , newDictionaryEnv
  , defaultSentencePersistenceBinFile
  , defaultSentencePersistenceIdxFile
  , defaultDictionaryBinFile
  ) where

import qualified External.Types as Ext
import qualified Internal.Types as Int

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Exception

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import Data.Word

import System.IO 

newtype (MonadAsyncException m, MonadIO m) => AppT m a = AppT
  { runApp :: ReaderT AppEnv m a }
  deriving ( Functor, Applicative, Monad, MonadReader AppEnv, MonadIO, MonadException, MonadAsyncException )

class (MonadIO m, MonadAsyncException m) => HasMutex m where
  getMutex :: m (MVar ())

instance (MonadAsyncException m) => HasMutex (AppT m) where
  getMutex = asks $ dEMutex . aEDictionaryEnv 

withMutex :: (MonadAsyncException m, HasMutex m) => m b -> m b
withMutex go = bracket_ acquireMutex releaseMutex go
  where
    acquireMutex = getMutex >>= liftIO . takeMVar 
    releaseMutex = getMutex >>= liftIO . flip putMVar ()

data AppEnv = AppEnv
  { aEDictionaryEnv :: DictionaryEnv
  , aESentencePersistenceEnv :: SentencePersistenceEnv
  }

data DictionaryEnv = DictionaryEnv
  { dEBinFile :: String
  , dEDictionaryExt2Int :: MVar (HM.HashMap Ext.Token Int.Token)
  , dEDictionaryInt2Ext :: MVar (IM.IntMap Ext.Token)
  , dECurrentIndex :: MVar Word32 
  , dEBinFileHandle :: Handle
  , dEMutex :: MVar ()
  }

data SentencePersistenceEnv = SentencePersistenceEnv
  { spEBinFile :: String
  , spEIdxFile :: String
  , spEBinFileHandle :: Handle
  , spEBIdxFileHandle :: Handle
  }

defaultSentencePersistenceBinFile = "sentences.bin" :: String

-- TODO: currently the index file only stores offsets for all
-- sentences in bin file, it doesn't store a function from sentenceId
-- to offset somehow
defaultSentencePersistenceIdxFile = "sentences.idx" :: String

newSentencePersistenceEnv = (SentencePersistenceEnv
  defaultSentencePersistenceBinFile
  defaultSentencePersistenceIdxFile)
  <$> openBinaryFile defaultSentencePersistenceBinFile WriteMode
  <*> openBinaryFile defaultSentencePersistenceIdxFile WriteMode

defaultDictionaryBinFile = "dictionary.bin" :: String

newDictionaryEnv = (DictionaryEnv defaultDictionaryBinFile)
  <$> newMVar HM.empty
  <*> newMVar IM.empty
  <*> newMVar 0 
  <*> openBinaryFile defaultDictionaryBinFile AppendMode 
  <*> newMVar ()
