{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Types
  ( AppT (..)
  , AppEnv (..)
  , DictionaryEnv (..)
  , SentencePersistenceEnv (..)
  , InvertedIndexEnv (..)
  , StanfordTokenizerEnv (..)
  , withMutex
  , Types.withFile
  , newDictionaryEnv
  , newSentencePersistenceEnv
  , newAppEnv
  -- , defaultSentencePersistenceBinFile
  -- , defaultSentencePersistenceIdxFile
  -- , defaultDictionaryBinFile
  ) where

import qualified External.Types as Ext
import qualified Internal.Types as Int

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Exception

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Word

import qualified Network.HTTP.Client as HTTP (Manager (..),
                                              newManager,
                                              defaultManagerSettings)

import System.IO 

newtype (MonadAsyncException m, MonadIO m) => AppT m a = AppT
  { runApp :: ReaderT AppEnv m a }
  deriving ( Functor, Applicative, Monad, MonadReader AppEnv,
             MonadIO, MonadException, MonadAsyncException )

withMutex :: (MonadAsyncException m) => MVar () -> m b -> m b
withMutex mutex go = bracket_ (acquireMutex mutex) (releaseMutex mutex) go
  where
    acquireMutex = liftIO . takeMVar 
    releaseMutex = liftIO . flip putMVar ()

-- TODO: pool open bin file handles in HasDictionary Monad
withFile
  :: (MonadAsyncException m, MonadIO m)
  => String
  -> IOMode
  -> (Handle -> m b) -> m b
withFile fileName ioMode go
  = bracket (openFile' fileName ioMode) (liftIO . hClose) go
  where
    openFile' fileName ioMode = liftIO $ openBinaryFile fileName ioMode

data AppEnv = AppEnv
  { aEDictionaryEnv :: DictionaryEnv
  , aESentencePersistenceEnv :: SentencePersistenceEnv
  , aEInvertedIndexEnv :: InvertedIndexEnv
  , aEStanfordTokenizerEnv :: StanfordTokenizerEnv
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
  , spEIdxFileHandle :: Handle
  , spEMutex :: MVar ()
  }

data InvertedIndexEnv = InvertedIndexEnv
  { iiEInvertedIndex :: MVar (IM.IntMap IS.IntSet)
  , iiEMutex :: MVar ()
  }

data StanfordTokenizerEnv = StanfordTokenizerEnv
  { stEUrl :: String
  , stEHttpManager :: HTTP.Manager
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
  <*> newMVar ()

defaultDictionaryBinFile = "dictionary.bin" :: String

newDictionaryEnv = (DictionaryEnv defaultDictionaryBinFile)
  <$> newMVar HM.empty
  <*> newMVar IM.empty
  <*> newMVar 0 
  <*> openBinaryFile defaultDictionaryBinFile WriteMode 
  <*> newMVar ()

newInvertedIndexEnv = InvertedIndexEnv
  <$> newMVar IM.empty
  <*> newMVar ()

defaultStanfordUrl =
  "http://localhost:9000/?properties={\"annotators\":\"tokenize,ssplit\",\"outputFormat\":\"json\"}" :: String

newStanfordTokenizerEnv = StanfordTokenizerEnv defaultStanfordUrl
  <$> HTTP.newManager HTTP.defaultManagerSettings

newAppEnv = AppEnv
  <$> newDictionaryEnv
  <*> newSentencePersistenceEnv
  <*> newInvertedIndexEnv
  <*> newStanfordTokenizerEnv
