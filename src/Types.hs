{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Types
  ( AppT (..)
  , DictionaryEnv (..)
  , newDictionaryEnv
  ) where

import qualified External.Types as Ext
import qualified Internal.Types as Int

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM

import System.IO

newtype MonadIO m => AppT m a = AppT
  { runApp :: ReaderT DictionaryEnv m a }
  deriving ( Functor, Applicative, Monad, MonadReader DictionaryEnv, MonadIO )

data DictionaryEnv = DictionaryEnv
  { dEDictionaryExt2Int :: MVar (HM.HashMap Ext.Token Int.Token)
  , dEDictionaryInt2Ext :: MVar (IM.IntMap Ext.Token)
  , dECurrentIndex :: MVar Int
  , dEBinFileHandle :: Handle
  , dEMutex :: MVar ()
  }

newDictionaryEnv = DictionaryEnv
  <$> newMVar HM.empty
  <*> newMVar IM.empty
  <*> newMVar 0 
  <*> openFile "dictionary.bin" WriteMode 
  <*> newMVar ()
