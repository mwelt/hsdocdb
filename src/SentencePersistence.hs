module SentencePersistence 
  ( HasSentencePersistence (..)
  , append
  , get
  )
  where

import qualified Internal.Types as Int
import Types

import Control.Concurrent.MVar
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader

import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString.Lazy as LBS
import Data.Word

import System.IO

-- TODO: Word32 will give us only 4GB of index file size!
type SentenceId = Word32 
type FilePosition = Word32 

class (Monad m, MonadIO m) => HasSentencePersistence m where
  getBinFile :: m String
  getBinFileHandle :: m Handle
  getSentenceId :: FilePosition -> m SentenceId 
  getFilePosition :: SentenceId -> m FilePosition

  getIdxFileHandle :: m Handle
  getIdxFile :: m String
  getCurrentSentenceId :: m SentenceId
  getMutex :: m (MVar ())
  close :: m ()

instance MonadIO m => HasSentencePersistence (AppT m) where
  getBinFileHandle = asks (spEBinFileHandle . aESentencePersistenceEnv)
  getBinFile = asks (spEBinFile . aESentencePersistenceEnv)
  getSentenceId fp = pure fp 
  getFilePosition sId = pure sId
  getIdxFileHandle = asks (spEIdxFileHandle . aESentencePersistenceEnv)
  getIdxFile = asks (spEIdxFile . aESentencePersistenceEnv)
  getCurrentSentenceId = pure 0 
  getMutex = asks (spEMutex . aESentencePersistenceEnv)
  close = do
    getBinFileHandle >>= liftIO . hClose
    getIdxFileHandle >>= liftIO . hClose

append
  :: (MonadAsyncException m, HasSentencePersistence m)
  => Int.Sentence
  -> m SentenceId
append sentence = getMutex >>= flip withMutex (go sentence) 

  where
    go sentence = do

      binFileH <- getBinFileHandle   
      idxFileH <- getIdxFileHandle

      offset <- liftIO $ do
        let binSentence = BP.runPut $ Int.putSentence sentence
        offset <- hTell binFileH
        LBS.hPut binFileH binSentence

        -- currently only store offsets in idx file
        let idxFileEntry = BP.runPut $ putIdxFileEntry offset
        LBS.hPut idxFileH idxFileEntry

        pure offset 

      getSentenceId (fromIntegral offset)

    putIdxFileEntry = BP.putWord32host . fromIntegral 
      

get :: HasSentencePersistence m => SentenceId -> m Int.Sentence
get sId = do
  filePos <- getFilePosition sId
  binFile <- getBinFile
  liftIO $ Types.withFile binFile ReadMode (go filePos) 

  where
    go filePos binFileH = liftIO $ do 
      hSeek binFileH AbsoluteSeek (fromIntegral filePos)
      lbs <- LBS.hGetContents binFileH
      pure $! BG.runGet Int.getSentence lbs  
