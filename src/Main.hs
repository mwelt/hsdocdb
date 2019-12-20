{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Internal.Types as Int
import qualified External.Types as Ext
import Types
import Test 
import qualified SentencePersistence as SP
import qualified Dictionary as D
import qualified NLP 

import Control.Monad.Reader
import Control.Exception
import Control.Concurrent 
import qualified Control.Concurrent.Chan as C
import Control.Concurrent.MVar

import Data.List
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text as T

import Prelude hiding (catch)

import System.Directory
import System.IO.Error hiding (catch)

-- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Sequence.html#v:cycleTaking

data ChanContent a = ChanContent a | Poison
  deriving (Show)
type MChan a = C.Chan (ChanContent a)

runQueueBasedWorker :: (a -> IO ()) -> MChan a -> MVar () -> IO ThreadId 
runQueueBasedWorker fn channel mutex = forkIO $ do
  takeMVar mutex
  loop channel mutex fn
  where
    loop channel mutex fn = do
      x <- readChan channel
      case x of
        ChanContent x' -> fn x' >> loop channel mutex fn
        Poison -> putMVar mutex ()
        
main :: IO ()
main = do
  let nThreads = 4

  c1 <- newChan :: IO (MChan a)
  c2 <- newChan :: IO (MChan a)

  consumerMutex <- newMVar ()
  processMutex <- replicateM nThreads $ newMVar ()

  -- start worker threads
  processingThreads <-
    mapM (runQueueBasedWorker (processing c2) c1) processMutex
  putStrLn $ "started processing threads " ++ (show processingThreads)

  -- start single final consumer thread
  consumingThread <- runQueueBasedWorker consuming c2 consumerMutex
  putStrLn $ "started consuming thread " ++ (show consumingThread)

  -- star producer thread
  producingThread <- producing c1 [1..100] nThreads
  putStrLn $ "started producing thread " ++ (show producingThread)

  -- wait for processing threads to be poisoned out 
  mapM_ takeMVar processMutex 
  -- poison the consumer queue
  writeChan c2 Poison
  -- wait for the consumer to terminate
  takeMVar consumerMutex

  where
    processing :: MChan Int -> Int -> IO ()
    processing c x = writeChan c $ ChanContent (x + 1)

    consuming :: Int -> IO ()
    consuming x = (putStrLn $ "yay! " ++ (show x))

    producing :: MChan Int -> [Int] -> Int -> IO ThreadId 
    producing channel xs nThreads = forkIO $ do
      -- write data to channel
      mapM_ (writeChan channel . ChanContent) xs
      -- poison channel with number of consuming threads
      replicateM_ nThreads $ writeChan channel Poison

    
    -- runComputingThread :: MChan Int -> MChan Int -> MVar () ->  IO ()
    -- runComputingThread c1 c2 mutex = do
    --   takeMVar mutex
    --   loopComputing c1 c2 mutex

    -- loopComputing c1 c2 mutex = do
    --     i1 <- readChan c1
    --     case i1 of
    --       ChanContent i -> writeChan c2 (ChanContent (i + 1))
    --         >> loopComputing c1 c2 mutex
    --       Poison -> writeChan c2 Poison >> putMVar mutex () 

    -- runReadingThread :: MChan Int -> MVar () -> IO ()
    -- runReadingThread c2 mutex = do
    --   takeMVar mutex 
    --   loopReading c2 mutex

    -- loopReading :: MChan Int -> MVar () -> IO () 
    -- loopReading c2 mutex = do
    --   i1 <- readChan c2
    --   case i1 of
    --     ChanContent i -> (putStrLn $ "yay! " ++ (show i))
    --       >> loopReading c2 mutex
    --     Poison -> putMVar mutex ()

    -- runWritingThread :: MChan Int -> [Int] -> IO ()
    -- runWritingThread c1 xs = do
    --   mapM_ (writeChan c1 . ChanContent) xs
    --   replicateM_ 4 $ writeChan c1 Poison

  
  -- appEnv <- newAppEnv
  -- runReaderT (runApp $ readDirectory "data/" ".abstr") appEnv 
-- readDirectory "data/" ".abstr"

filesToDelete = [ defaultSentencePersistenceBinFile 
                , defaultSentencePersistenceIdxFile
                , defaultDictionaryBinFile
                ]

deleteFiles :: AppT IO ()
deleteFiles = liftIO $ mapM_ removeIfExists filesToDelete 
  where
    removeIfExists :: FilePath -> IO ()
    removeIfExists fileName = removeFile fileName `catch` handleExists

    handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- Request is too long to be handled by server: 1149159 characters. Max length is 100000 characters.
readDirectory :: FilePath -> String -> AppT IO ()
readDirectory path fileEnding = do
  files <- liftIO $ listDirectory path 
  let files' = map ((++) path) $ filter (isSuffixOf fileEnding) files
  sIds <- concat <$> mapM importFile files'
  D.close
  SP.close
  --liftIO $ putStrLn ( (show . length $ sIds) ++ " sentences imported!" )

  where
    importFile :: FilePath -> AppT IO [Int.SentenceId] 
    importFile fp = do
      lines <- liftIO . fmap lines . readFile $ fp
      processLines 5 lines 

    processLines :: Int -> [String] -> AppT IO [Int.SentenceId]
    processLines _ [] = pure []  
    processLines n xs = do
      let lines = take n xs
      extDoc <- NLP.tokenizeByteString . BLU.fromString . concat $ lines 
      sentenceIds <- mapM addSentence extDoc
      -- liftIO . print . length $ sentenceIds
      (sentenceIds ++) <$> processLines n (drop n xs)
     
    addSentence :: Ext.Sentence -> AppT IO Int.SentenceId
    addSentence extSentence = do
      intSentence <- mapM D.addToken extSentence
      sId <- SP.append intSentence
      D.getCurrentIndex >>= liftIO . putStrLn . show
      pure sId 
