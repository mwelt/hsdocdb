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
import Control.Concurrent.MVar

import Data.List
import Data.List.Split
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Prelude hiding (catch)

import System.Directory
import System.IO.Error hiding (catch)

-- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Sequence.html#v:cycleTaking

runQueueBasedWorker
  :: (Show a)
  => AppEnv
  -> (a -> AppT IO ())
  -> MVar (Maybe a)
  -> MVar ()
  -> IO ThreadId 
runQueueBasedWorker env fn queue mutex = forkIO $
  runReaderT (runApp (go queue mutex fn)) env

  where
    go :: (Show a) => MVar (Maybe a) -> MVar () -> (a -> AppT IO ()) -> AppT IO ()
    go queue mutex fn = do
      liftIO $ do
        takeMVar $ mutex
        putStrLn "thread locked"
      loop queue mutex fn

    loop :: (Show a) => MVar (Maybe a) -> MVar () -> (a -> AppT IO ()) -> AppT IO () 
    loop queue mutex fn = do
      x <- liftIO . takeMVar $ queue 
      case x of
        Just x' -> fn x' >> loop queue mutex fn
        Nothing -> liftIO $ do
          -- inform other possible working threads
          putMVar queue Nothing
          -- unlock the thread is running mutex and stop
          putStrLn "unlocking thread"
          putMVar mutex ()
        

main :: IO ()
main = do

  env <- newAppEnv
  let nlpNThreads = aENLPNThreads env
  let c1 = aENLPInputChannel env
  let c2 = aENLPOutputChannel env
  consumerMutex <- newMVar ()
  processMutex <- replicateM nlpNThreads $ newMVar ()

  -- start producer thread
  producingThread <- producing env "data2/" ".abstr"
  putStrLn $ "started producing thread " ++ (show producingThread)

  -- start worker threads
  processingThreads <-
    mapM (runQueueBasedWorker env processingNLP c1) processMutex
  putStrLn $ "started processing threads " ++ (show processingThreads)

  -- -- start single final consumer thread
  consumingThread <- runQueueBasedWorker env consuming c2 consumerMutex
  putStrLn $ "started consuming thread " ++ (show consumingThread)

  -- wait for processing threads to be poisoned out 
  mapM_ takeMVar processMutex 
  -- poison the consumer queue
  putMVar c2 Nothing 
  -- wait for the consumer to terminate
  takeMVar consumerMutex

  close env

  where

    producing
      :: AppEnv
      -> FilePath
      -> String
      -> IO ThreadId 
    producing env path fileEnding =
      forkIO $ runReaderT (runApp (go path fileEnding)) env

    go :: FilePath -> String -> AppT IO ()
    go path fileEnding = do
      files <- liftIO $ listDirectory path 
      let files' = map ((++) path) $ filter (isSuffixOf fileEnding) files
      out <- asks aENLPInputChannel
      bss <- concat <$> mapM importFile files'
      liftIO $ do
        mapM_ ((putMVar out) . Just) bss 
        putMVar out Nothing
      
      -- nThreads <- asks aENLPNThreads
      -- liftIO $ replicateM_ nThreads $ writeChan outChan Poison

    importFile :: FilePath -> AppT IO [BS.ByteString] 
    importFile fp = do
      liftIO . putStrLn $ "processing file " ++ fp
      n <- asks aENLPChunkLen
      lines <- liftIO
        . fmap (
          (map (TE.encodeUtf8 . T.concat))
          . (chunksOf n)
          . T.lines)
        . TIO.readFile $ fp
      lines `seq` pure lines 

    processingNLP :: BS.ByteString -> AppT IO ()
    processingNLP bs = do
      extDoc <- NLP.tokenizeByteString bs
      out <- asks aENLPOutputChannel
      liftIO $ extDoc `seq` putMVar out (Just extDoc)
      --liftIO $ putMVar out (Just extDoc)

    consuming :: Ext.Document -> AppT IO ()
    consuming doc = do
      sIDs <- mapM addSentence doc 
      liftIO . putStrLn $ (show . length $ sIDs) ++ " sentences processed"

    addSentence :: Ext.Sentence -> AppT IO Int.SentenceId
    addSentence extSentence = do
      intSentence <- mapM D.addToken extSentence
      sId <- SP.append intSentence
      -- D.getCurrentIndex >>= liftIO . putStrLn . show
      pure sId 

    close env = do
      runReaderT (runApp close') env

    close' = do
      D.close
      SP.close
      
    -- processChunk :: MVar (Maybe Ext.Document) -> T.Text -> AppT IO ()
    -- processChunk outChan chunk = do
    --   extDoc <- NLP.tokenizeByteString . TE.encodeUtf8 . T.concat $ chunk 
    --   liftIO $ writeChan outChan $ ChanContent extDoc

      -- outChan <- asks aENLPInputChannel
      -- extDoc <- NLP.tokenizeByteString . BLU.fromString . concat $ lines
      -- liftIO . (writeChan outChan) . ChanContent $ extDoc
      -- extDoc <- NLP.tokenizeByteString . BLU.fromString . concat $ lines
      -- liftIO $ writeChan outChan (ChanContent extDoc)


    -- chunkLines :: Int -> [String] -> AppT IO [[String]] 
    -- chunkLines _ [] = pure []  
    -- chunkLines n xs = do
    --   let lines = take n xs
    --   ys <- chunkLines n (drop n xs)
    --   pure $ lines : ys




-- readDirectory :: FilePath -> String -> AppT IO ()
-- readDirectory path fileEnding = do
--   sIds <- concat <$> mapM importFile files'
   
--       -- write data to channel
--       mapM_ (writeChan channel . ChanContent) xs
--       -- poison channel with number of consuming threads
--       replicateM_ nThreads $ writeChan channel Poison

    
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
-- readDirectory :: FilePath -> String -> AppT IO ()
-- readDirectory path fileEnding = do
--   files <- liftIO $ listDirectory path 
--   let files' = map ((++) path) $ filter (isSuffixOf fileEnding) files
--   sIds <- concat <$> mapM importFile files'
--   D.close
--   SP.close
--   --liftIO $ putStrLn ( (show . length $ sIds) ++ " sentences imported!" )

--   where
     
