module InvertedIndex
  ( InvertedIndex (..)
  , HasInvertedIndex
  , put
  , get
  ) where

import qualified Internal.Types as Int
import Types
import Control.Concurrent.MVar
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.IO.Class

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS 

import System.IO

-- the principal dataStructure of an InvertedIndex
-- denoting an Int.Token to a Set of SentenceIds
type InvertedIndex = IM.IntMap (IS.IntSet) 

class (MonadIO m, Monad m) => HasInvertedIndex m where
  getInvertedIndex :: m InvertedIndex
  setInvertedIndex :: InvertedIndex -> m ()
  getMutex :: m (MVar ())

instance MonadIO m => HasInvertedIndex (AppT m) where
  getInvertedIndex = asks (iiEInvertedIndex . aEInvertedIndexEnv)
    >>= liftIO . readMVar
  setInvertedIndex ii = asks (iiEInvertedIndex . aEInvertedIndexEnv) 
    >>= liftIO . flip swapMVar ii >> pure ()
  getMutex = asks (iiEMutex . aEInvertedIndexEnv) 

put
  :: (HasInvertedIndex m, MonadAsyncException m)
  => Int.SentenceId
  -> Int.Sentence
  -> m () 
put sId ss = do
  mutex <- getMutex
  withMutex mutex (go (fromIntegral sId) ss)

  where
    go sId ss = do
      ii <- getInvertedIndex
      let ii' = foldl (foldII sId) ii ss 
      setInvertedIndex ii'

    foldII :: Int -> InvertedIndex -> Int.Token -> InvertedIndex
    foldII sId ii t = IM.alter (alterFn sId) (fromIntegral t) ii 

    alterFn sId (Just xs) = pure $ IS.insert sId xs
    alterFn sId Nothing = pure $ IS.singleton sId

get
  :: HasInvertedIndex m
  => Int.Token
  -> m IS.IntSet 
get t = getInvertedIndex >>= (\ii -> pure $ ii IM.! (fromIntegral t))
