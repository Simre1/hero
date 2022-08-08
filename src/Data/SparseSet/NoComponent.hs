module Data.SparseSet.NoComponent where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class
import Data.IORef
import Data.Vector.Unboxed (freeze)
import Data.Vector.Unboxed.Mutable qualified as VU
import Data.Word

data SparseSetNoComponent = SparseSetNoComponent
  { sparseSetSparse :: !(VU.IOVector Word32),
    sparseSetEntities :: !(IORef (VU.IOVector Word32)),
    sparseSetSize :: !(IORef Int)
  }

create :: Word32 -> Word32 -> IO (SparseSetNoComponent)
create sparseSize denseSize = do
  sparse <- VU.replicate (fromIntegral sparseSize) maxBound
  entities <- VU.new (fromIntegral denseSize) >>= newIORef
  size <- newIORef 0
  pure $ SparseSetNoComponent sparse entities size
{-# INLINE create #-}

insert :: SparseSetNoComponent -> Word32 -> IO ()
insert set@(SparseSetNoComponent sparse entitiesRef sizeRef) i = do
  index <- VU.read sparse (fromIntegral i)
  if index == maxBound
    then do
      entities <- readIORef entitiesRef
      nextIndex <- atomicModifyIORef' sizeRef (\i -> (succ i, i))
      let entitiesSize = VU.length entities
      entities <-
        if (nextIndex >= entitiesSize)
          then do
            entities <- readIORef entitiesRef
            newEntities <- VU.grow entities (entitiesSize `quot` 2)
            writeIORef entitiesRef newEntities
            pure newEntities
          else readIORef entitiesRef
      VU.write entities nextIndex i
      VU.write sparse (fromIntegral i) (fromIntegral nextIndex)
    else pure ()
{-# INLINE insert #-}

contains :: SparseSetNoComponent -> Word32 -> IO Bool
contains (SparseSetNoComponent sparse entities _) i = do
  v <- VU.read sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

size :: SparseSetNoComponent -> IO Int
size (SparseSetNoComponent _ _ sizeRef) = readIORef sizeRef
{-# INLINE size #-}

remove :: SparseSetNoComponent -> Word32 -> IO ()
remove (SparseSetNoComponent sparse entitiesRef sizeRef) i = do
  index <- VU.read sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do
      entities <- readIORef entitiesRef
      lastEntitiesIndex <- atomicModifyIORef' sizeRef (\x -> (pred x, pred x))
      lastKey <- VU.read entities lastEntitiesIndex
      VU.write entities (fromIntegral index) lastKey
      VU.write sparse (fromIntegral lastKey) index
      VU.write sparse (fromIntegral i) maxBound
{-# INLINE remove #-}

for :: MonadIO m => SparseSetNoComponent -> (Word32 -> m ()) -> m ()
for (SparseSetNoComponent _ entitiesRef sizeRef) f = do
  entities <- liftIO $ readIORef entitiesRef
  size <- liftIO $ readIORef sizeRef
  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ VU.read entities i
    f key
{-# INLINE for #-}

visualize (SparseSetNoComponent sparse entities sizeRef) = do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  freeze sparse >>= print
  putStr "Dense: "
  readIORef entities >>= freeze >>= print
