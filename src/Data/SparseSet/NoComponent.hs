module Data.SparseSet.NoComponent where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class
import Data.IORef
import Data.Vector.Primitive (freeze)
import Data.Vector.Primitive.Mutable qualified as V
import Data.Word

data SparseSetNoComponent = SparseSetNoComponent
  { sparseSetSparse :: {-# UNPACK #-} !(V.IOVector Word32),
    sparseSetEntities :: {-# UNPACK #-} !(IORef (V.IOVector Word32)),
    sparseSetSize :: {-# UNPACK #-} !(IORef Int)
  }

create :: Word32 -> Word32 -> IO (SparseSetNoComponent)
create sparseSize denseSize = do
  sparse <- V.replicate (fromIntegral sparseSize) maxBound
  entities <- V.new (fromIntegral denseSize) >>= newIORef
  size <- newIORef 0
  pure $ SparseSetNoComponent sparse entities size
{-# INLINE create #-}

insert :: SparseSetNoComponent -> Word32 -> IO ()
insert set@(SparseSetNoComponent sparse entitiesRef sizeRef) i = do
  index <- V.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then do
      entities <- readIORef entitiesRef
      nextIndex <- atomicModifyIORef' sizeRef (\i -> (succ i, i))
      let entitiesSize = V.length entities
      entities <-
        if (nextIndex >= entitiesSize)
          then do
            entities <- readIORef entitiesRef
            newEntities <- V.unsafeGrow entities (entitiesSize `quot` 2)
            writeIORef entitiesRef newEntities
            pure newEntities
          else readIORef entitiesRef
      V.unsafeWrite entities nextIndex i
      V.unsafeWrite sparse (fromIntegral i) (fromIntegral nextIndex)
    else pure ()
{-# INLINE insert #-}

contains :: SparseSetNoComponent -> Word32 -> IO Bool
contains (SparseSetNoComponent sparse entities _) i = do
  v <- V.unsafeRead sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

size :: SparseSetNoComponent -> IO Int
size (SparseSetNoComponent _ _ sizeRef) = readIORef sizeRef
{-# INLINE size #-}

remove :: SparseSetNoComponent -> Word32 -> IO ()
remove (SparseSetNoComponent sparse entitiesRef sizeRef) i = do
  index <- V.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do
      entities <- readIORef entitiesRef
      lastEntitiesIndex <- atomicModifyIORef' sizeRef (\x -> (pred x, pred x))
      lastKey <- V.unsafeRead entities lastEntitiesIndex
      V.unsafeWrite entities (fromIntegral index) lastKey
      V.unsafeWrite sparse (fromIntegral lastKey) index
      V.unsafeWrite sparse (fromIntegral i) maxBound
{-# INLINE remove #-}

for :: MonadIO m => SparseSetNoComponent -> (Word32 -> m ()) -> m ()
for (SparseSetNoComponent _ entitiesRef sizeRef) f = do
  entities <- liftIO $ readIORef entitiesRef
  size <- liftIO $ readIORef sizeRef
  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ V.unsafeRead entities i
    f key
{-# INLINE for #-}

visualize (SparseSetNoComponent sparse entities sizeRef) = do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  freeze sparse >>= print
  putStr "Dense: "
  readIORef entities >>= freeze >>= print
