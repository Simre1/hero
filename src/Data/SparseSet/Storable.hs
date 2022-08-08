module Data.SparseSet.Storable
  ( SparseSetStorable,
    create,
    insert,
    contains,
    lookup,
    unsafeLookup,
    size,
    remove,
    for,
    visualize,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Vector.Storable (freeze)
import Data.Vector.Storable.Mutable qualified as V
import Data.Word
import Prelude hiding (lookup)

data SparseSetStorable a = SparseSetStorable
  { sparseSetSparse :: {-# UNPACK #-} !(V.IOVector Word32),
    sparseSetEntities :: {-# UNPACK #-} !(IORef (V.IOVector Word32)),
    sparseSetDense :: {-# UNPACK #-} !(IORef (V.IOVector a)),
    sparseSetSize :: {-# UNPACK #-} !(IORef Int)
  }

create :: V.Storable a => Word32 -> Word32 -> IO (SparseSetStorable a)
create sparseSize denseSize = do
  !sparse <- V.replicate (fromIntegral sparseSize) maxBound
  !dense <- V.new (fromIntegral denseSize) >>= newIORef
  !entities <- V.new (fromIntegral denseSize) >>= newIORef
  !size <- newIORef 0
  pure $ SparseSetStorable sparse entities dense size
{-# INLINE create #-}

insert :: (V.Storable a) => SparseSetStorable a -> Word32 -> a -> IO ()
insert set@(SparseSetStorable sparse entitiesRef denseRef sizeRef) i a = do
  index <- V.unsafeRead sparse (fromIntegral i)
  dense <- readIORef denseRef
  if index /= maxBound
    then V.unsafeWrite dense (fromIntegral index) a
    else do
      nextIndex <- atomicModifyIORef' sizeRef (\i -> (succ i, i))
      let denseSize = V.length dense
      (dense, entities) <-
        if (nextIndex >= denseSize)
          then do
            dense <- readIORef denseRef
            newDense <- V.unsafeGrow dense (denseSize `quot` 2)
            writeIORef denseRef newDense
            entities <- readIORef entitiesRef
            newEntities <- V.unsafeGrow entities (denseSize `quot` 2)
            writeIORef entitiesRef newEntities
            pure (newDense, newEntities)
          else (,) <$> readIORef denseRef <*> readIORef entitiesRef
      V.unsafeWrite dense nextIndex a
      V.unsafeWrite entities nextIndex i
      V.unsafeWrite sparse (fromIntegral i) (fromIntegral nextIndex)
{-# INLINE insert #-}

contains :: V.Storable a => SparseSetStorable a -> Word32 -> IO Bool
contains (SparseSetStorable sparse entities dense _) i = do
  v <- V.unsafeRead sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

size :: SparseSetStorable a -> IO Int
size (SparseSetStorable _ entities _ sizeRef) = readIORef sizeRef
{-# INLINE size #-}

lookup :: V.Storable a => SparseSetStorable a -> Word32 -> IO (Maybe a)
lookup (SparseSetStorable sparse _ denseRef _) i = do
  index <- V.unsafeRead sparse (fromIntegral i)
  if index /= maxBound
    then readIORef denseRef >>= \dense -> Just <$> V.unsafeRead dense (fromIntegral index)
    else pure Nothing
{-# INLINE lookup #-}

unsafeLookup :: V.Storable a => SparseSetStorable a -> Word32 -> IO a
unsafeLookup (SparseSetStorable sparse _ denseRef _) i = do
  index <- V.unsafeRead sparse (fromIntegral i)
  readIORef denseRef >>= \dense -> V.unsafeRead dense (fromIntegral index)
{-# INLINE unsafeLookup #-}

remove :: V.Storable a => SparseSetStorable a -> Word32 -> IO ()
remove (SparseSetStorable sparse entitiesRef denseRef sizeRef) i = do
  index <- V.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do
      dense <- readIORef denseRef
      entities <- readIORef entitiesRef
      lastDenseIndex <- atomicModifyIORef' sizeRef (\x -> (pred x, pred x))

      lastElement <- V.unsafeRead dense lastDenseIndex
      lastKey <- V.unsafeRead entities lastDenseIndex

      V.unsafeWrite dense (fromIntegral index) lastElement
      V.unsafeWrite entities (fromIntegral index) lastKey

      V.unsafeWrite sparse (fromIntegral lastKey) index
      V.unsafeWrite sparse (fromIntegral i) maxBound
{-# INLINE remove #-}

for :: (MonadIO m, V.Storable a) => SparseSetStorable a -> (Word32 -> a -> m ()) -> m ()
for (SparseSetStorable _ entitiesRef denseRef sizeRef) f = do
  entities <- liftIO $ readIORef entitiesRef
  dense <- liftIO $ readIORef denseRef

  size <- liftIO $ readIORef sizeRef
  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ V.unsafeRead entities i
    val <- liftIO $ V.unsafeRead dense i
    f key val
{-# INLINE for #-}

visualize (SparseSetStorable sparse entities _ sizeRef) = do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  freeze sparse >>= print
  putStr "Dense: "
  readIORef entities >>= freeze >>= print
