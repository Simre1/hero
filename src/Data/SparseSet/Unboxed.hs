module Data.SparseSet.Unboxed
  ( SparseSetUnboxed,
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
import Data.Vector.Unboxing (freeze)
import Data.Vector.Unboxing.Mutable qualified as VU
import Data.Word
import Prelude hiding (lookup)

data SparseSetUnboxed a = SparseSetUnboxed
  { sparseSetSparse :: {-# UNPACK #-} !(VU.IOVector Word32),
    sparseSetEntities :: {-# UNPACK #-} !(IORef (VU.IOVector Word32)),
    sparseSetDense :: {-# UNPACK #-} !(IORef (VU.IOVector a)),
    sparseSetSize :: {-# UNPACK #-} !(IORef Int)
  }

create :: VU.Unboxable a => Word32 -> Word32 -> IO (SparseSetUnboxed a)
create sparseSize denseSize = do
  !sparse <- VU.replicate (fromIntegral sparseSize) maxBound
  !dense <- VU.new (fromIntegral denseSize) >>= newIORef
  !entities <- VU.new (fromIntegral denseSize) >>= newIORef
  !size <- newIORef 0
  pure $ SparseSetUnboxed sparse entities dense size
{-# INLINE create #-}

insert :: (VU.Unboxable a) => SparseSetUnboxed a -> Word32 -> a -> IO ()
insert set@(SparseSetUnboxed sparse entitiesRef denseRef sizeRef) i a = do
  index <- VU.unsafeRead sparse (fromIntegral i)
  dense <- readIORef denseRef
  if index /= maxBound
    then VU.unsafeWrite dense (fromIntegral index) a
    else do
      nextIndex <- atomicModifyIORef' sizeRef (\i -> (succ i, i))
      let denseSize = VU.length dense
      (dense, entities) <-
        if (nextIndex >= denseSize)
          then do
            dense <- readIORef denseRef
            newDense <- VU.unsafeGrow dense (denseSize `quot` 2)
            writeIORef denseRef newDense
            entities <- readIORef entitiesRef
            newEntities <- VU.unsafeGrow entities (denseSize `quot` 2)
            writeIORef entitiesRef newEntities
            pure (newDense, newEntities)
          else (,) <$> readIORef denseRef <*> readIORef entitiesRef
      VU.unsafeWrite dense nextIndex a
      VU.unsafeWrite entities nextIndex i
      VU.unsafeWrite sparse (fromIntegral i) (fromIntegral nextIndex)
{-# INLINE insert #-}

contains :: VU.Unboxable a => SparseSetUnboxed a -> Word32 -> IO Bool
contains (SparseSetUnboxed sparse entities dense _) i = do
  v <- VU.unsafeRead sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

size :: SparseSetUnboxed a -> IO Int
size (SparseSetUnboxed _ entities _ sizeRef) = readIORef sizeRef
{-# INLINE size #-}

lookup :: VU.Unboxable a => SparseSetUnboxed a -> Word32 -> IO (Maybe a)
lookup (SparseSetUnboxed sparse _ denseRef _) i = do
  index <- VU.unsafeRead sparse (fromIntegral i)
  if index /= maxBound
    then readIORef denseRef >>= \dense -> Just <$> VU.unsafeRead dense (fromIntegral index)
    else pure Nothing
{-# INLINE lookup #-}

unsafeLookup :: VU.Unboxable a => SparseSetUnboxed a -> Word32 -> IO a
unsafeLookup (SparseSetUnboxed sparse _ denseRef _) i = do
  index <- VU.unsafeRead sparse (fromIntegral i)
  readIORef denseRef >>= \dense -> VU.unsafeRead dense (fromIntegral index)
{-# INLINE unsafeLookup #-}

remove :: VU.Unboxable a => SparseSetUnboxed a -> Word32 -> IO ()
remove (SparseSetUnboxed sparse entitiesRef denseRef sizeRef) i = do
  index <- VU.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do
      dense <- readIORef denseRef
      entities <- readIORef entitiesRef
      lastDenseIndex <- atomicModifyIORef' sizeRef (\x -> (pred x, pred x))

      lastElement <- VU.unsafeRead dense lastDenseIndex
      lastKey <- VU.unsafeRead entities lastDenseIndex

      VU.unsafeWrite dense (fromIntegral index) lastElement
      VU.unsafeWrite entities (fromIntegral index) lastKey

      VU.unsafeWrite sparse (fromIntegral lastKey) index
      VU.unsafeWrite sparse (fromIntegral i) maxBound
{-# INLINE remove #-}

for :: (MonadIO m, VU.Unboxable a) => SparseSetUnboxed a -> (Word32 -> a -> m ()) -> m ()
for (SparseSetUnboxed _ entitiesRef denseRef sizeRef) f = do
  entities <- liftIO $ readIORef entitiesRef
  dense <- liftIO $ readIORef denseRef

  size <- liftIO $ readIORef sizeRef
  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ VU.unsafeRead entities i
    val <- liftIO $ VU.unsafeRead dense i
    f key val
{-# INLINE for #-}

visualize (SparseSetUnboxed sparse entities _ sizeRef) = do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  freeze sparse >>= print
  putStr "Dense: "
  readIORef entities >>= freeze >>= print
