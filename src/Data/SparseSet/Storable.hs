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
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Data.Word
import Prelude hiding (lookup)

data SparseSetStorable a = SparseSetStorable
  { sparseSetSparse :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetEntities :: {-# UNPACK #-} !(IORef (VPM.IOVector Word32)),
    sparseSetDense :: {-# UNPACK #-} !(IORef (VM.IOVector a)),
    sparseSetSize :: {-# UNPACK #-} !(IORef Int)
  }

create :: VM.Storable a => Word32 -> Word32 -> IO (SparseSetStorable a)
create sparseSize denseSize = do
  !sparse <- VPM.replicate (fromIntegral sparseSize) maxBound
  !dense <- VM.new (fromIntegral denseSize) >>= newIORef
  !entities <- VPM.new (fromIntegral denseSize) >>= newIORef
  !size <- newIORef 0
  pure $ SparseSetStorable sparse entities dense size
{-# INLINE create #-}

insert :: (VM.Storable a) => SparseSetStorable a -> Word32 -> a -> IO ()
insert set@(SparseSetStorable sparse entitiesRef denseRef sizeRef) i a = do
  
  index <- VPM.unsafeRead sparse (fromIntegral i)
  dense <- readIORef denseRef
  if index /= maxBound
    then VM.unsafeWrite dense (fromIntegral index) a
    else do
      nextIndex <- atomicModifyIORef' sizeRef (\i -> (succ i, i))
      let denseSize = VM.length dense
      (dense, entities) <-
        if (nextIndex >= denseSize)
          then do
            dense <- readIORef denseRef
            newDense <- VM.unsafeGrow dense (denseSize `quot` 2)
            writeIORef denseRef newDense
            entities <- readIORef entitiesRef
            newEntities <- VPM.unsafeGrow entities (denseSize `quot` 2)
            writeIORef entitiesRef newEntities
            pure (newDense, newEntities)
          else (,) <$> readIORef denseRef <*> readIORef entitiesRef
      VM.unsafeWrite dense nextIndex a
      VPM.unsafeWrite entities nextIndex i
      VPM.unsafeWrite sparse (fromIntegral i) (fromIntegral nextIndex)
{-# INLINE insert #-}

contains :: VM.Storable a => SparseSetStorable a -> Word32 -> IO Bool
contains (SparseSetStorable sparse entities dense _) i = do
  v <- VPM.unsafeRead sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

size :: SparseSetStorable a -> IO Int
size (SparseSetStorable _ entities _ sizeRef) = readIORef sizeRef
{-# INLINE size #-}

lookup :: VM.Storable a => SparseSetStorable a -> Word32 -> IO (Maybe a)
lookup (SparseSetStorable sparse _ denseRef _) i = do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index /= maxBound
    then readIORef denseRef >>= \dense -> Just <$> VM.unsafeRead dense (fromIntegral index)
    else pure Nothing
{-# INLINE lookup #-}

unsafeLookup :: VM.Storable a => SparseSetStorable a -> Word32 -> IO a
unsafeLookup (SparseSetStorable sparse _ denseRef _) i = do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  readIORef denseRef >>= \dense -> VM.unsafeRead dense (fromIntegral index)
{-# INLINE unsafeLookup #-}

remove :: VM.Storable a => SparseSetStorable a -> Word32 -> IO ()
remove (SparseSetStorable sparse entitiesRef denseRef sizeRef) i = do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do
      dense <- readIORef denseRef
      entities <- readIORef entitiesRef
      lastDenseIndex <- atomicModifyIORef' sizeRef (\x -> (pred x, pred x))

      lastElement <- VM.unsafeRead dense lastDenseIndex
      lastKey <- VPM.unsafeRead entities lastDenseIndex

      VM.unsafeWrite dense (fromIntegral index) lastElement
      VPM.unsafeWrite entities (fromIntegral index) lastKey

      VPM.unsafeWrite sparse (fromIntegral lastKey) index
      VPM.unsafeWrite sparse (fromIntegral i) maxBound
{-# INLINE remove #-}

for :: (MonadIO m, VM.Storable a) => SparseSetStorable a -> (Word32 -> a -> m ()) -> m ()
for (SparseSetStorable _ entitiesRef denseRef sizeRef) f = do
  entities <- liftIO $ readIORef entitiesRef
  dense <- liftIO $ readIORef denseRef

  size <- liftIO $ readIORef sizeRef
  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ VPM.unsafeRead entities i
    val <- liftIO $ VM.unsafeRead dense i

    f key val
{-# INLINE for #-}

visualize :: SparseSetStorable a -> IO ()
visualize (SparseSetStorable sparse entities _ sizeRef) = do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  VP.freeze sparse >>= print
  putStr "Dense: "
  readIORef entities >>= VP.freeze >>= print
