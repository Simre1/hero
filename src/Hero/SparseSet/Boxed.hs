module Hero.SparseSet.Boxed
  ( SparseSetBoxed,
    create,
    insert,
    contains,
    lookup,
    unsafeLookup,
    size,
    remove,
    for,
    visualize,
    growDense
  )
where

import Control.Monad (forM_ )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.IORef
    ( IORef,
      atomicModifyIORef,
      atomicModifyIORef',
      newIORef,
      readIORef )
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Word ( Word32 )
import Prelude hiding (lookup)

data SparseSetBoxed a = SparseSetBoxed
  { sparseSetSparse :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetEntities :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetDense :: {-# UNPACK #-} !(VM.IOVector a),
    sparseSetSize :: {-# UNPACK #-} !(IORef Int)
  }

create :: Word32 -> Word32 -> IO (SparseSetBoxed a)
create sparseSize denseSize = do
  !sparse <- VPM.replicate (fromIntegral sparseSize) maxBound
  !dense <- VM.new (fromIntegral denseSize)
  !entities <- VPM.new (fromIntegral denseSize)
  let !size = 0
  SparseSetBoxed sparse entities dense <$> newIORef size
{-# INLINE create #-}

insert :: SparseSetBoxed a -> Word32 -> a -> IO ()
insert (SparseSetBoxed sparse entities dense sizeRef) i a = do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index /= maxBound
    then VM.unsafeWrite dense (fromIntegral index) a
    else do
      nextIndex <- atomicModifyIORef' sizeRef (\size -> (succ size, size))
      let denseSize = VM.length dense
      VM.unsafeWrite dense nextIndex a
      VPM.unsafeWrite entities nextIndex i
      VPM.unsafeWrite sparse (fromIntegral i) (fromIntegral nextIndex)
{-# INLINE insert #-}

contains :: SparseSetBoxed a -> Word32 -> IO Bool
contains (SparseSetBoxed sparse _ _ _) i = do
  v <- VPM.unsafeRead sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

size :: SparseSetBoxed a -> IO Int
size (SparseSetBoxed _ _ _ sizeRef) = readIORef sizeRef
{-# INLINE size #-}

lookup :: SparseSetBoxed a -> Word32 -> IO (Maybe a)
lookup (SparseSetBoxed sparse _ dense _) i = do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index /= maxBound
    then Just <$> VM.unsafeRead dense (fromIntegral index)
    else pure Nothing
{-# INLINE lookup #-}

unsafeLookup :: SparseSetBoxed a -> Word32 -> IO a
unsafeLookup (SparseSetBoxed sparse _ dense _) i = do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  VM.unsafeRead dense (fromIntegral index)
{-# INLINE unsafeLookup #-}

remove :: SparseSetBoxed a -> Word32 -> IO ()
remove (SparseSetBoxed sparse entities dense sizeRef) i = do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do
      lastDenseIndex <- atomicModifyIORef sizeRef $ \size -> (pred size, pred size)

      lastElement <- VM.unsafeRead dense lastDenseIndex
      lastKey <- VPM.unsafeRead entities lastDenseIndex

      VM.unsafeWrite dense (fromIntegral index) lastElement
      VPM.unsafeWrite entities (fromIntegral index) lastKey

      VPM.unsafeWrite sparse (fromIntegral lastKey) index
      VPM.unsafeWrite sparse (fromIntegral i) maxBound
{-# INLINE remove #-}

for :: (MonadIO m) => SparseSetBoxed a -> (Word32 -> a -> m ()) -> m ()
for (SparseSetBoxed _ entities dense sizeRef) f = do
  size <- liftIO $ readIORef sizeRef

  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ VPM.unsafeRead entities i
    val <- liftIO $ VM.unsafeRead dense i

    f key val
{-# INLINE for #-}

growDense :: SparseSetBoxed a -> IO (SparseSetBoxed a)
growDense (SparseSetBoxed sparse entities dense sizeRef) = do
  let entitySize = VPM.length entities
  newDense <- VM.unsafeGrow dense (entitySize `quot` 2)
  newEntities <- VPM.unsafeGrow entities (entitySize `quot` 2)
  pure $ SparseSetBoxed sparse newEntities newDense sizeRef

visualize :: SparseSetBoxed a -> IO ()
visualize (SparseSetBoxed sparse entities sense sizeRef) = do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  VP.freeze sparse >>= print
  putStr "Dense: "
  VP.freeze entities >>= print
