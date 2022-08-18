module Hero.SparseSet.Unboxed
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
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as VM
import Data.Word ( Word32 )
import Prelude hiding (lookup)

data SparseSetUnboxed a = SparseSetUnboxed
  { sparseSetSparse :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetEntities :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetDense :: {-# UNPACK #-} !(VM.IOVector a),
    sparseSetSize :: {-# UNPACK #-} !(IORef Int)
  }

create :: VM.Unbox a => Word32 -> Word32 -> IO (SparseSetUnboxed a)
create sparseSize denseSize = do
  !sparse <- VPM.replicate (fromIntegral sparseSize) maxBound
  !dense <- VM.new (fromIntegral denseSize)
  !entities <- VPM.new (fromIntegral denseSize)
  let !size = 0
  SparseSetUnboxed sparse entities dense <$> newIORef size
{-# INLINE create #-}

insert :: (VM.Unbox a) => SparseSetUnboxed a -> Word32 -> a -> IO ()
insert (SparseSetUnboxed sparse entities dense sizeRef) i a = do
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

contains :: VM.Unbox a => SparseSetUnboxed a -> Word32 -> IO Bool
contains (SparseSetUnboxed sparse _ _ _) i = do
  v <- VPM.unsafeRead sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

size :: SparseSetUnboxed a -> IO Int
size (SparseSetUnboxed _ _ _ sizeRef) = readIORef sizeRef
{-# INLINE size #-}

lookup :: VM.Unbox a => SparseSetUnboxed a -> Word32 -> IO (Maybe a)
lookup (SparseSetUnboxed sparse _ dense _) i = do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index /= maxBound
    then Just <$> VM.unsafeRead dense (fromIntegral index)
    else pure Nothing
{-# INLINE lookup #-}

unsafeLookup :: VM.Unbox a => SparseSetUnboxed a -> Word32 -> IO a
unsafeLookup (SparseSetUnboxed sparse _ dense _) i = do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  VM.unsafeRead dense (fromIntegral index)
{-# INLINE unsafeLookup #-}

remove :: VM.Unbox a => SparseSetUnboxed a -> Word32 -> IO ()
remove (SparseSetUnboxed sparse entities dense sizeRef) i = do
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

for :: (MonadIO m, VM.Unbox a) => SparseSetUnboxed a -> (Word32 -> a -> m ()) -> m ()
for (SparseSetUnboxed _ entities dense sizeRef) f = do
  size <- liftIO $ readIORef sizeRef

  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ VPM.unsafeRead entities i
    val <- liftIO $ VM.unsafeRead dense i

    f key val
{-# INLINE for #-}

growDense :: VM.Unbox a => SparseSetUnboxed a -> IO (SparseSetUnboxed a)
growDense (SparseSetUnboxed sparse entities dense sizeRef) = do
  let entitySize = VPM.length entities
  newDense <- VM.unsafeGrow dense (entitySize `quot` 2)
  newEntities <- VPM.unsafeGrow entities (entitySize `quot` 2)
  pure $ SparseSetUnboxed sparse newEntities newDense sizeRef

visualize :: SparseSetUnboxed a -> IO ()
visualize (SparseSetUnboxed sparse entities sense sizeRef) = do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  VP.freeze sparse >>= print
  putStr "Dense: "
  VP.freeze entities >>= print
