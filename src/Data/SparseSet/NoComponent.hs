module Data.SparseSet.NoComponent
  ( SparseSetNoComponent,
    create,
    insert,
    contains,
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
import Data.Word
import Prelude hiding (lookup)

data SparseSetNoComponent = SparseSetNoComponent
  { sparseSetSparse :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetEntities :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetSize :: {-# UNPACK #-} !(IORef Int)
  }

create :: Word32 -> Word32 -> IO SparseSetNoComponent
create sparseSize denseSize = do
  !sparse <- VPM.replicate (fromIntegral sparseSize) maxBound
  !entities <- VPM.new (fromIntegral denseSize)
  let !size = 0
  SparseSetNoComponent sparse entities <$> newIORef size
{-# INLINE create #-}


insert :: SparseSetNoComponent -> Word32 -> IO ()
insert (SparseSetNoComponent sparse entities sizeRef) i = do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index /= maxBound
    then pure ()
    else do
      nextIndex <- atomicModifyIORef' sizeRef (\size -> (succ size, size))
      VPM.unsafeWrite entities nextIndex i
      VPM.unsafeWrite sparse (fromIntegral i) (fromIntegral nextIndex)
{-# INLINE insert #-}

contains :: SparseSetNoComponent -> Word32 -> IO Bool
contains (SparseSetNoComponent sparse _ _) i = do
  v <- VPM.unsafeRead sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

size :: SparseSetNoComponent -> IO Int
size (SparseSetNoComponent _ _ sizeRef) = readIORef sizeRef
{-# INLINE size #-}

remove :: SparseSetNoComponent -> Word32 -> IO ()
remove (SparseSetNoComponent sparse entities sizeRef) i = do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do
      lastDenseIndex <- atomicModifyIORef sizeRef $ \size -> (pred size, pred size)

      lastKey <- VPM.unsafeRead entities lastDenseIndex

      VPM.unsafeWrite entities (fromIntegral index) lastKey

      VPM.unsafeWrite sparse (fromIntegral lastKey) index
      VPM.unsafeWrite sparse (fromIntegral i) maxBound
{-# INLINE remove #-}

for :: (MonadIO m) => SparseSetNoComponent -> (Word32 -> m ()) -> m ()
for (SparseSetNoComponent _ entities sizeRef) f = do
  size <- liftIO $ readIORef sizeRef
  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ VPM.unsafeRead entities i
    f key
{-# INLINE for #-}

visualize :: SparseSetNoComponent -> IO ()
visualize (SparseSetNoComponent sparse entities sizeRef) = do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  VP.freeze sparse >>= print
  putStr "Dense: "
  VP.freeze entities >>= print
