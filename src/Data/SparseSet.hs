module Data.SparseSet where

import Data.IORef
import Data.Vector.Unboxed.Mutable qualified as VU
import Data.Word

data SparseSet a = SparseSet {
  sparseSetSparse :: !(VU.IOVector Word32),
  sparseSetEntities :: !(VU.IOVector Word32),
  sparseSetDense :: !(VU.IOVector a),
  sparseSetSize :: !(IORef Int)
}

create :: VU.Unbox a => Int -> Int -> IO (SparseSet a)
create sparseSize denseSize = do
  sparse <- VU.replicate sparseSize maxBound
  dense <- VU.new denseSize
  entities <- VU.new denseSize
  size <- newIORef 0
  pure $ SparseSet sparse entities dense size

insert :: (VU.Unbox a) => SparseSet a -> Word32 -> a -> IO ()
insert set@(SparseSet sparse entities dense sizeRef) i a = do
  index <- VU.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then do
      nextIndex <- atomicModifyIORef' sizeRef (\i -> (succ i, i))
      VU.unsafeWrite dense nextIndex a
      VU.unsafeWrite entities nextIndex i
      VU.unsafeWrite sparse (fromIntegral i) (fromIntegral nextIndex)
    else VU.unsafeWrite dense (fromIntegral index) a

contains :: VU.Unbox a => SparseSet a -> Word32 -> IO Bool
contains (SparseSet sparse entities dense _) i = do
  v <- VU.unsafeRead sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)

size :: SparseSet a -> IO Int
size (SparseSet _ entities _ sizeRef) = readIORef sizeRef

lookup :: VU.Unbox a => SparseSet a -> Word32 -> IO (Maybe a)
lookup (SparseSet sparse entities dense _) i = do
  index <- VU.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then pure Nothing
    else Just <$> VU.unsafeRead dense (fromIntegral index)

remove :: VU.Unbox a => SparseSet a -> Word32 -> IO ()
remove (SparseSet sparse entities dense sizeRef) i = do
  index <- VU.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do
      lastDenseIndex <- atomicModifyIORef' sizeRef (\x -> (pred x, x))

      lastElement <- VU.unsafeRead dense lastDenseIndex
      lastKey <- VU.unsafeRead entities lastDenseIndex

      VU.unsafeWrite dense (fromIntegral index) lastElement
      VU.unsafeWrite entities (fromIntegral index) lastKey

      
      VU.unsafeWrite sparse (fromIntegral lastKey) index
      VU.unsafeWrite sparse (fromIntegral i) maxBound
     
