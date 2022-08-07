module Data.SparseSet.NoComponent where

import Data.IORef
import Data.Vector.Unboxed.Mutable qualified as VU
import Data.Vector.Unboxed (freeze)
import Data.Word
import Control.Monad (forM, forM_)

data SparseSetNoComponent = SparseSetNoComponent {
  sparseSetSparse :: !(VU.IOVector Word32),
  sparseSetEntities :: !(VU.IOVector Word32),
  sparseSetSize :: !(IORef Int)
}

create :: Word32 -> Word32 -> IO (SparseSetNoComponent)
create sparseSize denseSize = do
  sparse <- VU.replicate (fromIntegral sparseSize) maxBound
  entities <- VU.new (fromIntegral denseSize)
  size <- newIORef 0
  pure $ SparseSetNoComponent sparse entities size
{-# INLINE create #-}


insert :: SparseSetNoComponent -> Word32 -> IO ()
insert set@(SparseSetNoComponent sparse entities sizeRef) i = do
  index <- VU.read sparse (fromIntegral i)
  if index == maxBound
    then do
      nextIndex <- atomicModifyIORef' sizeRef (\i -> (succ i, i))
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
remove (SparseSetNoComponent sparse entities sizeRef) i = do
  index <- VU.read sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do
      lastDenseIndex <- atomicModifyIORef' sizeRef (\x -> (pred x, pred x))

      lastKey <- VU.read entities lastDenseIndex 
    
      VU.write entities (fromIntegral index) lastKey
     
      VU.write sparse (fromIntegral lastKey) index
      VU.write sparse (fromIntegral i) maxBound
{-# INLINE remove #-}


for :: SparseSetNoComponent -> (Word32 -> IO ()) -> IO ()
for (SparseSetNoComponent _ entities sizeRef) f = do
  size <- readIORef sizeRef
  forM_ [0..pred size] $ \i -> do
    w <- VU.read entities i
    f w
{-# INLINE for #-}


visualize :: SparseSetNoComponent -> IO ()
visualize (SparseSetNoComponent sparse entities sizeRef) = do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  freeze sparse >>= print
  putStr "Dense: "
  freeze entities >>= print  
