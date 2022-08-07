module Data.SparseSet.Unboxed (
  SparseSetUnboxed,
  create,
  insert,
  contains,
  lookup,
  unsafeLookup,
  size,
  remove,
  for,
  visualize
) where

import Data.IORef
import Data.Vector.Unboxing.Mutable qualified as VU
import Data.Vector.Unboxing (freeze)
import Control.Monad
import Data.Word
import Prelude hiding (lookup)
import Control.Monad.IO.Class

data SparseSetUnboxed a = SparseSetUnboxed {
  sparseSetSparse :: !(VU.IOVector Word32),
  sparseSetEntities :: !(VU.IOVector Word32),
  sparseSetDense :: !(VU.IOVector a),
  sparseSetSize :: !(IORef Int)
}

create :: VU.Unboxable a => Word32 -> Word32 -> IO (SparseSetUnboxed a)
create sparseSize denseSize = do
  sparse <- VU.replicate (fromIntegral sparseSize) maxBound
  dense <- VU.new (fromIntegral denseSize)
  entities <- VU.new (fromIntegral denseSize)
  size <- newIORef 0
  pure $ SparseSetUnboxed sparse entities dense size

insert :: (VU.Unboxable a) => SparseSetUnboxed a -> Word32 -> a -> IO ()
insert set@(SparseSetUnboxed sparse entities dense sizeRef) i a = do
  index <- VU.read sparse (fromIntegral i)
  if index == maxBound
    then do
      nextIndex <- atomicModifyIORef' sizeRef (\i -> (succ i, i))
      VU.write dense nextIndex a
      VU.write entities nextIndex i
      VU.write sparse (fromIntegral i) (fromIntegral nextIndex)
    else VU.write dense (fromIntegral index) a

contains :: VU.Unboxable a => SparseSetUnboxed a -> Word32 -> IO Bool
contains (SparseSetUnboxed sparse entities dense _) i = do
  v <- VU.read sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)

size :: SparseSetUnboxed a -> IO Int
size (SparseSetUnboxed _ entities _ sizeRef) = readIORef sizeRef

lookup :: VU.Unboxable a => SparseSetUnboxed a -> Word32 -> IO (Maybe a)
lookup (SparseSetUnboxed sparse entities dense _) i = do
  index <- VU.read sparse (fromIntegral i)
  if index == maxBound
    then pure Nothing
    else Just <$> VU.read dense (fromIntegral index)

unsafeLookup :: VU.Unboxable a => SparseSetUnboxed a -> Word32 -> IO a
unsafeLookup (SparseSetUnboxed sparse entities dense _) i = do
  index <- VU.read sparse (fromIntegral i)
  VU.read dense (fromIntegral index)

remove :: VU.Unboxable a => SparseSetUnboxed a -> Word32 -> IO ()
remove (SparseSetUnboxed sparse entities dense sizeRef) i = do
  index <- VU.read sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do
      lastDenseIndex <- atomicModifyIORef' sizeRef (\x -> (pred x, pred x))

      lastElement <- VU.read dense lastDenseIndex
      lastKey <- VU.read entities lastDenseIndex 
    
      VU.write dense (fromIntegral index) lastElement
      VU.write entities (fromIntegral index) lastKey
     
      VU.write sparse (fromIntegral lastKey) index
      VU.write sparse (fromIntegral i) maxBound

for :: (MonadIO m, VU.Unboxable a) => SparseSetUnboxed a -> (Word32 -> a -> m ()) -> m ()
for (SparseSetUnboxed _ entities dense sizeRef) f = do
  size <- liftIO $ readIORef sizeRef
  forM_ [0..pred size] $ \i -> do
    key <- liftIO $ VU.read entities i
    comp <- liftIO $ VU.read dense i
    f key comp
  
visualize (SparseSetUnboxed sparse entities _ sizeRef) = do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  freeze sparse >>= print
  putStr "Dense: "
  freeze entities >>= print  
