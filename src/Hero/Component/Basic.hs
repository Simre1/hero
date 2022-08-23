module Hero.Component.Basic where

import Control.Arrow ((>>>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Default (Default (def))
import Data.IORef
import Foreign (Storable (..), castPtr, plusPtr)
import GHC.Clock
import Hero.Component.Component (Component (..))
import Hero.Component.Store.Global (Global, addGlobal, putGlobal)
import Hero.Component.Store.SparseSet (StorableSparseSet, storableSparseSet)
import Hero.System
import Hero.System.System (System)

-- | Component for 2D positioning which uses sparse sets for storage
data Position2D = Position2D {-# UNPACK #-} !Float {-# UNPACK #-} !Float deriving (Show, Eq, Ord)

instance Storable Position2D where
  sizeOf _ = 2 * sizeOf (undefined :: Float)
  alignment _ = 1
  peek ptr = do
    let floatPtr = castPtr ptr
    Position2D <$> peek floatPtr <*> peek (floatPtr `plusPtr` floatSize)
    where
      floatSize = sizeOf (undefined :: Float)
  poke ptr (Position2D x y) = do
    let floatPtr = castPtr ptr
    poke floatPtr x
    poke (floatPtr `plusPtr` floatSize) y
    where
      floatSize = sizeOf (undefined :: Float)

instance Component Position2D where
  type Store Position2D = StorableSparseSet

instance Num Position2D where
  (Position2D x1 y1) + (Position2D x2 y2) = Position2D (x1 + x2) (y1 + y2)
  (Position2D x1 y1) * (Position2D x2 y2) = Position2D (x1 * x2) (y1 * y2)
  abs (Position2D x y) = Position2D (abs x) (abs y)
  signum (Position2D x y) = Position2D (signum x) (signum y)
  fromInteger x = let x' = fromIntegral x in Position2D x' x'
  negate (Position2D x y) = Position2D (negate x) (negate y)

-- | Component for 2D rotation in radian which uses sparse sets for storage
newtype Rotation2D = Rotation2D Float deriving (Show, Eq, Ord, Storable)

instance Component Rotation2D where
  type Store Rotation2D = StorableSparseSet

-- | Global component which has the delta time
newtype TimeDelta = TimeDelta Float

instance Component TimeDelta where
  type Store TimeDelta = Global

-- | Sets up a `TimeDelta` and updates it.
addTimeDelta :: MonadIO m => System m i i
addTimeDelta =
  forward $
    addGlobal (TimeDelta 0)
      >>> withSetup
        (\_ -> newIORef 1e1000)
        ( \ref -> liftSystem $ \_ -> liftIO $ do
            currentTime <- getMonotonicTime
            delta <- atomicModifyIORef' ref (\lastTime -> (currentTime, currentTime - lastTime))
            pure $ TimeDelta (max 0 $ realToFrac delta)
        )
      >>> putGlobal @TimeDelta

-- | Global component which has the delta time
newtype Timer = Timer Float

instance Component Timer where
  type Store Timer = Global

-- | Sets up a `Timer` and updates it.
addTimer :: MonadIO m => System m i i
addTimer =
  forward $
    addGlobal (Timer 0)
      >>> withSetup
        (\_ -> getMonotonicTime >>= newIORef)
        ( \ref -> liftSystem $ \_ -> liftIO $ do
            startTime <- readIORef ref
            currentTime <- getMonotonicTime
            pure $ Timer (realToFrac $ currentTime - startTime)
        )
      >>> putGlobal @Timer

-- | Sets up a `Timer` and `TimeDelta` and updates them.
addTimingComponents :: MonadIO m => System m i i
addTimingComponents = addTimeDelta >>> addTimer