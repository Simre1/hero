module Hero.Component.Basic (Position2D (..)) where

import Foreign (Storable (..), castPtr, plusPtr)
import Hero.Component (Component (Store))
import Hero.Component.Store.SparseSet (StorableSparseSet)
import Hero.Component.Store.Global

data Position2D = Position2D {-# UNPACK #-} !Float {-# UNPACK #-} !Float deriving (Show, Eq, Ord)

instance Storable Position2D where
  sizeOf _ = 2 * sizeOf (undefined :: Float)
  alignment _ = 1
  peek ptr = do
    let floatPtr = castPtr ptr
    Position2D <$> peek floatPtr <*> peek (floatPtr `plusPtr` floatSize)
  poke ptr (Position2D x y) = do
    let floatPtr = castPtr ptr
    poke floatPtr x
    poke (floatPtr `plusPtr` floatSize) y

floatSize :: Int
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

newtype TimeDelta = TimeDelta Float
